require(readxl) # Excel-File reader
require(dplyr) # Data Manupulation
require(forecast) # Time Series 
require(tidyr) # splits a col into two based on the regular expression
require(car) # to set un-ordered factors ranking method, helmert - baseline is one method rest referring to it
require(dummies) # to reacte dummy variables for factor data
require(ggplot2) # Visualization 
require(usdm) # VIF
require(tseries) # ADF test

# Building the Rawdata frame - complete data ------
excel_sheets('dunnhumby - Breakfast at the Frat.xlsx')
rawdata = read_excel(path = 'dunnhumby - Breakfast at the Frat.xlsx', sheet = 'dh Transaction Data')
rawStoreData = read_excel(path = 'dunnhumby - Breakfast at the Frat.xlsx', sheet = 'dh Store Lookup')
rawdata = rawdata %>% 
  left_join(rawStoreData[which(!duplicated(rawStoreData$STORE_ID)),],
            by = c('STORE_NUM'='STORE_ID')) %>%
  left_join(read_excel(path = 'dunnhumby - Breakfast at the Frat.xlsx', sheet = 'dh Products Lookup'),
            by = 'UPC')
rawdata$WEEK_END_DATE = as.Date(rawdata$WEEK_END_DATE)
# Checking for missing values - 3 fields have missing data ----
Missing_data_Check <- function(data_set){
  NA_Count = sapply(data_set,function(y) sum(length(which(is.na(y))))) 
  Null_Count = sapply(data_set,function(y) sum(length(which(is.null(y)))))
  Length0_Count = sapply(data_set,function(y) sum(length(which(length(y)==0))))
  Empty_Count = sapply(data_set,function(y) {if(class(y) != 'Date') 
    return(sum(length(which(y==''))))
    else return(0)})
  Total_NonData = NA_Count+Null_Count+Length0_Count+Empty_Count
  return( Total_NonData )
}
Missing_data_Check(rawdata)
# Missing data handling ----
# Fixing 1.1 BasePrice - replace with the weekly product average spend
non_baseprice_missing_rawdata = rawdata[,1:25][!apply(rawdata[,9], 1, function(x) any(is.na(x))),] 
product_WeeklyAvg_basePrice = non_baseprice_missing_rawdata %>%
  dplyr::select(WEEK_END_DATE,UPC,BASE_PRICE)%>%
  group_by(UPC,WEEK_END_DATE) %>%
  mutate(AVG_BASE_PRICE= round(mean(BASE_PRICE),2)) %>%
  dplyr::select(WEEK_END_DATE,UPC,AVG_BASE_PRICE) %>%
  unique()
baseprice_missing_rawdata = rawdata[,1:25][apply(rawdata[,9], 1, function(x) any(is.na(x))),] 
baseprice_missing_rawdata = baseprice_missing_rawdata %>%
  left_join(product_WeeklyAvg_basePrice,
            by=c("WEEK_END_DATE","UPC"))
baseprice_missing_rawdata$BASE_PRICE = baseprice_missing_rawdata$AVG_BASE_PRICE
rawdata_BasePrice = rbind(non_baseprice_missing_rawdata,baseprice_missing_rawdata[-26])
rm(list = c('non_baseprice_missing_rawdata','product_WeeklyAvg_basePrice','baseprice_missing_rawdata'))
# Fixing 1.2 Price: replace by the weekly product average spend
non_price_missing_rawdata = rawdata_BasePrice[,1:25][!apply(rawdata_BasePrice[,8], 1, function(x) any(is.na(x))),] 
product_WeeklyAvg_Price = non_price_missing_rawdata %>%
  dplyr::select(WEEK_END_DATE,UPC,PRICE)%>%
  group_by(UPC,WEEK_END_DATE) %>%
  mutate(AVG_PRICE= round(mean(PRICE),2)) %>%
  dplyr::select(WEEK_END_DATE,UPC,AVG_PRICE) %>%
  unique()
price_missing_rawdata = rawdata_BasePrice[,1:25][apply(rawdata_BasePrice[,8], 1, function(x) any(is.na(x))),] 
price_missing_rawdata = price_missing_rawdata %>%
  left_join(product_WeeklyAvg_Price,
            by=c("WEEK_END_DATE","UPC"))
price_missing_rawdata$PRICE = price_missing_rawdata$AVG_PRICE
rawdata_BasePrice_Price = rbind(non_price_missing_rawdata,price_missing_rawdata[-26])
rm(list = c('non_price_missing_rawdata','product_WeeklyAvg_Price','price_missing_rawdata','rawdata_BasePrice'))
# Creating Discount based features & Modifying Volume units ----
rawdata_BasePrice_Price$DISCOUNT_PRICE = rawdata_BasePrice_Price$BASE_PRICE - rawdata_BasePrice_Price$PRICE
rawdata_BasePrice_Price$DISCOUNT_PERCENT = (rawdata_BasePrice_Price$DISCOUNT_PRICE/rawdata_BasePrice_Price$BASE_PRICE)*100
rawdata_BasePrice_Price$DISCOUNT = ifelse(rawdata_BasePrice_Price$DISCOUNT_PRICE!=0,1,0)
# re-defining features as factors
rawdata_factors = as.data.frame(unclass(rawdata_BasePrice_Price))
rawdata_factors$FEATURE = as.factor(rawdata_factors$FEATURE)
rawdata_factors$DISPLAY = as.factor(rawdata_factors$DISPLAY)
rawdata_factors$TPR_ONLY = as.factor(rawdata_factors$TPR_ONLY)
rawdata_factors$DISCOUNT = as.factor(rawdata_factors$DISCOUNT)
rawdata_factors$MSA_CODE = as.factor(rawdata_factors$MSA_CODE)
rm(rawdata_BasePrice_Price)
# Forecast of the next 12 Weeks overall Spends for the company -----
# building data as per weekly total spend happened
company_data = rawdata_factors %>%
                  dplyr::select(WEEK_END_DATE,SPEND,FEATURE,DISPLAY,TPR_ONLY,
                                BASE_PRICE,PRICE) %>%
                  group_by(WEEK_END_DATE) %>%
                  mutate(W_TOTAL_SPEND = sum(SPEND)) %>%
                  dplyr::select(WEEK_END_DATE,W_TOTAL_SPEND) %>%
                  unique() %>%
                  arrange(WEEK_END_DATE)
# reading spend data as TimeSeries, split and plot 
TS_data = ts(company_data$W_TOTAL_SPEND, start = c(2009,1), end = c(2011,52), frequency = 52)
TS_data_train = ts(company_data$W_TOTAL_SPEND[1:144], start = c(2009,1), end = c(2011,40), frequency = 52)
TS_data_test = ts(company_data$W_TOTAL_SPEND[145:156], start = c(2011,41), end = c(2011,52), frequency = 52)
plot(TS_data_train, xlab= '2009_1W - 2011_40W', ylab='Weekly Total Sales',main = "Company's Sales Trend - train data")
# plotting the decomposed times series
TS_data_train_decompose = decompose(TS_data_train)
plot(TS_data_train_decompose) 
# differenced series & autocorrelation
TS_data_train_D1 = diff(TS_data_train, lag = 1)
plot(TS_data_train_D1, xlab= '2009_1W - 2011_40W', ylab='',main = "1st differential - train data")
TS_data_train_D2 = diff(TS_data_train, lag = 2)
plot(TS_data_train_D2, xlab= '2009_1W - 2011_40W', ylab='',main = "2nd differential - train data")

TS_data_train_AutoCorr = acf(TS_data_train,lag.max = 50)
TS_data_train_PartialAutoCorr = pacf(TS_data_train, lag.max = 10)  
# Augmented Dickey-Fuller test, Ho:a unit root of a univarate time series
adf.test(TS_data_train/scale(TS_data_train-TS_data_train_decompose$seasonal),
         alternative = "stationary")

# Time series analysis for the next 12 Week sales prediction for the company.
# Exponential model
TS_data_train_expSmoothing = ets(TS_data_train)
TS_data_train_expSmoothing_forecast = forecast(TS_data_train_expSmoothing,h=12)
Accuracy_expSmoothing = accuracy(TS_data_train_expSmoothing_forecast,TS_data_test)
plot(TS_data_train_expSmoothing_forecast)
# Holt-Winters model - optimal values for alpha, beta, gamma
j=1
MAPE = numeric()
a1 = numeric()
b1 = numeric()
c1 = numeric()
for(a in seq(from=0.1,to= 0.9,by=0.1)){
  for(b in seq(from=0.1,to= 0.9,by=0.1)){
    for(c in seq(from=0.1,to= 0.9,by=0.1)){
      HW_Model = HoltWinters(TS_data_train, alpha=a, beta=b, gamma=c)
      hold_predict = forecast(HW_Model,12)
      MAPE[j] = sum(abs(hold_predict$mean[1:12]-TS_data_test[1:12])/TS_data_test[1:12])/length(TS_data_test)
      a1[j] = a
      b1[j] = b
      c1[j] =c
      j = j+1
    }
  }
  print(a)
}
temp = data.frame(MAPE = MAPE, alpha = a1, beta = b1, gamma = c1)
subset(temp, MAPE==min(MAPE))

TS_data_train_HW= HoltWinters(TS_data_train, alpha=0.1, beta=0.5, gamma=0.6)
TS_data_train_HW_forecast= forecast(TS_data_train_HW, 12)
plot(TS_data_train_HW_forecast)
Accuracy_HW=accuracy(TS_data_train_HW_forecast,TS_data_test)
# ARIMA model
TS_data_train_ARIMA=auto.arima(TS_data_train)
TS_data_train_ARIMA_forecast = forecast(TS_data_train_ARIMA, h=12)
plot(TS_data_train_ARIMA_forecast)
Accuracy_ARIMA=accuracy(TS_data_train_ARIMA_forecast,TS_data_test)
# TbATS model
TS_data_train_TBATS = tbats(TS_data_train)
TS_data_train_TBATS_forecast = forecast(TS_data_train_TBATS, h=12)
plot(TS_data_train_TBATS_forecast)
Accuracy_TBATS=accuracy(TS_data_train_TBATS_forecast,TS_data_test)
# NNETS model
TS_data_train_NN = nnetar(TS_data_train)
TS_data_train_NN_forecast = forecast(TS_data_train_NN, h=12)
plot(TS_data_train_NN_forecast)
Accuracy_NN=accuracy(TS_data_train_NN_forecast,TS_data_test)

Accuracy_expSmoothing
Accuracy_HW
Accuracy_ARIMA
Accuracy_TBATS
Accuracy_NN
# Prediction of sales for next 12 Weeks - ARIMA model
ARIMA_model_completedata=auto.arima(TS_data)
TS_data_forecast = forecast(ARIMA_model_completedata, h=12)
plot(TS_data_forecast)

# Hypothesis testing ----
# 1,2.Store AVG_WEEKLY_BASKETS contributors 
store_data = rawStoreData
store_data$PARKING_AVAIL = ifelse(is.na(store_data$PARKING_SPACE_QTY),0,1)
store_data$PARKING_SPACE_QTY = ifelse(is.na(store_data$PARKING_SPACE_QTY),0,store_data$PARKING_SPACE_QTY)

store_data$ADDRESS_STATE_PROV_CODE = as.factor(store_data$ADDRESS_STATE_PROV_CODE)
store_data$SEG_VALUE_NAME = as.factor(store_data$SEG_VALUE_NAME)
store_data$ADDRESS_CITY_NAME = as.factor(store_data$ADDRESS_CITY_NAME)

temp = dummy.data.frame(as.data.frame(store_data[,c(4,6,7,8,9)]),sep='_') #state,seg,parking,store size, bucket size
usdm::vif(as.data.frame(temp[c(1:3,5,6,8,9)])) # passing 3 states, 2 seg, parking and store size
usdm::vif(as.data.frame(store_data[,c(7,8)])) # parking, store size and bucket size

Store_St_Se_P_A_model = lm(AVG_WEEKLY_BASKETS~., data=temp[c(1:3,5,6,8:10)] )
summary(Store_St_Se_P_A_model)
Store_P_A = lm(AVG_WEEKLY_BASKETS~., data= store_data[,c(7,8,9)])
summary(Store_P_A)
# 3. No of visits and product sales relationship

# Promotional activities ----
# Product wise Significant promotion
all_product_list = unique(rawdata_factors$UPC)
Prom_Sig_mode_df = data.frame(
  productCode = numeric(0),
  Intercept_PValue = numeric(0),FEATURE_PValue = numeric(0),DISPLAY_PValue = numeric(0),TPR_ONLY_PValue = numeric(0),
  F_Length = numeric(0),D_Length = numeric(0),T_Length = numeric(0)
  )
Prom_Sig_mode_df_names = names(Prom_Sig_mode_df)
for(i in all_product_list){
  data1=subset(rawdata,UPC==i)
  data_a=data1[,c(7,10,11,12)]
  data_a$FEATURE=as.factor(data_a$FEATURE)
  data_a$DISPLAY=as.factor(data_a$DISPLAY)
  data_a$TPR_ONLY=as.factor(data_a$TPR_ONLY)
  model=lm(SPEND~FEATURE+DISPLAY+TPR_ONLY,data = data1)
  print(summary(model))
  temp = coef(summary(model))[, "Pr(>|t|)"]
  Prom_Sig_mode_df = rbind(Prom_Sig_mode_df,
                           data.frame(i,temp["(Intercept)"],
                                     temp["FEATURE"],temp["DISPLAY"],
                                     temp["TPR_ONLY"],length(unique(data1$FEATURE)),
                                     length(unique(data1$DISPLAY)),length(unique(data1$TPR_ONLY))))
}
colnames(Prom_Sig_mode_df) = Prom_Sig_mode_df_names
Prom_Sig_mode_df$F_Sig = ifelse(Prom_Sig_mode_df$FEATURE_PValue<=0.05,'F','')
Prom_Sig_mode_df$D_Sig = ifelse(Prom_Sig_mode_df$DISPLAY_PValue<=0.05,'D','')
Prom_Sig_mode_df$T_Sig = ifelse(Prom_Sig_mode_df$TPR_ONLY_PValue<=0.05,'T','')
Prom_Sig_mode_df=Prom_Sig_mode_df %>%
                    dplyr::select(c(1,9,10,11)) %>%
                    dplyr::mutate(Sig_modes = (F_Sig=='F')+(D_Sig=='D')+(T_Sig=='T')) %>%
                    dplyr::arrange(desc(Sig_modes))
# top 10 products Promotion Effectiveness
Top_50sales_products = c('1600027527','3800031838','7192100339','1600027528',
                       '1600027564','3800031829','3800039118','7192100337',
                       '1111085350'#,'1111009477','88491201426', - don't have enough data points
                       )
Product_PE_df = data.frame(numeric(),numeric())
for(Product in Top_50sales_products){
  temp = subset(rawdata_factors, UPC==Product)
  print(Product)
  temp$FEATURE = as.numeric(as.character(temp$FEATURE))
  temp$DISPLAY = as.numeric(as.character(temp$DISPLAY))
  temp$TPR_ONLY = as.numeric(as.character(temp$TPR_ONLY))
  prod_data = temp %>%
    dplyr::select(WEEK_END_DATE,SPEND,FEATURE,DISPLAY,TPR_ONLY,
                  BASE_PRICE,PRICE) %>%
    dplyr::group_by(WEEK_END_DATE) %>%
    dplyr::mutate(W_TOTAL_SPEND = sum(SPEND),
                 W_FEATURE = ifelse(sum(FEATURE)==0,0,1),
                 W_DISPLAY = ifelse(sum(DISPLAY)==0,0,1),
                 W_TPR_ONLY = ifelse(sum(FEATURE)==0,0,1),
                 W_BASE_PRICE = mean(BASE_PRICE),
                 W_PRICE = mean(PRICE)) %>%
    dplyr::select(WEEK_END_DATE,W_TOTAL_SPEND,W_FEATURE,
                  W_DISPLAY,W_TPR_ONLY,W_BASE_PRICE,W_PRICE) %>%
    unique()%>%
    dplyr::mutate(W_PROMO = ifelse(W_FEATURE+W_DISPLAY+W_TPR_ONLY == 0,0,1),
                  W_DISCOUNT = W_BASE_PRICE-W_PRICE )%>%
    dplyr::arrange(WEEK_END_DATE)
  TS_data = ts(prod_data$W_TOTAL_SPEND, start = c(2009,1), end = c(2011,52), frequency = 52)
  #plot(TS_data)
  Decomposition = decompose(TS_data)
  #Decomposition$seasonal
  #plot(Decomposition$seasonal)
  prod_data$W_Seasonal = as.numeric(Decomposition$seasonal)
  vif(data.frame(prod_data[,c(2,3,4,5,6,10)]))
  vif(data.frame(prod_data[,c(2,3,4,6,10)])) # removing TPR_ONLY due to collinearity
  LR_Model = lm(W_TOTAL_SPEND ~ W_BASE_PRICE+W_FEATURE+W_DISPLAY+W_Seasonal,
                data = prod_data)
  summary(LR_Model)
  Coef = LR_Model$coefficients
  Base_Line = Coef[1]+Coef[2]*(prod_data$W_BASE_PRICE)+(Coef[5]*prod_data$W_Seasonal)
  INC = Coef[3]*(prod_data$W_FEATURE)+(Coef[4]*prod_data$W_DISPLAY)
  prod_data$BASE_LINE = Base_Line
  prod_data$INCREMENTAL = INC
  P_Promo = subset(temp, FEATURE == 1 | DISPLAY == 1| TPR_ONLY == 1)
  P_Promo = P_Promo %>% 
    dplyr::select(WEEK_END_DATE,SPEND) %>%
    dplyr::group_by(WEEK_END_DATE) %>%
    dplyr::mutate(W_PROMO_SPEND = sum(SPEND)) %>%
    dplyr::select(WEEK_END_DATE,W_PROMO_SPEND) %>%
    unique()
  prod_data = prod_data %>%
    left_join(P_Promo, by = 'WEEK_END_DATE')
  prod_data$P1_PE = prod_data$INCREMENTAL / prod_data$W_PROMO_SPEND *100
  #View(data.frame(A=prod_data$INCREMENTAL+prod_data$BASE_LINE, B=prod_data$W_TOTAL_SPEND , C= prod_data$W_TOTAL_SPEND-prod_data$INCREMENTAL-prod_data$BASE_LINE))
  Product_PE_df = rbind(Product_PE_df,data.frame(Product,mean(prod_data$P1_PE,na.rm=T)))
}
colnames(Product_PE_df) = c('UPC','Promo_Effectiveness')
View(Product_PE_df)






















