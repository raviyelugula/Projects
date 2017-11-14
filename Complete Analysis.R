require(readxl) # Excel-File reader
require(dplyr) # Data Manupulation
require(forecast) # Time Series 
require(tidyr) # splits a col into two based on the regular expression
require(car) # to set un-ordered factors ranking method, helmert - baseline is one method rest referring to it
require(dummies) ## to reacte dummy variables for factor data
require(ggplot2)
require(usdm)

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

# Forecast of the next 12 Weeks overall Spends for the company -----
# building data as per weekly total spend happened
company_data = rawdata %>%
                  dplyr::select(WEEK_END_DATE,SPEND,FEATURE,DISPLAY,TPR_ONLY,
                                BASE_PRICE,PRICE) %>%
                  group_by(WEEK_END_DATE) %>%
                  mutate(W_TOTAL_SPEND = sum(SPEND)) %>%
                  dplyr::select(WEEK_END_DATE,W_TOTAL_SPEND) %>%
                  unique() %>%
                  arrange(WEEK_END_DATE)
# reading spend data as TimeSeries
TS_data = ts(company_data$W_TOTAL_SPEND, start = c(2009,1), end = c(2011,52), frequency = 52)
TS_data_train = ts(company_data$W_TOTAL_SPEND[1:144], start = c(2009,1), end = c(2011,40), frequency = 52)
TS_data_test = ts(company_data$W_TOTAL_SPEND[145:156], start = c(2011,41), end = c(2011,52), frequency = 52)
plot(TS_data_train)
plot(TS_data_test,type='b')
# arima mmodel builing and tuning
ARIMA_Train=auto.arima(TS_data_train)
Predicted_SPEND = forecast::forecast(ARIMA_Train, h=12 )
Acc=accuracy(TS_data_test,Predicted_SPEND$mean)
Acc
plot(Predicted_SPEND)
# model implementation
ARIMA_Full = arima(TS_data, order=c(2,1,1))
Forecast_12W = forecast::forecast(ARIMA_Full, h=12 )
Forecast_12W$mean
plot(Forecast_12W$mean,type='b', xlab='TIME',ylab='SPEND',main='Forecast for 12 Weeks overall Spend')
rm(list = c('company_data','TS_data','TS_data_train','TS_data_test','ARIMA_Train','Predicted_SPEND','ARIMA_Full'))

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
# re-calculating the product size to milli scale 
unique(rawdata_factors$PRODUCT_SIZE)
# 1 oz = 29.5735 ml # 1 LT = 1000 ml # 1 ML = 1 ml # 1 CT = 200 mg
rawdata_factors = extract(rawdata_factors, PRODUCT_SIZE, c("VOLUME", "V_UNITS"), "([^ ]+) (.*)")
rawdata_factors$VOLUME=as.numeric(rawdata_factors$VOLUME)
rawdata_factors$PRODUct_SIZE_Milli = ifelse(rawdata_factors$V_UNITS=='ML', rawdata_factors$VOLUME,
                                            ifelse(rawdata_factors$V_UNITS=='CT',rawdata_factors$VOLUME*200,
                                                   ifelse(rawdata_factors$V_UNITS=='LT',rawdata_factors$VOLUME*1000,
                                                          ifelse(rawdata_factors$V_UNITS=='OZ',rawdata_factors$VOLUME*29.5735,-1))))

rawdata_factors = rawdata_factors[,-c(25,26)]
write.csv(rawdata_factors,'rawdata_factors.csv',row.names = F)

# Store Wise Clustering ----
# Building a dataframe =, with store details and avg sales details
storedata2 = rawdata_factors %>%
                group_by(STORE_NUM) %>%
                dplyr::select(STORE_NUM,STORE_NAME,ADDRESS_CITY_NAME,
                              ADDRESS_STATE_PROV_CODE,MSA_CODE,SEG_VALUE_NAME,
                              PARKING_SPACE_QTY,SALES_AREA_SIZE_NUM,AVG_WEEKLY_BASKETS) %>%
                unique()
storedata1 = rawdata_factors %>%
                  group_by(STORE_NUM,WEEK_END_DATE) %>%
                  mutate(T_WEEKLY_SPEND1 = sum(SPEND),
                         T_WEEKLY_UNITS_SOLD1 = sum(UNITS),
                         T_WEEKLY_VISITS1 = sum(VISITS),
                         T_WEEKLY_HHS1 = sum(HHS)) %>%
                  dplyr::select(STORE_NUM,T_WEEKLY_SPEND1,T_WEEKLY_UNITS_SOLD1,
                                T_WEEKLY_VISITS1,T_WEEKLY_HHS1,WEEK_END_DATE) %>%
                  unique() %>%
                  group_by(STORE_NUM) %>%
                    mutate(AVG_WEEKLY_T_SPEND = mean(T_WEEKLY_SPEND1),
                           AVG_WEEKLY_T_UNITS_SOLD = mean(T_WEEKLY_UNITS_SOLD1),
                           AVG_WEEKLY_T_VISITS = mean(T_WEEKLY_VISITS1),
                           AVG_WEEKLY_T_HHS = mean(T_WEEKLY_HHS1)) %>%
                    dplyr::select(STORE_NUM,AVG_WEEKLY_T_SPEND,AVG_WEEKLY_T_UNITS_SOLD,
                                  AVG_WEEKLY_T_VISITS,AVG_WEEKLY_T_HHS)  %>%
                  unique()

storedata = left_join(storedata1,storedata2,by='STORE_NUM')
rm(list = c('storedata1','storedata2'))
storedata$PARKING_FLAG = as.factor(ifelse(is.na(storedata$PARKING_SPACE_QTY),0,1))
storedata$PARKING_NEW = ifelse(is.na(storedata$PARKING_SPACE_QTY),0,storedata$PARKING_SPACE_QTY)
# Optimal centers for Clustering 
k_max = 10
C_data = storedata[,c(15,12,13,4)] #PARKING_NEW,SALES_AREA_SIZE_NUM,AVG_WEEKLY_BASKETS,AVG_WEEKLY_T_VISITS
set.seed(1234)
wss = sapply(1:k_max, 
              function(k){kmeans(C_data, k, nstart=10,iter.max = 10 )$tot.withinss})
wss
plot(1:k_max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     main='Elbeow method using wss')
# Clustering
set.seed(1234)
storedata$C3=kmeans(storedata[,c(15,12,13,4)],centers = 3)$cluster
rm(C_data)
# Cluster's dataframe creating
Clusterdata = storedata %>%
                group_by(C3) %>%
                mutate(AVG_WEEKLY_C_T_SPEND = mean(AVG_WEEKLY_T_SPEND),
                       AVG_WEEKLY_C_T_UNITS_SOLD = mean(AVG_WEEKLY_T_UNITS_SOLD),
                       AVG_WEEKLY_C_T_VISITS = mean(AVG_WEEKLY_T_VISITS),
                       AVG_WEEKLY_C_T_HHS = mean(AVG_WEEKLY_T_HHS),
                       AVG_WEEKLY_C_BASKETS = mean(AVG_WEEKLY_BASKETS),
                       SALES_AREA_C_SIZE_NUM = mean(SALES_AREA_SIZE_NUM),
                       NUM_STORE_WITH_PARKING = sum(as.numeric(PARKING_FLAG)),
                       NUM_STORES = n()) %>%
                dplyr::select(C3,AVG_WEEKLY_C_T_SPEND,NUM_STORE_WITH_PARKING,
                              NUM_STORES,AVG_WEEKLY_C_T_HHS,AVG_WEEKLY_C_BASKETS,
                              AVG_WEEKLY_C_T_UNITS_SOLD,AVG_WEEKLY_C_T_VISITS,
                              SALES_AREA_C_SIZE_NUM) %>%
                unique()
# Cluster visualization
ggplot(storedata)+
  geom_point(aes(x= SALES_AREA_SIZE_NUM,
                 y=AVG_WEEKLY_BASKETS,
                 color=as.factor(C3),
                 shape = as.factor(PARKING_FLAG),
                 size = AVG_WEEKLY_T_VISITS)) +
  labs(title = "Store Cluster", x = "Store Area", y = "Average Weekly Baskets",
       color = "Cluster", shape = "Parking Availability", size= "Average Weekly Visits")

# Promotion Significance Product wise ----
require(sjPlot)
require(broom)
Promo_Sig = data.frame(
  productCode = numeric(0),
  Intercept_PValue = numeric(0),
  FEATURE_PValue = numeric(0),
  DISPLAY_PValue = numeric(0),
  TPR_ONLY_PValue = numeric(0),
  F_Length = numeric(0),
  D_Length = numeric(0),
  T_Length = numeric(0)
)
Promo_Sig_Names =names(Promo_Sig)
Product_list=unique(rawdata_factors$UPC)
for(i in Product_list){
  print(i)
  data1=subset(rawdata_factors,UPC==i)
  data1$FEATURE=as.numeric(data1$FEATURE)
  data1$DISPLAY=as.numeric(data1$DISPLAY)
  data1$TPR_ONLY=as.numeric(data1$TPR_ONLY)
  model=lm(SPEND~FEATURE+DISPLAY+TPR_ONLY,data = data1)
  print(summary(model))
  out=tidy(model)
  temp = coef(summary(model))[, "Pr(>|t|)"]
  Promo_Sig = rbind(Promo_Sig,data.frame(i,temp["(Intercept)"],
                                   temp["FEATURE"],temp["DISPLAY"],
                                   temp["TPR_ONLY"],length(unique(data1$FEATURE)),
                                   length(unique(data1$DISPLAY)),length(unique(data1$TPR_ONLY))))
}
colnames(Promo_Sig) = Promo_Sig_Names
Promo_Sig$F_Sig = ifelse(Promo_Sig$FEATURE_PValue<=0.05,'F','')
Promo_Sig$D_Sig = ifelse(Promo_Sig$DISPLAY_PValue<=0.05,'D','')
Promo_Sig$T_Sig = ifelse(Promo_Sig$TPR_ONLY_PValue<=0.05,'T','')
Promo_Sig$Sig_modes = (Promo_Sig$F_Sig=='F')+(Promo_Sig$D_Sig=='D')+(Promo_Sig$T_Sig=='T')

# Productwise Spend forecast for next 12 Weeks----
Product_forecast = data.frame(numeric())
P_TOP_LIST = c('1600027527','3800031838','7192100339','1600027528','1600027564','3800031829','3800039118','7192100337','1111085350','88491201426','1111009477')
for(Product in P_TOP_LIST){
  print(Product)
  P1= subset(rawdata_factors, UPC== Product)  
  P1$FEATURE=as.numeric(P1$FEATURE)
  P1$DISPLAY=as.numeric(P1$DISPLAY)
  P1$TPR_ONLY=as.numeric(P1$TPR_ONLY)
  P1_data = P1 %>%
    dplyr::select(WEEK_END_DATE,SPEND,FEATURE,DISPLAY,TPR_ONLY,
                  BASE_PRICE,PRICE) %>%
    group_by(WEEK_END_DATE) %>%
    mutate(W_TOTAL_SPEND = sum(SPEND),
           W_FEATURE = ifelse(sum(FEATURE)==0,0,1),
           W_DISPLAY = ifelse(sum(DISPLAY)==0,0,1),
           W_TPR_ONLY = ifelse(sum(FEATURE)==0,0,1),
           W_BASE_PRICE = mean(BASE_PRICE),
           W_PRICE = mean(PRICE)) %>%
    dplyr::select(WEEK_END_DATE,W_TOTAL_SPEND,W_FEATURE,
                  W_DISPLAY,W_TPR_ONLY,W_BASE_PRICE,W_PRICE) %>%
    unique()%>%
    mutate(W_PROMO = ifelse(W_FEATURE+W_DISPLAY+W_TPR_ONLY == 0,0,1),
           W_DISCOUNT = W_BASE_PRICE-W_PRICE )%>%
    arrange(WEEK_END_DATE)
  print(length(P1_data$W_TOTAL_SPEND))
  TS_data = ts(P1_data$W_TOTAL_SPEND, start = c(2009,1), end = c(2011,52), frequency = 52)
  TS_data_train = ts(P1_data$W_TOTAL_SPEND[1:144], start = c(2009,1), end = c(2011,40), frequency = 52)
  TS_data_test = ts(P1_data$W_TOTAL_SPEND[145:156], start = c(2011,41), end = c(2011,52), frequency = 52)

  ARIMA_Train=auto.arima(TS_data_train)
  Predicted_SPEND = forecast::forecast(ARIMA_Train, h=12)
  Acc=accuracy(TS_data_test,Predicted_SPEND$mean)
  print(Acc)

  ARIMA_Full = auto.arima(TS_data)
  Forecast_12W = forecast::forecast(ARIMA_Full, h=12 )
  Forecast_12W$mean
  print(Forecast_12W$mean)
  Product_forecast = rbind(Product_forecast,Forecast_12W$mean)
  name = paste(Product,'_Forecast_SPEND_12W.png', sep = '')
  png(filename =paste0('plots/',name), width = 800, height = 600, units = 'px')
  print(plot(Forecast_12W$mean,type='b', xlab='TIME',ylab='SPEND',main= Product))
  dev.off()
}
rownames(Product_forecast) = c('1600027527','3800031838','7192100339','1600027528','1600027564','3800031829','3800039118','7192100337','1111085350','88491201426','1111009477')
colnames(Product_forecast) = c('2016W2','2016W3','2016W4','2016W5','2016W6','2016W7','2016W8','2016W9','2016W10','2016W11','2016W12','2016W13')

# Top Products Product Effectiveness ----
P_TOP_LIST = c('1600027527', 
               '3800031838', 
               '7192100339', 
               '1600027528', 
               '1600027564', 
               '3800031829', 
               '3800039118', 
               '7192100337', 
               '1111085350', 
               #'88491201426',
               '1111009477' 
               )
data = read.csv("work1.csv")
data$WEEK_END_DATE=as.Date(data$WEEKEND_DATE,format="%d-%b-%y")
for(i in P_TOP_LIST){
  P1= subset(data, UPC== i)  
  P1$FEATURE=as.numeric(P1$FEATURE)
  P1$DISPLAY=as.numeric(P1$DISPLAY)
  P1$TPR_ONLY=as.numeric(P1$TPR_ONLY)
  P1_data = P1 %>%
    dplyr::select(WEEK_END_DATE,SPEND,FEATURE,DISPLAY,TPR_ONLY,
                  BASE_PRICE,PRICE) %>%
    group_by(WEEK_END_DATE) %>%
    mutate(W_TOTAL_SPEND = sum(SPEND),
           W_FEATURE = ifelse(sum(FEATURE)==0,0,1),
           W_DISPLAY = ifelse(sum(DISPLAY)==0,0,1),
           W_TPR_ONLY = ifelse(sum(FEATURE)==0,0,1),
           W_BASE_PRICE = mean(BASE_PRICE),
           W_PRICE = mean(PRICE)) %>%
    dplyr::select(WEEK_END_DATE,W_TOTAL_SPEND,W_FEATURE,
                  W_DISPLAY,W_TPR_ONLY,W_BASE_PRICE,W_PRICE) %>%
    unique()%>%
    mutate(W_PROMO = ifelse(W_FEATURE+W_DISPLAY+W_TPR_ONLY == 0,0,1),
           W_DISCOUNT = W_BASE_PRICE-W_PRICE )%>%
    arrange(WEEK_END_DATE)
  
  TS_data = ts(P1_data$W_TOTAL_SPEND, start = c(2009,1), end = c(2011,52), frequency = 52)
  Decomposition = decompose(TS_data)
  P1_data$W_Seasonal = Decomposition$seasonal
  print(i)
  #print(vif(data.frame(P1_data[,c(3,4,5,6,10)])))
  print(vif(data.frame(P1_data[,c(3,4,6,10)]))) # removing TPR_ONLY due to collinearity
  LR_Model = lm(W_TOTAL_SPEND ~ W_BASE_PRICE+W_FEATURE+W_DISPLAY+W_TPR_ONLY+W_Seasonal,
                data = P1_data)
  summary(LR_Model)
  Coef = LR_Model$coefficients
  
  Base_Line = Coef[1]+Coef[2]*(P1_data$W_BASE_PRICE)+(Coef[5]*P1_data$W_Seasonal)
  INC = Coef[3]*(P1_data$W_FEATURE)+(Coef[4]*P1_data$W_DISPLAY)
  
  P1_data$BASE_LINE = Base_Line
  P1_data$INCREMENTAL = INC
  
  ggplot(P1_data, aes(x=WEEK_END_DATE ))+
    geom_line(aes(y=W_TOTAL_SPEND),color='black',show.legend = F)+
    geom_line(aes(y=BASE_LINE),color='blue',show.legend = F)+
    geom_line(aes(y=INCREMENTAL),color='red',show.legend = F)+
    ylab('SALES')
  
}
  

vif(data.frame(P1_data[,c(2,3,4,6,10)])) # removing TPR_ONLY due to collinearity








