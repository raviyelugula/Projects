require(readxl) # read excel files
require(dplyr)
excel_sheets('dunnhumby - Breakfast at the Frat.xlsx')
rawdata = read_excel(path = 'dunnhumby - Breakfast at the Frat.xlsx', sheet = 'dh Transaction Data')
names(rawdata)
rawStoreData = read_excel(path = 'dunnhumby - Breakfast at the Frat.xlsx', sheet = 'dh Store Lookup')
rawdata = rawdata %>% 
              left_join(rawStoreData[which(!duplicated(rawStoreData$STORE_ID)),],
                        by = c('STORE_NUM'='STORE_ID')) %>%
              left_join(read_excel(path = 'dunnhumby - Breakfast at the Frat.xlsx', sheet = 'dh Products Lookup'),
                        by = 'UPC')
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
rawdata$WEEK_END_DATE = as.Date(rawdata$WEEK_END_DATE)
Missing_data_Check(rawdata)
glimpse(rawdata)
non_baseprice_missing_rawdata = rawdata[,1:25][!apply(rawdata[,9], 1, function(x) any(is.na(x))),] 
product_WeeklyAvg_basePrice = non_baseprice_missing_rawdata %>%
                                select(WEEK_END_DATE,UPC,BASE_PRICE)%>%
                                group_by(UPC,WEEK_END_DATE) %>%
                                mutate(AVG_BASE_PRICE= round(mean(BASE_PRICE),2))
product_WeeklyAvg_basePrice = unique(product_WeeklyAvg_basePrice[,-3])
baseprice_missing_rawdata = rawdata[,1:25][apply(rawdata[,9], 1, function(x) any(is.na(x))),] 
baseprice_missing_rawdata = baseprice_missing_rawdata %>%
                              left_join(product_WeeklyAvg_basePrice,by=c("WEEK_END_DATE","UPC"))
baseprice_missing_rawdata$BASE_PRICE = baseprice_missing_rawdata$AVG_BASE_PRICE
rawdata_BasePrice = rbind(non_baseprice_missing_rawdata,baseprice_missing_rawdata[-26])
rm(list = c('non_baseprice_missing_rawdata','product_WeeklyAvg_basePrice','baseprice_missing_rawdata'))
non_price_missing_rawdata = rawdata_BasePrice[,1:25][!apply(rawdata_BasePrice[,8], 1, function(x) any(is.na(x))),] 
product_WeeklyAvg_Price = non_price_missing_rawdata %>%
                                select(WEEK_END_DATE,UPC,PRICE)%>%
                                group_by(UPC,WEEK_END_DATE) %>%
                                mutate(AVG_PRICE= round(mean(PRICE),2))
product_WeeklyAvg_Price = unique(product_WeeklyAvg_Price[,-3])
price_missing_rawdata = rawdata_BasePrice[,1:25][apply(rawdata_BasePrice[,8], 1, function(x) any(is.na(x))),] 
price_missing_rawdata = price_missing_rawdata %>%
                              left_join(product_WeeklyAvg_Price,by=c("WEEK_END_DATE","UPC"))
price_missing_rawdata$PRICE = price_missing_rawdata$AVG_PRICE
rawdata_BasePrice_Price = rbind(non_price_missing_rawdata,price_missing_rawdata[-26])
rm(list = c('non_price_missing_rawdata','product_WeeklyAvg_Price','price_missing_rawdata','rawdata_BasePrice'))
Missing_data_Check(rawdata_BasePrice_Price)

rawdata_BasePrice_Price$DISCOUNT_PRICE = rawdata_BasePrice_Price$BASE_PRICE - rawdata_BasePrice_Price$PRICE
rawdata_BasePrice_Price$DISCOUNT_PERCENT = (rawdata_BasePrice_Price$DISCOUNT_PRICE/rawdata_BasePrice_Price$BASE_PRICE)*100
rawdata_BasePrice_Price$DISCOUNT = ifelse(rawdata_BasePrice_Price$DISCOUNT_PRICE!=0,1,0)
rawdata_factors = as.data.frame(unclass(rawdata_BasePrice_Price))
rawdata_factors$FEATURE = as.factor(rawdata_factors$FEATURE)
rawdata_factors$DISPLAY = as.factor(rawdata_factors$DISPLAY)
rawdata_factors$TPR_ONLY = as.factor(rawdata_factors$TPR_ONLY)
rawdata_factors$DISCOUNT = as.factor(rawdata_factors$DISCOUNT)
rawdata_factors$MSA_CODE = as.factor(rawdata_factors$MSA_CODE)
sapply(rawdata_factors,class)

sapply(rawdata_factors[,c(10:12,15:17,22,24,28)],class)
require(klaR)
cluster = kmodes(rawdata_factors[,c(10:12,15:17,22,24,28)],4)
require(cluster)
#system.time(pam(daisy(rawdata_factors[,c(10:12,15:17,22,24,28)], metric="gower", type=list(symm=1:9)), k=4))
#system.time(kmodes(rawdata_factors[,c(10:12,15:17,22,24,28)], modes=2))

unique(rawdata_factors$PRODUCT_SIZE)
# 1 oz = 29.5735 ml
# 1 LT = 1000 ml
# 1 ML = 1 ml
# 1 CT = 200 mg
require(tidyr)
rawdata_factors = extract(rawdata_factors, PRODUCT_SIZE, c("VOLUME", "V_UNITS"), "([^ ]+) (.*)")
rawdata_factors$VOLUME=as.numeric(rawdata_factors$VOLUME)
rawdata_factors$PRODUct_SIZE_Milli = ifelse(rawdata_factors$V_UNITS=='ML', rawdata_factors$VOLUME,
                                            ifelse(rawdata_factors$V_UNITS=='CT',rawdata_factors$VOLUME*200,
                                                   ifelse(rawdata_factors$V_UNITS=='LT',rawdata_factors$VOLUME*1000,
                                                          ifelse(rawdata_factors$V_UNITS=='OZ',rawdata_factors$VOLUME*29.5735,-1))))

## 14 variables
names(rawdata_factors)
require(dummies)
names(rawdata_factors[,c(9:12,15:17,19:20,22,24,27,30)])
sapply(rawdata_factors[,c(9:12,15:17,19:20,22,24,27,30)],class)
dependents = dummy.data.frame(rawdata_factors[,c(9:12,15:17,19:20,22,24,27,30)],sep='.')
scaled_dependents = as.data.frame(scale(dependents))
require(corrplot)
require(RColorBrewer)
round(cor(scaled_dependents),2)

dependents_noColnames = scaled_dependents
colnames(dependents_noColnames) = c(1:ncol(scaled_dependents))
corrplot(cor(dependents_noColnames), type="upper",
         method = 'circle' ,order="hclust", add = F,
         col=brewer.pal(n=4, name="RdBu"),  
         outline = T)
require(usdm)
vif(scaled_dependents)
sapply(rawdata_factors[,c(9,19:20,27,30)],class)
vif(as.data.frame(scale(rawdata_factors[,c(9,27,19:20,30)])))

linearmodel = lm(SPEND~PRICE+BASE_PRICE+SALES_AREA_SIZE_NUM+
                   AVG_WEEKLY_BASKETS+DISCOUNT_PRICE+
                   PRODUct_SIZE_Milli, data = rawdata_factors) 
summary(linearmodel)
plot(linearmodel)

linearmodel2 = lm((SPEND^2)~PRICE+BASE_PRICE+SALES_AREA_SIZE_NUM+
                   AVG_WEEKLY_BASKETS+DISCOUNT_PRICE+
                   PRODUct_SIZE_Milli, data = rawdata_factors) 
summary(linearmodel2)
plot(linearmodel2)

## 8 variables
names(rawdata_factors)
require(dummies)
names(rawdata_factors[,c(9:12,15,24,27,29)])
sapply(rawdata_factors[,c(9:12,15,24,27,29)],class)
dependents = dummy.data.frame(rawdata_factors[,c(9:12,15,24,27,29)],sep='.')
names(dependents)
scaled_dependents = as.data.frame(scale(dependents))
require(corrplot)
require(RColorBrewer)
round(cor(scaled_dependents),2)
dependents_noColnames = scaled_dependents
colnames(dependents_noColnames) = c(1:ncol(dependents))
corrplot(cor(dependents_noColnames), type="upper",
         method = 'circle' ,order="hclust", add = F,
         col=brewer.pal(n=4, name="RdBu"),  
         outline = T)

require(usdm)
vif(scaled_dependents)
sapply(rawdata_factors[,c(9:12,15,24,27,29)],class)
vif(as.data.frame(scale(rawdata_factors[,c(9,27)])))

## 4 variables
names(rawdata_factors)
require(dummies)
names(rawdata_factors[,c(9,15,24,27)])
sapply(rawdata_factors[,c(9,15,24,27)],class)
dependents = dummy.data.frame(rawdata_factors[,c(9,15,24,27)],sep='.')
names(dependents)
scaled_dependents = as.data.frame(scale(dependents))
require(corrplot)
require(RColorBrewer)
round(cor(scaled_dependents),2)
dependents_noColnames = scaled_dependents
colnames(dependents_noColnames) = c(1:ncol(dependents))
corrplot(cor(dependents_noColnames), type="upper",
         method = 'circle' ,order="hclust", add = F,
         col=brewer.pal(n=4, name="RdBu"),  
         outline = T)

require(usdm)
vif(scaled_dependents)
vif(as.data.frame(scale(rawdata_factors[,c(9,27)])))
 

## Random Forest
require(randomForest)
# Dense Forest
set.seed(123)
RF_model = randomForest(x = rawdata_factors[,c(9:12,15:17,19:20,22,24,27,30)],
                        y = rawdata_factors[,c(7)],
                        data_set = rawdata_factors,
                        ntree =  500,  # large number because we want to build as many trees as possible
                        mtry =  4,
                        nodesize = 5000)
RF_model
plot(RF_model, main = "High number of trees vs OOB error")

# Adjusting mtry - 2 techinques - tureRF and gridsearch
set.seed(123)
tuned_RF_model = tuneRF(x = rawdata_factors[,c(9:12,15:17,19:20,22,24,27,30)],
                        y = rawdata_factors[,c(7)],
                        data_set = rawdata_factors,
                        mtryStart = 2,
                        stepFactor = 1.5,
                        improve = 0.001,
                        ntreeTry = 300,
                        nodesize = 5000,
                        doBest = T, trace = T, plot = T,importance = T)
tuned_RF_model #tune_RF - 19






