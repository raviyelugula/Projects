require(readxl) # read excel files
require(dplyr) # for join and create new col by mutate

### Building the Rawdata frame
excel_sheets('dunnhumby - Breakfast at the Frat.xlsx')
rawdata = read_excel(path = 'dunnhumby - Breakfast at the Frat.xlsx', sheet = 'dh Transaction Data')
rawStoreData = read_excel(path = 'dunnhumby - Breakfast at the Frat.xlsx', sheet = 'dh Store Lookup')
rawdata = rawdata %>% 
              left_join(rawStoreData[which(!duplicated(rawStoreData$STORE_ID)),],
                        by = c('STORE_NUM'='STORE_ID')) %>%
              left_join(read_excel(path = 'dunnhumby - Breakfast at the Frat.xlsx', sheet = 'dh Products Lookup'),
                        by = 'UPC')
rawdata$WEEK_END_DATE = as.Date(rawdata$WEEK_END_DATE)
rm(rawStoreData)

### Checking for missing values - 3 fields have missing data
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

### Fixing 1.1 BasePrice
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

### Fixing 1.2 Price
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

### Creating Discount based features
rawdata_BasePrice_Price$DISCOUNT_PRICE = rawdata_BasePrice_Price$BASE_PRICE - rawdata_BasePrice_Price$PRICE
rawdata_BasePrice_Price$DISCOUNT_PERCENT = (rawdata_BasePrice_Price$DISCOUNT_PRICE/rawdata_BasePrice_Price$BASE_PRICE)*100
rawdata_BasePrice_Price$DISCOUNT = ifelse(rawdata_BasePrice_Price$DISCOUNT_PRICE!=0,1,0)

### re-defining features as factors
rawdata_factors = as.data.frame(unclass(rawdata_BasePrice_Price))
rawdata_factors$FEATURE = as.factor(rawdata_factors$FEATURE)
rawdata_factors$DISPLAY = as.factor(rawdata_factors$DISPLAY)
rawdata_factors$TPR_ONLY = as.factor(rawdata_factors$TPR_ONLY)
rawdata_factors$DISCOUNT = as.factor(rawdata_factors$DISCOUNT)
rawdata_factors$MSA_CODE = as.factor(rawdata_factors$MSA_CODE)
rm(rawdata_BasePrice_Price)

### re-calculating the product size to milli scale 
unique(rawdata_factors$PRODUCT_SIZE)
# 1 oz = 29.5735 ml
# 1 LT = 1000 ml
# 1 ML = 1 ml
# 1 CT = 200 mg
require(tidyr) # splits a col into two based on the regular expression
rawdata_factors = extract(rawdata_factors, PRODUCT_SIZE, c("VOLUME", "V_UNITS"), "([^ ]+) (.*)")
rawdata_factors$VOLUME=as.numeric(rawdata_factors$VOLUME)
rawdata_factors$PRODUct_SIZE_Milli = ifelse(rawdata_factors$V_UNITS=='ML', rawdata_factors$VOLUME,
                                            ifelse(rawdata_factors$V_UNITS=='CT',rawdata_factors$VOLUME*200,
                                                   ifelse(rawdata_factors$V_UNITS=='LT',rawdata_factors$VOLUME*1000,
                                                          ifelse(rawdata_factors$V_UNITS=='OZ',rawdata_factors$VOLUME*29.5735,-1))))

rawdata_factors = rawdata_factors[,-c(25,26)]
write.csv(rawdata_factors,'rawdata_factors.csv',row.names = F)

### Top Store : ANDERSON TOWNE CTR, Top Product : GM HONEY NUT CHEERIOS analysis
##  Building the dataset
TopP_TopS_data = rawdata_factors[(rawdata_factors$STORE_NUM==2277& rawdata_factors$UPC==1600027527),]
TopP_TopS_data$Promo = ifelse(TopP_TopS_data$FEATURE == 0 &
                                TopP_TopS_data$DISPLAY  == 0 &
                                TopP_TopS_data$TPR_ONLY == 0,0,1)
write.csv(TopP_TopS_data,'TopP_TopS_data.csv',row.names = F)
##  Creating BaseLine Spend 
Promdata=TopP_TopS_data[TopP_TopS_data$Promo==1,]
BaseLineSpend_3Y=mean(TopP_TopS_data$SPEND[TopP_TopS_data$Promo==0])
Promdata$INCR_SPEND = Promdata$SPEND - BaseLineSpend_3Y
NonPromdata = TopP_TopS_data[TopP_TopS_data$Promo==0,] %>% 
        group_by(format(WEEK_END_DATE,'%Y'))%>%
        mutate( BaseLineSpend_eachY= mean(SPEND))
unique(NonPromdata$BaseLineSpend_eachY)
write.csv(Promdata,'Promdata.csv',row.names = F)
## analysing SPEND influencer for Top Product in the top Store
require(car) # to set un-ordered factors ranking method, helmert - baseline is one method rest referring to it
options(contrasts = c("contr.helmert", "contr.poly"))
Anova(lm(SPEND~FEATURE+DISPLAY+TPR_ONLY, data = Promdata[,c(7,10,11,12)]),type=3)
summary(lm(SPEND~FEATURE+DISPLAY+TPR_ONLY, data = Promdata[,c(7,10,11,12)]))
Anova(lm(SPEND~FEATURE*DISPLAY, data = Promdata[,c(7,10,11,12)]),type=3)
summary(lm(SPEND~FEATURE*DISPLAY, data = Promdata[,c(7,10,11,12)]))
Anova(lm(SPEND~TPR_ONLY*DISPLAY, data = Promdata[,c(7,10,11,12)]),type=3)
alias(lm(SPEND~TPR_ONLY*DISPLAY, data = Promdata[,c(7,10,11,12)]))
Anova(lm(SPEND~FEATURE*TPR_ONLY, data = Promdata[,c(7,10,11,12)]),type=3)
alias(lm(SPEND~FEATURE*TPR_ONLY, data = Promdata[,c(7,10,11,12)]))
options(contrasts = c("contr.treatment", "contr.poly"))
rm(list=c('Promdata','NonPromdata','TopP_TopS_data'))

### Store Wise Clustering
##  Building a dataframe =, with store details and avg sales details
storedata = rawdata_factors %>%
              group_by(STORE_NUM) %>%
              mutate(AVG_WEEKLY_SPEND = mean(SPEND),
                     AVG_WEEKLY_UNITS_SOLD = mean(UNITS),
                     AVG_WEEKLY_VISITS = mean(VISITS),
                     AVG_WEEKLY_HHS = mean(HHS)) %>%
              dplyr::select(STORE_NUM,STORE_NAME,ADDRESS_CITY_NAME,
                            ADDRESS_STATE_PROV_CODE,MSA_CODE,SEG_VALUE_NAME,
                            PARKING_SPACE_QTY,SALES_AREA_SIZE_NUM,AVG_WEEKLY_BASKETS,
                            AVG_WEEKLY_SPEND,AVG_WEEKLY_UNITS_SOLD,AVG_WEEKLY_VISITS,
                            AVG_WEEKLY_HHS)

storedata = unique(storedata)
storedata$PARKING_FLAG = as.factor(ifelse(is.na(storedata$PARKING_SPACE_QTY),0,1))
storedata$PARKING_NEW = ifelse(is.na(storedata$PARKING_SPACE_QTY),0,storedata$PARKING_SPACE_QTY)
##  Clustering 
require(dummies) ## to reacte dummy variables for factor data
names(storedata[,c(15,8,9,12)])
# temp = dummy.data.frame(as.data.frame(storedata[,c(14,8,9,12)]),sep='_')
# usdm::vif(temp[2:5])
set.seed(1234)
storedata$C4=kmeans(storedata[,c(15,8,9,12)],centers = 4)$cluster
# rm(temp)
## Cluster's dataframe creating
Clusterdata = storedata %>%
                group_by(C4) %>%
                mutate(AVG_WEEKLY_SPEND1 = mean(AVG_WEEKLY_SPEND),
                       AVG_WEEKLY_UNITS_SOLD1 = mean(AVG_WEEKLY_UNITS_SOLD),
                       AVG_WEEKLY_VISITS1 = mean(AVG_WEEKLY_VISITS),
                       AVG_WEEKLY_HHS1 = mean(AVG_WEEKLY_HHS),
                       AVG_WEEKLY_BASKETS1 = mean(AVG_WEEKLY_BASKETS),
                       SALES_AREA_SIZE_NUM1 = mean(SALES_AREA_SIZE_NUM),
                       NUM_STORE_WITH_PARKING = sum(as.numeric(PARKING_FLAG)),
                       NUM_STORES = n() )%>%
                dplyr::select(C4,AVG_WEEKLY_SPEND1,NUM_STORE_WITH_PARKING,NUM_STORES,AVG_WEEKLY_HHS1,AVG_WEEKLY_BASKETS1,
                              AVG_WEEKLY_UNITS_SOLD1,AVG_WEEKLY_VISITS1,SALES_AREA_SIZE_NUM1)
Clusterdata = unique(Clusterdata)
write.csv(Clusterdata,'Clusterdata.csv',row.names = F)
write.csv(storedata,'storedata.csv',row.names = F)
write.csv(storedata[c(1,16)],'Clus.csv',row.names = F)
require(ggplot2)
ggplot(storedata)+
  geom_point(aes(x= SALES_AREA_SIZE_NUM,
                 y=AVG_WEEKLY_BASKETS,
                 color=as.factor(C4),
                 shape = as.factor(PARKING_FLAG),
                 size = AVG_WEEKLY_VISITS))











