<<<<<<< HEAD
require(usdm)
require(forecast)
require(ggplot2)
require(dplyr)
data = read.csv("work1.csv")
data$WEEKEND_DATE=as.Date(data$WEEKEND_DATE,format="%d-%b-%y")

P1 = data 
P1= subset(data, UPC== '1600027527')  
P1_data = P1 %>%
            dplyr::select(WEEKEND_DATE,SPEND,FEATURE,DISPLAY,TPR_ONLY,
                   BASE_PRICE,PRICE) %>%
            group_by(WEEKEND_DATE) %>%
            mutate(W_TOTAL_SPEND = sum(SPEND),
                   W_FEATURE = ifelse(sum(FEATURE)==0,0,1),
                   W_DISPLAY = ifelse(sum(DISPLAY)==0,0,1),
                   W_TPR_ONLY = ifelse(sum(FEATURE)==0,0,1),
                   W_BASE_PRICE = mean(BASE_PRICE),
                   W_PRICE = mean(PRICE)) %>%
            dplyr::select(WEEKEND_DATE,W_TOTAL_SPEND,W_FEATURE,
                   W_DISPLAY,W_TPR_ONLY,W_BASE_PRICE,W_PRICE) %>%
            unique()%>%
            mutate(W_PROMO = ifelse(W_FEATURE+W_DISPLAY+W_TPR_ONLY == 0,0,1),
                   W_DISCOUNT = W_BASE_PRICE-W_PRICE )%>%
            arrange(WEEKEND_DATE)

TS_data = ts(P1_data$W_TOTAL_SPEND, start = c(2009,1), end = c(2011,52), frequency = 52)
#plot(TS_data)
Decomposition = decompose(TS_data)
#Decomposition$seasonal
#plot(Decomposition$seasonal)
P1_data$W_Seasonal = Decomposition$seasonal

vif(data.frame(P1_data[,c(2,3,4,5,6,10)]))
vif(data.frame(P1_data[,c(2,3,4,6,10)])) # removing TPR_ONLY due to collinearity
LR_Model = lm(W_TOTAL_SPEND ~ W_BASE_PRICE+W_FEATURE+W_DISPLAY+W_Seasonal,
              data = P1_data)
summary(LR_Model)
Coef = LR_Model$coefficients

Base_Line = Coef[1]+Coef[2]*(P1_data$W_BASE_PRICE)+(Coef[5]*P1_data$W_Seasonal)
INC = Coef[3]*(P1_data$W_FEATURE)+(Coef[4]*P1_data$W_DISPLAY)

P1_data$BASE_LINE = Base_Line
P1_data$INCREMENTAL = INC

ggplot(P1_data, aes(x=WEEKEND_DATE ))+
  geom_line(aes(y=W_TOTAL_SPEND),color='black',show.legend = F)+
  geom_line(aes(y=BASE_LINE),color='blue',show.legend = F)+
  geom_line(aes(y=INCREMENTAL),color='red',show.legend = F)+
  ylab('SALES')











TS_data_train = ts(P1_data$W_TOTAL_SPEND[1:144], start = c(2009,1), end = c(2011,40), frequency = 52)
TS_data_test = ts(P1_data$W_TOTAL_SPEND[145:156], start = c(2011,41), end = c(2011,52), frequency = 52)
plot(TS_data_train)
plot(TS_data_test)




ARIMA_Train=auto.arima(TS_data_train)
Predicted_SPEND = forecast::forecast(ARIMA_Train, h=12 )
Acc=accuracy(TS_data_test,Predicted_SPEND$mean)
plot(Predicted_SPEND)
Acc

ARIMA_Full = auto.arima(TS_data)
Forecast_12W = forecast::forecast(ARIMA_Full, h=12 )
Forecast_12W$mean
plot(Forecast_12W$mean,type='b', xlab='TIME',ylab='SPEND',main='Forecast for 12 Weeks overall Spend')

Product_forecast = data.frame(numeric())
P_TOP_LIST = c('1600027527','3800031838','7192100339','1600027528','1600027564','3800031829','3800039118','7192100337','1111085350','88491201426','1111009477')
for(Product in P_TOP_LIST){
  print(Product)
  P1= subset(data, UPC== Product)  
  P1_data = P1 %>%
    dplyr::select(WEEKEND_DATE,SPEND,FEATURE,DISPLAY,TPR_ONLY,
                  BASE_PRICE,PRICE) %>%
    group_by(WEEKEND_DATE) %>%
    mutate(W_TOTAL_SPEND = sum(SPEND),
           W_FEATURE = ifelse(sum(FEATURE)==0,0,1),
           W_DISPLAY = ifelse(sum(DISPLAY)==0,0,1),
           W_TPR_ONLY = ifelse(sum(FEATURE)==0,0,1),
           W_BASE_PRICE = mean(BASE_PRICE),
           W_PRICE = mean(PRICE)) %>%
    dplyr::select(WEEKEND_DATE,W_TOTAL_SPEND,W_FEATURE,
                  W_DISPLAY,W_TPR_ONLY,W_BASE_PRICE,W_PRICE) %>%
    unique()%>%
    mutate(W_PROMO = ifelse(W_FEATURE+W_DISPLAY+W_TPR_ONLY == 0,0,1),
           W_DISCOUNT = W_BASE_PRICE-W_PRICE )%>%
    arrange(WEEKEND_DATE)
  
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

=======
require(usdm)
require(dplyr)
require(forecast)
require(ggplot2)
data = read.csv("work1.csv")
data$WEEKEND_DATE=as.Date(data$WEEKEND_DATE,format="%d-%b-%y")
P1 = subset(data, UPC=='1600027527') 
P1_data = P1 %>%
            select(WEEKEND_DATE,SPEND,FEATURE,DISPLAY,TPR_ONLY,
                   BASE_PRICE,PRICE) %>%
            group_by(WEEKEND_DATE) %>%
            mutate(W_TOTAL_SPEND = sum(SPEND),
                   W_FEATURE = ifelse(sum(FEATURE)==0,0,1),
                   W_DISPLAY = ifelse(sum(DISPLAY)==0,0,1),
                   W_TPR_ONLY = ifelse(sum(FEATURE)==0,0,1),
                   W_BASE_PRICE = mean(BASE_PRICE),
                   W_PRICE = mean(PRICE)) %>%
            select(WEEKEND_DATE,W_TOTAL_SPEND,W_FEATURE,
                   W_DISPLAY,W_TPR_ONLY,W_BASE_PRICE,W_PRICE) %>%
            unique()%>%
            mutate(W_PROMO = ifelse(W_FEATURE+W_DISPLAY+W_TPR_ONLY == 0,0,1),
                   W_DISCOUNT = W_BASE_PRICE-W_PRICE )%>%
            arrange(WEEKEND_DATE)

TS_data = ts(P1_data$W_TOTAL_SPEND, start = c(2009,1), end = c(2011,52), frequency = 52)
plot(TS_data)
Decomposition = decompose(TS_data)
Decomposition$seasonal
plot(Decomposition$seasonal)

P1_data$W_Seasonal = Decomposition$seasonal

vif(data.frame(P1_data[,c(2,3,4,5,6,10)]))
vif(data.frame(P1_data[,c(2,3,4,6,10)])) # removing TPR_ONLY due to collinearity
LR_Model = lm(W_TOTAL_SPEND ~ W_BASE_PRICE+W_FEATURE+W_DISPLAY+W_Seasonal,
              data = P1_data)
summary(LR_Model)
Coef = LR_Model$coefficients

Base_Line = Coef[1]+Coef[2]*(P1_data$W_BASE_PRICE)+(Coef[5]*P1_data$W_Seasonal)
INC = Coef[3]*(P1_data$W_FEATURE)+(Coef[4]*P1_data$W_DISPLAY)

P1_data$BASE_LINE = Base_Line
P1_data$INCREMENTAL = INC

ggplot(P1_data, aes(x=WEEKEND_DATE ))+
  geom_line(aes(y=W_TOTAL_SPEND),color='black',show.legend = F)+
  geom_line(aes(y=BASE_LINE),color='blue',show.legend = F)+
  geom_line(aes(y=INCREMENTAL),color='red',show.legend = F)+
  ylab('SALES')
  
  

>>>>>>> 5de802d22b74125f5835b687353365c79d80df95
