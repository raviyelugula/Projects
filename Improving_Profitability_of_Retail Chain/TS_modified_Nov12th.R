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
  
  

