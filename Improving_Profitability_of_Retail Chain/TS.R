#Read data from Work file
data.work=read.csv("work1.csv")
data.work$timestamp=as.Date(data.work$WEEKEND_DATE,format="%d-%b-%y")
data.work=data.work[-1]

require(dplyr)
new.df = data.work %>% 
          group_by(timestamp) %>% 
          mutate(SPEND_SUM = sum(SPEND))
subs.df=new.df[,25:26]
subs.new=unique(subs.df[,1:2])

library(smooth)
library(forecast)
library(graphics)
library(datasets)

sales.ts=ts(subs.new[,-1],start =c(2009,1),end =c(2011,44),frequency = 52)
#sales.ts=ts(subs.new[,-1],start =c(2009,1),end =c(2011,52),frequency = 52)
plot(sales.ts)
sales.comp<- decompose(sales.ts)
plot(sales.comp,title(main="Decomposition of Sales"))
sales.comp1<- decompose(sales.ts, type = 'multiplicative')
plot(sales.comp1)

require(tseries)
adf.test(sales.ts/scale(sales.ts-sales.comp$seasonal), alternative = "stationary")



j=1
MAPE = numeric()
a1 = numeric()
b1 = numeric()
c1 = numeric()
sales.ts.dev = head(sales.ts,143)
sales.ts.hold = head(sales.ts,5)
for(a in seq(from=0.1,to= 0.9,by=0.1)){
  for(b in seq(from=0.1,to= 0.9,by=0.1)){
    for(c in seq(from=0.1,to= 0.9,by=0.1)){
      HW_Model = HoltWinters(sales.ts.dev, alpha=a, beta=b, gamma=c)
      hold_predict = forecast(HW_Model,5)
      MAPE[j] = sum(abs(hold_predict$mean[1:5]-sales.ts.hold[1:5])/sales.ts.hold[1:5])/length(sales.ts.hold)
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

sales.fit1= HoltWinters(sales.ts, alpha=0.1, beta=0.4, gamma=0.8)
plot(sales.fit1)

# predictive accuracy #
library(forecast)

# predict next 12 future values #
sales.fut= forecast(sales.fit1, 18)
plot(sales.fut)
#sales.fs=ts(subs.new[,-1],start =c(2011,45),end =c(2012,2),frequency = 52)
actual = tail(subs.new[2],8)
forcast = sales.fut$mean[11:18]
err=MAPE(actual$SPEND_SUM,forcast)
err

# ANOVA Test
data.work$DISPLAY = as.factor(data.work$DISPLAY)
require(ez)
anova( aov(SPEND ~ DISPLAY, data=data.work) )
Mean_data.work = data.work %>%
  select(DISPLAY,SPEND) %>%
  group_by(DISPLAY) %>%
  mutate (M = mean(SPEND, na.rm = T))
unique(Mean_data.work[,c(1,3)])




# Differenced Series & Autocorrelation #
d1=diff(sales.ts)
d2=diff(sales.ts, lag=2)
AUTOCRL=acf(sales.ts, lag.max=10) # Autocorrelation 
pacf(sales.ts, lag.max=10) # Partial autocorrelation