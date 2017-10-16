data = read.csv(file = 'C:/Users/ravin/Documents/GitHub/Projects/Improving_Profitability_of_Retail Chain/work1.csv')
names(data)
.arg1 = sum(data$SPEND)
a <- rep(1, length(.arg1))
a[findpeaks(.arg1,
            threshold=quantile(.arg1,.005)
            ,sortstr=FALSE)[,2]]=0
a

quantile(.arg1,.38)
quantile(c(1,2,3,4,5,6,7,8,9,10),.99)





