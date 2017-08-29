### Focus       		: This file helps to calculate RSI, SMA, EMA 
###                   Momentum, K%, D%, MACD, R%            
### CreatedOn   		: 05-06-2017 
### Author      		: Ravindranadh 
### Location    		: GreatLakes Institute of Managment, Chennai
### ProjectName 		:
### Latest Revision : V 0.1

############################ Single Full Data Frame #######################################

setwd("G:/BHAV/Unzip/")     
getwd()     

read_Multiple_filies <- function(path) {                            ### function definition
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)         ### No factors
  temp_list <- lapply(files, read.csv, stringsAsFactors = F)        ### creating list in each loop
  do.call(rbind, temp_list)                                         ### binding all the list to make it a Dataframe
}

CompleteData <- read_Multiple_filies("G:/BHAV/Unzip/")
CompleteData$TIMESTAMP = as.Date(CompleteData$TIMESTAMP, format="%d-%b-%Y")   ### char to data converstion for easy calculations

#######################################   Individual data frames   ################################

library(RSQLite)   # these 3 required for sqldf package loading
library(proto)
library(gsubfn)
library(tcltk)     ## for proper working of sqldf()

library(sqldf)     # sqldf() is in this package

#companyNames <- sqldf("select distinct SYMBOL from CompleteData limit 5") # can write any kind of select if needed to get the selective companies
#companyNames <- c("MRF","LT","TCS")
#class(companyNames)
#for (i in 1:length(companyNames))    ## creating individual dataframes
#{
#  stock <- paste("df_", companyNames[i], sep = "")
#  assign(stock,sqldf(paste("select SYMBOL,OPEN,CLOSE,TIMESTAMP,TOTTRDQTY from CompleteData where SYMBOL like '",companyNames[i],"'",sep= "")))
#}


Tcs_DF <- sqldf("select SYMBOL,OPEN,CLOSE,HIGH,LOW,TOTTRDVAL, TIMESTAMP,TOTTRDQTY from CompleteData where SYMBOL like 'TCS'")   #### creating single comapny dataframe
sapply(Tcs_DF,class)
Tcs_DF <- transform(Tcs_DF,TIMESTAMP = as.numeric(TIMESTAMP))   ## timestamp numeric conversion for eassy oderding and mapping and roll function takes only numeric matrix

################################## RSI Calculation #####################################

library(dplyr)       ### to create new rows -- delta change in Close price (1day)

Tcs_DF = Tcs_DF[order(Tcs_DF$TIMESTAMP),]

Tcs_DF = Tcs_DF %>% 
  group_by(SYMBOL) %>% 
  mutate(DELTA_CLOSE = CLOSE - lag(CLOSE, default = NA )) 

Tcs_DF = Tcs_DF %>%      ### Defining the change as GAIN/LOSS
  mutate(DELTA_CLOSE_GAIN =ifelse(Tcs_DF$DELTA_CLOSE>=0,Tcs_DF$DELTA_CLOSE, 0 )) %>%
  mutate(DELTA_CLOSE_LOSS = ifelse(Tcs_DF$DELTA_CLOSE<0,-1*Tcs_DF$DELTA_CLOSE, 0 ))

Tcs_DF[is.na(Tcs_DF)] = 0       ### Smoothing the data by replacing the NA values in 1st row as 0.

library(zoo)

Temp= rollapply(Tcs_DF$DELTA_CLOSE_GAIN,14,sum)/14    #### MOving Avgerage of 14days Gain
Tcs_DF$GAIN = c(rep(0,14),Temp[-1])

rm(Temp)

Temp= rollapply(Tcs_DF$DELTA_CLOSE_LOSS,14,sum)/14    #### MOving Avgerage of 14days Loss
Tcs_DF$LOSS = c(rep(0,14),Temp[-1])

rm(Temp)

Tcs_DF$RS = Tcs_DF$GAIN/Tcs_DF$LOSS                   ### RS calculation 
Tcs_DF$RSI = ifelse(Tcs_DF$LOSS ==0, 100, 100-(100/(1+Tcs_DF$RS)))    ### RSI Calculation

Tcs_DF= Tcs_DF[,!names(Tcs_DF) %in% c("DELTA_CLOSE","DELTA_CLOSE_GAIN","DELTA_CLOSE_LOSS","GAIN","LOSS","RS")]  ## removing the helping feilds created of RSI calculation

############################ EMA Calculations  #######################################

days= 20
library(TTR)
Tcs_DF$EMA = EMA(Tcs_DF$CLOSE,n=days,ratio = 2/(1+days))
Tcs_DF$RSI2=RSI(Tcs_DF$CLOSE,n=14)

############################# Moving Avg ###############################################

Tcs_DF$MA_TRDQTY_10 = SMA(Tcs_DF$TOTTRDQTY,10) 
Tcs_DF$MA_TOTTRDVAL_14 = SMA(Tcs_DF$TOTTRDVAL,14)

##################################### Momentum calculation #################################


Tcs_DF = Tcs_DF[order(Tcs_DF$TIMESTAMP),]   ### ordering the data as per timestamp
Tcs_DF = Tcs_DF %>% 
  mutate(DELTA_CLOSE_14D =lag(CLOSE, n=13L, default = NA )) 

Tcs_DF$M = (Tcs_DF$CLOSE/Tcs_DF$DELTA_CLOSE_14D)*100


################################## Stochastic Oscillator K% and D% both slow and fast ############################

library(RcppRoll)

Tcs_DF$HIGH_14D =c(rep(NA,13),roll_max(Tcs_DF$HIGH,14))
Tcs_DF$LOW_14D =c(rep(NA,13),roll_min(Tcs_DF$LOW,14))

Tcs_DF$K = ((Tcs_DF$CLOSE- Tcs_DF$LOW_14D)/(Tcs_DF$HIGH_14D- Tcs_DF$LOW_14D))*100
library(TTR)
Tcs_DF$Slow_K = SMA(Tcs_DF$K, 3)

Tcs_DF$D = SMA(Tcs_DF$K,3)
Tcs_DF$Slow_D = SMA(Tcs_DF$Slow_K,3)

################################# MACD calculation ##########################################

Tcs_DF$EMA_12 = EMA(Tcs_DF$CLOSE,n=12,ratio = 2/(1+12))
Tcs_DF$EMA_26 = EMA(Tcs_DF$CLOSE,n=26,ratio = 2/(1+26))
Tcs_DF$MACD = Tcs_DF$EMA_12- Tcs_DF$EMA_26
Tcs_DF$Signal = EMA(Tcs_DF$MACD,n=9, ratio = 2/(1+9))
Tcs_DF$MACDHis=Tcs_DF$MACD-Tcs_DF$Signal



################################  Larry William's R calculation ############################

Tcs_DF$R = ((Tcs_DF$HIGH_14D - Tcs_DF$CLOSE)/(Tcs_DF$HIGH_14D - Tcs_DF$LOW_14D))*100

Tcs_DF= Tcs_DF[,!names(Tcs_DF) %in% c("HIGH_14D","LOW_14D","EMA_12","EMA_26")]  ## removing the helping feilds created of other calculation
Tcs_DF$TIMESTAMP = as.Date(Tcs_DF$TIMESTAMP)

##############################  Volume Moving Averages ############################################

##Tcs_DF$VMA = Tcs_DF$TOTTRDVAL/Tcs_DF$MA_TOTTRDVAL_14 ## incorrect 
Tcs_DF$VMA = Tcs_DF$TOTTRDQTY/Tcs_DF$MA_TRDQTY_10*100


############################## 3D, 10D 22D Max and Min of CLOSE PRICE #####################

Tcs_DF$CLOSE_Max_3D = c(rep(NA,2), roll_max(lag(Tcs_DF$CLOSE),3))
Tcs_DF$CLOSE_Max_10D = c(rep(NA,9), roll_max(lag(Tcs_DF$CLOSE),10))
Tcs_DF$CLOSE_Max_22D = c(rep(NA,21), roll_max(lag(Tcs_DF$CLOSE),22))

Tcs_DF$CLOSE_Min_3D = c(rep(NA,2), roll_min(lag(Tcs_DF$CLOSE),3))
Tcs_DF$CLOSE_Min_10D = c(rep(NA,9), roll_min(lag(Tcs_DF$CLOSE),10))
Tcs_DF$CLOSE_Min_22D = c(rep(NA,21), roll_min(lag(Tcs_DF$CLOSE),22))

#################################  CCI Commodity Channel Index #####################
 
Tcs_DF$TP = (Tcs_DF$HIGH + Tcs_DF$LOW + Tcs_DF$CLOSE)/3
Tcs_DF$TP_20_SMA = SMA(Tcs_DF$TP,20)
Tcs_DF$TP_ABS_Delta = abs(Tcs_DF$TP - Tcs_DF$TP_20_SMA)

TP_20D_MeanDeviation = sum(Tcs_DF$TP_ABS_Delta,na.rm = T)/20

Tcs_DF$CCI= (Tcs_DF$TP- Tcs_DF$TP_20_SMA)/(0.015 * TP_20D_MeanDeviation)

Tcs_DF= Tcs_DF[,!names(Tcs_DF) %in% c("TP","TP_20_SMA","TP_ABS_Delta")]  ## removing the helping feilds created of RSI calculation

tail(Tcs_DF[,c("RSI","RSI2","MACDHis","TIMESTAMP")],20)


#write.csv(Tcs_DF,'G:/testing.csv')

Tcs_DF_backup = Tcs_DF





Tcs_DF = Tcs_DF_backup
names(Tcs_DF)
Tcs_DF$RSI = round(Tcs_DF$RSI,1)
#Tcs_DF= Tcs_DF[,names(Tcs_DF) %in% c("TIMESTAMP","RSI")]

plot(Tcs_DF$TIMESTAMP, Tcs_DF$RSI, type = 'l')
abline(h=seq(30,70,by = 20), col ="tomato")


library(zoo)
Tcs_DF$Current = Tcs_DF$RSI
Tcs_DF$Previous = lag(Tcs_DF$Current)
Tcs_DF$CuBand = ifelse(Tcs_DF$Current<=30,20,
                       ifelse(Tcs_DF$Current<=50,40,
                              ifelse(Tcs_DF$Current<=70,60,
                                     ifelse(Tcs_DF$Current<=100,80,-100))))
Tcs_DF$PrBand =lag(Tcs_DF$CuBand)


BoundCrossed <- function(pre,curr){
  return( ifelse(pre==curr & curr == 80, 70,
                 ifelse(pre==curr & curr == 60, NA,
                        ifelse(pre==curr & curr == 40, NA,
                               ifelse(pre==curr & curr == 20, NA,
                                      ifelse(pre==80 & curr == 60, 70,
                                             ifelse(pre==80 & curr == 40, 50,
                                                    ifelse(pre==80 & curr == 20, 30,
                                                           ifelse(pre==60 & curr == 40, 50,
                                                                  ifelse(pre==60 & curr == 20, 30,
                                                                         ifelse(pre==40 & curr == 20, 30,
                                                                                ifelse(pre==20 & curr == 40, 30,
                                                                                       ifelse(pre==20 & curr == 60, 50,
                                                                                              ifelse(pre==20 & curr == 80, 70,
                                                                                                     ifelse(pre==40 & curr == 60, 50,
                                                                                                            ifelse(pre==40 & curr == 80, 70,
                                                                                                                   ifelse(pre==30 & curr == 80, 70,NA))))))))))))))))
  )
}


Tcs_DF$Bound = BoundCrossed(pre = Tcs_DF$PrBand, curr = Tcs_DF$CuBand)
library(zoo)
Tcs_DF$BoundNew = na.locf(Tcs_DF$Bound, na.rm = F)

RSI_Rule_temp <- function(pre, curr, preBound, preBoundReg){
  return(
    ifelse((pre<=30 & curr >=30 & curr<50)|(pre<=50 & curr>=50 & curr <70 & preBound =="SELL_BUY"), 'BUY',
           ifelse(((pre>50 & pre<=70 & curr >=70)&(preBound=="BUY"))|(pre<=50 & curr>=50 & curr <70 & preBound =="BUY"), 'BUY_SELL',
                  ifelse(((pre >=70 & curr <=70 & curr >50)&(preBound=="BUY_SELL" & preBoundReg == 60))|(curr<=50 & pre>=50 &pre<70 & preBound=="BUY_SELL" &preBoundReg==80), 'SELL',
                         ifelse((pre < 50 & pre >=30 & curr <=30 & preBound=="SELL" & preBoundReg== 40)|(curr<=50 & pre>=50 &pre<70 & preBound=="SELL"& preBoundReg== 60), 'SELL_BUY','NA'))))
  )
}

Tcs_DF$RSI_Decision[1]="NA"
Tcs_DF$pretemp[1] ="NA"
Tcs_DF$pretempReg[1]= NA
Tcs_DF$pretemp[2] ="NA"
Tcs_DF$pretempReg[2]= NA

for(i in 2:nrow(Tcs_DF)){
  Tcs_DF$RSI_Decision[i] = RSI_Rule_temp(Tcs_DF$PrBand[i], Tcs_DF$CuBand[i], Tcs_DF$pretemp[i], Tcs_DF$pretempReg[i])
  if(i != nrow(Tcs_DF)){
    Tcs_DF$pretemp[i+1]= ifelse(Tcs_DF$RSI_Decision[i]!="NA",Tcs_DF$RSI_Decision[i],Tcs_DF$pretemp[i])
    Tcs_DF$pretempReg[i+1]= ifelse( Tcs_DF$RSI_Decision[i]!="NA",Tcs_DF$CuBand[i],Tcs_DF$pretempReg[i] )
  }
  
}

names(Tcs_DF)
Tcs_DF$RSI_Decision[which(Tcs_DF$RSI_Decision=="NA")] =""



plot(x=Tcs_DF$TIMESTAMP,y=Tcs_DF$RSI,type = "l")
abline(h=seq(30,70,by = 20), col ="tomato")
text(x=Tcs_DF$TIMESTAMP,y=Tcs_DF$RSI,labels= Tcs_DF$RSI_Decision)


write.csv(Tcs_DF,'G:/BHAV/rsitest.csv')
Tcs_DF= Tcs_DF[,!names(Tcs_DF) %in% c("Current","Previous","CuBand","PrBand","Bound","BoundNew","pretemp","pretempReg")]

## MACDHis Decision coding ----

Tcs_DF = Tcs_DF_backup
names(Tcs_DF)
Tcs_DF$MACDHis = round(Tcs_DF$MACDHis,1)
Tcs_DF= Tcs_DF[,names(Tcs_DF) %in% c("TIMESTAMP","MACDHis")]


names(Tcs_DF)
tail(Tcs_DF,30)
Tcs_DF$MACDHis_CuSign = ifelse(Tcs_DF$MACDHis>=0 , 1, 0)
Tcs_DF$MACDHis_PreSign = lag(Tcs_DF$MACDHis_CuSign)

MACDHis_Rule <- function(Pre, Cur, PreDec, Cur_V, Pre_v){
  return(
    ifelse(Pre!=Cur & Pre==1, "SELL",
           ifelse(Pre!=Cur & Cur==1 , "BUY",
          ifelse(Pre==Cur & Pre == 0 & PreDec == "SELL" & Cur_V >= Pre_v, "NA",
                         ifelse(Pre==Cur & Pre == 0 & PreDec == "SELL" & Cur_V < Pre_v, "LM",
                                ifelse(Pre==Cur & Pre == 0 & PreDec == "LM" & Cur_V <= Pre_v, "NA",
                                       ifelse(Pre==Cur & Pre == 0 & PreDec == "LM" & Cur_V > Pre_v, "LC",
                                              ifelse(Pre==Cur & Pre == 0 & PreDec == "LC" & Cur_V >= Pre_v, "NA",
                                                     ifelse(Pre==Cur & Pre == 0 & PreDec == "LC" & Cur_V < Pre_v, "LM",
          ifelse(Pre==Cur & Pre == 1 & PreDec == "BUY" & Cur_V >= Pre_v, "NA",
                 ifelse(Pre==Cur & Pre == 1 & PreDec == "BUY" & Cur_V < Pre_v, "UM",
                        ifelse(Pre==Cur & Pre == 1 & PreDec == "UM" & Cur_V <= Pre_v, "NA",
                               ifelse(Pre==Cur & Pre == 1 & PreDec == "UM" & Cur_V > Pre_v, "UC",
                                      ifelse(Pre==Cur & Pre == 1 & PreDec == "UC" & Cur_V >= Pre_v, "NA",
                                             ifelse(Pre==Cur & Pre == 1 & PreDec == "UC" & Cur_V < Pre_v, "UM","NA"))))))))))))))
  )
}

Tcs_DF$MACDHis_Decision[1] = "NA"
Tcs_DF$MACDHis_PreDec[1] = "NA"
Tcs_DF$MACDHis_PreDec[2] = "NA"

for(i in 2:nrow(Tcs_DF)){
  Tcs_DF$MACDHis_Decision[i] = MACDHis_Rule(Tcs_DF$MACDHis_PreSign[i],Tcs_DF$MACDHis_CuSign[i],
                                         Tcs_DF$MACDHis_PreDec[i],abs(Tcs_DF$MACDHis[i]),abs(Tcs_DF$MACDHis[i-1]))
  if(i != nrow(Tcs_DF)){
    Tcs_DF$MACDHis_PreDec[i+1] = ifelse( Tcs_DF$MACDHis_Decision[i]!="NA", Tcs_DF$MACDHis_Decision[i],Tcs_DF$MACDHis_PreDec[i] )
  }
}

Tcs_DF$MACDHis_Decision[which(Tcs_DF$MACDHis_Decision=="NA")]= ""

#Tcs_DF = Tcs_DF_backup
Tcs_DF2= Tcs_DF
# 40- 75

plot(x= Tcs_DF2$TIMESTAMP, y = Tcs_DF2$MACDHis , type = 'l')
abline(h = 0, col='springgreen3')
text(x= Tcs_DF2$TIMESTAMP, y = Tcs_DF2$MACDHis, labels = Tcs_DF2$MACDHis_Decision)

write.csv(Tcs_DF2,'G:/BHAV/macdhis.csv')
 


















