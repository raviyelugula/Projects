## Setting up directories
setwd("G:/GL/M_CRM/")
getwd()

#### reading data for MGM HOtel Business
MGM_Hotel_data = read.csv("Data Set - MGM Grand Hotel.csv",header = T)
names(MGM_Hotel_data)

## removing data observations where PLAYERID is NA or NULL
MGM_Hotel_data_Filtered = MGM_Hotel_data[-which(MGM_Hotel_data$PLAYER_ID=="" |  is.na(MGM_Hotel_data$PLAYER_ID) ),]

## Individual NET Profit calculation
MGM_Hotel_data_Filtered$ROOMNET = MGM_Hotel_data_Filtered$ROOMDEBIT - MGM_Hotel_data_Filtered$ROOMCREDIT
MGM_Hotel_data_Filtered$OTHERNET = MGM_Hotel_data_Filtered$OTHERDEBIT - MGM_Hotel_data_Filtered$OTHERCREDIT
MGM_Hotel_data_Filtered$FNBNET = MGM_Hotel_data_Filtered$FNBDEBIT - MGM_Hotel_data_Filtered$FNBCREDIT
MGM_Hotel_data_Filtered$ENTERTNET = MGM_Hotel_data_Filtered$ENTERTNDEBIT - MGM_Hotel_data_Filtered$ENTERTNCREDIT
MGM_Hotel_data_Filtered$RETLNET = MGM_Hotel_data_Filtered$RETLDEBIT - MGM_Hotel_data_Filtered$RETLCREDIT

## Frequency calculation per customer
count = as.data.frame(table(MGM_Hotel_data_Filtered$PLAYER_ID))
names(count) = c('PLAYER_ID', 'FREQUENCY')

## converting the factors to date to apply min and max fun
MGM_Hotel_data_Filtered$FOL_ACT_ARR_DATE = as.Date(MGM_Hotel_data_Filtered$FOL_ACT_ARR_DATE,format = '%d-%b-%Y')
MGM_Hotel_data_Filtered$FOL_ACT_DEP_DATE = as.Date(MGM_Hotel_data_Filtered$FOL_ACT_DEP_DATE,format = '%d-%b-%Y')

## creating groups based in PayerID
library(dplyr)
MGM_Hotel_data_Filtered_GroupBy =
  MGM_Hotel_data_Filtered %>% 
  group_by(PLAYER_ID) %>% 
  summarise(DEBIT_TTL = sum(DEBIT_TTL),
            CREDIT_TTL = sum(CREDIT_TTL),
            FOL_ACT_DEP_DATE = max(FOL_ACT_DEP_DATE),
            FOL_ACT_ARR_DATE = min(FOL_ACT_ARR_DATE),
            TOTAL_CHARGES = sum(TOTAL_CHARGES),
            ROOMNET = sum(ROOMNET),
            OTHERNET = sum(OTHERNET),
            FNBNET = sum(FNBNET),
            ENTERTNET = sum(ENTERTNET),
            RETLNET = sum(RETLNET)
  ) 

## concatinating the Frequency data
MGM_Hotel_data_Filtered_GroupBy = merge(MGM_Hotel_data_Filtered_GroupBy,count,
                                        by.x = 'PLAYER_ID',
                                        by.y = 'PLAYER_ID'
)


## reading casino data base
MGM_Casion_data = read.csv("Data Set - MGM Grand.csv",header = T)

## Frequency calcaulation
casino_count = as.data.frame(table(MGM_Casion_data$ï..PLAYER_ID))
names(casino_count) = c('PLAYER_ID', 'CASINO_FREQUENCY')

## converting factors to date for aggregation functions
MGM_Casion_data$MINDATE = as.Date(MGM_Casion_data$MINDATE,format = '%d-%b-%Y')
MGM_Casion_data$MAXDATE = as.Date(MGM_Casion_data$MAXDATE,format = '%d-%b-%Y')
MGM_Casion_data$BIRTHDAY = as.Date(MGM_Casion_data$BIRTHDAY,format = '%d-%b-%Y')

## Grouping by per Player
MGM_Casino_data_GroupBy =
  MGM_Casion_data %>% 
  group_by(ï..PLAYER_ID) %>% 
  summarise(MINDATE = min(MINDATE),
            MAXDATE = max(MAXDATE),
            TABLETHEO = sum(TABLETHEO),
            TABLEWIN = sum(TABLEWIN),
            SLOTTHEO = sum(SLOTTHEO),
            SLOTACTUAL = sum(SLOTACTUAL),
            ROOM = sum(ROOM),
            FANDB = sum(FANDB),
            SHOW = sum(SHOW),
            OTHER = sum(OTHER),
            BIRTHDAY = max(BIRTHDAY),
            YEAR = max(YEAR),
            TOTALTHEO = sum(TOTALTHEO,na.rm =T),
            TOTALCOMP = sum(TOTALCOMP,na.rm =T)
  ) 


## combining player data with his frequency to CASINO
MGM_Casino_data_GroupBy = merge(MGM_Casino_data_GroupBy,casino_count,
                                by.x = 'ï..PLAYER_ID',
                                by.y = 'PLAYER_ID'
)

## left join of Hotel data and casino data
MGM_final_DF = merge(
  MGM_Hotel_data_Filtered_GroupBy,
  MGM_Casino_data_GroupBy,
  by.x = 'PLAYER_ID',
  by.y = 'ï..PLAYER_ID',
  all.x = T
)

## Average Hotel charges on per person
MGM_final_DF$Monetary_Hotel = MGM_final_DF$TOTAL_CHARGES/MGM_final_DF$FREQUENCY

## number of days from Last depeature to max date in the DB - this is to calculate recency later 
MGM_final_DF$DateDiff = (max(MGM_final_DF$FOL_ACT_DEP_DATE)- MGM_final_DF$FOL_ACT_DEP_DATE)

## Recency calculation
# quantile_R= quantile(MGM_final_DF$DateDiff)
# MGM_final_DF$R = ordered(ifelse(MGM_final_DF$DateDiff <= quantile_R[2], 4 ,
#                                 ifelse(MGM_final_DF$DateDiff <= quantile_R[3],3,
#                                        ifelse(MGM_final_DF$DateDiff <= quantile_R[4], 2,
#                                               ifelse(MGM_final_DF$DateDiff <= quantile_R[5], 1
#                                               ))))
#                          ,levels=c(4,3,2,1))
# table(MGM_final_DF$R)

MGM_final_DF$R = ordered(ifelse(MGM_final_DF$DateDiff <= 365, 4 ,
                                 ifelse(MGM_final_DF$DateDiff <= 620,3,
                                        ifelse(MGM_final_DF$DateDiff <= 900, 2,
                                               ifelse(MGM_final_DF$DateDiff <= 1097, 1
                                               ))))
                          ,levels=c(1,2,3,4))
table(MGM_final_DF$R)

quantile(MGM_final_DF$FREQUENCY)

## Frequency calculation
freq_factor =as.factor(MGM_final_DF$FREQUENCY)
class(freq_factor)
summary(freq_factor)

write.csv(summary(freq_factor),'freq_factor.csv')

MGM_final_DF$Fq = ordered(ifelse(MGM_final_DF$FREQUENCY <= 1, 1 ,
                                 ifelse(MGM_final_DF$FREQUENCY <= 5,2,
                                        ifelse(MGM_final_DF$FREQUENCY <= 41, 3
                                        )))
                          ,levels=c(3,2,1))
table(MGM_final_DF$Fq )

## Profit calculations
MGM_final_DF$ROOMProfit = (MGM_final_DF$ROOMNET * 0.20)
MGM_final_DF$FNBProfit = (MGM_final_DF$FNBNET * 0.28)
MGM_final_DF$ENTERTProfit = (MGM_final_DF$ENTERTNET * 0.50)
MGM_final_DF$RETLProfit = (MGM_final_DF$RETLNET * 0.25)
MGM_final_DF$OTHERProfit = (MGM_final_DF$OTHERNET * 0.20)
MGM_final_DF$TABLEProfit = (MGM_final_DF$TABLETHEO * 0.25)
MGM_final_DF$SLOTProfit = (MGM_final_DF$SLOTACTUAL * 0.10)
MGM_final_DF$TABLEWINProfit = (MGM_final_DF$TABLEWIN * 0.25)
MGM_final_DF$SLOTTHEOProfit = (MGM_final_DF$SLOTTHEO * 0.10)

## Monetory calculation
avgFreq = sum(MGM_final_DF$FREQUENCY)/nrow(MGM_final_DF)
Mon_Mean = avgFreq * mean(MGM_final_DF$Monetary_Hotel)
avgFreq
Mon_Mean
quantile(MGM_final_DF$Monetary_Hotel)
min(MGM_final_DF$Monetary_Hotel)
max(MGM_final_DF$Monetary_Hotel)

# quantile_M= quantile(MGM_final_DF$Monetary_Hotel)
# MGM_final_DF$M = ordered(ifelse(MGM_final_DF$Monetary_Hotel <= quantile_M[2], 1 ,
#                                 ifelse(MGM_final_DF$Monetary_Hotel <= quantile_M[3],2,
#                                        ifelse(MGM_final_DF$Monetary_Hotel <= quantile_M[4], 3,
#                                               ifelse(MGM_final_DF$Monetary_Hotel <= quantile_M[5], 4
#                                               ))))
#                          ,levels=c(4,3,2,1))
# 
# table(MGM_final_DF$M )

quantile_M= quantile(MGM_final_DF$Monetary_Hotel)
MGM_final_DF$M = ordered(ifelse(MGM_final_DF$Monetary_Hotel <= 75, 1 ,
                                ifelse(MGM_final_DF$Monetary_Hotel <= 300,2,
                                       ifelse(MGM_final_DF$Monetary_Hotel <= 1500, 3,
                                              ifelse(MGM_final_DF$Monetary_Hotel <= 3921, 4
                                              ))))
                         ,levels=c(4,3,2,1))

table(MGM_final_DF$M )


## Bucket calculation
MGM_final_DF$bucket= (as.numeric(MGM_final_DF$R) *100)+(as.numeric(MGM_final_DF$Fq) * 10)+(as.numeric(MGM_final_DF$M))
MGM_final_DF$Age = ifelse(((max(MGM_final_DF$FOL_ACT_DEP_DATE)- MGM_final_DF$BIRTHDAY)/365)>10, 
                          ((max(MGM_final_DF$FOL_ACT_DEP_DATE)- MGM_final_DF$BIRTHDAY)/365), NA)


summary(as.factor(MGM_final_DF$bucket))
write.csv(summary(as.factor(MGM_final_DF$bucket)),"bucket_summary.csv")
write.csv(MGM_final_DF,"mgm_final_df.csv")

## Bucket wise data frame 
MGM_BuketWise_data =
  MGM_final_DF %>% 
  group_by(bucket) %>% 
  summarise(ROOMProfit = sum(ROOMProfit),
            FNBProfit = sum(FNBProfit),
            ENTERTProfit = sum(ENTERTProfit),
            RETLProfit = sum(RETLProfit),
            OTHERProfit = sum(OTHERProfit),
            TABLEProfit = sum(TABLEProfit,na.rm =T),
            TABLEWINProfit = sum(TABLEWINProfit,na.rm =T),
            SLOTProfit = sum(SLOTProfit,na.rm =T),
            SLOTTHEOProfit = sum(SLOTTHEOProfit,na.rm =T),
            B_frequency = sum(PLAYER_ID/PLAYER_ID),
            MIN_AGE = min(Age,na.rm =T),
            MAX_AGE = max(Age,na.rm =T),
            Median_AGE = median(Age,na.rm =T),
            TOTALCOMP=sum(TOTALCOMP,na.rm =T),
            TOTALTHEO = sum(TOTALTHEO,na.rm=T)
  ) 


MGM_BuketWise_data$NetCasinoEarnings = MGM_BuketWise_data$TOTALTHEO -MGM_BuketWise_data$TOTALCOMP
MGM_BuketWise_data$CaionProfit = MGM_BuketWise_data$TABLEWINProfit+MGM_BuketWise_data$SLOTProfit
MGM_BuketWise_data$HotelProfit = MGM_BuketWise_data$ROOMProfit+
  MGM_BuketWise_data$FNBProfit+
  MGM_BuketWise_data$ENTERTProfit+
  MGM_BuketWise_data$RETLProfit+
  MGM_BuketWise_data$OTHERProfit
MGM_BuketWise_data$TOTALPROFITS = MGM_BuketWise_data$CaionProfit + MGM_BuketWise_data$HotelProfit
MGM_BuketWise_data$HOTELPROFIT_Share = round((MGM_BuketWise_data$HotelProfit/MGM_BuketWise_data$TOTALPROFITS)*100,2)
MGM_BuketWise_data$CASINOPROFIT_Share = round((MGM_BuketWise_data$CaionProfit/MGM_BuketWise_data$TOTALPROFITS)*100,2)

write.csv(MGM_BuketWise_data,'MGM_BuketWise_data.csv')


















