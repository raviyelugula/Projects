rawdata_factors$Promo = ifelse(rawdata_factors$FEATURE==1 | rawdata_factors$DISPLAY==1 | rawdata_factors$TPR_ONLY==1,1, 0)
Product_list = unique(rawdata$UPC)
Product_NA_data = data.frame(integer(),integer(),integer())
PE = data.frame(integer(),integer())
for(PID in Product_list){
  PID = '1600027527'# '1111085345'
  P = subset(rawdata_factors, UPC== PID) 
  P_Promo = subset(P, Promo == 1)
  P_NoPromo = subset(P, Promo == 0)
  P_PROMO_TREND = P_Promo %>% 
    dplyr::select(WEEK_END_DATE,SPEND) %>%
    dplyr::group_by(WEEK_END_DATE) %>%
    dplyr::mutate(PROMO_SPEND = sum(SPEND)) %>%
    dplyr::select(WEEK_END_DATE,PROMO_SPEND) %>%
    unique()
  P_NoPROMO_TREND = P_NoPromo %>% 
    dplyr::select(WEEK_END_DATE,SPEND) %>%
    dplyr::group_by(WEEK_END_DATE) %>%
    dplyr::mutate(NoPROMO_SPEND = sum(SPEND)) %>%
    dplyr::select(WEEK_END_DATE,NoPROMO_SPEND) %>%
    unique()
  P_Total = P %>%
    dplyr::select(WEEK_END_DATE,SPEND) %>%
    dplyr::group_by(WEEK_END_DATE) %>%
    dplyr::mutate(TOTAL_SPEND = sum(SPEND)) %>%
    dplyr::select(WEEK_END_DATE,TOTAL_SPEND) %>%
    unique()
  P_W_PROMO = P %>%
    dplyr::select(WEEK_END_DATE,Promo) %>%
    dplyr::group_by(WEEK_END_DATE) %>%
    dplyr::mutate(W_PROMO = ifelse(sum(Promo)==0,0,1)) %>%
    dplyr::select(WEEK_END_DATE,W_PROMO) %>%
    unique()
  P_Complete = P_Total %>%
    full_join(P_NoPROMO_TREND,by= 'WEEK_END_DATE') %>%
    full_join(P_PROMO_TREND,by= 'WEEK_END_DATE') %>%
    full_join(P_W_PROMO, by= 'WEEK_END_DATE')
  P_Complete$BASE_SPEND = runmin(P_Complete$TOTAL_SPEND,k=4)
  P_Complete$INC_SPEND = P_Complete$TOTAL_SPEND - P_Complete$BASE_SPEND
  P_Complete$PROMO_EFFECTIVENESS = (P_Complete$INC_SPEND/P_Complete$PROMO_SPEND)*100
  temp =subset(P_Complete, P_Complete$W_PROMO == 1)
  PromEff= mean(temp$PROMO_EFFECTIVENESS,na.rm  = T)
  print.noquote(paste0(PID,' ',round(PromEff,2)))
  PE = rbind(PE,data.frame(PID,PromEff))
}
write.csv(PE,"PE.csv",row.names = F)

temp = o %>% left_join(PE, by = c('productCode'='PID'))






