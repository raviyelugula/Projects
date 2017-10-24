require(readxl) # read excel files
require(dplyr)
excel_sheets('dunnhumby - Breakfast at the Frat.xlsx')
rawdata = read_excel(path = 'dunnhumby - Breakfast at the Frat.xlsx', sheet = 'dh Transaction Data')
names(rawdata)
rawdata = rawdata %>% 
              left_join(read_excel(path = 'dunnhumby - Breakfast at the Frat.xlsx', sheet = 'dh Store Lookup'),
                        by = c('STORE_NUM'='STORE_ID')) %>%
              left_join(read_excel(path = 'dunnhumby - Breakfast at the Frat.xlsx', sheet = 'dh Products Lookup'),
                        by = 'UPC')
Missing_data_Check <- function(data_set){
  NA_Count = sapply(data_set,function(y) sum(length(which(is.na(y))))) 
  Null_Count = sapply(data_set,function(y) sum(length(which(is.null(y)))))
  Length0_Count = sapply(data_set,function(y) sum(length(which(length(y)==0))))
  Empty_Count = sapply(data_set,function(y) sum(length(which(y==''))))
  Total_NonData = NA_Count+Null_Count+Length0_Count+Empty_Count
  return( Total_NonData )
}
Missing_data_Check(rawdata)
glimpse(rawdata)
 


