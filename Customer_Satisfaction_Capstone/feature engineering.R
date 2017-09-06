### Focus       		: Feature Engineering on Customer Satisfaction dataset           
### CreatedOn   		: 05-sep-2017 
### Author      		: Ravindranadh 
### Location    		: GreatLakes Institute of Managment, Chennai
### ProjectName 		:
### Latest Revision : V 1.0


Missing_data_Check <- function(data_set){
  NA_Count = sapply(data_set,function(y) sum(length(which(is.na(y))))) 
  Null_Count = sapply(data_set,function(y) sum(length(which(is.null(y)))))
  Length0_Count = sapply(data_set,function(y) sum(length(which(length(y)==0))))
  Empty_Count = sapply(data_set,function(y) sum(length(which(y==''))))
  Total_NonData = NA_Count+Null_Count+Length0_Count+Empty_Count
  return( Total_NonData )
}

library(readxl)
dataset = read_excel('Workingdata.xlsx')

if(length(which(Missing_data_Check(dataset)>0))==0){
  print("No Missing data")
}else{
  Missing_data_Check(dataset)
}

AllQuestionsMissing = subset(dataset, is.na(dataset$`q3.1 - Understanding service needs and providing estimates for cost and repair time`) &
               is.na(dataset$`q3.2 - Providing you with a clear explanation of job done including tips for maintenance`)&
               is.na(dataset$`q3.3 - Education on the advantages of using genuine spares`)&
               is.na(dataset$`q3.4 - Actual time taken for delivery compared to the estimated repair time`)&
               is.na(dataset$`q3.5 - Reasonableness of the costs charged for the job done`)&
               is.na(dataset$`q4.1 - Your overall experience at the workshop`)&
               is.na(dataset$`q4.2 - Courtesy provided to you by our service staff`)&
               is.na(dataset$`q4.3 - Locational convenience, seating arrangement, facilities and general environment`)&
               is.na(dataset$`q6a - How satisfied are you with the work carried-out by Lucas Indian Service?`)&
               is.na(dataset$`q6b-Based on your experience how likely are you to recommend the workshop for maintenance and repair to others?`))

AnyOneQuestionsMissing = subset(dataset, is.na(dataset$`q3.1 - Understanding service needs and providing estimates for cost and repair time`) |
                               is.na(dataset$`q3.2 - Providing you with a clear explanation of job done including tips for maintenance`)|
                               is.na(dataset$`q3.3 - Education on the advantages of using genuine spares`)|
                               is.na(dataset$`q3.4 - Actual time taken for delivery compared to the estimated repair time`)|
                               is.na(dataset$`q3.5 - Reasonableness of the costs charged for the job done`)|
                               is.na(dataset$`q4.1 - Your overall experience at the workshop`)|
                               is.na(dataset$`q4.2 - Courtesy provided to you by our service staff`)|
                               is.na(dataset$`q4.3 - Locational convenience, seating arrangement, facilities and general environment`)|
                               is.na(dataset$`q6a - How satisfied are you with the work carried-out by Lucas Indian Service?`)|
                               is.na(dataset$`q6b-Based on your experience how likely are you to recommend the workshop for maintenance and repair to others?`))

Qdataset = dataset[,18:27]


