### Focus       		: Feature Engineering on Customer Satisfaction dataset           
### CreatedOn   		: 05-sep-2017 
### Author      		: Ravindranadh 
### Location    		: GreatLakes Institute of Managment, Chennai
### ProjectName 		: 
### Latest Revision : V 1.0

### data set Reading
require(readxl)
Dataset = read_excel('Workingdata.xlsx')
names(Dataset)
colnames(Dataset) = c(colnames(Dataset[,1:17]),'Q1','Q2','Q3','Q4','Q5','Q6','Q7','Q8','Q9','Q10',colnames(Dataset[,28]),'Remarks')

### Missing value Analysis
Missing_data_Check <- function(data_set){
  NA_Count = sapply(data_set,function(y) sum(length(which(is.na(y))))) 
  Null_Count = sapply(data_set,function(y) sum(length(which(is.null(y)))))
  Length0_Count = sapply(data_set,function(y) sum(length(which(length(y)==0))))
  Empty_Count = sapply(data_set,function(y) sum(length(which(y==''))))
  Total_NonData = NA_Count+Null_Count+Length0_Count+Empty_Count
  return( Total_NonData )
}

Missing_Values = Missing_data_Check(Dataset)
MissingValuesPlot = barplot(Missing_Values,col = 'gray',
                            main ='Missing Values across all attributes',
                            xlab = '',ylab = "Missing count",xaxt = 'n')
text(x = MissingValuesPlot, y = Missing_Values,
     label = Missing_Values,col = "red", cex = 0.8)
axis(1, at=MissingValuesPlot, labels=names(Missing_Values), 
     tick=FALSE, las=2, line=-0.5, cex.axis=0.5)

MissingValuesPlotQ = barplot(Missing_Values[18:27],col = 'gray',
                             main ='Missing responses for Questions',
                             xlab = '',ylab = "Missing count",xaxt = 'n')
text(x = MissingValuesPlotQ, y = Missing_Values[18:27],
     label = Missing_Values[18:27],col = "red", cex = 0.8)
axis(1, at=MissingValuesPlotQ, labels=names(Missing_Values[18:27]), 
     tick=FALSE, las=2, line=-0.5, cex.axis=0.5)

missing_all = subset(Dataset, is.na(Dataset$Q1) &
                       is.na(Dataset$Q2)&
                       is.na(Dataset$Q3)&
                       is.na(Dataset$Q4)&
                       is.na(Dataset$Q5)&
                       is.na(Dataset$Q6)&
                       is.na(Dataset$Q7)&
                       is.na(Dataset$Q8)&
                       is.na(Dataset$Q9)&
                       is.na(Dataset$Q10))

missing_anyone = subset(Dataset, is.na(Dataset$Q1) |
                          is.na(Dataset$Q2)|
                          is.na(Dataset$Q3)|
                          is.na(Dataset$Q4)|
                          is.na(Dataset$Q5)|
                          is.na(Dataset$Q6)|
                          is.na(Dataset$Q7)|
                          is.na(Dataset$Q8)|
                          is.na(Dataset$Q9)|
                          is.na(Dataset$Q10))

Qdataset = Dataset[,18:27]
Pdataset = Dataset[,c(1,18:28)]
for(i in 1:10){
  dataset_name = paste('missing_Q',i%%11,sep='')
  temp = subset(Qdataset, is.na(Qdataset[i%%11])&
                  (!is.na(Qdataset[ifelse((i+1)%%10 == 0, 10 ,(i+1)%%10 )])&
                     !is.na(Qdataset[ifelse((i+2)%%10 == 0, 10 ,(i+2)%%10 )])&
                     !is.na(Qdataset[ifelse((i+3)%%10 == 0, 10 ,(i+3)%%10 )])&
                     !is.na(Qdataset[ifelse((i+4)%%10 == 0, 10 ,(i+4)%%10 )])&
                     !is.na(Qdataset[ifelse((i+5)%%10 == 0, 10 ,(i+5)%%10 )])&
                     !is.na(Qdataset[ifelse((i+6)%%10 == 0, 10 ,(i+6)%%10 )])&
                     !is.na(Qdataset[ifelse((i+7)%%10 == 0, 10 ,(i+7)%%10 )])&
                     !is.na(Qdataset[ifelse((i+8)%%10 == 0, 10 ,(i+8)%%10 )])&
                     !is.na(Qdataset[ifelse((i+9)%%10 == 0, 10 ,(i+9)%%10 )])))
  assign(dataset_name, temp)
}
rm(temp)

## Distribution analysis
par(mfrow=c(2,5),oma=c(0,0,2,0))
plot(factor(Dataset$Q1),pch=1,xlab='Question 1')
plot(factor(Dataset$Q2),pch=2,xlab='Question 2')
plot(factor(Dataset$Q3),pch=3,xlab='Question 3')
plot(factor(Dataset$Q4),pch=4,xlab='Question 4')
plot(factor(Dataset$Q5),pch=5,xlab='Question 5')
plot(factor(Dataset$Q6),pch=6,xlab='Question 6')
plot(factor(Dataset$Q7),pch=7,xlab='Question 7')
plot(factor(Dataset$Q8),pch=8,xlab='Question 8')
plot(factor(Dataset$Q9),pch=9,xlab='Question 9')
plot(factor(Dataset$Q10),pch=10,xlab='Question 10')
title("Frequency Distribution for all 10 Questions",outer = T)

plot(factor(Dataset$Q1),pch=1,xlab='Question 1')

Names = c('missing_all','missing_anyone','missing_Q1','missing_Q2','missing_Q3',
          'missing_Q4','missing_Q5','missing_Q6','missing_Q7','missing_Q8',
          'missing_Q9','missing_Q10')
Missing_Counts = c(nrow(missing_all),nrow(missing_anyone),nrow(missing_Q1),nrow(missing_Q2),nrow(missing_Q3),
                   nrow(missing_Q4),nrow(missing_Q5),nrow(missing_Q6),nrow(missing_Q7),nrow(missing_Q8),
                   nrow(missing_Q9),nrow(missing_Q10))
names(Missing_Counts) = Names
MissingCountPlot = barplot(Missing_Counts,col = 'gray',
                           main ='Missing Value trend',
                           xlab = '',ylab = "Missing count",xaxt = 'n')
text(x = MissingCountPlot, y = Missing_Counts,
     label = Missing_Counts,col = "red", cex = 0.8,font = 2)
axis(1, at=MissingCountPlot, labels=names(Missing_Counts), 
     tick=FALSE, las=2, line=-0.5, cex.axis=0.8)

### Correlation Analysis on Questions data
Qdataset_no_missing=Qdataset[!apply(Qdataset, 1, function(x) any(x=="" | is.na(x))),] 
Pdataset_no_missing=Pdataset[!apply(Pdataset, 1, function(x) any(x=="" | is.na(x))),] 

Correlation=cor(Qdataset_no_missing)
require(corrplot)
require(RColorBrewer)

corrplot(Correlation, type="upper",
         method = 'circle' ,order="hclust", add = F,
         col=brewer.pal(n=4, name="RdBu"),  
         outline = T)
rm(list = Names)

### Multicollinearity check
require(usdm)
vif(data.frame(Qdataset_no_missing)) ## VIF >4 can be consider for multicolinearity

### Factor Analysis on Non missing data set of 10 Questions
require(psych)
pca = principal(Pdataset_no_missing[,2:11],nfactors = 10,rotate = 'none')
pca
plot(pca$values,type="b",col = 'tomato',
     xlab = 'Components',ylab = 'Engine Values',
     main = 'Scree plot for all possible components') # Scree Plot

pca_reduced = principal(Pdataset_no_missing[,2:11], nfactors = 6, rotate = 'none')
pca_reduced

pca_rotated = principal(Pdataset_no_missing[,2:11], nfactors = 6, rotate = 'varimax')
pca_rotated

rm(Correlation)
Pdataset_no_missing_scaled = scale(Pdataset_no_missing[,2:12])
Pdataset_no_missing_master = cbind(Pdataset_no_missing,pca_rotated$scores,
                                   Scaled_Satindex=Pdataset_no_missing_scaled[,11])
#orthogonality
round(pca_rotated$r.scores,5) #after factorizaation
round(cor(Pdataset_no_missing_master[13:18]),5) #after factorizaation - same code but default function
round(cor(Pdataset_no_missing_master[2:11]),5) #before factorizaation
#Regression on Factors - entire non missing data 
n = names(Pdataset_no_missing_master[13:18])
formula = as.formula(paste("Scaled_Satindex ~", paste(n, collapse = " + ")))
Linear_regression = lm(formula,
                       data = Pdataset_no_missing_master)
summary(Linear_regression)

### With 2 Factors 
pca_reduced2 = principal(Pdataset_no_missing[,2:11], nfactors = 2, rotate = 'none')
pca_reduced2

pca_rotated2 = principal(Pdataset_no_missing[,2:11], nfactors = 2, rotate = 'varimax')
pca_rotated2

Pdataset_no_missing_scaled2 = scale(Pdataset_no_missing[,2:12])
Pdataset_no_missing_master2 = cbind(Pdataset_no_missing,pca_rotated2$scores,
                                    Scaled_Satindex=Pdataset_no_missing_scaled2[,11])
#orthogonality
round(pca_rotated2$r.scores,5) #after factorizaation
round(cor(Pdataset_no_missing_master2[13:14]),5) #after factorizaation - same code but default function
round(cor(Pdataset_no_missing_master2[2:11]),5) #before factorizaation
#Regression on Factors - entire non missing data 
n2 = names(Pdataset_no_missing_master2[13:14])
formula2 = as.formula(paste("Scaled_Satindex ~", paste(n2, collapse = " + ")))
Linear_regression2 = lm(formula2,
                        data = Pdataset_no_missing_master2)
summary(Linear_regression2)

## Clustering
Qdataset_no_missing=Qdataset[!apply(Qdataset, 1, function(x) any(x=="" | is.na(x))),] 
Cdataset = Dataset[,c(1:6,11:12,28)]
Cdataset_no_missing=Cdataset[!apply(Cdataset,1,function(x) any(x=="" | is.na(x))),]
KMeans_Cdataset = Cdataset_no_missing[,c(4,6,7,8,9)]
KMeans_Cdataset$Month = factor(KMeans_Cdataset$Month,
                               labels = c(1:length(unique(KMeans_Cdataset$Month))))
KMeans_Cdataset$`Planner Group code` = factor(KMeans_Cdataset$`Planner Group code`,
                                              labels = c(1:length(unique(KMeans_Cdataset$`Planner Group code`))))
KMeans_Cdataset$Product = factor(KMeans_Cdataset$Product,
                                 labels = c(1,2))
wcss = vector()
set.seed(123)
for (i in 1:25) wcss[i] = sum(kmeans(KMeans_Cdataset[3:4], i)$withinss)
plot(x = 1:25,
     y = wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')


set.seed(123)
kmeans = kmeans(x = KMeans_Cdataset[3], centers = 4)
y_kmeans = kmeans$cluster

### Missing Value handling - State
Dataset_M = Dataset
Dataset_M[which(Dataset_M$State == 'Chattisgarh'),'State'] = 'Chhattisgarh'
Dataset_M[which(Dataset_M$State == 'Orissa'),'State'] = 'Odisha'
Dataset_M[which(Dataset_M$State == 'W Bengal'),'State'] = 'West Bengal'
Dataset_M[which(Dataset_M$State == 'TN'),'State'] = 'Tamil Nadu'

Dataset_M$State = tolower(trimws(Dataset_M$State))
Dataset_M$`City-final` = tolower(trimws(Dataset_M$`City-final`))
Dataset_M$City = tolower(trimws(Dataset_M$City))
Dataset_M$Location = tolower(trimws(Dataset_M$Location))

States_Cities = read_excel(path = 'Cities_States.xlsx')
States_Cities$`Name of City` = trimws(tolower(States_Cities$`Name of City`),which = 'both')
States_Cities$State = trimws(tolower(States_Cities$State),which = 'both')

Dataset_M$State = trimws(tolower(Dataset_M$State),which = 'both')
Dataset_M$Location = trimws(tolower(Dataset_M$Location),which = 'both')

### Missing States handling with the help of location
require(readxl)
Locations_Assigned = read_excel(path = 'data/Cities_States.xlsx')
excel_sheets(path = 'data/Workingdata.xlsx')
Locations_Missed = read_excel(path = 'data/Workingdata.xlsx',sheet = 'State_City_Missing')

Locations_Assigned$Location = tolower(trimws(Locations_Assigned$Location))
Locations_Missed$Location = tolower(trimws(Locations_Missed$Location))

require(stringdist)
DistanceNameMatrix<-matrix(NA, ncol = length(Locations_Missed$Location),
                           nrow = length(Locations_Assigned$Location))
for(i in 1:length(Locations_Missed$Location)) {
  for(j in 1:length(Locations_Assigned$Location)) { 
    DistanceNameMatrix[j,i]<-stringdist(tolower(Locations_Missed[i,]$Location),
                                        tolower(Locations_Assigned[j,]$Location),
                                        method ='jw')      
  }
}

Match_Location_DF<-NULL
MinName<-apply(DistanceNameMatrix, 1, base::min)
for(i in 1:nrow(DistanceNameMatrix)){
  S2<-match(MinName[i],DistanceNameMatrix[i,])
  S1<-i
  Match_Location_DF<-rbind(data.frame(Missed_Id=S2,Assigned_Id=S1,
                                      Missed=Locations_Missed[S2,]$Location, 
                                      Assigned=Locations_Assigned[S1,]$Location, 
                                      adist=MinName[i],
                                      method='jm'),
                           Match_Location_DF)
}

Match_Location_DF = Match_Location_DF[Match_Location_DF$adist<=0.05,]   
Match_Location_DF = Match_Location_DF[order(Match_Location_DF$Assigned_Id),]
Match_Location_DF = Match_Location_DF[!duplicated(Match_Location_DF[,3:4]),] 

for(i in 1:nrow(Match_Location_DF)){
  temp_Assigned_Id = Match_Location_DF$Assigned_Id[i]
  temp_state = Locations_Assigned$State[temp_Assigned_Id]
  temp_location = as.character(Match_Location_DF$Missed[i])
  Dataset_M = within(Dataset_M,State[Location == temp_location ] <- temp_state) 
}  ### 4546 state are mapped !!!

#write.csv(Dataset_M,'test.csv')

### Missing States handling with the help of Cities
excel_sheets(path = 'data/Workingdata.xlsx')
Cities_Missed = read_excel(path = 'data/Workingdata.xlsx',sheet = 'State_Missing_CityDetails')

Locations_Assigned$Location = tolower(trimws(Locations_Assigned$Location))
Cities_Missed$City = tolower(trimws(Cities_Missed$City))

DistanceNameMatrix<-matrix(NA, ncol = length(Cities_Missed$City),
                           nrow = length(Locations_Assigned$Location))
for(i in 1:length(Cities_Missed$City)) {
  for(j in 1:length(Locations_Assigned$Location)) { 
    DistanceNameMatrix[j,i]<-stringdist(tolower(Cities_Missed[i,]$City),
                                        tolower(Locations_Assigned[j,]$Location),
                                        method ='jw')      
  }
}

Match_Location_DF<-NULL
MinName<-apply(DistanceNameMatrix, 1, base::min)
for(i in 1:nrow(DistanceNameMatrix)){
  S2<-match(MinName[i],DistanceNameMatrix[i,])
  S1<-i
  Match_Location_DF<-rbind(data.frame(Missed_Id=S2,Assigned_Id=S1,
                                      Missed=Cities_Missed[S2,]$City, 
                                      Assigned=Locations_Assigned[S1,]$Location, 
                                      adist=MinName[i],
                                      method='jm'),
                           Match_Location_DF)
}
backup_Match_Location_DF = Match_Location_DF

Match_Location_DF = Match_Location_DF[Match_Location_DF$adist==0,]   
Match_Location_DF = Match_Location_DF[order(Match_Location_DF$Assigned_Id),]
Match_Location_DF = Match_Location_DF[!duplicated(Match_Location_DF[,3:4]),] 

for(i in 1:nrow(Match_Location_DF)){
  temp_Assigned_Id = Match_Location_DF$Assigned_Id[i]
  temp_state = Locations_Assigned$State[temp_Assigned_Id]
  temp_location = as.character(Match_Location_DF$Missed[i])
  Dataset_M = within(Dataset_M,State[Location == temp_location ] <- temp_state) 
}  ### 3257 state are mapped !!!

write.csv(Dataset_M,'Dataset_M.csv',row.names = F)


## Clustering based on States

Originaldata = read.csv('datset_M.csv',header = T)
Originaldata$State = tolower(trimws(Originaldata$State))
Originaldata= Originaldata %>%
  inner_join(OrderState,by='State')
Originaldata=Originaldata[-(which(is.na(Originaldata$Planner.Group.code))),]
Originaldata$StateNames = Originaldata$State
Originaldata$Planner.Group.code = factor(Originaldata$Planner.Group.code,
                                         labels = c(1:length(unique(Originaldata$Planner.Group.code))))
Originaldata$State = factor(Originaldata$State,
                            labels = c(1:22))
Originaldata$Product = factor(Originaldata$Product,
                              labels = c(1:length(unique(Originaldata$Product))))
set.seed(123)
kmeans = kmeans(x = Originaldata[11:12], centers = 4)
Originaldata$clusterCode = kmeans$cluster

Originaldata = Originaldata[,c(11,12,15,30,31,32)]
require(plyr)
require(dplyr)
require(ggplot2)
Originaldata = ddply(Originaldata, .(Planner.Group.code), mutate, count = length(unique(State)))
OrderState = read_excel('StateOrder.xlsx')
Originaldata %>% 
  ggplot(aes(x = State, y = Planner.Group.code,
             color= factor(count),shape =factor(clusterCode)))+
  geom_point()+
  ggtitle('Cluster(Planner_Product) across states')+
  xlab('State Codes')+ylab('Planner Codes')+
  scale_x_discrete(labels = levels(factor(Originaldata$OrderState)))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

Originaldata %>% 
  ggplot(aes(x = State, y = Planner.Group.code,
             color= factor(count)))+
  geom_point()+
  ggtitle('PlannerCodes across states')+
  xlab('State Codes')+ylab('Planner Codes')+
  #scale_x_discrete(labels = levels(factor(Originaldata$OrderState)))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### Individual variable study
Dataset_No_Missing=Dataset[!apply(Dataset, 1, function(x) any(x=="" | is.na(x))),] 
# dim(Dataset)
# [1] 18797    29
# dim(Qdataset_no_missing)
# [1] 17544    10

  

### should work on -- Planner group and State relation

# length(unique(Dataset_M$`Planner Group code`))
# require(dplyr)
# Planner_State_count=Dataset_M %>%
#   group_by(Dataset_M$`Planner Group code`) %>%
#   summarise(ifelse(sum(is.na(State))>0,n_distinct(State)-1,n_distinct(State)))
# Planner_State_count = as.data.frame(Planner_State_count)
# colnames(Planner_State_count) = c('PlannerGroupCode','State_Count')
# Planner_State_count
# 
# Planner_State_count2=Dataset_M %>%select(`Planner Group code`,State)
# group_by(Dataset_M$`Planner Group code`) 
# 
# write.csv(Dataset_M,'tedt.csv')
