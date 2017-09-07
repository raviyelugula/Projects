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
Correlation=cor(Qdataset_no_missing)
require(corrplot)
require(RColorBrewer)

corrplot(Correlation, type="upper",
         method = 'circle' ,order="hclust", add = F,
         col=brewer.pal(n=4, name="RdBu"),  
         outline = T)

### Factor Analysis on Non missing data set of 10 Questions
require(psych)
pca = principal(Qdataset_no_missing,nfactors = ncol(Qdataset_no_missing),rotate = 'none')
pca

pca_reduced = principal(Qdataset_no_missing, nfactors = 1, rotate = 'none')
pca_reduced

pca_rotated = principal(Qdataset_no_missing, nfactors = 1, rotate = 'varimax')
pca_rotated

### Missing Value handling - State
Dataset_M = Dataset
Dataset_M[which(Dataset_M$State == 'Chattisgarh'),'State'] = 'Chhattisgarh'
Dataset_M[which(Dataset_M$State == 'Orissa'),'State'] = 'Odisha'
Dataset_M[which(Dataset_M$State == 'W Bengal'),'State'] = 'West Bengal'
Dataset_M[which(Dataset_M$State == 'TN'),'State'] = 'Tamil Nadu'

States_Cities = read_excel(path = 'Cities_States.xlsx')
States_Cities$`Name of City` = trimws(toupper(States_Cities$`Name of City`),which = 'both')
States_Cities$State = trimws(toupper(States_Cities$State),which = 'both')

Dataset_M$State = trimws(toupper(Dataset_M$State),which = 'both')
Dataset_M$Location = trimws(toupper(Dataset_M$Location),which = 'both')

### Missing location handling
require(readxl)
Locations_Assigned = read_excel(path = 'data/Cities_States.xlsx')
excel_sheets(path = 'data/Workingdata.xlsx')
Locations_Missed = read_excel(path = 'data/Workingdata.xlsx',sheet = 'Locations_Missed')

Locations_Assigned$Location = tolower(trimws(Locations_Assigned$Location))
Locations_Missed$Location = tolower(trimws(Locations_Missed$Location))

a = vector()
for(i in 1:nrow(Locations_Assigned)){
  a[i] = stringdist(Locations_Missed[15,1],
                    Locations_Assigned[i,]$Location,
                    method = 'jw')  
}

Locations_Missed[15,1]
Locations_Assigned[which(a %in% min(a)),]$Location
min(a)

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
  Match_Location_DF<-rbind(data.frame(S2=S2,S1=S1,
                                      s2name=Locations_Missed[S2,]$Location, 
                                      s1name=Locations_Assigned[S1,]$Location, 
                                      adist=MinName[i],
                                      method='jm'),
                           Match_Location_DF)
}
View(Match_Location_DF)
                                    

