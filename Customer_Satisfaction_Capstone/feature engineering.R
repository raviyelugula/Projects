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
Correlation=cor(Qdataset_no_missing)
require(corrplot)
require(RColorBrewer)

corrplot(Correlation, type="upper",
         method = 'circle' ,order="hclust", add = F,
         col=brewer.pal(n=4, name="RdBu"),  
         outline = T)

### Factor Analysis on Non missing data set of 10 Questions
Qdataset_no_missing=Qdataset[!apply(Qdataset, 1, function(x) any(x=="" | is.na(x))),] 
require(psych)
pca = principal(Qdataset_no_missing,nfactors = ncol(Qdataset_no_missing),rotate = 'none')
pca

pca_reduced = principal(Qdataset_no_missing, nfactors = 1, rotate = 'none')
pca_reduced

pca_rotated = principal(Qdataset_no_missing, nfactors = 1, rotate = 'varimax')
pca_rotated


