### Focus       		: Feature Engineering on Customer Satisfaction dataset           
### CreatedOn   		: 05-sep-2017 
### Author      		: Ravindranadh 
### Location    		: GreatLakes Institute of Managment, Chennai
### ProjectName 		:
### Latest Revision : V 1.0

### data set Reading
require(readxl)
dataset = read_excel('Workingdata.xlsx')
names(dataset)
colnames(dataset) = c(colnames(dataset[,1:17]),'Q1','Q2','Q3','Q4','Q5','Q6','Q7','Q8','Q9','Q10',colnames(dataset[,28]),'Remarks')

### Missing value Analysis
Missing_data_Check <- function(data_set){
  NA_Count = sapply(data_set,function(y) sum(length(which(is.na(y))))) 
  Null_Count = sapply(data_set,function(y) sum(length(which(is.null(y)))))
  Length0_Count = sapply(data_set,function(y) sum(length(which(length(y)==0))))
  Empty_Count = sapply(data_set,function(y) sum(length(which(y==''))))
  Total_NonData = NA_Count+Null_Count+Length0_Count+Empty_Count
  return( Total_NonData )
}

Missing_Values = Missing_data_Check(dataset)
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

missing_all = subset(dataset, is.na(dataset$Q1) &
                       is.na(dataset$Q2)&
                       is.na(dataset$Q3)&
                       is.na(dataset$Q4)&
                       is.na(dataset$Q5)&
                       is.na(dataset$Q6)&
                       is.na(dataset$Q7)&
                       is.na(dataset$Q8)&
                       is.na(dataset$Q9)&
                       is.na(dataset$Q10))

missing_anyone = subset(dataset, is.na(dataset$Q1) |
                          is.na(dataset$Q2)|
                          is.na(dataset$Q3)|
                          is.na(dataset$Q4)|
                          is.na(dataset$Q5)|
                          is.na(dataset$Q6)|
                          is.na(dataset$Q7)|
                          is.na(dataset$Q8)|
                          is.na(dataset$Q9)|
                          is.na(dataset$Q10))

Qdataset = dataset[,18:27]
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

# names = c('missing_all','missing_anyone','missing_Q1','missing_Q2','missing_Q3',
#           'missing_Q4','missing_Q5','missing_Q6','missing_Q7','missing_Q8',
#           'missing_Q9','missing_Q10')
# for(i in 1:12){
#   j = nrow(noquote(names[i]))
# }


Qdataset_no_missing=Qdataset[!apply(Qdataset, 1, function(x) any(x=="" | is.na(x))),] 
require(psych)
colnames(Qdataset_no_missing) = c('Q1','Q2','Q3','Q4','Q5','Q6','Q7','Q8','Q9','Q10')
pca = principal(Qdataset_no_missing,nfactors = ncol(Qdataset_no_missing),rotate = 'none')
pca

pca_1 = principal(Qdataset_no_missing, nfactors = 1, rotate = 'none')
pca_1

pca_rotated = principal(Qdataset_no_missing, nfactors = 1, rotate = 'varimax')
pca_rotated

Correlation=cor(Qdataset_no_missing)

require(corrplot)
require(RColorBrewer)

corrplot(Correlation, type="upper",
         method = 'circle' ,order="hclust", add = F,
         col=brewer.pal(n=4, name="RdBu"),  
         outline = T)

