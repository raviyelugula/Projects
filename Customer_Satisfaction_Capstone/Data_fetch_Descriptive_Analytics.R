### Focus       		: Find missing values and individual frequencies analysis
### CreatedOn   		: 05-sep-2017 
### Author      		: Ravindranadh 
### Location    		: GreatLakes Institute of Managment, Chennai
### ProjectName 		: 
### Latest Revision : V 1.0

require(plyr)
require(dplyr)
require(ggplot2)
require(readxl)
require(corrplot)
require(RColorBrewer)
require(usdm)
require(psych)
require(stringdist)

barColour = 'royalblue'
bgColour = 'gray75'

par(bg = bgColour)

### Data set reading ----
require(readxl)
Dataset = read_excel('Workingdata.xlsx')
names(Dataset)
colnames(Dataset) = c(colnames(Dataset[,1:17]),'Q1','Q2','Q3','Q4','Q5','Q6','Q7','Q8','Q9','Q10',colnames(Dataset[,28]),'Remarks')

### Missing data analysis ----
Missing_data_Check <- function(data_set){
  NA_Count = sapply(data_set,function(y) sum(length(which(is.na(y))))) 
  Null_Count = sapply(data_set,function(y) sum(length(which(is.null(y)))))
  Length0_Count = sapply(data_set,function(y) sum(length(which(length(y)==0))))
  Empty_Count = sapply(data_set,function(y) sum(length(which(y==''))))
  Total_NonData = NA_Count+Null_Count+Length0_Count+Empty_Count
  return( Total_NonData )
}
Missing_Values = Missing_data_Check(Dataset)
ValuesBarPlot <- function(intVector,title){
  Plot = barplot(intVector,col = barColour,
                              main = title,
                              xlab = '',ylab = "Missing count",xaxt = 'n')
  text(x = Plot, y = intVector,
       label = intVector,col = "red", cex = 0.8,font=2)
  axis(1, at=Plot, labels=names(intVector), 
       tick=FALSE, las=2, line=-0.5, cex.axis=0.5)
}
ValuesBarPlot(Missing_Values,'Missing Values across all attributes')
ValuesBarPlot(Missing_Values[18:27],'Missing Values in Questions')

### Distribution analysis of variables ----
BarPlot <- function(dataset,i){
  colnames(dataset) = c('X')
  label = paste('Question ',i,collapse = '')
  plot(factor(dataset$X),pch=i,xlab=label,col = barColour)
}
BoxPlot <- function(dataset,i){
  colnames(dataset) = c('X')
  label = paste('Question ',i,collapse = '')
  boxplot(dataset$X,pch=i,xlab=label,col = barColour)
}

par(mfrow=c(2,5),oma=c(0,0,2,0))
for(i in 1:10){
  BarPlot(Dataset[i+17],i)
}
title("Frequency Distribution for all 10 Questions",outer = T)
par(mfrow=c(1,1))

par(mfrow=c(2,5),oma=c(0,0,2,0))
for(i in 1:10){
  BoxPlot(Dataset[i+17],i)
}
title("Box plots for all 10 Questions",outer = T)
par(mfrow=c(1,1))

