### Focus       		: Feature Additions and Realtion Analysis
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

### Feature Additions - Factoring the Q Scores ----
Dataset$Q1Factor = as.factor(ifelse(Dataset$Q1 %in% c(1,2,3),'NotSatisfied',
                                    ifelse(Dataset$Q1 %in% c(4,5),'Satisfied',Dataset$Q1)))
Dataset$Q2Factor = as.factor(ifelse(Dataset$Q2 %in% c(1,2,3),'NotSatisfied',
                                    ifelse(Dataset$Q2 %in% c(4,5),'Satisfied',Dataset$Q2)))
Dataset$Q3Factor = as.factor(ifelse(Dataset$Q3 %in% c(1,2,3),'NotSatisfied',
                                    ifelse(Dataset$Q3 %in% c(4,5),'Satisfied',Dataset$Q3)))
Dataset$Q4Factor = as.factor(ifelse(Dataset$Q4 %in% c(1,2,3),'NotSatisfied',
                                    ifelse(Dataset$Q4 %in% c(4,5),'Satisfied',Dataset$Q4)))
Dataset$Q5Factor = as.factor(ifelse(Dataset$Q5 %in% c(1,2,3),'NotSatisfied',
                                    ifelse(Dataset$Q5 %in% c(4,5),'Satisfied',Dataset$Q5)))
Dataset$Q6Factor = as.factor(ifelse(Dataset$Q6 %in% c(1,2,3),'NotSatisfied',
                                    ifelse(Dataset$Q6 %in% c(4,5),'Satisfied',Dataset$Q6)))
Dataset$Q7Factor = as.factor(ifelse(Dataset$Q7 %in% c(1,2,3),'NotSatisfied',
                                    ifelse(Dataset$Q7 %in% c(4,5),'Satisfied',Dataset$Q7)))
Dataset$Q8Factor = as.factor(ifelse(Dataset$Q8 %in% c(1,2,3),'NotSatisfied',
                                    ifelse(Dataset$Q8 %in% c(4,5),'Satisfied',Dataset$Q8)))
Dataset$Q9Factor = as.factor(ifelse(Dataset$Q9 %in% c(1,2,3),'NotSatisfied',
                                    ifelse(Dataset$Q9 %in% c(4,5),'Satisfied',Dataset$Q9)))
Dataset$Q10Factor = as.factor(ifelse(Dataset$Q10 %in% c(1,2,3),'NotSatisfied',
                                     ifelse(Dataset$Q10 %in% c(4,5),'Satisfied',Dataset$Q10)))
### Distribution analysis added features----
par(mfrow=c(2,5),oma=c(0,0,2,0))
for(i in 1:10){
  BarPlot(Dataset[i+29],i)
}
title("Frequency Distribution for all 10 Factored Questions",outer = T)
par(mfrow=c(1,1))
### Correlation Analysis on Questions data ----
QuestionNoMissing=Dataset[,18:28][!apply(Dataset[,18:28], 1, function(x) any(x=="" | is.na(x))),] 
require(corrplot)
require(RColorBrewer)
round(cor(QuestionNoMissing[1:10]),2)
corrplot(cor(QuestionNoMissing[1:10]), type="upper",
         method = 'circle' ,order="hclust", add = F,
         col=brewer.pal(n=4, name="RdBu"),  
         outline = T)
require(usdm)
vif(data.frame(QuestionNoMissing[1:10]))
### Factor Analysis on Non missing data set of 10 Questions ----
require(psych)
pca = principal(QuestionNoMissing[1:10],nfactors = 10,rotate = 'none')
pca
plot(pca$values,type="b",col = barColour,
     xlab = 'No of Principal Components',ylab = 'Engine Values',
     main = 'Scree plot for all possible components') # Scree Plot
abline(h = 1,col='orangered')
pca_reduced = principal(QuestionNoMissing[1:10], nfactors = 6, rotate = 'none')
pca_reduced
pca_rotated = principal(QuestionNoMissing[1:10], nfactors = 6, rotate = 'varimax')
pca_rotated
round(pca_rotated$r.scores,5) # all rotated factors are orthogonal

Scaled_Satindex=scale(QuestionNoMissing[,11])
colnames(Scaled_Satindex)= c('Scaled_Satindex')
QuestionNoMissing = cbind(QuestionNoMissing,pca_rotated$scores,Scaled_Satindex)

pca_reduced2 = principal(QuestionNoMissing[1:10], nfactors = 2, rotate = 'none')
pca_reduced2
pca_rotated2 = principal(QuestionNoMissing[1:10], nfactors = 2, rotate = 'varimax')
pca_rotated2
round(pca_rotated2$r.scores,5) # all rotated factors are orthogonal

QuestionNoMissing2 = cbind(QuestionNoMissing[1:11],pca_rotated2$scores,Scaled_Satindex)

### MultiLinear Regression ----
formula = as.formula(paste("Scaled_Satindex ~", 
                           paste(names(QuestionNoMissing[12:17]), collapse = " + ")))
Linear_regression = lm(formula,
                       data = QuestionNoMissing)
summary(Linear_regression)
plot(Linear_regression)

formula2 = as.formula(paste("Scaled_Satindex ~", 
                            paste(names(QuestionNoMissing2[12:13]), collapse = " + ")))
Linear_regression2 = lm(formula2,
                        data = QuestionNoMissing2)
summary(Linear_regression2)
plot(Linear_regression2)



