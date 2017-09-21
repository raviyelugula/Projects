QuestionNoMissing_S1=StateWiseDF_List[[18]][,18:28][!apply(StateWiseDF_List[[18]][,18:28], 1, function(x) any(x=="" | is.na(x))),] 
StateWiseDF_List[[18]]$StateCorrected[1]
require(corrplot)
require(RColorBrewer)
round(cor(QuestionNoMissing_S1[1:10]),2)
corrplot(cor(QuestionNoMissing_S1[1:10]), type="upper",
         method = 'circle' ,order="hclust", add = F,
         col=brewer.pal(n=4, name="RdBu"),  
         outline = T)
require(usdm)
vif(data.frame(QuestionNoMissing_S1[1:10]))
### Factor Analysis on Non missing data set of 10 Questions ----
require(psych)
pca_s1 = principal(QuestionNoMissing_S1[1:10],nfactors = 10,rotate = 'none')
pca_s1
plot(pca_s1$values,type="b",col = barColour,
     xlab = 'No of Principal Components',ylab = 'Engine Values',
     main = 'Scree plot for all possible components') # Scree Plot
abline(h = 1,col='orangered')
pca_reduced_s1 = principal(QuestionNoMissing_S1[1:10], nfactors = 6, rotate = 'none')
pca_reduced_s1
pca_rotated_s1 = principal(QuestionNoMissing_S1[1:10], nfactors = 6, rotate = 'varimax')
pca_rotated_s1
round(pca_rotated_s1$r.scores,5) # all rotated factors are orthogonal

Scaled_Satindex=scale(QuestionNoMissing_S1[,11])
colnames(Scaled_Satindex)= c('Scaled_Satindex')
QuestionNoMissing_S1 = cbind(QuestionNoMissing_S1,pca_rotated_s1$scores,Scaled_Satindex)

### MultiLinear Regression ----
formula_s1 = as.formula(paste("Scaled_Satindex ~", 
                           paste(names(QuestionNoMissing_S1[12:17]), collapse = " + ")))
Linear_regression_s1 = lm(formula_s1,
                       data = QuestionNoMissing_S1)
summary(Linear_regression_s1)
plot(Linear_regression_s1)
