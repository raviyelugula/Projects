LowSat_Data = subset(Dataset, Dataset$Satindex<=3)
LowSat_Data_1 = subset(Dataset, Dataset$Satindex<=1)
LowSat_Data_2 = subset(Dataset, Dataset$Satindex<=2 & Dataset$Satindex>1)
LowSat_Data_3 = subset(Dataset, Dataset$Satindex<=3 & Dataset$Satindex>2)

QuestionNoMissing_L=LowSat_Data[,1:28][!apply(LowSat_Data[,18:28], 1, function(x) any(x=="" | is.na(x))),] 
round(cor(QuestionNoMissing_L[1:10]),2)
corrplot(cor(QuestionNoMissing_L[1:10]), type="upper",
         method = 'circle' ,order="hclust", add = F,
         col=brewer.pal(n=4, name="RdBu"),  
         outline = T)
vif(data.frame(QuestionNoMissing_L[1:10]))

formula_L = as.formula(paste("Satindex ~", 
                           paste(names(QuestionNoMissing_L[1:10]), collapse = " + ")))
Linear_regression_L = lm(formula_L,
                       data = QuestionNoMissing)
summary(Linear_regression_L)

StateWiseDF_List_L = split(QuestionNoMissing_L,f = QuestionNoMissing_L$State)
Karnataka_data = StateWiseDF_List_L[[3]][,1:28][!apply(StateWiseDF_List_L[[3]][,18:28], 1, function(x) any(x=="" | is.na(x))),] 
round(cor(Karnataka_data[18:27]),2)
corrplot(cor(Karnataka_data[18:27]), type="upper",
         method = 'circle' ,order="hclust", add = F,
         col=brewer.pal(n=4, name="RdBu"),  
         outline = T)
vif(data.frame(Karnataka_data[18:27]))
formula_L = as.formula(paste("Satindex ~", 
                             paste(names(Karnataka_data[18:27]), collapse = " + ")))
Linear_regression_L = lm(formula_L,
                         data = QuestionNoMissing)
summary(Linear_regression_L)



