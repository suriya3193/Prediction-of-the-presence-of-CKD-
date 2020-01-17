library(ROSE)
library(caTools)
library(corrplot)

#loading the data

CKD_data<-read.csv("E:/Healthcare project/Chronic Kidney Disease Dataset.csv", header = TRUE,sep = ",")
View (CKD_data)
CKD_data<-subset(CKD_data, CKD_data$ID >=1)

#converting the variables to factors


CKD_data$Female<-as.factor(CKD_data$Female)
CKD_data$Racegrp<-as.factor(CKD_data$Racegrp)
CKD_data$Educ<-as.factor(CKD_data$Educ)
CKD_data$Unmarried<-as.factor(CKD_data$Unmarried)
CKD_data$Income<-as.factor(CKD_data$Income)
CKD_data$Insured<-as.factor(CKD_data$Insured)
CKD_data$Obese<-as.factor(CKD_data$Obese)
CKD_data$Dyslipidemia<-as.factor(CKD_data$Dyslipidemia)
CKD_data$PVD<-as.factor(CKD_data$PVD)
CKD_data$Activity<-as.factor(CKD_data$Activity)
CKD_data$PoorVision<-as.factor(CKD_data$PoorVision)
CKD_data$Smoker<-as.factor(CKD_data$Smoker)
CKD_data$Hypertension<-as.factor(CKD_data$Hypertension)
CKD_data$Fam.Hypertension<-as.factor(CKD_data$Fam.Hypertension)
CKD_data$Diabetes<-as.factor(CKD_data$Diabetes)
CKD_data$Fam.Diabetes<-as.factor(CKD_data$Fam.Diabetes)
CKD_data$Stroke<-as.factor(CKD_data$Stroke)
CKD_data$CVD<-as.factor(CKD_data$CVD)
CKD_data$Fam.CVD<-as.factor(CKD_data$Fam.CVD)
CKD_data$CHF<-as.factor(CKD_data$CHF)
CKD_data$Anemia<-as.factor(CKD_data$Anemia)
CKD_data$CKD<-as.factor(CKD_data$CKD)

#Correlation 

my_num_data <- CKD_data[, sapply(CKD_data, is.numeric)]
res<-cor(my_num_data, use = "complete.obs")
View(res)

corrplot(res)

#Splitting the data

CKD_train <- subset(CKD_data, CKD_data$ID <= 6000)
CKD_test <- subset(CKD_data, CKD_data$ID > 6000)

CKD_sampling<-CKD_train[complete.cases(CKD_train), ]
View(CKD_sampling)
table(CKD_sampling$CKD)

#Treating class Imbalance
CKD_rose <- ROSE(CKD ~ ., data = CKD_sampling, seed = 1)$data
table(CKD_rose$CKD)

library(DMwR)
CKD_smote <- SMOTE(CKD ~., CKD_sampling, seed = 2)

#ANOVA Test
aov<- aov(CKD_rose$Age ~ CKD_rose$CKD)
summary(aov)

aov<- aov(CKD_rose$Weight ~ CKD_rose$CKD)
summary(aov)

aov<- aov(CKD_rose$Height ~ CKD_rose$CKD)
summary(aov)

aov<- aov(CKD_rose$BMI ~ CKD_rose$CKD)
summary(aov)

aov<- aov(CKD_rose$Waist ~ CKD_rose$CKD)
summary(aov)

aov<- aov(CKD_rose$SBP ~ CKD_rose$CKD)
summary(aov)

aov<- aov(CKD_rose$DBP ~ CKD_rose$CKD)
summary(aov)

aov<- aov(CKD_rose$HDL ~ CKD_rose$CKD)
summary(aov)

aov<- aov(CKD_rose$LDL ~ CKD_rose$CKD)
summary(aov)

aov<- aov(CKD_rose$Total.Chol ~ CKD_rose$CKD)
summary(aov)


#Chisquare Test

chisq.test(CKD_rose$CKD,CKD_rose$Hypertension)

chisq.test(CKD_rose$CKD,CKD_rose$Female)

chisq.test(CKD_rose$CKD,CKD_rose$Racegrp)

chisq.test(CKD_rose$CKD,CKD_rose$Educ)

chisq.test(CKD_rose$CKD,CKD_rose$Unmarried)

chisq.test(CKD_rose$CKD,CKD_rose$Income)

chisq.test(CKD_rose$CKD,CKD_rose$Insured)

chisq.test(CKD_rose$CKD,CKD_rose$Obese)

chisq.test(CKD_rose$CKD,CKD_rose$Dyslipidemia)

chisq.test(CKD_rose$CKD,CKD_rose$PVD)

chisq.test(CKD_rose$CKD,CKD_rose$Activity)

chisq.test(CKD_rose$CKD,CKD_rose$PoorVision)

chisq.test(CKD_rose$CKD,CKD_rose$Smoker)

chisq.test(CKD_rose$CKD,CKD_rose$Fam.Hypertension)

chisq.test(CKD_rose$CKD,CKD_rose$Diabetes)

chisq.test(CKD_rose$CKD,CKD_rose$Fam.Diabetes)

chisq.test(CKD_rose$CKD,CKD_rose$CVD)

chisq.test(CKD_rose$CKD,CKD_rose$Fam.CVD)

chisq.test(CKD_rose$CKD,CKD_rose$Stroke)

chisq.test(CKD_rose$CKD,CKD_rose$CHF)

chisq.test(CKD_rose$CKD,CKD_rose$Anemia)

CKD_rose$CHM<-NULL
CKD_rose$Unmarried<-NULL
CKD_rose$CareSource<-NULL

#Train and Test
set.seed(123)
smp_size <- floor(0.70 * nrow(CKD_rose))

train_ind <- sample(seq_len(nrow(CKD_rose)), size = smp_size)
CKD_rosetrain <- CKD_rose[train_ind, ]
CKD_rosetest <- CKD_rose[-train_ind, ]

#Model
model <- glm (CKD ~ Age+Female+Racegrp+Educ+Unmarried+Income+Insured+BMI+SBP+DBP+HDL+LDL+PVD+Activity+PoorVision+Smoker+Hypertension+Fam.Hypertension+Diabetes+Stroke+CVD+Fam.CVD+CHF+Anemia, data = CKD_rosetrain, family = binomial)
summary(model)
exp(coef(model))

predict_result <- predict(model,CKD_rosetest,type = 'response')
fitted.results <- ifelse(predict_result > 0.5,1,0)
misClasificError <- mean(fitted.results != CKD_rosetest$CKD)
print(paste('Accuracy',1-misClasificError))

#Predict
CKD_test$CKD<-NULL
CKD_test<-CKD_test[complete.cases(CKD_test), ]
CKD_test$CKD<-NA
View(CKD_test)
predict_result <- predict(model,CKD_test,type = 'response')
fitted.results <- ifelse(predict_result > 0.5,1,0)
View(fitted.results)
table(fitted.results)


