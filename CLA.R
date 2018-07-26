
# Installing the required Packages and Libraries

package_rqd = c("openxlsx","dplyr","xlsx","caret","C50","rpart" , "randomForest", "clusterSim","pROC","e1071","MASS","ROSE","GeneNet","clusterSim","pscl")

install_pckg = package_rqd[!(package_rqd %in% installed.packages()[,"Package"])]
if(length(install_pckg)>0)
  install.packages(install_pckg)

library(openxlsx)
library(dplyr)
library(xlsx)
library(dplyr)
library(caret)
library(rpart)
library(corrplot)
library(pROC)
library(ROCR)
library(MASS)
library(ROSE)
library(pscl)

#### Reading File

dloss_rendata = read.xlsx("Input file for Modelling.xlsx", sheet =  "input data")
model_data = dloss_rendata[,6:13]
model_data$Code = as.factor(model_data$Code)

############### Summary stats ##########################

df_ytdloss= filter(dloss_rendata, dloss_rendata$Client_Type== "YTD Losses")
data.Summary.loss <- t(data.frame(apply(df_ytdloss[,6:length(df_ytdloss)], 2, SummaryStats))) 
write.csv(data.Summary.loss,"data.Summary.loss.csv")

df_ytdrenewal = filter(dloss_rendata, dloss_rendata$Client_Type== "YTD Renewals")
data.Summary.renewal <- t(data.frame(apply(df_ytdrenewal[,6:length(df_ytdrenewal)], 2, SummaryStats))) 
write.csv(data.Summary.renewal,"data.Summary.renewal.csv")

############# Data Modeling #############################

# dividing the data into train and test

set.seed(345)
sample_selection = sample(1:nrow(model_data), .85*nrow(model_data))
df_train_glm = model_data[sample_selection,]
df_test_glm = model_data[-sample_selection,]

df_train_glm$Code = as.factor(df_train_glm$Code)

# Logistic Model

model_churn_ns = glm( Code ~ ., data = df_train_glm, family = "binomial")

summary(model_churn_ns) # Summary of Model


################### Variable Importance ###############################

imp_vars_glm = as.data.frame(varImp(model_churn_ns, scale= TRUE))
rownames(imp_vars_glm)[order(imp_vars_glm$Overall, decreasing = TRUE)]

#### Mc-fadden Psuedo R2 value ###############

Mcfadden_value = as.list(pR2(model_churn_ns))
Mcfadden_value$McFadden # Mcfadden value is .63

################ Train AUC value 

pred = prediction(predict(model_churn_ns), df_train_glmCode)
perf = performance(pred, "tpr", "fpr")
plot(perf, main = "ROC Curve", xlab= "False positive rate", ylab = "True Positive Rate", type ="b",lwd=2,colorize = TRUE)
lines(x=c(0,1),y=c(0,1), lwd=2, col ="red")

auc.tmp = performance(pred,"auc")
auc =   as.numeric(auc.tmp@y.values)
auc    

# train Prediction & Confusion MAtrrix

df_train_glm$prob = predict(model_churn_ns, df_train_glm,type= "response")
df_train_glm$predcode = ifelse(df_train_glm$prob >= 0.5,1,0)
confusionMatrix(df_train_glm$predcode, df_train_glm$Code)

# test Prediction & Confusion MAtrrix

df_test_glm$prob = predict(model_churn_ns, df_test_glm,type= "response")
df_test_glm$predcode = ifelse(df_test_glm$prob >= 0.5,1,0)
confusionMatrix(df_test_glm$predcode, df_test_glm$Code)

# Prediction of Overall/Pilot LM/Renewal CLients/Current Clients/Unseen Data

dloss_rendata$Code= as.factor(dloss_rendata$Code)
dloss_rendata$prob = predict(model_churn_ns, dloss_rendata,type= "response")
dloss_rendata$predcode = ifelse(dloss_rendata$prob >= 0.5,1,0)
confusionMatrix(dloss_rendata$predcode, dloss_rendata$Code)
write.csv(dloss_rendata,"inputdataprob.csv")

df_LMClients= read.xlsx("Input file for Modelling.xlsx", sheetName = "Pilot LM CLients Data")
df_LMClients$prob = predict(model_churn_ns, df_LMClients,type= "response")
df_LMClients$predcode = ifelse(df_LMClients$prob >= 0.5,1,0)
write.csv(df_LMClients,"df_LMClients.csv")

df_renewal= read.xlsx("Input file for Modelling.xlsx", sheetName = "ren clients data")
df_renewal$FU.Int.WFs.Pend...4.Days = 4.42
df_renewal$prob = predict(model_churn_ns, df_renewal,type= "response")
df_renewal$predcode = ifelse(df_renewal$prob >= 0.5,1,0)
write.csv(df_renewal,"df_renewal.csv")

df_current= read.xlsx("Input file for Modelling.xlsx", sheetName = "Current Clients data ")
df_current$prob = predict(model_churn_ns, df_current,type= "response")
df_current$predcode = ifelse(df_current$prob >= 0.5,1,0)
write.csv(df_current,"df_current.csv")


##################################### Test Data #########################################

## Test data losses and renwals post MAy'17
df_testdata = read.xlsx("Input file for Modelling.xlsx", sheetName = "test data")
df_testdata$predprob = predict(model_churn_ns,df_testdata, type = "response")
df_testdata$codepred = ifelse(df_testdata$predprob  >= 0.5,1,0)
confusionMatrix(df_testdata$codepred,df_testdata$Code) ## using non standardised data
write.csv(df_testdata,"df_testdata.csv")



############## likelihodd ratio test ###################
#H0 : model_churn_ns is true
#Ha : model_churn_ns is not true
library(lmtest)
lrtest(model_churn_standard, model_churn_ns) 
#p value is 1 so we fail to reject the null hypothesis that org data model is best

############## Hosmer- Lemeshow Test ###################

#H0 : model_churn_standard fits the data
#Ha : model_churn_standard does not fit the data
install.packages("MKmisc")
install.packages("ResourceSelection")
library(MKmisc)
library(ResourceSelection)
HLgof.test(fit = fitted(model_churn_standard), obs = df_train_glmstandard$Code)
hoslem.test(df_train_glmstandard$Code,fitted(model_churn_standard), g =10)
#p value is < < 2.2e-16 so we fail to accept the null hypothesis that org data model is best


###########  Wald Test #####################

#H0: Coeffcient is significantly different from zero; removing variable will not harm the model
#HA: Coeffcient is not significantly different from zero Removing variable will harm the variable
install.packages("survey")
library(survey)
regTermTest(model_churn_standard,"X2DaysTAT") #  p= 0.94509  ; we fail to reject the null hypothesis
regTermTest(model_churn_standard,"Defects.Pending") # p= 0.71126  we fail to reject the null hypothesis
regTermTest(model_churn_standard,"Defects.Started") #  p= 0.38193 we fail to reject the null hypothesis
regTermTest(model_churn_standard,"FU.Int.WFs.Pend...4.Days") # p = p= 7.6331e-07  we fail to accept the null hypothesis
regTermTest(model_churn_standard,"OFS.Calls") # p= 0.0056067 8 ;  we fail to accept the null hypothesis
regTermTest(model_churn_standard,"Slo") #  p= 0.67744  ; we reject the null hypothesis
regTermTest(model_churn_standard,"SVC.Center.CSAT") # p= 0.37204  ; we fail to reject the null hypothesis



# wald test with no scaled values ###########

regTermTest(model_churn_ns,"X2DaysTAT") # p= 0.94509 ; we fail to reject  the null hypothesis
regTermTest(model_churn_ns,"Defects.Pending") # p= 0.71126  we fail to reject the null hypothesis
regTermTest(model_churn_ns,"Defects.Started") #  p= 0.38193  we fail to reject the null hypothesis
regTermTest(model_churn_ns,"FU.Int.WFs.Pend...4.Days") #p= 7.6331e-07 we fail to accept  the null hypothesis
regTermTest(model_churn_ns,"OFS.Calls") # p= 0.0056067 ; we fail to accept  the null hypothesis
regTermTest(model_churn_ns,"Slo") # p= 0.67744; we fail to reject the null hypothesis
regTermTest(model_churn_ns,"SVC.Center.CSAT") #p= 0.37204 ; we fail to reject the null hypothesis


####################### Reduced Model #####################

#### Reading File


dloss_rendata = read.xlsx("Input file for Modelling.xlsx", sheetName = "input data")
model_data = dloss_rendata[,6:13]
model_data$Code = as.factor(model_data$Code)

############### Summary stats ##########################

df_ytdloss= filter(dloss_rendata, dloss_rendata$Client_Type== "YTD Losses")
data.Summary.loss <- t(data.frame(apply(df_ytdloss[,6:length(df_ytdloss)], 2, SummaryStats))) 
write.csv(data.Summary.loss,"data.Summary.loss.csv")

df_ytdrenewal = filter(dloss_rendata, dloss_rendata$Client_Type== "YTD Renewals")
data.Summary.renewal <- t(data.frame(apply(df_ytdrenewal[,6:length(df_ytdrenewal)], 2, SummaryStats))) 
write.csv(data.Summary.renewal,"data.Summary.renewal.csv")

############################################################

set.seed(345)
sample_selection = sample(1:nrow(model_data), .85*nrow(model_data))
df_train_glm = model_data[sample_selection,]
df_test_glm = model_data[-sample_selection,]
df_train_glm$Code = as.factor(df_train_glm$Code)
str(df_train_glm)

# df_train = trainControl(method ="repeatedcv", number = 10, repeats = 50)
#model_train = train(Code ~ .,trControl = df_train, data = df_train_glm, method= "rf")

model_churn_reduced = glm( Code ~ FU.Int.WFs.Pend...4.Days + Escalated.Calls, data = df_train_glm, family = "binomial")
summary(model_churn_reduced)

df_LMClientsreduced= read.xlsx("Input file for Modelling.xlsx", sheetName = "Pilot LM CLients Data")
df_LMClientsreduced$prob = predict(model_churn_ns, df_LMClientsreduced,type= "response")
df_LMClientsreduced$predcode = ifelse(df_LMClientsreduced$prob >= 0.5,1,0)
write.csv(df_LMClientsreduced,"df_LMClientsreduced.csv")

df_renewalreduced = read.xlsx("Input file for Modelling.xlsx", sheetName = "ren clients data")
df_renewalreduced$FU.Int.WFs.Pend...4.Days = 4.42
df_renewalreduced$prob = predict(model_churn_ns, df_renewalreduced,type= "response")
df_renewalreduced$predcode = ifelse(df_renewalreduced$prob >= 0.5,1,0)
write.csv(df_renewalreduced,"df_renewalreduced.csv")

df_currentreduced = read.xlsx("Input file for Modelling.xlsx", sheetName = "Current Clients data ")
df_currentreduced$prob = predict(model_churn_ns, df_currentreduced,type= "response")
df_currentreduced$predcode = ifelse(df_currentreduced$prob >= 0.5,1,0)
write.csv(df_currentreduced,"df_currentreduced.csv")

df_testdatareduced = read.xlsx("Input file for Modelling.xlsx", sheetName = "test data")
df_testdatareduced$predprob = predict(model_churn_ns,df_testdatareduced, type = "response")
df_testdatareduced$codepred = ifelse(df_testdatareduced$predprob  >= 0.5,1,0)
confusionMatrix(df_testdatareduced$codepred,df_testdatareduced$Code) ## using non standardised data
write.csv(df_testdatareduced,"df_testdatareduced.csv")

############################### model after removing escalated calls ##############

set.seed(345)
sample_selection = sample(1:nrow(model_data), .85*nrow(model_data))
df_train_glm = model_data[sample_selection,]
df_test_glm = model_data[-sample_selection,]
df_train_glm$Code = as.factor(df_train_glm$Code)
colnames(df_train_glm)
reqd_cols = c('Defects.Pending',"Defects.Started","2DaysTAT","FU.Int.WFs.Pend.>.4.Days","Service.Level.Occurrence","SVC.Center.CSAT","Code")

df_train_reduced = df_train_glm[,reqd_cols]
str(df_train_reduced)
model_exl_ecscalls = glm( Code ~. ,data = df_train_reduced, family = "binomial")
summary(model_exl_ecscalls)

dloss_rendata$Code= as.factor(dloss_rendata$Code)
dloss_rendata$probexlesccalss = predict(model_exl_ecscalls, dloss_rendata,type= "response")
dloss_rendata$predcodeexlsesccalls = ifelse(dloss_rendata$probexlesccalss >= 0.5,1,0)
confusionMatrix(dloss_rendata$predcodeexlsesccalls, dloss_rendata$Code)
write.csv(dloss_rendata,"inputdataprobexlesccalls.csv")
colnames(df_train_reduced)

### last 5 months data 

setwd("C:\\Users\\a0726785\\Desktop\\LM ENO analysis")
LMlossdata = read.xlsx("Loss Clients Last 5 months.xlsx", sheet = "input data")
LMlossdata$predprob = predict(model_exl_ecscalls,LMlossdata,type = "response")
write.csv(LMlossdata,"LMlossdata.csv")

############ Aug till Oct prob calculation

setwd("Z:\\Team IEG GGN\\Abhinav\\CLA Project_Final\\Phase 5 - Client Churn Model LM Clients\\Relationship Rating- Text Analysis\\Dashboard excluding Escalated Calls")
df_Lmaugoct= read.xlsx("Input File LM Aug till Oct.xlsx", sheetName = "Input FIle")
df_Lmaugoct$prob = predict(model_exl_ecscalls,df_Lmaugoct, type = "response")
write.csv(df_Lmaugoct, "AugtillOCtLmdataProb.csv")

############ Aug till Oct Loss Renewal Prob calculation

setwd("Z:\\Team IEG GGN\\Abhinav\\CLA Project_Final\\Phase 5 - Client Churn Model LM Clients")
lossRenewalaugoct= read.xlsx("LM ops Testdata post July LosRen.xlsx", sheet = "testdata")
lossRenewalaugoct$ActualCode = ifelse(lossRenewalaugoct$Client.Type == "Loss", 1,0)
lossRenewalaugoct$prob = predict(model_exl_ecscalls,lossRenewalaugoct, type = "response")
lossRenewalaugoct$PredCode = ifelse(lossRenewalaugoct$prob >= 0.5,1,0)
confusionMatrix(lossRenewalaugoct$PredCode,lossRenewalaugoct$ActualCode)
write.csv(lossRenewalaugoct,"lossRenewalaugoctProb.csv")

########### train accuracy

install.packages("e1071")
library(e1071)

df_train_reduced$prob = predict(model_exl_ecscalls,df_train_reduced, type = "response")
df_train_reduced$predcode = ifelse(df_train_reduced$prob >= 0.5,1,0)
confusionMatrix(df_train_reduced$predcode,df_train_reduced$Code)

########### test accuracy

df_test_glm$prob = predict(model_exl_ecscalls,df_test_glm, type = "response")
df_test_glm$predcode = ifelse(df_test_glm$prob >= 0.5,1,0)
confusionMatrix(df_test_glm$predcode,df_test_glm$Code)
colnames(df_test_glm)

########## Current Clients details ###############################

setwd("C:\\Users\\a0726785\\Desktop")
df_currentclientsnew = read.xlsx("data left post removing ofs calls.xlsx", sheetName = "current clients input file")
df_currentclientsnew$prob = predict(model_exl_ecscalls,df_currentclientsnew, type = "response")
df_currentclientsnew$predcode = ifelse(df_currentclientsnew$prob >= 0.5,1,0)
write.csv(df_currentclientsnew,"df_currentclientsnew.csv")

imp_vars_glm = as.data.frame(varImp(model_exl_ecscalls, scale= TRUE))
rownames(imp_vars_glm)[order(imp_vars_glm$Overall, decreasing = TRUE)]

setwd("Z:\\Team IEG GGN\\Abhinav\\CLA Project_Final\\Phase 5 - Client Churn Model LM Clients\\Input File")
df_testdatareduced = read.xlsx("Input file for Modelling.xlsx", sheetName = "test data")
df_testdatareduced$redprob = predict(model_exl_ecscalls,df_testdatareduced, type = "response")
df_testdatareduced$codereduced = ifelse(df_testdatareduced$redprob  >= 0.5,1,0)
confusionMatrix(df_testdatareduced$codereduced,df_testdatareduced$Code) ## using non standardised data
write.csv(df_testdatareduced,"df_testdatareduced.csv")

################################################################

SummaryStats <- function(x) {
  nmiss <- sum(is.na(x))
  a <- x[!is.na(x)]
  n <- length(a)
  mean <- mean(a)
  sd <- sd(a)
  min <- min(a)
  p1 <- quantile(a,0.01)
  p5 <- quantile(a,0.05)
  p10 <- quantile(a,0.1)
  q1 <- quantile(a,0.25)
  q2 <- quantile(a, 0.5)
  q3 <- quantile(a, 0.75)
  p90 <- quantile(a, 0.9)
  p99 <- quantile(a,0.99)
  max <- max(a)
  UC <- mean(a) + 3* sd(a)
  LC <- mean(a) - 3* sd(a)
  Oulierflag <- max(a) > UC | min(a) < LC
  return(c(n=n, nmiss = nmiss,Oulierflag = Oulierflag , mean = mean, sd = sd, min = min, p1 = p1,
           p5=p5, p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p99=p99, max= max, UC= UC, LC = LC))
  
}