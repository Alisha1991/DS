#Read data file
mydata <- read.csv("C:/Users/nikhi/Desktop/Data Science/Decision Trees/Cardiotocographic.csv")

# Understand your data
str(mydata)
summary(mydata)

View(mydata)

# This will create the new variable
mydata$NSPF <- as.factor(mydata$NSP)

# Partition data into test and training
set.seed(1234)
pd <- sample(2, nrow(mydata), replace = TRUE, prob = c(0.8,0.2))

train <- mydata[pd==1,]
nrow(train)

test <- mydata[pd==2,]
nrow(test)

#Decision tree with party
#install.packages("party")
library(party)

mytree <- ctree(NSPF~LB+AC+FM, data = train)
plot(mytree)

# Pruning tree
mytree2 <- ctree(NSPF~LB+AC+FM, mydata, controls=ctree_control(mincriterion=0.99, minsplit=500))

print(mytree2)
plot(mytree2)
plot(mytree2,type="simple")

# Predict
predict(mytree2, test, type = "prob")
predict(mytree2, test)

#Misclassification error
tab<-table(predict(mytree2), train$NSPF)
print(tab)

1-sum(diag(tab))/sum(tab)

# Misclassification the test data
testpred <- predict(mytree2, newdata = test)
tab <- table(testpred, test$NSPF)
1 - sum(diag(tab))/sum(tab)
print(tab)







