

# Libraries
install.packages("naivebayes")
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)

# Data
data <- read.csv(file.choose(), header = T)
str(data)
xtabs(~admit+rank, data = data)

data$rank <- as.factor(data$rank)
data$admit <- as.factor(data$admit)

# Visualization
pairs.panels(data[-1])

data %>%
         ggplot(aes(x=admit, y=gpa, fill = admit)) +
         geom_boxplot() +
         ggtitle("Box Plot")


data %>% ggplot(aes(x=gpa, fill = admit)) +
         geom_density(alpha=0.8, color= 'black') +
         ggtitle("Density Plot")

# Data Partition
set.seed(1234)
ind <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
train <- data[ind == 1,]
test <- data[ind == 2,]

# Naive Bayes Model
 
train %>%
         filter(admit == "0") %>%
         summarise(mean(gre), sd(gre))

train %>%
  filter(admit == "1") %>%
  summarise(mean(gre), sd(gre))



plot(model)

# Predict
p <- predict(model, train, type = 'prob')
head(cbind(p, train))

# Confusion Matrix - train data
library(caret)

confusionMatrix(p1, train$admit)
confusionMatrix(p2, test$admit)


p1 <- predict(model, train)
(tab1 <- table(p1, train$admit))
1 - sum(diag(tab1)) / sum(tab1)

# Confusion Matrix - test data
p2 <- predict(model, test)
(tab2 <- table(p2, test$admit))
1 - sum(diag(tab2)) / sum(tab2)
