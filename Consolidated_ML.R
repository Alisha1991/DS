

#Factor Analysis: works well with Ordinal variable / Dimensionality Reduction Technique
# Clear environment
rm(list = ls())

# SET THE WORKING DIRECTORY BY BROWSING DIRECTORY
dir<-choose.dir()
setwd(dir)

#IMPORTING FILE BY BROWISING FILE
file<-choose.files()                             # Browse the file
getwd()

SAQ <- read.csv("/Users/sb/Dropbox/Public/Projects and case studies/Analytics/Corporate training/AON/March 2018/Day 2/FA/SAQ.csv", header = TRUE)   # Import file
str(SAQ, list.len = 150)               # STRUCTURE OF THE DATA for 150 columns
names(SAQ)                                # COLUMN NAMES OF DATA SET
View(SAQ)

# FACTOR ANALYSIS 
corrm<- cor(SAQ)                                 # CORRELATION MATRIX
write.csv(corrm,"/Users/sb/Dropbox/Public/Projects and case studies/Analytics/Corporate training/AON/March 2018/Day 2/FA/Corrm.csv")    
require(psych)
require(GPArotation)

# DECIDING NUMBER OF FACTORS USING SCREE PLOT & KAISER TEST (NUMBER OF EIGEN VALUES OVER 1)
require(dplyr)
eigen_values <- mutate(data.frame(eigen(corrm)$values) ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))  # CALCULATING VARIANCE, CUMULATIVE VARIANCE etc... 
write.csv(eigen_values, "/Users/sb/Dropbox/Public/Projects and case studies/Analytics/Corporate training/AON/March 2018/Day 2/FA/EigenValues.csv")  ### EXPORTING EIGEN VALUE SUMMARY

scree(corrm, factors=T, pc=T, main="scree plot", hline=NULL,add=FALSE) ### SCREE PLOT
FA<-fa(r=corrm, 6, rotate="varimax", fm="ml")               ### CONDUCTING FACTOR ANALYSIS
print(FA)                                                    # PRINT THE RESULTS
FA_SORT<-fa.sort(FA)                              # SORTING THE LOADINGS
ls(FA_SORT)                                               # LISTING OUT THE OBJECTS
FA_SORT$loadings
#FA_SORT$e.values                                # FINDING EIGEN VALUES FROM THE RESULTS
Loadings<-data.frame(FA_SORT$loadings[1:ncol(SAQ),]) ### CAPTURING ONLY LOADINGS INTO DATA FRAME

write.csv(Loadings, "/Users/sb/Dropbox/Public/Projects and case studies/Analytics/Corporate training/AON/March 2018/Day 2/FA/loadings.csv") 




#####CLUSTERING: to reduce the variables/metrics in a model - to segment data  


setwd("F:/BigData/BIG DATA(R+HADOOP)/Week-4")
getwd()

#Importing Data
telco<-read.csv("telco_csv.csv")

#Exploring data
View(telco)
str(telco)
names(telco)

# User written function for creating descriptive statistics
mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  pctls<-quantile(a,probs=c(0.01, 0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99))
  max <- max(a)
  return(c(n=n, nmiss=nmiss,  mean=m, stdev=s,min = min, pctls=pctls,max=max))
}

vars <- c( "region","tenure" ,"age" ,"marital" ,"address" , "income","ed" ,"employ",
           "retire","gender","reside","tollfree", "equip","callcard", "wireless", "longmon",
           "tollmon",  "equipmon", "cardmon",  "wiremon",  "multline", "voice", "pager" ,
           "internet","callid", "callwait", "forward", "confer", "ebill","custcat")

diag_stats<-t(data.frame(apply(telco[vars], 2, mystats)))  
write.csv(diag_stats, "diag_stats.csv")

#Outliers - capping at 90th percentile/95th percentile
telco$longmon[telco$longmon>42]<- 42
telco$tollmon[telco$tollmon>63]<- 63

View(telco)
inputdata_final <-telco[vars]

#Prepare final Data -- standardizing the data

inputdata_final = data.frame(scale(inputdata_final))
clus <- c("tollmon",
          "callwait",
          "forward",
          "pager",
          "voice",
          "equipmon",
          "internet",
          "tenure",
          "longmon",
          "multline",
          "income",
          "wiremon",
          "cardmon")
inputdata_clus <- inputdata_final[clus]
View(inputdata_final)





# Determine number of clusters -- Elbow Method

k.max <- 15
data <- Final_data ## Scaled Data

wss <- sapply(1:k.max, 
              function(k){
                kmeans(Ali_12[vars], k, nstart=50,iter.max = 15 )$tot.withinss
              }
)
wss

plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# Building clusters using k-means clustering 
cluster_three <- kmeans(inputdata_clus,3)
cluster_four <- kmeans(inputdata_clus,4)
cluster_four$cluster - {to view the information}
telco_new <-cbind(telco,km_clust_3=cluster_three$cluster,km_clust_4=cluster_four$cluster )
View(telco_new)

# Graph based on k-means - Optional
require(cluster)
clusplot(inputdata_clus, #dataframe
         cluster_three$cluster, #clusterdata
         color = TRUE, #color
         #shade = TRUE, # Lines in clusters
         lines =6, # lines connecting centroids
         labels = 2 # Labels clusters and cases)
         
         # Profiling -- Converting into factors
         telco_new$km_clust_3=factor(telco_new$km_clust_3)
         telco_new$km_clust_4=factor(telco_new$km_clust_4)
         require(tables)
         
         profile1 <-tabular(1+tenure+age+marital+address+income+ed+employ+retire+gender+reside+tollfree+
                              equip+callcard+wireless+longmon+tollmon+equipmon+cardmon+wiremon+multline+
                              voice+pager+internet+callid+callwait+forward+confer+ebill ~ mean +
                              (mean*km_clust_3)+(mean*km_clust_4)+(mean*km_clust_5)+(mean*km_clust_6), data=telco_new)
         
         profile1<-data.frame(profile1)
         View(profile1)
         
         Profile2 <-tabular(1~length+(length*km_clust_3)+(length*km_clust_4)+(length*km_clust_5)+(length*km_clust_6),
                            data=telco_new)
         profile2<-data.frame(Profile2)
         
         write.csv(profile1,"profile1.csv",row.names = F)
         write.csv(profile2,"profile2.csv",row.names = F)
         
         
         
         
         # Logistic Regression : Classification Technique , Data Cleaning, Regression Steps and, Deciling
         
         # Directory setting
         setwd("Z:\\Team IEG GGN\\Alisha\\CIBA Analytics P3\\Linear\\Lin Reg - Credit Card Case study")
         
         data<-read.csv("Credit Card Data.csv")
         names(data)
         str(data)
         
         
         
         # Converting to Categorical / Numerical
         data$Purpose<-as.factor(data$Purpose)
         data$Sex...Marital.Status<-as.factor((data$Sex...Marital.Status))
         data$Badloan<-as.factor(data$Badloan)
         
         data$Length.of.current.employment<-as.numeric(data$Length.of.current.employment)
         data$Payment.Status.of.Previous.Credit<-as.numeric(data$Payment.Status.of.Previous.Credit)
         
         num<-c("Duration.of.Credit..month.","Payment.Status.of.Previous.Credit",
                "Credit.Amount","No.of.dependents","Age..years.",
                "No.of.Credits.at.this.Bank","Length.of.current.employment",
                "Instalment.per.cent","Duration.in.Current.address")
         
         cat<-c("Account.Balance","Purpose","Sex...Marital.Status","Type.of.apartment",
                "Most.valuable.available.asset","Occupation","Telephone",
                "Foreign.Worker", "Guarantors","Value.Savings.Stocks",
                "Concurrent.Credits")
         
         # MISSING VALUES 
         is.na(colSums(data[,num]))
         
         # Summary Statistics :Descriptive Statistics
         mystats <- function(x) {
           nmiss<-sum(is.na(x))
           a <- x[!is.na(x)]
           m <- mean(a)
           n <- length(a)
           s <- sd(a)
           min <- min(a)
           p1<-quantile(a,0.01)
           p5<-quantile(a,0.05)
           p10<-quantile(a,0.10)
           q1<-quantile(a,0.25)
           q2<-quantile(a,0.5)
           q3<-quantile(a,0.75)
           p90<-quantile(a,0.90)
           p95<-quantile(a,0.95)
           p99<-quantile(a,0.99)
           max <- max(a)
           UC <- m+3*s
           LC <- m-3*s
           outlier_flag<- max>UC | min<LC
           return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m, stdev=s,min = min, p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max, UC=UC, LC=LC ))
         }
         
         # Applying function on the Numeric data
         
         diag_stats<-data.frame(t(apply(data[num], 2, mystats)))
         View(diag_stats)
         
         
         
         
         # Treating missing values
         data[num] <- apply(data.frame(data[,num]), 2, function(x){x <- replace(x, is.na(x), mean(x, na.rm=TRUE))})
         data[cat] <- apply(data.frame(data[,cat]), 2, function(x){x <- replace(x, is.na(x), which.max(prop.table(table(x))))})
         
         # Handling outliers in numeric variables -- Cap the highest value at 99th percentile
         plot(data$Credit.Amount)
         data$Credit.Amount[data$Credit.Amount>16000] <- quantile(data$Credit.Amount,.99, na.rm = T)
         
         # Checking the distribution of DV
         hist(data$Credit.Amount)
         hist(log(data$Credit.Amount)) # looks normal
         data$ln_Credit.Amount = log(data$Credit.Amount)
         
         require(car)
         scatterplot(data$ln_Credit.Amount,data$Duration.of.Credit..month.)
         scatterplotMatrix(~ln_Credit.Amount+Duration.of.Credit..month.+Credit.Amount+No.of.dependents+Age..years. +No.of.Credits.at.this.Bank+
                             Instalment.per.cent+Duration.in.Current.address, data=data)
         
         # Correlation
         require(corrplot)
         corrplot(cor(data[, num],use="pairwise.complete.obs"), method = "circle", tl.cex = 0.7)
         
         # Set the seed to make your partition reproductible
         smp_size <- floor (0.70 * nrow(data))
         
         set.seed(123)
         train_ind <- sample (seq_len(nrow(data)), size = smp_size)
         train <- data[train_ind, ]
         test <- data[-training, ]
         
         write.csv(train,"traindata.csv")
         
         #Designing Model
         
         fit<-lm(ln_Credit.Amount~Account.Balance+
                   Duration.of.Credit..month.+ Payment.Status.of.Previous.Credit+ I(Value.Savings.Stocks=="3")+
                   No.of.dependents+Guarantors+
                   I(Sex...Marital.Status=="2")+
                   Type.of.apartment+
                   Most.valuable.available.asset+
                   I(Occupation=="3")+data=train)
         
         summary(fit)
         vif(fit)
         plot(fit)
         hist(fit$residuals)
         
         require(MASS)
         step3<- stepAIC(fit,direction="both")
         
         # High affecting values
         influence(fit)
         leveragePlots(fit)
         influencePlot(fit,id.method = "identity",main="Influence Plot")
         cooksd<-cooks.distance(fit)
         influential<-as.numeric(names(cooksd)/cooksd>4*mean(cooksd,na.rm=T))
         influential<-as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])
         head(influential,)
         
         train1<-cbind(train, ln_Credit.Amount_p=predict(fit2), Credit.Amount_p=exp(predict(fit2)) )
         
         
         
         
         
         # Find the decile locations 
         decLocations <- quantile(train1$Credit.Amount_p, probs = seq(0.1,0.9,by=0.1))
         
         # Use findInterval with -Inf and Inf as upper and lower bounds
         train1$decile <- findInterval(train1$Credit.Amount_p,c(-Inf,decLocations, Inf))
         summary(train1$decile)
         xtabs(~decile,train1)
         View(train1)
         
         require(dplyr)
         train1$decile<-factor(train1$decile)
         decile_by<-group_by(train1, decile)
         train1_DA<-summarise(decile_by, count=n(), Avg_pred_amt=mean(Credit.Amount_p), Avg_actual_amt= mean(Credit.Amount))
         train1_DA<-arrange(train1_DA, desc(decile))
         View(train1_DA)
         
         write.csv(train1_DA,"train1_DA.csv")
         
         # DECILE ANALYSIS - Testing
         test1<-cbind(test, ln_Credit.Amount_p=predict(fit2, newdata=test), Credit.Amount_p=exp(predict(fit2, newdata=test)) )
         
         # find the decile locations 
         decLocations <- quantile(test1$Credit.Amount_p, probs = seq(0.1,0.9,by=0.1))
         
         
         # use findInterval with -Inf and Inf as upper and lower bounds
         test1$decile <- findInterval(test1$Credit.Amount_p,c(-Inf,decLocations, Inf))
         summary(test1$decile)
         xtabs(~decile,test1)
         
         require(dplyr)
         test1$decile<-factor(test1$decile)
         decile_by<-group_by(test1, decile)
         test1_DA<-summarise(decile_by, count=n(), Avg_pred_amt=mean(Credit.Amount), 
                             Avg_actual_amt= mean(Credit.Amount_p))
         test1_DA<-arrange(test1_DA, desc(decile))
         View(test1_DA)
         write.csv(test1_DA,"test1_DA.csv")
         
         
        # Decision Tree
         
         setwd("Z:\\Team IEG GGN\\Alisha\\CIBA Analytics P3")
         install.packages("ggplot2")
         install.packages("magrittr")
         install.packages("RGtk2")
         install.packages("stringi")
         install.packages("stringr")
         install.packages("tidyr")
         install.packages("dplyr")
         install.packages("rattle"); 
         install.packages("rpart.plot");
         install.packages("RColorBrewer")
         install.packages("party"); 
         install.packages("partykit")
         
         cust_data<-read.csv("reg data.csv")
         
         library(rpart)      	                                   # Popular decision tree algorithm
         library(rattle)			# Fancy tree plot
         library(rpart.plot)			# Enhanced tree plots
         library(RColorBrewer)		# Color selection for fancy tree plot
         library(party)		                  # Alternative decision tree algorithm
         library(partykit)			# Convert rpart object to BinaryTree
         
         fit <- rpart(SeriousDlqin2yrs ~ 
                        RevolvingUtilizationOfUnsecuredLines +
                        NumberOfTime30.59DaysPastDueNotWorse+
                        MonthlyIncome+ NumberOfDependents, 
                      method="class", data=cust_data,control=rpart.control(minsplit = 2000))
         
         summary(fit)  # detailed summary of splits
         print(fit)
         
         fancyRpartPlot(fit,cex=0.7)
         printcp(fit)
         
         pfit<-prune(fit,cp=0.014493)
         fancyRpartPlot(pfit,cex=.6)
         
         
         #___________________________ANOVA TREE_________________________________________#
         
         mydata<-read.csv("car_sales.csv")
         names(mydata)
         
         fit <- rpart(Sales_in_thousands ~ X__year_resale_value + Price_in_thousands+
                        Engine_size+Horsepower+Wheelbase+Width+
                        Length+Curb_weight+Fuel_capacity+Fuel_efficiency, 
                      method="anova", control = rpart.control(minsplit = 25), data=mydata)
         
         # Tree Plots 
         fancyRpartPlot(fit,cex=.8)
         
         #_______________________________ C50__________________________________#
         
         install.packages("C50")
         require(C50)
         cust_data$SeriousDlqin2yrs<-as.factor(cust_data$SeriousDlqin2yrs)
         treeModel <- C5.0(x = cust_data[, -20], y = cust_data$SeriousDlqin2yrs)
         summary(treeModel)
         plot(treeModel,subtree=1)
         
         
         
         #- - - - - - - - - - 
         ChurnTrain<-read.csv("ChurnTrain.csv")
         ruleModel <- C5.0(churn ~ ., data = ChurnTrain, rules = FALSE)
         ruleModel
         summary(ruleModel)
         
         t1<-cbind(ChurnTrain, predcited=predict(ruleModel, newdata=ChurnTrain, type="class"))
         table(t1$churn, t1$predcited)
         
         #_________________________ PARTY PACKAGE - CATEGORICAL _____________________#
         
         library(party)
         library(rattle)
         fit2<-ctree(churn~., data=ChurnTrain, control = ctree_control(maxdepth=4))
         plot(fit2, main="Conditional Inference Tree for customer data")
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         RFM SEGMETNATION - Recency/Frequency/Monetary 
         
         Recency_cuts <- quantile(Customer_data$No_of_days_frm_L_pur, probs = seq(0.25,0.75,by=0.25)) - Column that depicts Recency
         Freq_cuts<-quantile(Customer_data$No_baskets, probs = seq(0.25,0.75,by=0.25))
         Monetary_cuts<-quantile(Customer_data$total_sale, probs = seq(0.25,0.75,by=0.25))
         
         Customer_data$recency_seg <-findInterval(Customer_data$No_of_days_frm_L_pur,c(-Inf,Recency_cuts, Inf))
         Customer_data$freq_seg  <-findInterval(Customer_data$No_baskets,c(-Inf,Freq_cuts, Inf))
         Customer_data$monetary_seg  <-findInterval(Customer_data$total_sale,c(-Inf,Monetary_cuts, Inf))
         
         xtabs(~recency_seg,Customer_data)
         xtabs(~freq_seg,Customer_data)
         xtabs(~monetary_seg,Customer_data)
         
         (seg_size<-xtabs(~freq_seg+monetary_seg,Customer_data))
         (seg_val<-xtabs(total_sale~freq_seg+monetary_seg,Customer_data))
         prop.table(seg_size)*100
         prop.table(seg_val)*100
         
         # Labeling 
         
         #Customer_data$Recency_label<-NULL
         Customer_data$Recency_label[Customer_data$recency_seg == 1] <- "1.Active"
         Customer_data$Recency_label[Customer_data$recency_seg == 2 | Customer_data$recency_seg == 3] <- "2.At Risk"
         Customer_data$Recency_label[Customer_data$recency_seg ==4] <- "3.Churn"
         table(Customer_data$Recency_label)
         
         #Customer_data$label<- NULL 
         Customer_data$label[Customer_data$monetary_seg == 4 & Customer_data$freq_seg ==1] <- "1.Silver"
         Customer_data$label[Customer_data$monetary_seg == 3 & Customer_data$freq_seg ==1] <- "1.Silver"
         Customer_data$label[Customer_data$monetary_seg == 2 & Customer_data$freq_seg ==2] <- "1.Silver"
         Customer_data$label[Customer_data$monetary_seg == 3 & Customer_data$freq_seg ==2] <- "1.Silver"
         Customer_data$label[Customer_data$monetary_seg == 4 & Customer_data$freq_seg ==4] <- "2.Gold"
         Customer_data$label[Customer_data$monetary_seg == 1 & Customer_data$freq_seg ==3] <- "1.Silver"
         Customer_data$label[Customer_data$monetary_seg == 2 & Customer_data$freq_seg ==3] <- "1.Silver"
         Customer_data$label[Customer_data$monetary_seg == 3 & Customer_data$freq_seg ==3] <- "2.Gold"
         Customer_data$label[Customer_data$monetary_seg == 4 & Customer_data$freq_seg ==3] <- "2.Gold"
         Customer_data$label[Customer_data$monetary_seg == 1 & Customer_data$freq_seg ==4] <- "1.Silver"
         Customer_data$label[Customer_data$monetary_seg == 2 & Customer_data$freq_seg ==4] <- "2.Gold"
         Customer_data$label[Customer_data$monetary_seg == 3 & Customer_data$freq_seg ==4] <- "2.Gold"
         Customer_data$label[Customer_data$monetary_seg == 4 & Customer_data$freq_seg ==4] <- "3.Premium"
         Customer_data$label[is.na(Customer_data$label)==T] <- "0.Standard"
         
         Customer_data$Recency_label<-factor(Customer_data$Recency_label)
         Customer_data$label<-factor(Customer_data$label)
         
         
         #Segment sizes for Active customers
         (segment_cnt_table<-xtabs(~label+Recency_label, Customer_data))
         (segment_value_table<-xtabs(total_sale~label+Recency_label, Customer_data))
         
         prop.table(segment_cnt_table)*100
         prop.table(segment_value_table)*100
         
         
         
         
         
         
         
         
         
         
         
         
         # Time Series
         
         setwd("Z:\\Team IEG GGN\\Alisha\\CIBA Analytics P3")
         
         tsdata <- read.csv("TS.csv")
         View(tsdata)
         
         # Frequency == Number of weeks, Start - End Bracket (Weekly/Monthly/Quarterly)
         myts   <- ts(tsdata$Sales, start=c(2008, 1), end=c(2011, 4), frequency=4) 
         myts
         plot(myts)
         
         # Type -- Additive(No increase in Time)
         # Multiplicative --Increasing with Time (Always use multiplicative)
         
         decompose(myts, type = c("multiplicative"))
         plot(decompose(myts, type = c("multiplicative")))
         
         #_________________________ PATTERN LESS DATA - Exponential Smoothening __________________________#
         
         #Both the HoltWinters() function in the base installation, and  the ets() function in the forecast package, can be used to fit exponential models
         #Forecast package -- simple exponential - models level
         
         library(forecast)
         
         # Both FALSE - Single Exp Smoothening , Alpha - Trend , Beta - Seasonality
         fit1 <- HoltWinters(myts, beta=FALSE, gamma=FALSE)
         accuracy(fit1$fitted, myts)
         
         #Double Exponential - models level and trend (Check MAPE)
         fit2 <- HoltWinters(myts, gamma=FALSE)
         accuracy(fit2$fitted, myts)
         forecast(fit2)
         
         forecast(fit2, 4)
         plot(forecast(fit2, 4))
         
         #____________________________ETS(Expo Smoothening Trend- PATTERN DATA )_________________________#
         
         fit4 <- ets(myts)
         summary(fit4)
         accuracy(fit4$fitted, myts)
         forecast(fit4,4)
         plot(forecast(fit4,4))
         
         #___________________________________ A R I M A _____________________________________________  #
         
         # Automated forecasting using an ARIMA model (p,d,q)
         acf(myts) # For p , Autocorrelation Check Graph and see the correlation value
         pacf(myts) # For q ,Partial Autocorrelation
         
         # To get D ( ingegrated ) t-series package Augumented Dickey Fuller (to check stationary)
         install.packages("tseries")
         library(tseries)
         adf.test(myts)
         ndiffs(myts)
         
         # To get automatically 
         fit <- auto.arima(myts)
         summary(fit)
         
         
         
         
         
         # Concordance
         
         
         # Assuming the input is a stored binomial GLM object
         Concordance = function(GLM.binomial) {
           outcome_and_fitted_col = cbind(GLM.binomial$y, GLM.binomial$fitted.values)
           # get a subset of outcomes where the event actually happened
           ones = outcome_and_fitted_col[outcome_and_fitted_col[,1] == 1,]
           # get a subset of outcomes where the event didn't actually happen
           zeros = outcome_and_fitted_col[outcome_and_fitted_col[,1] == 0,]
           # Equate the length of the event and non-event tables
           if (length(ones[,1])>length(zeros[,1])) {ones = ones[1:length(zeros[,1]),]}
           else {zeros = zeros[1:length(ones[,1]),]}
           # Following will be c(ones_outcome, ones_fitted, zeros_outcome, zeros_fitted)
           ones_and_zeros = data.frame(ones, zeros)
           # initiate columns to store concordant, discordant, and tie pair evaluations
           conc = rep(NA, length(ones_and_zeros[,1]))
           disc = rep(NA, length(ones_and_zeros[,1]))
           ties = rep(NA, length(ones_and_zeros[,1]))
           for (i in 1:length(ones_and_zeros[,1])) {
             # This tests for concordance
             if (ones_and_zeros[i,2] > ones_and_zeros[i,4])
             {conc[i] = 1
             disc[i] = 0
             ties[i] = 0}
             # This tests for a tie
             else if (ones_and_zeros[i,2] == ones_and_zeros[i,4])
             {
               conc[i] = 0
               disc[i] = 0
               ties[i] = 1
             }
             # This should catch discordant pairs.
             else if (ones_and_zeros[i,2] < ones_and_zeros[i,4])
             {
               conc[i] = 0
               disc[i] = 1
               ties[i] = 0
             }
           }
           # Here we save the various rates
           conc_rate = mean(conc, na.rm=TRUE)
           disc_rate = mean(disc, na.rm=TRUE)
           tie_rate = mean(ties, na.rm=TRUE)
           Somers_D<-conc_rate - disc_rate
           gamma<- (conc_rate - disc_rate)/(conc_rate + disc_rate)
           #k_tau_a<-2*(sum(conc)-sum(disc))/(N*(N-1)
           return(list(concordance=conc_rate, num_concordant=sum(conc), discordance=disc_rate, num_discordant=sum(disc), tie_rate=tie_rate,num_tied=sum(ties),
                       somers_D=Somers_D, Gamma=gamma))
           
         }
         
         #Concordance(m4)
         
         
         
         
         
         
         
         
         
         
         
         #Ensemble Learning
         
         ################### ENSEMBLE MODELS ####################################
         
         rm(list=ls())
         
         #Load Library
         library(h2o)   
         library(randomForest)
         library(rpart)
         require(graphics)
         
         #Setwd
         setwd("C:/Users/admin/Desktop/5. Ensemble Learning")
         
         #Input
         train <- read.csv(file="InputDT.csv", header = TRUE, sep = ",")
         test <- read.csv(file="TestDT.csv", header = TRUE, sep = ",")
         
         ########## DECISION TREE - Model Building ###############
         rpartfit <- rpart(Status_Final~., data=train)
         
         #Prediction with modified model
         rpart_pred1 <- predict(rpartfit, test, type = "class")
         
         #Rpart - Prediction accuracy on Test Data
         table(rpart_pred1,test$Status_Final)
         mean(rpart_pred1==test$Status_Final)
         
         ############ RF with Caret ############## 
         
         library(caret)
         
         #Build Model
         modfit <- train(Status_Final~ .,method="rf",data=train)
         
         #Training Data
         pred <- predict(modfit,train)
         table(pred,train$Status_Final)
         mean(pred==train$Status_Final)
         
         #Test Data
         pred.rf<-predict(modfit,newdata=test)
         table(pred.rf,test$Status_Final)
         mean(pred.rf==test$Status_Final)
         
         ########## ADABOOSTING METHOD ###############
         library(adabag)
         adaboost<-boosting(Status_Final~., data=train, boos=TRUE, mfinal=100,coeflearn='Breiman')
         summary(adaboost)   #mfinal=20 indicates the times of repeated process is 20
         
         #Perf on valid data
         adaboost_pred1 <-predict(adaboost,test)
         table(test$Status_Final,adaboost_pred1$class)
         mean(test$Status_Final==adaboost_pred1$class)
         
         
         ######## WITH H2O ###########
         
         ## Create an H2O cloud 
         h2o.init(
           nthreads=-1,            ## -1: use all available threads
           max_mem_size = "8G")    ## specify the memory size for the H2O cloud
         
         h2o.removeAll()           ## Clean slate - just in case the cluster was already running
         
         # Load a file from disk
         train <- h2o.importFile(path = normalizePath("inputDT.csv"))
         test <- h2o.importFile(path = normalizePath("TestDT.csv"))
         
         #Assignment within H2o
         train <- h2o.assign(train, "train.hex")   #Train data: H2O name train.hex
         test <- h2o.assign(test, "test.hex")     #Test data: H2O name test.hex
         
         ###################### MODEL-1: RANDOM FOREST - run our first predictive model with RF ###################
         rf1 <- h2o.randomForest(         
           training_frame = train,        
           validation_frame = test,      
           x=2:13,                        # the predictor columns, by column index
           y=1,                             # the target index (what we are predicting)
           model_id = "rf_covType_v1",    # name the model in H2O
           # not required, but helps use Flow
           ntrees = 250,                  # use 50-250 trees, stopping criteria will decide finally...
           stopping_rounds = 2,           # Stop fitting new trees when the 2-tree
           score_each_iteration = T,      # Predict against training and validation for each tree.
           seed = 1500000)  
         
         #Performance Evaluation
         summary(rf1)    
         rf1@model$validation_metrics   # A more direct way to access the validation 
         rf1@model$validation_metrics@metrics$AUC    #AUC
         rf1@model$validation_metrics@metrics$Gini   #Gini
         
         final_predictions_rf1<-h2o.predict(object = rf1,newdata = test)
         mean(final_predictions_rf1$predict==test$Status_Final)  # test set accuracy
         
         #h2o.hit_ratio_table(rf1,valid = FALSE, xval=FALSE)[1,2]
         
         ################ A Modified RF Model ####################
         rf2 <- h2o.randomForest(        ##
           training_frame = train,       ##
           validation_frame = test,     ##
           x=2:13,                       ##
           y=1,                         ##
           model_id = "rf_covType2",     ## 
           ntrees = 250,                 ##
           max_depth = 5,               ## Increase depth, from 20
           stopping_rounds = 15,          ##
           stopping_tolerance = 0.0001,    ##
           score_each_iteration = T,     ##
           seed=5555555)                 ##
         
         #Performance Evaluation
         summary(rf2)    
         rf2@model$validation_metrics   # A more direct way to access the validation 
         rf2@model$validation_metrics@metrics$AUC    #AUC
         rf2@model$validation_metrics@metrics$Gini   #Gini
         
         final_predictions_rf2<-h2o.predict(object = rf2,newdata = test)
         mean(final_predictions_rf2$predict==test$Status_Final)  # Test set accuracy
         
         ######################### NOW TRY GBM MODELS #########################
         ## MODEL-1: DEFAULT GBM ##
         gbm1 <- h2o.gbm(training_frame = train,        # the H2O frame for training
                         validation_frame = test,      # the H2O frame for validation (not required)
                         x=2:13,                        # the predictor columns, by column index
                         y=1,                          # the target index (what we are predicting)
                         model_id = "gbm_covType1",     # name the model in H2O
                         seed = 55005)      
         
         
         #Performance Evaluation
         summary(gbm1)    
         gbm1@model$validation_metrics   # A more direct way to access the validation 
         gbm1@model$validation_metrics@metrics$AUC    #AUC
         gbm1@model$validation_metrics@metrics$Gini   #Gini
         
         final_predictions_gbm1<-h2o.predict(object = gbm1,newdata = test)
         mean(final_predictions_gbm1$predict==test$Status_Final)  # test set accuracy
         
         #### MODEL-2: MODIFIED GBM ########
         gbm3 <- h2o.gbm(
           training_frame = train,     ##
           validation_frame = test,   ##
           x=2:13,                     ##
           y=1,                       ## 
           ntrees = 250,                ## add a few trees (from 20, though default is 50)
           learn_rate = 0.05,           ## increase the learning rate even further
           max_depth = 4,             ## 
           sample_rate = 0.4,          ## use a random 50% of the rows to fit each tree
           col_sample_rate = 0.85,       ## use 85% of the columns to fit each tree
           stopping_rounds = 25,        ## 
           stopping_tolerance = 0.0005,  ##
           score_each_iteration = T,   ##
           model_id = "gbm_covType3",  ##
           seed = 25000000)             ##             ##
         
         
         #Performance Evaluation
         summary(gbm3)    
         gbm3@model$validation_metrics   ## A more direct way to access the validation 
         gbm3@model$validation_metrics@metrics$AUC    #AUC
         gbm3@model$validation_metrics@metrics$Gini   #Gini
         
         final_predictions_gbm3<-h2o.predict(
           object = gbm3,
           newdata = test)
         mean(final_predictions_gbm3$predict==test$Status_Final)  ## test set accuracy
         
         # All done, shutdown H2O    
         h2o.shutdown(prompt=FALSE)
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         # Significance Testing
         
         getwd()
         read.csv("Credit Card Data.csv")
         
         my_data<-read.csv("Credit Card Data.csv")
         View(mydata)
         
         #Data Summary
         nrow(mydata)
         names(mydata)
         library(psych)
         describe(mydata)
         
         t.test(Calls.Resulting.in.DB.Workflow ~ ClientType, data = df_test) # P value 0.9; there is no difference in mean
         
         # 1 - Simple t test (not Z because population Std Dev is not known, sample size is small)
         t.test(mydata$Post_usage_1month,mu=50) # Rejecting Null Hypothesis
         
         # 2 - Pre and Post or Before and After ( Paired T test ) 
         t.test(mydata$Post_usage_1month,mydata$pre_usage,paired=TRUE,conf.level = 0.90)
         
         # 3- Males and Females - 2 Sample Independent T test 
         t.test(mydata$Latest_mon_usage~mydata$sex,data=mydata,var.equal=TRUE) # Equal Variance
         t.test(mydata$Latest_mon_usage~mydata$sex,data=mydata,var.equal=FALSE) # Unequal Variance
         
         # Since the p values in both the cases are almost equal we can assume that variance is Equal - To check variance use Histogram 
         var.test(women$`Height in cms`,women$`Weight in kgs`)
         
         # 4 - Segments are more than 2 so we will use One way ANOVA
         Anova_Segment<-aov(Latest_mon_usage~factor(segment),data=mydata)
         summary(Anova_Segment)
         # F value greater than 1 , difference is observed , across groups is different
         model.tables(Anova_Segment,"means")
         # To calculate the frequency of each factor
         table(mydata$segment)
         
         # 5 - Region and Segment are categorical - Chi square (Ho: They are independent, H1: They are associated)
         # Method 1
         region<-as.character(mydata$region)
         segment<-as.character(mydata$segment)
         chisq.test(region,segment)
         
         #Method 2
         tab<-xtabs(~region+segment,data=mydata)
         chisq.test(tab)
         
         # 6 - Correlation/ Relationship between 2 variables
         cor(mydata$pre_usage,mydata$Latest_mon_usage,method = 'pearson')
         
         # Same unit for both the variables
         cov(mydata$pre_usage,mydata$Latest_mon_usage)
         plot(mydata$pre_usage,mydata$Latest_mon_usage , main ="Correlation Plot",xlab="Pre Usage",
              ylab="Latest Month")
         
         
         
         
         
         
         
         
         
         
         
         
         
         #####################################################################################################
         ##################################### Linear Regression ######################################################
         
         getwd()
         
         Creditdata<-read.csv("Credit Card Data.csv")
         View(Creditdata)
         
         names(Creditdata)
         nrow(Creditdata)
         ncol(Creditdata)
         
         library(psych)
         describe(Creditdata)
         str(Creditdata)
         
         # Converting to categorical values
         
         Creditdata$Purpose=as.factor(Creditdata$Purpose) 
         Creditdata$Sex...Marital.Status=as.factor(Creditdata$Sex...Marital.Status) 
         Creditdata$Type.of.apartment=as.factor(Creditdata$Type.of.apartment)
         Creditdata$Most.valuable.available.asset=as.factor(Creditdata$Most.valuable.available.asset) 
         Creditdata$Occupation=as.factor(Creditdata$Occupation) 
         Creditdata$Telephone=as.factor(Creditdata$Telephone) 
         Creditdata$Foreign.Worker=as.factor(Creditdata$Foreign.Worker)
         
         # List of numeric and categorical Data variable
         
         num_var=c("Account.Balance","Duration.of.Credit..month.",
                   "Payment.Status.of.Previous.Credit", 
                   "Credit.Amount","Value.Savings.Stocks","No.of.dependents","Age..years.",
                   "Concurrent.Credits","No.of.Credits.at.this.Bank","Length.of.current.employment", 
                   "Instalment.per.cent","Duration.in.Current.address", "Guarantors") 
         
         cat_var=c("Purpose","Sex...Marital.Status","Type.of.apartment","Most.valuable.available.asset",
                   "Occupation","Telephone","Foreign.Worker")
         
         
         # Creating User defined function 
         
         var_Summ=function(x){
           if(class(x)=="numeric"){
             Var_Type=class(x)
             n<-length(x)
             nmiss<-sum(is.na(x))
             mean<-mean(x,na.rm=T)
             std<-sd(x,na.rm=T)
             var<-var(x,na.rm=T)
             min<-min(x,na.rm=T)
             p1<-quantile(x,0.01,na.rm=T)
             p5<-quantile(x,0.05,na.rm=T)
             p10<-quantile(x,0.1,na.rm=T)
             q1<-quantile(x,0.25,na.rm=T)
             q2<-quantile(x,0.5,na.rm=T)
             q3<-quantile(x,0.75,na.rm=T)
             p90<-quantile(x,0.9,na.rm=T)
             p95<-quantile(x,0.95,na.rm=T)
             p99<-quantile(x,0.99,na.rm=T)
             max<-max(x,na.rm=T)
             UC1=mean(x,na.rm=T)+3*sd(x,na.rm=T)
             LC1=mean(x,na.rm=T)-3*sd(x,na.rm=T)
             UC2=quantile(x,0.99,na.rm=T)
             LC2=quantile(x,0.01,na.rm=T)
             iqr=IQR(x,na.rm=T)
             UC3=q3+1.5*iqr
             LC3=q1-1.5*iqr
             ot1<-max>UC1 | min<LC1 
             ot2<-max>UC2 | min<LC2 
             ot3<-max>UC3 | min<LC3
             
             
             return(c(Var_Type=Var_Type,n=n,nmiss=nmiss,mean=mean,std=std,var=var,min=min,
                      p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,
                      p95=p95,p99=p99,max=max,ot_m1=ot1,ot_m2=ot2,ot_m2=ot3))
           }
           else{
             Var_Type=class(x)
             n<-length(x)
             nmiss<-sum(is.na(x))
             fre<-table(x)
             prop<-prop.table(table(x))
             #x[is.na(x)]<-x[which.max(prop.table(table(x)))]
             
             return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,freq=fre,proportion=prop))
           }
         }
         
         var_Summ(my_data$Account.Balance)
         
         # Vector of numerical variable
         num_var=sapply(Creditdata,is.numeric)
         other_var=!sapply(Creditdata,is.numeric)
         View(num_var)
         
         diag_stats<-data.frame(t(apply(Creditdata[num_var], 2, var_Summ)))
         
         # Missing Values Treatment # 2 for column
         
         Creditdata[num_var] <- apply(data.frame(Creditdata[,num_var]), 2, function(x){x <- replace(x, is.na(x), mean(x, na.rm=TRUE))})
         Creditdata[cat_var] <- apply(data.frame(Creditdata[,cat_var]), 2, function(x){x <- replace(x, is.na(x), which.max(prop.table(table(x))))})
         
         
         # Handling outliers 
         
         library(psych)
         describe(Creditdata)
         
         Creditdata$Credit.Amount[Creditdata$Credit.Amount>16000]<-quantile(Creditdata$Credit.Amount,0.99,na.rm=T)
         View(Creditdata)
         
         help("quantile")
         
         # Distribution of Dependent Variable
         hist(Creditdata$Credit.Amount)
         hist(log(Creditdata$Credit.Amount)) # Normal
         
         # Scatterplot 
         require(car) 
         scatterplotMatrix(~Duration.of.Credit..month.+Credit.Amount+No.of.dependents+Age..years. +No.of.Credits.at.this.Bank+ Instalment.per.cent+
                             Duration.in.Current.address, data=Creditdata)
         
         # Corrplot
         library(corrplot) 
         corrplot(cor(Creditdata[, num_var],use="pairwise.complete.obs"), method = "number", tl.cex = 0.5)
         
         
         
         
         # Train and test data (70:30)
         
         sample_size<-floor(0.70*nrow(Creditdata)) # Floor function to round off
         
         set.seed(123)
         train_ind<-sample(1:nrow(Creditdata),size = sample_size)
         train<-Creditdata[train_ind,]
         test<-Creditdata[-train_ind,]
         
         # Regression Run # Check R^2 and F statistic value (Higher is better to compare)
         fit<-lm(Credit.Amount~Account.Balance+ Duration.of.Credit..month.+
                   Payment.Status.of.Previous.Credit+ 
                   Value.Savings.Stocks+ No.of.dependents+ Age..years.+ Concurrent.Credits+ 
                   No.of.Credits.at.this.Bank+ Length.of.current.employment+ 
                   Instalment.per.cent+ Duration.in.Current.address+ Guarantors, data=train) 
         
         summary(fit)
         
         # VIF -- Accepted till value 4 
         library(car)
         vif(fit)
         
         ## Next step remove metrics that have LOW business value
         
         # Stepwise Regression # Droping-significant impact on R^2
         # keep variables with atleast 80% CIF(p=0.50)
         
         library(MASS)
         step3<-stepAIC(fit,direction="both") # Select the one with Min AIC (Akakie Information Criteria)
         # Drop insignificant variables RUN regression again and check the accuracy of the sample
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         Neural Tree
         
         concrete<-read.csv("concrete.csv") str(concrete) normalize<-function(x){return((x-min(x))/(max(x)-min(x)))} normlize(c(2,6,7,10,15)) 
         concrete_ravi<-as.data.frame(lapply(concrete,normalize)) str(concrete_ravi) 
         credit_train<-concrete_ravi[1:750,] credit_test<-concrete_ravi[751:1030,] 
         install.packages("neuralnet") library(neuralnet) 
         credit_model<-neuralnet(strength~.,data=credit_train) credit_model<-neuralnet(strength~cement ,data=credit_train) credit_model<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data=credit_train) credit_model<-neuralnet(strength~.,data=credit_train) credit_model<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data=credit_train) credit_model<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data=credit_train) 
         plot(credit_model) credit_model<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data=credit_train,hidden=2) 
         plot(credit_model) credit_model<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data=credit_train,hidden=10) 
         plot(credit_model) credit_model<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data=credit_train) 
         plot(credit_model) 
         model_results<-compute(credit_model,credit_test[1:8]) cor(credit_test[9],model_results) 
         str(model_results) cor(credit_test[9],model_results) cor(model_results,credit_test[9]) 
         credit_test[9] model_results 
         abc<-model_results$net.result cor(abc,credit_test[9]) 
         model_results$neurons model_results$net.result 
         cor(model_results$net.result,credit_test[9]) model_results<-compute(credit_model,credit_test[1:8]) 
         credit_model<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data=credit_train,hidden=5) 
         plot(credit_model) model_results<-compute(credit_model,credit_test[1:8]) cor(model_results$net.result,credit_test[9]) 
         
         
         
         
         
         
         
         Text Analytics : Word Cloud and Sentiment Analysis
         
         
         library(tm)
         library(wordcloud)
         library(RColorBrewer)
         library(ggplot2)
         library(ROAuth)
         require(RCurl)
         library(stringr)
         library(RSentiment)
         library(ggmap)
         library(dplyr)
         library(plyr)
         
         getwd()
         setwd("Z:\\Team IEG GGN\\Alisha\\Text Analytics Code")
         
         speech = "Dibya.txt"
         modi_txt = readLines(speech)
         
         # Corpus is a collection of documents #
         modi<-Corpus(VectorSource(modi_txt))
         inspect(modi)[1:10]
         
         ### Cleaning Data ###
         
         modi<-tm_map(modi,stripWhitespace)
         modi<-tm_map(modi,tolower)
         modi<-tm_map(modi,removeNumbers)
         modi<-tm_map(modi,removePunctuation)
         modi<-tm_map(modi,removeWords, stopwords('english'))
         modi<-tm_map(modi,removeWords, c('please','hello','june',
                                          'kindly','thank','sent','may','pay'))
         
         inspect(modi)[1:10]
         
         ### Term Documentation Matrix -- frequency of words ###
         
         tdm_modi<-TermDocumentMatrix(modi)
         TDM1<-as.matrix(tdm_modi)
         
         # Most common words
         v = sort(rowSums(TDM1), decreasing = TRUE)
         # that occur atleast 20 times
         (freq.terms <- findFreqTerms(tdm_modi, lowfreq = 20))
         print(freq.terms)
         
         term.freq <- rowSums(as.matrix(tdm_modi))
         term.freq <- subset(term.freq, term.freq >= 20)
         df <- data.frame(term = names(term.freq), freq = term.freq)
         Words<-sort(term.freq,decreasing = TRUE)
         Word_list<-data.frame(Name=names(Words),freqrency=term.freq)
         Word_list
         
         ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") + xlab("Terms") + ylab("Count") + coord_flip() +theme(axis.text=element_text(size=7))
         
         barplot(df[1:10,]$freq, las = 2, names.arg = df[1:10,]$term,
                 col ="Grey", main ="Most frequently occuring words",
                 ylab = "Word frequencies")
         
         # WORD CLOUD #
         # max.words = Number of word in WC -- rot.per=% of vertical text--scale=diff between largest and smallest font #
         
         wordcloud (modi, max.words=25, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(5,'Dark2'))
         
         
         ##################### Sentiment Analysis ######################
         
         
         # Word sentiment
         
         calculate_score(v)
         m<-calculate_sentiment(modi_txt)
         nrow(m)
         Final_data<-data.frame(text=modi_txt,Sentiments=m$sentiment)
         nrow(Final_data)
         
         
         # List of Positive and Negetive Words (Text files )
         
         positives= readLines("positive_words.txt")
         negatives = readLines("negative_words.txt")
         
         # Calculating score for one tweets 
         
         score <- function(tweet1, positive_words, negative_words) {
           word_list = str_split(tweet1, " ")
           words_1 = unlist(word_list)
           # compare words to the dictionaries of positive & negative terms
           positive_matches = match(words_1, positive_words)
           #print(positive_matches)
           negative_matches = match(words_1, negative_words)
           #print(negative_matches)
           # get the position of the matched term or NA
           # we just want a TRUE/FALSE
           positive_matches = !is.na(positive_matches)
           negative_matches = !is.na(negative_matches)
           
           # final score
           score = sum(positive_matches) - sum(negative_matches)
           return(score)
         }
         
         #Calculating scores of all text 
         
         sentiment_scores = function(tweets, positive_words, negative_words){
           n <- length(tweets)
           finalScore <- numeric(n)
           i <- 1
           while(i <= n) {
             finalScore[i] <- score(tweets[i], positive_words, negative_words)
             i <- i + 1
           }
           
           return(finalScore)
         }
         
         # Naming the Data as tweets
         tweets <- modi
         temp <- sentiment_scores(tweets, positives, negatives)
         View(temp)
         freq_table <- table(temp)
         freq_table1<-data.frame(table(temp))
         
         
         
         
         
         
         #Creaitng data and plotting pie chart
         
         extremely_satisfied <- sum(temp > 2)
         satisfied<- sum(temp >=1 & temp <= 2)
         Neutral <- sum(temp == 0)
         Frustrated <- sum(temp < -2)
         Dissatisfied <- sum(temp <= -1 & temp >= -2)
         
         table_1 <- data.frame(extremely_satisfied,satisfied,Neutral,Frustrated,Dissatisfied)
         new_vector <- c(extremely_satisfied, satisfied, Neutral,Frustrated,Dissatisfied)
         percentlabels<- round(100*new_vector/sum(new_vector), 1)
         pielabels<- paste(percentlabels, "%", sep="")
         
         # Creaitng data for overall and drawing pie chart
         
         positive <- sum(temp > 0)
         negative <- sum(temp < 0)
         neutral <- sum(temp == 0)
         table_2 <- data.frame(positive ,negative,neutral)
         
         create_table <- c(positive,negative,Neutral)
         percentlabels_1<- round(100*create_table/sum(create_table), 1)
         pielabels_1<- paste(percentlabels_1, "%", sep="")
         
         #plotting all graphs
         get( getOption( "device" ) )()
         set.seed(1)
         par(mfrow=c(2,2))
         freq_table1$Freq <- as.numeric(as.character(freq_table1$Freq))
         ylim <- c(0, 1.1*max(freq_table1$Freq))
         
         xx<-barplot(freq_table, xaxt='n',width=.85, ylim=ylim, cex.names=0.5, col="aquamarine3", cex.lab=1:1)
         title(xlab ="Scores",ylab="frequency",main="Scores Distribution")
         
         ## Add text at top of bars
         text(x = xx, y = freq_table1$Freq, label = freq_table1$Freq, pos = 3, cex = 0.8, col = "red")
         
         ## Add x-axis labels 
         axis(1, at=xx, labels=freq_table1$temp, tick=FALSE, las=1, line=-0.5, cex.axis=0.8)
         #axis(1, xaxp=c(-6,11,18), las=1)
         
         pie(new_vector, labels = pielabels, col = c("coral", "cyan3", "red","darkgrey","yellow") ,cex.lab=1:1)
         legend( "topright", c("extremely_satisfied","satisfied","Neutral","Frustrated","Dissatisfied"), cex=0.8, 
                 fill=c("coral","cyan3","red","darkgrey","yellow"))
         
         pie(create_table, labels = pielabels_1, col = c("green", "cyan3", "red" ),cex.lab=1.1)
         legend("topright", c("+ve","-ve","Neutral"), cex = 0.8, fill =c("green","cyan3","red"))
         
         
         
         
         
         
         
         
         
         
         
         