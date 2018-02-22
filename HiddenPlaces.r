---
title: "HiddenPlaces_OR568_FinalProject"
author: "Anya, Nuria, Rachel, Vamsi"
date: "May 14, 2017"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

############################################################################################################################
############################################################################################################################
##################################################------ LOADING PACKAGES ------############################################
############################################################################################################################
#########################--Please Note: Some packages will be loaded through out the code as needed --- ####################
############################################################################################################################
############################################################################################################################

#install package 
```{r}
#install.packages("plyr")
#install.packages("dplyr")
#install.packages("lubridate")
#install.packages("rpart")
#install.packages("randomForest")
library(randomForest)
library(rpart)
library(lubridate)
library(dplyr)
library(plyr)
```

############################################################################################################################
############################################################################################################################
##################################################------ LOADING DATA ------################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################


```{r}

##Loading 4 different instances of the dataset, so each modeler an preform their own cleaning.

data_lm <- read.csv("data_merged_R.csv")
data_glm <-read.csv("data_merged_R.csv")
data_gb <-read.csv("data_merged_R.csv")
data_rf <-read.csv("data_merged_R.csv")
  
```

############################################################################################################################
############################################################################################################################
##################################################------  MODELING SECTION    ------########################################
############################################################################################################################
############################################################################################################################
###################################################-------LINEAR  MODELNG--------###########################################
############################################################################################################################
```{r}
                                                    ##### LINEAR MODEL ##### 

##Rename the columns

colnames(data_lm)[colnames(data_lm)=="Rate_crimes_pt1_violent_2016"] <- "CrimeViolent"
colnames(data_lm)[colnames(data_lm)=="Rate_crimes_pt1_property_2016"] <- "CrimeProperty"
colnames(data_lm)[colnames(data_lm)=="House_Median_price_per_sqft_2016"] <- "House"
colnames(data_lm)[colnames(data_lm)=="income_Sum"] <- "income"

data_merged <-data_lm

##change data type
mode(data_merged)
data_merged$Latitude  <-as.numeric(as.list(data_merged$Latitude))
data_merged$Longitude<-as.numeric(as.list(data_merged$Longitude))
data_merged$review_count<-as.numeric(as.list(data_merged$review_count))
data_merged$rating <-as.numeric(as.list(data_merged$rating))
data_merged$price <-as.numeric(as.list(data_merged$price))
data_merged$CrimeViolent<-as.numeric(as.list(data_merged$CrimeViolent))
data_merged$CrimeProperty<-as.numeric(as.list(data_merged$CrimeProperty))
data_merged$House <-as.numeric(as.list(data_merged$House))
data_merged$Income <-as.numeric(as.list(data_merged$income))

# normalizing the data
data_merged1 <- data_merged %>% mutate_each_(funs(scale(.) %>% as.vector), vars=c("review_count","rating","price","CrimeViolent","CrimeProperty","House","Income"))

 data = data_merged1
```

```{r}
##Transforming the datasets

 year_rf<-year(as.Date(as.character(data$Date),"%m/%d/%Y"))
 month_rf<-month(as.Date(as.character(data$Date),"%m/%d/%Y"),label=TRUE)
 dayofwk_rf<-wday(as.Date(as.character(data$Date),"%m/%d/%Y"),label=TRUE)

 
 #Develop new column for time of inspection (morning or afternoon)

 hour_rf<-hour(as.POSIXct(data$Time,"%H:%M",tz="EST"))
 daytime_rf1<-function(x) {ifelse(x>12,"PM","AM")}
 AM_PM_rf<-sapply(hour_rf,daytime_rf1)

 
 #Add new columns to dataset

 data$Year<-year_rf
 data$Month<-month_rf
 data$Dayofwk<-dayofwk_rf
 data$AM_PM<-AM_PM_rf
 
 
 #cleaning data 
 nrow(data[which(data$NewCategory=="empanadas"), ])
 data <- data[!(data$NewCategory=="dominican"), ]
 data <- data[!(data$NewCategory=="falafel"), ]
 data <- data[!(data$NewCategory=="olittler_establishment"), ]
 data <- data[!(data$NewCategory=="argentine"), ]
 data <- data[!(data$NewCategory=="empanadas"), ]
 data <- data[!(data$NewCategory=="salvadoran"), ]


 nrow(data[is.na(data$CrimeViolent), ])
 nrow(data[is.na(data$is_closed), ])
 data <- data[!is.na(data$is_closed), ]
 data <- data[!is.na(data$Longitude), ]
 data <- data[!(data$Date=="3/16/2014"), ]
 data <- data[!(data$Date=="8/1/2014"), ]
 data <- data[!(data$Date=="6/28/2013"), ]

 #remove NA
data<-na.omit(data)
data<-subset(data, Inspection_Type_Code!='HACCP')
```

```{r}
 #linear model
lim=lm(Critical.Violations~  NewCategory+ review_count+ rating+ price+ Inspection_Type_Code+ Non.Critical.Violations+ Critical.Violations.Corrected.On.Site+ Critical.Violations.To.Be.Resolved+ Non.Critical.Violations.Corrected.On.Site+ Non.Critical.Violations.To.Be.Resolved+ CrimeProperty+ House + income ,data=data)
summary(lim)
#backward
backward = step(lim, direction="backward")
summary(backward)
```

```{r}

#partitioning the datasets
row = nrow(data)
trainindex <- sample(row, 13100, replace=FALSE)
training <- data[trainindex, ]
validation <- data[-trainindex, ]

#training the data (final- reduced)
model = lm(Critical.Violations~ (Month+ Dayofwk+ Zipcode+ is_closed+ Latitude+ Longitude+ review_count+ price+ Inspection_Type_Code+ CrimeProperty+ House)^2 ,data=training)

#predicting
prediction<-predict(model, validation, se.fit=T)  

#lines plot
plot(validation$Critical.Violations, type="n", ylab="Actual vs Predicted", main="Actual Vs Predicted ")
lines(lowess(validation$Critical.Violations, f=.0000001), col=3, lwd=2)
#plot(prediction$fit, type="n", ylab="Predicted", main="Predicted values")
lines(lowess(prediction$fit, f=.0000001), col=2, lwd=2)
#legend
legend(5000,19,c("Predicted","Actual"),lty=c(1,1),col=c("red","green"), lwd=3) 

##plots
plot(validation$Critical.Violations, prediction$fit, xlab="Actual", ylab="Predicted")
plot(prediction$fit)
plot(model$residuals, model$fitted.values, xlab="Residuals", ylab="Fitted Values", main="Residuals vs Fitted" )
```

```{r}
#Cross validating
#validation <- validation[,-41]
validation = cbind(validation, prediction$fit)
colnames(validation)[42] <- "predicted"

```

```{r}
#Rmse
sqrt( mean( (validation$Critical.Violations - validation$predicted)^2, na.rm = TRUE) )


#confusion matrix -Rachel
crit_v <-function(x) {
  if (x>2) as.numeric(1) else as.numeric(0)
}
pred_rf1 <- prediction$fit
actual_rf1 <- validation$Critical.Violations

pred_m_rf1 <- as.matrix(lapply(prediction$fit,crit_v))
actual_m_rf1 <- as.matrix(lapply(actual_rf1,crit_v))
false_neg <- function(x,y) {
  if(x == 1 & y == 0) 1 else 0
}
false_pos <- function(x,y) {
  if(x == 0 & y == 1) 1 else 0
}
true_neg <- function(x,y) {
  if(x == 0 & y == 0) 1 else 0
}
true_pos <- function(x,y) {
  if(x == 1 & y == 1) 1 else 0
}
true_pos_rf1<-rep(NA,length(pred_rf1))
true_neg_rf1<-rep(NA,length(pred_rf1))
false_pos_rf1<-rep(NA,length(pred_rf1))
false_neg_rf1<-rep(NA,length(pred_rf1))
for(i in 1:length(pred_rf1)) {
  true_pos_rf1[i] <- true_pos(actual_m_rf1[i],pred_m_rf1[i])
  true_neg_rf1[i] <- true_neg(actual_m_rf1[i],pred_m_rf1[i])
  false_pos_rf1[i] <- false_pos(actual_m_rf1[i],pred_m_rf1[i])
  false_neg_rf1[i] <- false_neg(actual_m_rf1[i],pred_m_rf1[i])
}


#Develop confusion matrix values

con_rf1 <- c(true_pos_rf1, false_pos_rf1, false_neg_rf1, true_neg_rf1)
con_rf1 <- matrix(con_rf1,nrow = length(pred_rf1),ncol=4)

TP_rf1<-sum(con_rf1[,1])
FP_rf1<-sum(con_rf1[,2])
FN_rf1<-sum(con_rf1[,3])
TN_rf1<-sum(con_rf1[,4])
Con_title_rf1<-c("TP","FP","FN","TN")
Confusion_rf1<-c(TP_rf1,FP_rf1,FN_rf1,TN_rf1)
(Confusion_rf1<-matrix(Confusion_rf1,nrow=2,ncol=2))

#Calculate Accuracy

(Accuracy_rf1<-(TP_rf1+TN_rf1)/nrow(validation))
```

############################################################################################################################
############################################################################################################################
##################################################------  MODELING SECTION    ------########################################
############################################################################################################################
############################################################################################################################
###############################################-------GENERAL LINEAR  MODELNG--------#######################################
############################################################################################################################

```{r}
colnames(data_glm)[colnames(data_glm)=="Rate_crimes_pt1_violent_2016"] <- "CrimeViolent"
colnames(data_glm)[colnames(data_glm)=="Rate_crimes_pt1_property_2016"] <- "CrimeProperty"
colnames(data_glm)[colnames(data_glm)=="House_Median_price_per_sqft_2016"] <- "House"
colnames(data_glm)[colnames(data_glm)=="income_Sum"] <- "Income"
 #remove NA
data_glm<-na.omit(data_glm)
data_glm<-subset(data_glm, Inspection_Type_Code!='HACCP')

data_merged <-data_glm

mode(data_merged)
data_merged$Latitude  <-as.numeric(as.list(data_merged$Latitude))
data_merged$Longitude<-as.numeric(as.list(data_merged$Longitude))
data_merged$review_count<-as.numeric(as.list(data_merged$review_count))
data_merged$rating <-as.numeric(as.list(data_merged$rating))
data_merged$price <-as.numeric(as.list(data_merged$price))
data_merged$CrimeViolent<-as.numeric(as.list(data_merged$CrimeViolent))
data_merged$CrimeProperty<-as.numeric(as.list(data_merged$CrimeProperty))
data_merged$House <-as.numeric(as.list(data_merged$House))
data_merged$Income <-as.numeric(as.list(data_merged$Income))
```
Normalize Numeric Data
```{r}
data_merged1 <- data_merged %>% mutate_each_(funs(scale(.) %>% as.vector), vars=c("review_count","rating","CrimeViolent","CrimeProperty","House","Income"))
```

For Principal Component
```{r}
data_merged2<- matrix(data = NA, nrow = dim(data_merged)[1], ncol = dim(data_merged)[2])
```

```{r}
colnames(data_merged2) <- c("X1","Zipcode","Permit.ID","yelp_id","Name","Address","Latitude","Longitude","State","NewCategory","is_closed","review_count","rating","price","Inspection.ID","Date","Time","Inspection_Type_ID","Inspection_Type_Code","Critical.Violations","Non.Critical.Violations","Critical.Violations.Corrected.On.Site","Critical.Violations.To.Be.Resolved","Non.Critical.Violations.Corrected.On.Site","Non.Critical.Violations.To.Be.Resolved","Income1","Income2","Income3","Income4","Income5","Income6","CrimeViolent","CrimeProperty","House","Income" )
rownames(data_merged2) <- rownames(data_merged2, do.NULL = F, prefix = "row_")
```

Transform data from list to numeric form and put in the matrix data_merged1
```{r}
for (i in 1:dim(data_merged)[2]) {
    data_merged2[,i] <- c(as.numeric(data_merged[[i]]))
}
mode(data_merged2)
```


Take out dependent/predictor variables
```{r}
data_merged3 <-subset(data_merged2, select = -c(Critical.Violations,Non.Critical.Violations,Critical.Violations.Corrected.On.Site,Critical.Violations.To.Be.Resolved,Non.Critical.Violations.Corrected.On.Site,Non.Critical.Violations.To.Be.Resolved,yelp_id,Name,Address,State, NewCategory,Inspection_Type_Code,Income1,Income2,Income3,Income4,Income5,Income6,Date, X1, is_closed, Time ))

data_merged4<- subset(data_merged, select = c(review_count,rating,price,CrimeViolent,CrimeProperty,House,Income))
```


Normalize data for Principal Component Analysis
```{r}
prin_comp <-prcomp(na.omit(data_merged3), scale=TRUE)
names(prin_comp)
prin_comp1 <- prcomp(na.omit(data_merged4), scale=TRUE)
```
Mean of Variables
```{r}
prin_comp$center
prin_comp1$center

```
Standard Deviation of Variables
```{r}
prin_comp$scale
prin_comp1$scale
```
Rotation Measure
```{r}
prin_comp$rotation
prin_comp$rotation[1:5,1:4]
prin_comp1$rotation
prin_comp1$rotation[1:5,1:4]
```
Plot resultant principal components
```{r}
biplot(prin_comp,scale=0)
biplot(prin_comp1,scale=0)
```
Proportion of variance explained
```{r}
std_dev <- prin_comp$sdev
pr_var <- std_dev^2
pr_var [1:10]
```
Proportion of Variance Explained
```{r}
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]
```
Scree Plot:  A scree plot is used to access components or factors which explains the most of variability in the data. It represents values in descending order. 
```{r}
plot(prop_varex, xlab = "Principal Component",
             ylab = "Proportion of Variance Explained",
             type = "b", main  = "Scree Plot")

```
Cumulative Scree Plot: Shows that ten components results in a variance close to 99%. We should use ten variables in our model. 
```{r}
plot(cumsum(prop_varex), xlab = "Principal Component",
              ylab = "Cumulative Proportion of Variance Explained",
              type = "b", main = "Cumulative Scree Plot")
```

For all models
```{r}
require(lubridate)
data_merged1$month = month(as.Date(data_merged$Date))
data_merged1$day = day(as.Date(data_merged$Date))
```


```{r}
data_glm = data_merged1

```

create variables
```{r}
data_glm$Critical.Violations.binomial<-with(data_merged1,Critical.Violations>=2)
data_glm$price = factor(data_merged1$price, levels= c("1","2","3","4"), labels=c("low","medium","high","expensive"))
data_glm$Inspection_Type_ID = factor(data_merged1$Inspection_Type_ID, levels=c("1","2","4","5","6","7"))
levels(data_glm$Inspection_Type_ID) = c("Routine", "follow-up inspection","complaint","Pre-operational","License","Other")
```

```{r}
CriticalViolations1<-glm(Critical.Violations.binomial ~ 1+(Inspection_Type_ID+Income+price+rating+House+CrimeProperty+review_count)^2, data  = data_glm, family = binomial)

```

```{r}
summary(CriticalViolations1)
```
```{r}
(library(boot))
```

```{r}
cv_CriticalViolations1 <-cv.glm(data_glm, CriticalViolations1, K=10)
summary(cv_CriticalViolations1)
```

```{r}
pvals = coef(summary(CriticalViolations1))[,4]
sum(pvals<0.1)
summ = summary(CriticalViolations1)
str(summ, max = 1)
summ = summary(CriticalViolations1)
pvals = summ$coefficients[,4]
mnames = names(pvals[pvals<0.0001])
mnames
```
```{r}
CriticalViolations1$aic

```

```{r}
require(coefplot)

```

```{r}
coefplot(reduced)
```

```{r}
data_glm1 = data_glm
data_glm1$Expensive=0
data_glm1$Expensive[data_glm$price=="expensive"]=1
data_glm1$Medium=0
data_glm1$Medium[data_glm$price=="medium"]=1
data_glm1$High=0
data_glm1$High[data_glm$price=="high"]=1
data_glm1$Other=0
data_glm1$Other[data_glm$Inspection_Type_ID=="Other"]=1
data_glm1$License=0
data_glm1$License[data_glm$Inspection_Type_ID=="License"]=1
data_glm1$Preoperational=0
data_glm1$Preoperational[data_glm$Inspection_Type_ID=="Pre-operational"]=1
data_glm1$Complaint=0
data_glm1$Complaint[data_glm$Inspection_Type_ID=="Complaint"]=1
data_glm1$Followup=0
data_glm1$Followup[data_glm$Inspection_Type_ID=="follow-up inspection"]=1
 
```

```{r}
reduced = glm(Critical.Violations.binomial~1+
                Medium+
                High+
                Expensive+
                Other+
                License+
                Preoperational+
                Complaint+
                Followup+
                Income+
                House+
                review_count+
CrimeProperty+Followup:Income+Followup:Medium+Other:Medium+Other:High+Other:rating+Followup:CrimeProperty+Followup:review_count+Income:Expensive+Income:CrimeProperty+Expensive:rating+High:review_count+rating:review_count+License:Income+Preoperational:Medium+Complaint:High+Complaint:Expensive+License:House+Other:CrimeProperty+Other:review_count+Income:House+Income:review_count+Medium:review_count+Expensive:review_count+House:CrimeProperty,family=binomial,data=data_glm1)
summary(reduced)
```


```{r}
backwards = step(CriticalViolations1,trace=0)
```

```{r}
summary(backwards)
pvals1 = coef(summary(backwards))[,4]
sum(pvals<0.1)
summ1 = summary(backwards)
str(summ1, max = 1)
summ1 = summary(backwards)
pvals1 = summ1$coefficients[,4]
mnames1 = names(pvals[pvals<0.0001])
mnames1
```

```{r}
backwards$aic
reduced$aic
CriticalViolations1$aic
```


```{r}
cv_res1=cv.glm(data_glm,CriticalViolations1,K=10)
cv_res1$delta[1]
cv_res2=cv.glm(data_glm1,reduced,K=10)
cv_res2$delta[1]
cv_res3=cv.glm(data_glm,backwards,K=10)
cv_res3$delta[1]
```
Backwards and CriticalViolations1(full model) have almost identical AIC values and prediction errors. 
```{r}
bestmodel= CriticalViolations1
newmodel = coefplot:::buildModelCI(bestmodel, outerCI=2, innerCI=1, predictors = c("Inspection_Type_IDfollow-up inspection", "Inspection_Type_IDLicense","Inspection_Type_IDLicense", "Inspection_Type_IDOther","Income", "pricemedium", "pricehigh"))

coefplot(newmodel)
```

In sample Probability estimates
```{r}
pred <- predict(bestmodel, data_glm, type="response")
Violation <- data_glm$Critical.Violations.binomial
data.frame(Violation,pred)[1:20,]
```
Plot residuals
```{r}
plot(bestmodel,which=1)
plot(predict(bestmodel),residuals(bestmodel))
abline(h=0,lty=2,col="grey")
```


```{r}

rule <- 1/4 # move this around to see how these change

sum( (pred>rule)[Violation==0] )/sum(pred>rule) ## false positive rate
sum( (pred<rule)[Violation==1] )/sum(pred<rule) ## false negative rate

sum( (pred>rule)[Violation==1] )/sum(Violation==1) ## sensitivity
sum( (pred<rule)[Violation==0] )/sum(Violation==0) ## specificity

```

```{r}
# another way to calculate specificity and sensitivity
mean( (pred>1/4)[Violation==1] )# sensitivity
mean( (pred<1/4)[Violation==0] )# specificity
```

```{r}
roc <- function(p,y, ...){
  y <- factor(y)
  n <- length(p)
  p <- as.vector(p)
  Q <- p > matrix(rep(seq(0,1,length=100),n),ncol=100,byrow=TRUE)
  specificity <- colMeans(!Q[y==levels(y)[1],])
  sensitivity <- colMeans(Q[y==levels(y)[2],])
  plot(1-specificity, sensitivity, type="l", ...)
  abline(a=0,b=1,lty=2,col=8)
}

roc(p=pred, y=Violation, bty="n")
points(x= 1-mean((pred<.25)[Violation==0]), 
       y=mean((pred>.25)[Violation==1]), 
       cex=1.5, pch=20, col='red') 
## a standard `max prob' (p=.5) rule
points(x= 1-mean((pred<.5)[Violation==0]), 
       y=mean((pred>.5)[Violation==1]), 
       cex=1.5, pch=20, col='blue') 
legend("topleft",fill=c("red","blue"),
       legend=c("p=1/4","p=1/2"),bty="n",title="cutoff")
```
```{r, message=FALSE, warning=FALSE}
install.packages(SDMTools)
library(SDMTools)
```

```{r}
xx = rep("negative", 21921)
xx[pred>0.5]="positive"
table(xx,Violation)

Accuracy = accuracy(Violation, pred, threshold = .5)
Accuracy
```




############################################################################################################################
############################################################################################################################
##################################################------  MODELING SECTION    ------########################################
############################################################################################################################
############################################################################################################################
###################################################-------GRADIENT BOOSTING--------#########################################
############################################################################################################################


```{r, message=FALSE, warning=FALSE}
#load packages
library (DiagrammeR)
library(ggplot2)
library(MatrixModels)
library(readr)
library(lattice)
library(stringr)
library(caret)
library(car)
library(Matrix)
library(DiagrammeR)
# install.packages("drat", repos="https://cran.rstudio.com")
# drat:::addRepo("dmlc")
# install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")

require(xgboost)
```

```{r}

##Load data
data<-data_gb


#Subset the data

data <- data[, !(colnames(data) %in% c("State", "Permit.ID", "yelp_id", "Name", "Address", "Inspection.ID", "ID", "Date", "Time", "Latitude", "Longitude",'Non.Critical.Violations','Critical.Violations.Corrected.On.Site','Critical.Violations.To.Be.Resolved',"Inspection_Type_ID","income_Sum","i","Non.Critical.Violations.Corrected.On.Site","Non.Critical.Violations.To.Be.Resolved"))]

data$review_count<-as.factor(data$review_count)
data$rating<-as.factor(data$rating)
data$price<-as.factor(data$price)
```

```{r, message=TRUE, warning=TRUE}

#Create dummy variables

# one-hot-encoding categorical features
ohe_feats = c('NewCategory', 'Inspection_Type_Code','is_closed','review_count','rating','price','Critical.Violations','X.1.under..25.000'
              ,'X.25.000.under..50.000','X.50.000.under..75.000','X.75.000.under..100.000','X.100.000.under..200.000','X.200.000.or.more','Rate_crimes_pt1_violent_2016'
              ,'Rate_crimes_pt1_property_2016','House_Median_price_per_sqft_2016','Zipcode')
dummies <- dummyVars(~ NewCategory+ Inspection_Type_Code+is_closed++review_count+rating+price+Critical.Violations+X.1.under..25.000+
                       +X.25.000.under..50.000+X.50.000.under..75.000+X.75.000.under..100.000+X.100.000.under..200.000+X.200.000.or.more+Rate_crimes_pt1_violent_2016+Rate_crimes_pt1_property_2016+House_Median_price_per_sqft_2016+Zipcode, data = data)
df_all_ohe <- as.data.frame(predict(dummies, newdata = data))
df_all_combined <- cbind(data[,-c(which(colnames(df_all_ohe) %in% ohe_feats))],df_all_ohe)
```

```{r}
df_all_combined$Critical.Violations[df_all_combined$Critical.Violations == 0] <- 0
df_all_combined$Critical.Violations[df_all_combined$Critical.Violations > 0] <- 1

#Create train and test
## 60% of the sample size
# smp_size <- floor(0.60 * nrow(df_all_combined))
# train_ind <- sample(seq_len(nrow(df_all_combined)), size = smp_size)
# 
# df_train <- df_all_combined[train_ind, ]
# df_test <- df_all_combined[-train_ind, ]
# df_all = rbind(df_train,df_test)
# 
# 
# 
# 
# 


require(caTools)
set.seed(25675) 
# sample = sample.split(df_all_combined$Critical.Violations, SplitRatio = .60)
# df_train = subset(df_all_combined, sample == TRUE)
# df_test  = subset(df_all_combined, sample == TRUE)
# 


rownames(df_all_combined)<-1:nrow(df_all_combined)
rows <- sample(x=1:nrow(df_all_combined),size=0.6*nrow(df_all_combined))
df_train <- df_all_combined[rows,]
df_test <- df_all_combined[! rownames(df_all_combined) %in% rows,]
```



```{r}
#Remove predicting variable
# df_removed <- subset( df_test, select = -Critical.Violations )
# 
# X = df_removed
# y2 <- (df_train$Critical.Violations)  ## Set Label for the models
# X_test = df_all_combined[df_all_combined$Critical.Violations %in% df_test$Critical.Violations,]
# 
# length(X_test)
# length(y)

##Tune


xgb <- xgboost(data = data.matrix(df_train), 
               label = y2, 
               eta = 0.1,
               max_depth = 15, 
               nround=15, 
               subsample = 0.5,
               colsample_bytree = 0.5,
               seed = 1,
               objective = "binary:logistic",
               nthread = 3
)
```

 
```{r}


# predict values in test set
y_pred <- predict (xgb, data.matrix(df_test)) #[,-1])

err <- as.numeric(sum(as.integer(y_pred > 0.5) != y))/length(y)     ##"test-error= 1.00637196336121" - Prediction accuracy 92.30%
print(paste("test-error=", err)) 

# Nth.delete<-function(dataframe, n)dataframe[-(seq(n,to=nrow(dataframe),by=n)),]
# y_pred<-as.data.frame(y_pred)
# 
# y_pred_new<-Nth.delete(y_pred, 2)
# 
# within(dat,{ 
#              C1_A =ifelse(C1=='A',1,0)
#              C1_B =ifelse(C1=='B',1,0)})

#Get Imporant Predictor Features out:
# Get the feature real names
names <- dimnames(data.matrix(X[,-1]))[[2]]

# Compute feature importance matrix
# importance_matrix <- xgb.importance(names, model = xgb)
# xgb.plot.importance(importance_matrix = importance_matrix, top_n = 10)

# # Nice tree graph
# xgb.plot.tree(feature_names = names, model = xgb, n_first_tree = 2)

# 
# ##Test the results
# test_InspectionType <- chisq.test(df_train$review_count, df_train$Critical.Violations)
# ##We test for: Inspection_Type_Code, review_count, NewCategory, X.1.under..25.000, rating, price,Rate_crimes_pt1_violent_2016
# print(test_InspectionType)
```

```{r}
###ROC Curve
## plot the ROC curve for classification of y with p
roc <- function(p,y, ...){
  y <- factor(y)
  n <- length(p)
  p <- as.vector(p)
  Q <- p > matrix(rep(seq(0,1,length=100),n),ncol=100,byrow=TRUE)
  specificity <- colMeans(!Q[y==levels(y)[1],])
  sensitivity <- colMeans(Q[y==levels(y)[2],])
  plot(1-specificity, sensitivity, type="l", ...)
  abline(a=0,b=1,lty=2,col=8)
}
```


```{r}

y1 <- (df_test$Critical.Violations) 
 
roc(p=y_pred, y=y1, bty="n")
## our 1/5 rule cutoff
points(x= 1-mean((y_pred<.2)[y1==0]), 
       y=mean((y_pred>.2)[y1==1]), 
       cex=1.5, pch=20, col='red')
## a standard `max prob' (p=.5) rule
points(x= 1-mean((y_pred<.5)[y1==0]), 
       y=mean((y_pred>.5)[y1==1]), 
       cex=1.5, pch=20, col='blue') 
legend("topleft",fill=c("red","blue"),
       legend=c("p=1/5","p=1/2"),bty="n",title="cutoff")

# write.csv(y1, file = "y1.csv")
# write.csv(y_pred_new, file = "y_pred_new.csv")
```



```{r}

gbmFit1 <- train(as.factor(outcome1) ~ ., data = trainData[,-26], method = "gbm", trControl = fitControl,verbose = FALSE)
gbm_dev <- predict(gbmFit1, trainData,type= "prob")[,2] 
gbm_ITV1 <- predict(gbmFit1, trainData_ens1,type= "prob")[,2] 
gbm_ITV2 <- predict(gbmFit1, testData_ens1,type= "prob")[,2]



auc(trainData$Disbursed,gbm_dev)
auc(trainData_ens1$Disbursed,gbm_ITV1)
auc(testData_ens1$Disbursed,gbm_ITV2)


```
############################################################################################################################
############################################################################################################################
##################################################------  MODELING SECTION    ------########################################
############################################################################################################################
############################################################################################################################
#####################################################-------RANDOM FOREST--------###########################################
############################################################################################################################




```{r}
#install.packages("plyr")
#install.packages("dplyr")
#install.packages("lubridate")
#install.packages("rpart")
#install.packages("randomForest")
library(randomForest)
library(rpart)
library(lubridate)
library(dplyr)
library(plyr)
```

################################################################
Load Dataset and Develop DC Restaurant Maps

#Check working directory
```{r}
getwd()
```

#Read in DC Restaurant dataset
```{r}
data_rf <-read.csv("data_merged_R.csv")
```

#Create map of Washington, DC
```{r}
# install.packages("ggplot2")
# install.packages("ggmap")
library(ggplot2)
library(ggmap)
dc_map <- get_map(location = "Washington, DC", maptype = "roadmap", zoom = 13)
```

# Create poit maps of inspections
```{r}
ggmap(dc_map, extent = "panel") + geom_point(aes(x = Longitude, y = Latitude), colour = "red", alpha = 0.3, size = 1, data =data_rf)
```

#Create heat map for inspections with 2 or more critical violations, filter for critical vioaltions
```{r}
dc_crit_viol<-data_rf[order(data_rf$Critical.Violations),]
dc_crit_viol<-subset(dc_crit_viol,dc_crit_viol[,20] > 2)
```

```{r}
ggmap(dc_map,extent="panel") + geom_density2d(data=dc_crit_viol, aes(x=Longitude,y=Latitude))+ 
  stat_density2d(data = dc_crit_viol, aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") + scale_alpha(range = c(0, 0.3), guide = FALSE)
```

################################################################
Data cleanup/prep

#Develop function to fix establishment categories
```{r}
fix_newcat <- function (x) {
  if(x=="vegetarian") "Vegan/vegeterian" else
    if (x=="Sea Food") "seafood" else
      if(x=="olittler_establishment") "Other_establishment" else
        if(x=="restaurants") "Other_establishment" else 
          if(x=="Indian/Packistani") "Indian/Pakistani" else
            if(x=="Delivery") "Other_establishment" else
              if(x=="falafel") "European" else
    if(x=="juicebars") "Other_establishment" else
      if(x=="Kosher") "Other_establishment" else
        if(x=="Russian") "Other_establishment" else
          if(x=="empanadas") "Other_establishment" else
            if(x=="Peruvian") "Latin" else
              if(x=="Argentine") "Latin" else
                if(x=="Dominican") "caribbean" else
                  if(x=="malaysian") "Asian" else
                    if(x=="Portuguese") "European" else
                      if (x=="Islander") "caribbean" else as.character(x)
}

```

#Add new column for fixed establishment category
```{r}
data_rf$RestCategory<-sapply(data_rf$NewCategory,fix_newcat)
```

#Develop new columns for year, month, and day of the week
```{r}
year_rf<-year(as.Date(as.character(data_rf$Date),"%m/%d/%Y"))
month_rf<-month(as.Date(as.character(data_rf$Date),"%m/%d/%Y"),label=TRUE)
dayofwk_rf<-wday(as.Date(as.character(data_rf$Date),"%m/%d/%Y"),label=TRUE)
```

#Develop new column for time of inspection (morning or afternoon)
```{r}
hour_rf<-hour(as.POSIXct(data_rf$Time,"%H:%M",tz="EST"))
daytime_rf<-function(x) {ifelse(x>12,"PM","AM")}
AM_PM_rf<-sapply(hour_rf,daytime_rf)
```

#Add new columns to dataset
```{r}
data_rf$Year<-year_rf
data_rf$Month<-month_rf
data_rf$Dayofwk<-dayofwk_rf
data_rf$AM_PM<-AM_PM_rf
```

#Add binary variable for predicted value: >= 2 = 1, and < 0 = 0
## Function to create binary variable
```{r}
crit_v <-function(x) {
  if (x>2) as.numeric(1) else as.numeric(0)
}
```

## Apply function to dataset
```{r}
data_rf$bin <- sapply(data_rf$Critical.Violations,crit_v)
```

#Remove missing values in dataset
```{r}
data_rf<-na.omit(data_rf)
```

#Format numeric variables
```{r}
mode(data_rf)
data_rf$Latitude  <-as.numeric(as.list(data_rf$Latitude))
data_rf$Longitude<-as.numeric(as.list(data_rf$Longitude))
data_rf$review_count<-as.numeric(as.list(data_rf$review_count))
data_rf$rating <-as.numeric(as.list(data_rf$rating))
data_rf$Rate_crimes_pt1_violent_2016<-as.numeric(as.list(data_rf$Rate_crimes_pt1_violent_2016))
data_rf$Rate_crimes_pt1_property_2016<-as.numeric(as.list(data_rf$Rate_crimes_pt1_property_2016))
data_rf$House_Median_price_per_sqft_2016<-as.numeric(as.list(data_rf$House_Median_price_per_sqft_2016))
data_rf$X.1.under..25.000<-as.numeric(as.list(data_rf$X.1.under..25.000))
data_rf$X.25.000.under..50.000<-as.numeric(as.list(data_rf$X.25.000.under..50.000))
data_rf$X.50.000.under..75.000<-as.numeric(as.list(data_rf$X.50.000.under..75.000))
data_rf$X.75.000.under..100.000<-as.numeric(as.list(data_rf$X.75.000.under..100.000))
data_rf$X.100.000.under..200.000<-as.numeric(as.list(data_rf$X.100.000.under..200.000))
data_rf$X.200.000.or.more<-as.numeric(as.list(data_rf$X.200.000.or.more))
data_rf$income_Sum <-as.numeric(as.list(data_rf$income_Sum))
data_rf$bin<-as.numeric(as.list(data_rf$bin))
```

#Normalize relevant columns data
```{r}
data_rf <- data_rf %>% mutate_each_(funs(scale(.) %>% as.vector), vars=c("Latitude","Longitude","review_count","rating","Rate_crimes_pt1_violent_2016","Rate_crimes_pt1_property_2016","House_Median_price_per_sqft_2016","X.1.under..25.000","X.25.000.under..50.000","X.50.000.under..75.000","X.75.000.under..100.000","X.100.000.under..200.000","X.200.000.or.more","income_Sum"))

```

#Format factor variables (so no characters)
```{r}
data_rf$Zipcode<-as.factor(data_rf$Zipcode)
data_rf$price <-as.factor(data_rf$price)
data_rf$RestCategory<-as.factor(data_rf$RestCategory)
data_rf$Month<-as.factor(data_rf$Month)
data_rf$Dayofwk<-as.factor(data_rf$Dayofwk)
data_rf$AM_PM<-as.factor(data_rf$AM_PM)
data_rf$Inspection_Type_Code<-as.factor(data_rf$Inspection_Type_Code)
```

################################################################
Create training and testing sets for Random Forest Model

#Set seed value #1
```{r}
set.seed(1467)
```

#Separate data into train and test datasets: split 60% and 40%, respectively
```{r}
rownames(data_rf)<-1:nrow(data_rf)
rows <- sample(x=1:nrow(data_rf),size=0.6*nrow(data_rf))

train_rf <- data_rf[rows,]
test_rf <- data_rf[! rownames(data_rf) %in% rows,]
```

################################################################
Build Correlation plot for Training Dataset

```{r}
drf_1<-data.frame(Latitude=train_rf$Latitude,Longitude=train_rf$Longitude,ReviewCount=train_rf$review_count,Rating=train_rf$rating,Income25K=train_rf$X.1.under..25.000,Income50K=train_rf$X.25.000.under..50.000,Income75K=train_rf$X.50.000.under..75.000,Income100K=train_rf$X.75.000.under..100.000,Income200K=train_rf$X.100.000.under..200.000,Income200KPlus=train_rf$X.200.000.or.more,Income=train_rf$income_Sum,PropertyCrime=train_rf$Rate_crimes_pt1_property_2016,ViolentCrime=train_rf$Rate_crimes_pt1_violent_2016,HousePrice=train_rf$House_Median_price_per_sqft_2016)
```

#Create correlation plot
```{r}
Mrf <- cor(drf_1)
```

```{r}
install.packages("corrplot")
library(corrplot)
corrplot(Mrf,method="circle")
```

################################################################
Scatterplots 
 - Investigate relationships for variables that are highly correlated

#Plot Latitude vs. Longitude
```{r}
plot(train_rf$Latitude,train_rf$Longitude)
abline(lm(train_rf$Latitude ~ train_rf$Longitude), col="red")
```
There seems to be a few values that are significantly different from others.  Possible issue for model.

#Plot Income25K vs. Income50K
```{r}
plot(train_rf$X.1.under..25.000,train_rf$X.25.000.under..50.000)
abline(lm(train_rf$X.1.under..25.000 ~ train_rf$X.25.000.under..50.000), col="red")
```
Strong positve correlation.  Consider only including one of these variables.


#Plot Violent Crime Rate vs. Property Crime Rate
```{r}
plot(train_rf$Rate_crimes_pt1_violent_2016,train_rf$Rate_crimes_pt1_property_2016)
abline(lm(train_rf$Rate_crimes_pt1_violent_2016 ~ train_rf$Rate_crimes_pt1_property_2016), col="red")
```

#Plot Income vs. Violent Crime Rate
```{r}
plot(train_rf$income_Sum,train_rf$Rate_crimes_pt1_violent_2016)
abline(lm(train_rf$income_Sum ~ train_rf$Rate_crimes_pt1_violent_2016), col="red")
```

################################################################
Training Dataset Plots/Boxplots for Category Variables

#Plot Number of critical violations as a function of Month
```{r}
boxplot(Critical.Violations~Month,data=train_rf,ylab="Number of Critical Violations",xlab="Month",labels=TRUE,las=2)
```
Range of Second and Third Quantiles seem very similar for all Months except September and October, which are lower.
Medians seem very similar except for April, where it is higher.

#Plot Number of critical violations as a function of Day of the Week
```{r}
boxplot(Critical.Violations~Dayofwk,data=train_rf,ylab="Number of Critical Violations",xlab="Day of the Week",labels=TRUE,las=2)
```
The median seems to be the same for Mon - Sat.  There is little variation between Mon - Fri.  Saturday has more variation than Mon-Fri.  Sunday has a considerably different distribution than the other days.  In order to simplify model, consider changing factor variable from a level for each day to the following levels: "Weekday"", "Saturday", and "Sunday".

#Plot Number of critical violations as a function of Time of Day (morning or afternoon)
```{r}
boxplot(Critical.Violations~AM_PM,data=train_rf,ylab="Number of Critical Violations",xlab="Time of Day",labels=TRUE,las=2)
```
There seems to be variability for morning inspections than for afternoon inspections.

#Plot Number of critical violations as a function of Inspection Type
```{r}
boxplot(Critical.Violations~Inspection_Type_Code,data=train_rf,ylab="Number of Critical Violations",xlab="Inspection Type",lables=TRUE,las=2)
```

#Plot Number of critical violations as a function of Restaurant Categories
```{r}
boxplot(Critical.Violations~RestCategory,data=train_rf,ylab="Number of Critical Violations",xlab="Restaurant Category",labels=TRUE,las=2)
```

#Plot Yelp Rating vs. Number of Critical Violations
```{r}
(m=max((data_rf$rating)-min(data_rf$rating))/5)
```

```{r}
plot((train_rf$rating-min(data_rf$rating))/m,train_rf$Critical.Violations,xlab="Yelp Rating",ylab="Number of Critical Violations")
```

################################################################
Run Boruta Feature Selection

#Install Boruta package
```{r}
#install.packages("Boruta")
library(Boruta)
```

#Set Seed
```{r}
set.seed(1603)
```

#Check importance of variables
```{r}
model_bor1<-Boruta(bin ~ Zipcode + Latitude + Longitude + RestCategory + review_count + rating + price + Inspection_Type_Code + X.1.under..25.000 + X.25.000.under..50.000 + X.50.000.under..75.000 + X.75.000.under..100.000 + X.100.000.under..200.000+ X.200.000.or.more + income_Sum + Rate_crimes_pt1_violent_2016 + Rate_crimes_pt1_property_2016 + House_Median_price_per_sqft_2016 + Month + Dayofwk + AM_PM,data=train_rf,doTrace=2)
print(model_bor1)
```
(Takes a while to run--at least 7 min.)
All 21 variables deemed important.

```{r}
plot(model_bor1,xlab="",xaxt="n")
zbor1<-lapply(1:ncol(model_bor1$ImpHistory),function(i) model_bor1$ImpHistory[is.finite(model_bor1$ImpHistory[,i]),i])
names(zbor1)<-colnames(model_bor1$ImpHistory)
Labels<-sort(sapply(zbor1,median))
axis(side=1,las=2,labels=names(Labels),at = 1:ncol(model_bor1$ImpHistory),ces.axis = 0.7)
```

```{r}
model_bor1_df <- attStats(model_bor1)
class(model_bor1_df)
print(model_bor1_df)
```

```{r}
plotImpHistory(model_bor1,colCode=c("green","yellow","red","blue"),col = NULL,type="l",lty=1,pch=0)
```
################################################################
Construct Random Forest Model #1 (Base Model)

```{r}
#install.packages("lattice")
library(lattice)
library(caret)
library(randomForest)
```


#Set seed value #2 
```{r}
set.seed(1927)
```

#Build Random Forest Model
```{r}
model_rf1 <-randomForest(bin ~ Zipcode + Latitude + Longitude + RestCategory + review_count + rating + price + Inspection_Type_Code + Month + Dayofwk + AM_PM,data=train_rf,replace=TRUE,ntree=100)
```

#Plot importance of variables
```{r}
imp_rf1 <- importance(model_rf1)
vars_rf1 <- dimnames(imp_rf1)[(1)]
imp_rf1 <- data.frame(vars=vars_rf1,imp=as.numeric(imp_rf1[,1]))
imp_rf1 <- imp_rf1[order(imp_rf1$imp,decreasing=TRUE),] 
```

```{r}
par(mfrow=c(1,2))
varImpPlot(model_rf1,main="Variable Importance Plot: Base Model")
```


#Plot Error vs. Tree
```{r}
plot(model_rf1,main="Error vs. No. of trees plot: Base Model")
```

################################################################
Test Random Forest Model #1 (Base Model)

#Run Prediction
```{r}
pred_rf1 <- predict(model_rf1, test_rf)
pred_rf1 <- as.matrix(pred_rf1)
actual_rf <- as.matrix(test_rf$bin)
```

#Plot actual vs. predicted
```{r}
plot(actual_rf,pred_rf1,xlab="Actual",ylab="Predicted",main="Critical Violation Category")
```

## plot the ROC curve for classification of y with p
```{r}
roc <- function(p,y, ...){
  y <- factor(y)
  n <- length(p)
  p <- as.vector(p)
  Q <- p > matrix(rep(seq(0,1,length=100),n),ncol=100,byrow=TRUE)
  specificity <- colMeans(!Q[y==levels(y)[1],])
  sensitivity <- colMeans(Q[y==levels(y)[2],])
  plot(1-specificity, sensitivity, type="l", ...)
  abline(a=0,b=1,lty=2,col=8)
}
```


```{r}
roc(p=pred_rf1, y=actual_rf, bty="n")
## our 1/5 rule cutoff
points(x= 1-mean((pred_rf1<.2)[actual_rf==0]), 
       y=mean((pred_rf1>.2)[actual_rf==1]), 
       cex=1.5, pch=20, col='red')
## a standard `max prob' (p=.5) rule
points(x= 1-mean((pred_rf1<.5)[actual_rf==0]), 
       y=mean((pred_rf1>.5)[actual_rf==1]), 
       cex=1.5, pch=20, col='blue') 
legend("topleft",fill=c("red","blue"),
       legend=c("p=1/5","p=1/2"),bty="n",title="cutoff")

```

#set decision point
```{r}
dp_rf1 <- 0.5
```

# Develop functions to sort results for confusion matrix
```{r}
false_neg1 <- function(x,y) {
  if(x == 1 & y < dp_rf1) 1 else 0
}
false_pos1 <- function(x,y) {
  if(x == 0 & y >= dp_rf1) 1 else 0
}
true_neg1 <- function(x,y) {
  if(x == 0 & y < dp_rf1) 1 else 0
}
true_pos1 <- function(x,y) {
  if(x == 1 & y >= dp_rf1) 1 else 0
}
```

#Sort results for confusion matrix
```{r}
true_pos_rf1<-rep(NA,length(pred_rf1))
true_neg_rf1<-rep(NA,length(pred_rf1))
false_pos_rf1<-rep(NA,length(pred_rf1))
false_neg_rf1<-rep(NA,length(pred_rf1))
for(i in 1:length(pred_rf1)) {
  true_pos_rf1[i] <- true_pos1(actual_rf[i],pred_rf1[i])
  true_neg_rf1[i] <- true_neg1(actual_rf[i],pred_rf1[i])
  false_pos_rf1[i] <- false_pos1(actual_rf[i],pred_rf1[i])
  false_neg_rf1[i] <- false_neg1(actual_rf[i],pred_rf1[i])
}
```

#Develop confuson matrix values
```{r}
con_rf1 <- c(true_pos_rf1, false_pos_rf1, false_neg_rf1, true_neg_rf1)
con_rf1 <- matrix(con_rf1,nrow = length(pred_rf1),ncol=4)
```

```{r}
TP_rf1<-sum(con_rf1[,1])
FP_rf1<-sum(con_rf1[,2])
FN_rf1<-sum(con_rf1[,3])
TN_rf1<-sum(con_rf1[,4])
Con_title_rf1<-c("TP","FP","FN","TN")
Confusion_rf1<-c(TP_rf1,FP_rf1,FN_rf1,TN_rf1)
(Confusion_rf1<-matrix(Confusion_rf1,nrow=2,ncol=2))
```

#Calculate Accuracy
```{r}
(Accuracy_rf1<-(TP_rf1+TN_rf1)/nrow(test_rf))
```

################################################################
Construct Random Forest Model #2 (Reduced Model)
(Drop 10 least important variables: X.1.under..25.000; X.25.000.under..50.000; X.50.000.under..75.000; X.75.000.under..100.000; X.100.000.under..200.000; X.200.000.or.more; income_Sum; Rate_crimes_pt1_violent_2016; Rate_crimes_pt1_property_2016; and House_Median_price_per_sqft_2016.)

#Set seed value #2 
```{r}
set.seed(1927)
```

#Build Random Forest Model
```{r}
model_rf2 <-randomForest(bin ~ Zipcode + Latitude + Longitude + RestCategory + review_count + rating + price + Inspection_Type_Code + Month + Dayofwk + AM_PM,data=train_rf,replace=TRUE,ntree=100)
```

#Plot importance of variables
```{r}
imp_rf2 <- importance(model_rf2)
vars_rf2 <- dimnames(imp_rf2)[(1)]
imp_rf2 <- data.frame(vars=vars_rf2,imp=as.numeric(imp_rf2[,1]))
imp_rf2 <- imp_rf2[order(imp_rf2$imp,decreasing=TRUE),] 
```

```{r}
par(mfrow=c(1,2))
varImpPlot(model_rf2,main="Variable Importance Plot: Reduced Model")
```



#Plot Error vs. Tree
```{r}
plot(model_rf2,main="Error vs. No. of trees plot: Reduced Model")
```

################################################################
Test Random Forest Model #2 (Reduced Model)


#Run Prediction
```{r}
pred_rf2 <- predict(model_rf2, test_rf)
pred_rf2 <- as.matrix(pred_rf2)
actual_rf <- as.matrix(test_rf$bin)
```

#Plot actual vs. predicted
```{r}
plot(actual_rf,pred_rf2,xlab="Actual",ylab="Predicted",main="Critical Violation Category")
```

## plot the ROC curve for classification of y with p
```{r}
roc(p=pred_rf2, y=actual_rf, bty="n")
## our 1/5 rule cutoff
points(x= 1-mean((pred_rf2<.2)[actual_rf==0]), 
       y=mean((pred_rf2>.2)[actual_rf==1]), 
       cex=1.5, pch=20, col='red')
## a standard `max prob' (p=.5) rule
points(x= 1-mean((pred_rf2<.5)[actual_rf==0]), 
       y=mean((pred_rf2>.5)[actual_rf==1]), 
       cex=1.5, pch=20, col='blue') 
legend("topleft",fill=c("red","blue"),
       legend=c("p=1/5","p=1/2"),bty="n",title="cutoff")

```

#set decision point
```{r}
dp_rf2 <- 0.5
```

# Develop functions to sort results for confusion matrix
```{r}
false_neg2 <- function(x,y) {
  if(x == 1 & y < dp_rf2) 1 else 0
}
false_pos2 <- function(x,y) {
  if(x == 0 & y >= dp_rf2) 1 else 0
}
true_neg2 <- function(x,y) {
  if(x == 0 & y < dp_rf2) 1 else 0
}
true_pos2 <- function(x,y) {
  if(x == 1 & y >= dp_rf2) 1 else 0
}
```

#Sort results for confusion matrix
```{r}
true_pos_rf2<-rep(NA,length(pred_rf2))
true_neg_rf2<-rep(NA,length(pred_rf2))
false_pos_rf2<-rep(NA,length(pred_rf2))
false_neg_rf2<-rep(NA,length(pred_rf2))
for(i in 1:length(pred_rf2)) {
  true_pos_rf2[i] <- true_pos2(actual_rf[i],pred_rf2[i])
  true_neg_rf2[i] <- true_neg2(actual_rf[i],pred_rf2[i])
  false_pos_rf2[i] <- false_pos2(actual_rf[i],pred_rf2[i])
  false_neg_rf2[i] <- false_neg2(actual_rf[i],pred_rf2[i])
}
```

#Develop confuson matrix values
```{r}
con_rf2 <- c(true_pos_rf2, false_pos_rf2, false_neg_rf2, true_neg_rf2)
con_rf2 <- matrix(con_rf2,nrow = length(pred_rf2),ncol=4)
```

```{r}
TP_rf2<-sum(con_rf2[,1])
FP_rf2<-sum(con_rf2[,2])
FN_rf2<-sum(con_rf2[,3])
TN_rf2<-sum(con_rf2[,4])
Con_title_rf2<-c("TP","FP","FN","TN")
Confusion_rf2<-c(TP_rf2,FP_rf2,FN_rf2,TN_rf2)
(Confusion_rf2<-matrix(Confusion_rf2,nrow=2,ncol=2))
```

#Calculate Accuracy
```{r}
(Accuracy_rf2<-(TP_rf2+TN_rf2)/nrow(test_rf))
```

################################################################
Compare Random Forest Model 1 and 2 ROCs

```{r}
roc(p=pred_rf1, y=actual_rf, bty="n",col="red")
par(new=TRUE)
roc(p=pred_rf2, y=actual_rf, bty="n",col="blue",axes = FALSE, xlab = "", ylab = "")
legend("topleft",fill=c("red","blue"),
       legend=c("Base","Reduced"),bty="n",title="model")

```

So, the Reduced Model performed better than the Base Model.


################################################################
Construct Random Forest Model #3 (Hypothesis Model)
(Isolate hypothesis variables: Inspection_Type_Code; and rating;)

#Set seed value #2 
```{r}
set.seed(1927)
```

#Build Random Forest Model
```{r}
model_rf3 <-randomForest(bin ~ rating + Inspection_Type_Code, data=train_rf,replace=TRUE,ntree=100)
```

#Plot importance of variables
```{r}
imp_rf3 <- importance(model_rf3)
vars_rf3 <- dimnames(imp_rf3)[(1)]
imp_rf3 <- data.frame(vars=vars_rf3,imp=as.numeric(imp_rf3[,1]))
imp_rf3 <- imp_rf3[order(imp_rf3$imp,decreasing=TRUE),] 
```

```{r}
par(mfrow=c(1,2))
varImpPlot(model_rf3,main="Variable Importance Plot: Hypothesis Model")
```

#Plot Error vs. Tree
```{r}
plot(model_rf3,main="Error vs. No. of trees plot: Hypothesis Model")
```

################################################################
Test Random Forest Model #3 (Hypothesis Model)


#Run Prediction
```{r}
pred_rf3 <- predict(model_rf3, test_rf)
pred_rf3 <- as.matrix(pred_rf3)
actual_rf <- as.matrix(test_rf$bin)
```

#Plot actual vs. predicted
```{r}
plot(actual_rf,pred_rf3,xlab="Actual",ylab="Predicted",main="Critical Violation Category")
```

## plot the ROC curve for classification of y with p
```{r}
roc(p=pred_rf3, y=actual_rf, bty="n")
## our 1/5 rule cutoff
points(x= 1-mean((pred_rf3<.2)[actual_rf==0]), 
       y=mean((pred_rf3>.2)[actual_rf==1]), 
       cex=1.5, pch=20, col='red')
## a standard `max prob' (p=.5) rule
points(x= 1-mean((pred_rf3<.5)[actual_rf==0]), 
       y=mean((pred_rf3>.5)[actual_rf==1]), 
       cex=1.5, pch=20, col='blue') 
legend("topleft",fill=c("red","blue"),
       legend=c("p=1/5","p=1/2"),bty="n",title="cutoff")

```

#set decision point
```{r}
dp_rf3 <- 0.5
```

# Develop functions to sort results for confusion matrix
```{r}
false_neg_rf <- function(x,y,z) {
  if(x == 1 & y < z) 1 else 0
}
false_pos_rf <- function(x,y,z) {
  if(x == 0 & y >= z) 1 else 0
}
true_neg_rf <- function(x,y,z) {
  if(x == 0 & y < z) 1 else 0
}
true_pos_rf <- function(x,y,z) {
  if(x == 1 & y >= z) 1 else 0
}
```

#Sort results for confusion matrix
```{r}
true_pos_rf3<-rep(NA,length(pred_rf3))
true_neg_rf3<-rep(NA,length(pred_rf3))
false_pos_rf3<-rep(NA,length(pred_rf3))
false_neg_rf3<-rep(NA,length(pred_rf3))
for(i in 1:length(pred_rf3)) {
  true_pos_rf3[i] <- true_pos_rf(actual_rf[i],pred_rf3[i],---
title: "DCR_Random_Forest_RAS"
output: html_document
---

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document.

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

################################################################
Install Packages

#install package for plyr
```{r}
#install.packages("plyr")
library(plyr)
```

#install package for dplyr
```{r}
#install.packages("dplyr")
library(dplyr)
```

#install package for lubridate
```{r}
#install.packages("lubridate")
library(lubridate)
```

#knit 
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#Install package for trees
```{r}
#install.packages("rpart")
library(rpart)
```

#Install package for Random Forest
```{r}
#install.packages("randomForest")
library(randomForest)
```


################################################################
Load Dataset and Develop DC Restaurant Maps

#Check working directory
```{r}
getwd()
```

#Read in DC Restaurant dataset
```{r}
data_rf <-read.csv("data/data_merged_R.csv")
```

#Create map of Washington, DC
```{r}
# install.packages("ggplot2")
# install.packages("ggmap")
library(ggplot2)
library(ggmap)
dc_map <- get_map(location = "Washington, DC", maptype = "roadmap", zoom = 13)
```

# Create poit maps of inspections
```{r}
ggmap(dc_map, extent = "panel") + geom_point(aes(x = Longitude, y = Latitude), colour = "red", alpha = 0.3, size = 1, data =data_rf)
```

#Create heat map for inspections with 2 or more critical violations, filter for critical vioaltions
```{r}
dc_crit_viol<-data_rf[order(data_rf$Critical.Violations),]
dc_crit_viol<-subset(dc_crit_viol,dc_crit_viol[,20] > 2)
```

```{r}
ggmap(dc_map,extent="panel") + geom_density2d(data=dc_crit_viol, aes(x=Longitude,y=Latitude))+ 
  stat_density2d(data = dc_crit_viol, aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") + scale_alpha(range = c(0, 0.3), guide = FALSE)
```

################################################################
Data cleanup/prep

#Develop function to fix establishment categories
```{r}
fix_newcat <- function (x) {
  if(x=="vegetarian") "Vegan/vegeterian" else
    if (x=="Sea Food") "seafood" else
      if(x=="olittler_establishment") "Other_establishment" else
        if(x=="restaurants") "Other_establishment" else 
          if(x=="Indian/Packistani") "Indian/Pakistani" else
            if(x=="Delivery") "Other_establishment" else
              if(x=="falafel") "European" else
    if(x=="juicebars") "Other_establishment" else
      if(x=="Kosher") "Other_establishment" else
        if(x=="Russian") "Other_establishment" else
          if(x=="empanadas") "Other_establishment" else
            if(x=="Peruvian") "Latin" else
              if(x=="Argentine") "Latin" else
                if(x=="Dominican") "caribbean" else
                  if(x=="malaysian") "Asian" else
                    if(x=="Portuguese") "European" else
                      if (x=="Islander") "caribbean" else as.character(x)
}

```

#Add new column for fixed establishment category
```{r}
data_rf$RestCategory<-sapply(data_rf$NewCategory,fix_newcat)
```

#Develop new columns for year, month, and day of the week
```{r}
year_rf<-year(as.Date(as.character(data_rf$Date),"%m/%d/%Y"))
month_rf<-month(as.Date(as.character(data_rf$Date),"%m/%d/%Y"),label=TRUE)
dayofwk_rf<-wday(as.Date(as.character(data_rf$Date),"%m/%d/%Y"),label=TRUE)
```

#Develop new column for time of inspection (morning or afternoon)
```{r}
hour_rf<-hour(as.POSIXct(data_rf$Time,"%H:%M",tz="EST"))
daytime_rf<-function(x) {ifelse(x>12,"PM","AM")}
AM_PM_rf<-sapply(hour_rf,daytime_rf)
```

#Add new columns to dataset
```{r}
data_rf$Year<-year_rf
data_rf$Month<-month_rf
data_rf$Dayofwk<-dayofwk_rf
data_rf$AM_PM<-AM_PM_rf
```

#Add binary variable for predicted value: >= 2 = 1, and < 0 = 0
## Function to create binary variable
```{r}
crit_v <-function(x) {
  if (x>2) as.numeric(1) else as.numeric(0)
}
```

## Apply function to dataset
```{r}
data_rf$bin <- sapply(data_rf$Critical.Violations,crit_v)
```

#Remove missing values in dataset
```{r}
data_rf<-na.omit(data_rf)
```

#Format numeric variables
```{r}
mode(data_rf)
data_rf$Latitude  <-as.numeric(as.list(data_rf$Latitude))
data_rf$Longitude<-as.numeric(as.list(data_rf$Longitude))
data_rf$review_count<-as.numeric(as.list(data_rf$review_count))
data_rf$rating <-as.numeric(as.list(data_rf$rating))
data_rf$Rate_crimes_pt1_violent_2016<-as.numeric(as.list(data_rf$Rate_crimes_pt1_violent_2016))
data_rf$Rate_crimes_pt1_property_2016<-as.numeric(as.list(data_rf$Rate_crimes_pt1_property_2016))
data_rf$House_Median_price_per_sqft_2016<-as.numeric(as.list(data_rf$House_Median_price_per_sqft_2016))
data_rf$X.1.under..25.000<-as.numeric(as.list(data_rf$X.1.under..25.000))
data_rf$X.25.000.under..50.000<-as.numeric(as.list(data_rf$X.25.000.under..50.000))
data_rf$X.50.000.under..75.000<-as.numeric(as.list(data_rf$X.50.000.under..75.000))
data_rf$X.75.000.under..100.000<-as.numeric(as.list(data_rf$X.75.000.under..100.000))
data_rf$X.100.000.under..200.000<-as.numeric(as.list(data_rf$X.100.000.under..200.000))
data_rf$X.200.000.or.more<-as.numeric(as.list(data_rf$X.200.000.or.more))
data_rf$income_Sum <-as.numeric(as.list(data_rf$income_Sum))
data_rf$bin<-as.numeric(as.list(data_rf$bin))
```

#Normalize relevant columns data
```{r}
data_rf <- data_rf %>% mutate_each_(funs(scale(.) %>% as.vector), vars=c("Latitude","Longitude","review_count","rating","Rate_crimes_pt1_violent_2016","Rate_crimes_pt1_property_2016","House_Median_price_per_sqft_2016","X.1.under..25.000","X.25.000.under..50.000","X.50.000.under..75.000","X.75.000.under..100.000","X.100.000.under..200.000","X.200.000.or.more","income_Sum"))

```

#Format factor variables (so no characters)
```{r}
data_rf$Zipcode<-as.factor(data_rf$Zipcode)
data_rf$price <-as.factor(data_rf$price)
data_rf$RestCategory<-as.factor(data_rf$RestCategory)
data_rf$Month<-as.factor(data_rf$Month)
data_rf$Dayofwk<-as.factor(data_rf$Dayofwk)
data_rf$AM_PM<-as.factor(data_rf$AM_PM)
data_rf$Inspection_Type_Code<-as.factor(data_rf$Inspection_Type_Code)
```

################################################################
Create training and testing sets for Random Forest Model

#Set seed value #1
```{r}
set.seed(1467)
```

#Separate data into train and test datasets: split 60% and 40%, respectively
```{r}
rownames(data_rf)<-1:nrow(data_rf)
rows <- sample(x=1:nrow(data_rf),size=0.6*nrow(data_rf))

train_rf <- data_rf[rows,]
test_rf <- data_rf[! rownames(data_rf) %in% rows,]
```

################################################################
Build Correlation plot for Training Dataset

```{r}
drf_1<-data.frame(Latitude=train_rf$Latitude,Longitude=train_rf$Longitude,ReviewCount=train_rf$review_count,Rating=train_rf$rating,Income25K=train_rf$X.1.under..25.000,Income50K=train_rf$X.25.000.under..50.000,Income75K=train_rf$X.50.000.under..75.000,Income100K=train_rf$X.75.000.under..100.000,Income200K=train_rf$X.100.000.under..200.000,Income200KPlus=train_rf$X.200.000.or.more,Income=train_rf$income_Sum,PropertyCrime=train_rf$Rate_crimes_pt1_property_2016,ViolentCrime=train_rf$Rate_crimes_pt1_violent_2016,HousePrice=train_rf$House_Median_price_per_sqft_2016)
```

#Create correlation plot
```{r}
Mrf <- cor(drf_1)
```

```{r}
#install.packages("corrplot")
library(corrplot)
corrplot(Mrf,method="circle")
```

################################################################
Scatterplots 
 - Investigate relationships for variables that are highly correlated

#Plot Latitude vs. Longitude
```{r}
plot(train_rf$Latitude,train_rf$Longitude)
abline(lm(train_rf$Latitude ~ train_rf$Longitude), col="red")
```
There seems to be a few values that are significantly different from others.  Possible issue for model.

#Plot Income25K vs. Income50K
```{r}
plot(train_rf$X.1.under..25.000,train_rf$X.25.000.under..50.000)
abline(lm(train_rf$X.1.under..25.000 ~ train_rf$X.25.000.under..50.000), col="red")
```
Strong positve correlation.  Consider only including one of these variables.


#Plot Violent Crime Rate vs. Property Crime Rate
```{r}
plot(train_rf$Rate_crimes_pt1_violent_2016,train_rf$Rate_crimes_pt1_property_2016)
abline(lm(train_rf$Rate_crimes_pt1_violent_2016 ~ train_rf$Rate_crimes_pt1_property_2016), col="red")
```

#Plot Income vs. Violent Crime Rate
```{r}
plot(train_rf$income_Sum,train_rf$Rate_crimes_pt1_violent_2016)
abline(lm(train_rf$income_Sum ~ train_rf$Rate_crimes_pt1_violent_2016), col="red")
```

################################################################
Training Dataset Plots/Boxplots for Category Variables

#Plot Number of critical violations as a function of Month
```{r}
boxplot(Critical.Violations~Month,data=train_rf,ylab="Number of Critical Violations",xlab="Month",labels=TRUE,las=2)
```
Range of Second and Third Quantiles seem very similar for all Months except September and October, which are lower.
Medians seem very similar except for April, where it is higher.

#Plot Number of critical violations as a function of Day of the Week
```{r}
boxplot(Critical.Violations~Dayofwk,data=train_rf,ylab="Number of Critical Violations",xlab="Day of the Week",labels=TRUE,las=2)
```
The median seems to be the same for Mon - Sat.  There is little variation between Mon - Fri.  Saturday has more variation than Mon-Fri.  Sunday has a considerably different distribution than the other days.  In order to simplify model, consider changing factor variable from a level for each day to the following levels: "Weekday"", "Saturday", and "Sunday".

#Plot Number of critical violations as a function of Time of Day (morning or afternoon)
```{r}
boxplot(Critical.Violations~AM_PM,data=train_rf,ylab="Number of Critical Violations",xlab="Time of Day",labels=TRUE,las=2)
```
There seems to be variability for morning inspections than for afternoon inspections.

#Plot Number of critical violations as a function of Inspection Type
```{r}
boxplot(Critical.Violations~Inspection_Type_Code,data=train_rf,ylab="Number of Critical Violations",xlab="Inspection Type",lables=TRUE,las=2)
```

#Plot Number of critical violations as a function of Restaurant Categories
```{r}
boxplot(Critical.Violations~RestCategory,data=train_rf,ylab="Number of Critical Violations",xlab="Restaurant Category",labels=TRUE,las=2)
```

#Plot Yelp Rating vs. Number of Critical Violations
```{r}
(m=max((data_rf$rating)-min(data_rf$rating))/5)
```

```{r}
plot((train_rf$rating-min(data_rf$rating))/m,train_rf$Critical.Violations,xlab="Yelp Rating",ylab="Number of Critical Violations")
```

################################################################
Run Boruta Feature Selection

#Install Boruta package
```{r}
#install.packages("Boruta")
library(Boruta)
```

#Set Seed
```{r}
set.seed(1603)
```

#Check importance of variables
```{r}
model_bor1<-Boruta(bin ~ Zipcode + Latitude + Longitude + RestCategory + review_count + rating + price + Inspection_Type_Code + X.1.under..25.000 + X.25.000.under..50.000 + X.50.000.under..75.000 + X.75.000.under..100.000 + X.100.000.under..200.000+ X.200.000.or.more + income_Sum + Rate_crimes_pt1_violent_2016 + Rate_crimes_pt1_property_2016 + House_Median_price_per_sqft_2016 + Month + Dayofwk + AM_PM,data=train_rf,doTrace=2)
print(model_bor1)
```
(Takes a while to run--at least 7 min.)
All 21 variables deemed important.

```{r}
plot(model_bor1,xlab="",xaxt="n")
zbor1<-lapply(1:ncol(model_bor1$ImpHistory),function(i) model_bor1$ImpHistory[is.finite(model_bor1$ImpHistory[,i]),i])
names(zbor1)<-colnames(model_bor1$ImpHistory)
Labels<-sort(sapply(zbor1,median))
axis(side=1,las=2,labels=names(Labels),at = 1:ncol(model_bor1$ImpHistory),ces.axis = 0.7)
```

```{r}
model_bor1_df <- attStats(model_bor1)
class(model_bor1_df)
print(model_bor1_df)
```

```{r}
plotImpHistory(model_bor1,colCode=c("green","yellow","red","blue"),col = NULL,type="l",lty=1,pch=0)
```
################################################################
Construct Random Forest Model #1 (Base Model)

```{r}
#install.packages("lattice")
library(lattice)
library(caret)
library(randomForest)
```


#Set seed value #2 
```{r}
set.seed(1927)
```

#Build Random Forest Model
```{r}
model_rf1 <-randomForest(bin ~ Zipcode + Latitude + Longitude + RestCategory + review_count + rating + price + Inspection_Type_Code + Month + Dayofwk + AM_PM,data=train_rf,replace=TRUE,ntree=100)
```

#Plot importance of variables
```{r}
imp_rf1 <- importance(model_rf1)
vars_rf1 <- dimnames(imp_rf1)[(1)]
imp_rf1 <- data.frame(vars=vars_rf1,imp=as.numeric(imp_rf1[,1]))
imp_rf1 <- imp_rf1[order(imp_rf1$imp,decreasing=TRUE),] 
```

```{r}
par(mfrow=c(1,2))
varImpPlot(model_rf1,main="Variable Importance Plot: Base Model")
```
```{r}

```

#Plot Error vs. Tree
```{r}
plot(model_rf1,main="Error vs. No. of trees plot: Base Model")
```

################################################################
Test Random Forest Model #1 (Base Model)

#Run Prediction
```{r}
pred_rf1 <- predict(model_rf1, test_rf)
pred_rf1 <- as.matrix(pred_rf1)
actual_rf <- as.matrix(test_rf$bin)
```

#Plot actual vs. predicted
```{r}
plot(actual_rf,pred_rf1,xlab="Actual",ylab="Predicted",main="Critical Violation Category")
```

## plot the ROC curve for classification of y with p
```{r}
roc <- function(p,y, ...){
  y <- factor(y)
  n <- length(p)
  p <- as.vector(p)
  Q <- p > matrix(rep(seq(0,1,length=100),n),ncol=100,byrow=TRUE)
  specificity <- colMeans(!Q[y==levels(y)[1],])
  sensitivity <- colMeans(Q[y==levels(y)[2],])
  plot(1-specificity, sensitivity, type="l", ...)
  abline(a=0,b=1,lty=2,col=8)
}
```


```{r}
roc(p=pred_rf1, y=actual_rf, bty="n")
## our 1/5 rule cutoff
points(x= 1-mean((pred_rf1<.2)[actual_rf==0]), 
       y=mean((pred_rf1>.2)[actual_rf==1]), 
       cex=1.5, pch=20, col='red')
## a standard `max prob' (p=.5) rule
points(x= 1-mean((pred_rf1<.5)[actual_rf==0]), 
       y=mean((pred_rf1>.5)[actual_rf==1]), 
       cex=1.5, pch=20, col='blue') 
legend("topleft",fill=c("red","blue"),
       legend=c("p=1/5","p=1/2"),bty="n",title="cutoff")

```

#set decision point
```{r}
dp_rf1 <- 0.5
```

# Develop functions to sort results for confusion matrix
```{r}
false_neg1 <- function(x,y) {
  if(x == 1 & y < dp_rf1) 1 else 0
}
false_pos1 <- function(x,y) {
  if(x == 0 & y >= dp_rf1) 1 else 0
}
true_neg1 <- function(x,y) {
  if(x == 0 & y < dp_rf1) 1 else 0
}
true_pos1 <- function(x,y) {
  if(x == 1 & y >= dp_rf1) 1 else 0
}
```

#Sort results for confusion matrix
```{r}
true_pos_rf1<-rep(NA,length(pred_rf1))
true_neg_rf1<-rep(NA,length(pred_rf1))
false_pos_rf1<-rep(NA,length(pred_rf1))
false_neg_rf1<-rep(NA,length(pred_rf1))
for(i in 1:length(pred_rf1)) {
  true_pos_rf1[i] <- true_pos1(actual_rf[i],pred_rf1[i])
  true_neg_rf1[i] <- true_neg1(actual_rf[i],pred_rf1[i])
  false_pos_rf1[i] <- false_pos1(actual_rf[i],pred_rf1[i])
  false_neg_rf1[i] <- false_neg1(actual_rf[i],pred_rf1[i])
}
```

#Develop confuson matrix values
```{r}
con_rf1 <- c(true_pos_rf1, false_pos_rf1, false_neg_rf1, true_neg_rf1)
con_rf1 <- matrix(con_rf1,nrow = length(pred_rf1),ncol=4)
```

```{r}
TP_rf1<-sum(con_rf1[,1])
FP_rf1<-sum(con_rf1[,2])
FN_rf1<-sum(con_rf1[,3])
TN_rf1<-sum(con_rf1[,4])
Con_title_rf1<-c("TP","FP","FN","TN")
Confusion_rf1<-c(TP_rf1,FP_rf1,FN_rf1,TN_rf1)
(Confusion_rf1<-matrix(Confusion_rf1,nrow=2,ncol=2))
```

#Calculate Accuracy
```{r}
(Accuracy_rf1<-(TP_rf1+TN_rf1)/nrow(test_rf))
```

################################################################
Construct Random Forest Model #2 (Reduced Model)
(Drop 10 least important variables: X.1.under..25.000; X.25.000.under..50.000; X.50.000.under..75.000; X.75.000.under..100.000; X.100.000.under..200.000; X.200.000.or.more; income_Sum; Rate_crimes_pt1_violent_2016; Rate_crimes_pt1_property_2016; and House_Median_price_per_sqft_2016.)

#Set seed value #2 
```{r}
set.seed(1927)
```

#Build Random Forest Model
```{r}
model_rf2 <-randomForest(bin ~ Zipcode + Latitude + Longitude + RestCategory + review_count + rating + price + Inspection_Type_Code + Month + Dayofwk + AM_PM,data=train_rf,replace=TRUE,ntree=100)
```

#Plot importance of variables
```{r}
imp_rf2 <- importance(model_rf2)
vars_rf2 <- dimnames(imp_rf2)[(1)]
imp_rf2 <- data.frame(vars=vars_rf2,imp=as.numeric(imp_rf2[,1]))
imp_rf2 <- imp_rf2[order(imp_rf2$imp,decreasing=TRUE),] 
```

```{r}
par(mfrow=c(1,2))
varImpPlot(model_rf2,main="Variable Importance Plot: Reduced Model")
```



#Plot Error vs. Tree
```{r}
plot(model_rf2,main="Error vs. No. of trees plot: Reduced Model")
```

################################################################
Test Random Forest Model #2 (Reduced Model)


#Run Prediction
```{r}
pred_rf2 <- predict(model_rf2, test_rf)
pred_rf2 <- as.matrix(pred_rf2)
actual_rf <- as.matrix(test_rf$bin)
```

#Plot actual vs. predicted
```{r}
plot(actual_rf,pred_rf2,xlab="Actual",ylab="Predicted",main="Critical Violation Category")
```

## plot the ROC curve for classification of y with p
```{r}
roc(p=pred_rf2, y=actual_rf, bty="n")
## our 1/5 rule cutoff
points(x= 1-mean((pred_rf2<.2)[actual_rf==0]), 
       y=mean((pred_rf2>.2)[actual_rf==1]), 
       cex=1.5, pch=20, col='red')
## a standard `max prob' (p=.5) rule
points(x= 1-mean((pred_rf2<.5)[actual_rf==0]), 
       y=mean((pred_rf2>.5)[actual_rf==1]), 
       cex=1.5, pch=20, col='blue') 
legend("topleft",fill=c("red","blue"),
       legend=c("p=1/5","p=1/2"),bty="n",title="cutoff")

```

#set decision point
```{r}
dp_rf2 <- 0.5
```

# Develop functions to sort results for confusion matrix
```{r}
false_neg2 <- function(x,y) {
  if(x == 1 & y < dp_rf2) 1 else 0
}
false_pos2 <- function(x,y) {
  if(x == 0 & y >= dp_rf2) 1 else 0
}
true_neg2 <- function(x,y) {
  if(x == 0 & y < dp_rf2) 1 else 0
}
true_pos2 <- function(x,y) {
  if(x == 1 & y >= dp_rf2) 1 else 0
}
```

#Sort results for confusion matrix
```{r}
true_pos_rf2<-rep(NA,length(pred_rf2))
true_neg_rf2<-rep(NA,length(pred_rf2))
false_pos_rf2<-rep(NA,length(pred_rf2))
false_neg_rf2<-rep(NA,length(pred_rf2))
for(i in 1:length(pred_rf2)) {
  true_pos_rf2[i] <- true_pos2(actual_rf[i],pred_rf2[i])
  true_neg_rf2[i] <- true_neg2(actual_rf[i],pred_rf2[i])
  false_pos_rf2[i] <- false_pos2(actual_rf[i],pred_rf2[i])
  false_neg_rf2[i] <- false_neg2(actual_rf[i],pred_rf2[i])
}
```

#Develop confuson matrix values
```{r}
con_rf2 <- c(true_pos_rf2, false_pos_rf2, false_neg_rf2, true_neg_rf2)
con_rf2 <- matrix(con_rf2,nrow = length(pred_rf2),ncol=4)
```

```{r}
TP_rf2<-sum(con_rf2[,1])
FP_rf2<-sum(con_rf2[,2])
FN_rf2<-sum(con_rf2[,3])
TN_rf2<-sum(con_rf2[,4])
Con_title_rf2<-c("TP","FP","FN","TN")
Confusion_rf2<-c(TP_rf2,FP_rf2,FN_rf2,TN_rf2)
(Confusion_rf2<-matrix(Confusion_rf2,nrow=2,ncol=2))
```

#Calculate Accuracy
```{r}
(Accuracy_rf2<-(TP_rf2+TN_rf2)/nrow(test_rf))
```

################################################################
Compare Random Forest Model 1 and 2 ROCs

```{r}
roc(p=pred_rf1, y=actual_rf, bty="n",col="red")
par(new=TRUE)
roc(p=pred_rf2, y=actual_rf, bty="n",col="blue",axes = FALSE, xlab = "", ylab = "")
legend("topleft",fill=c("red","blue"),
       legend=c("Base","Reduced"),bty="n",title="model")

```

So, the Reduced Model performed better than the Base Model.


################################################################
Construct Random Forest Model #3 (Hypothesis Model)
(Isolate hypothesis variables: Inspection_Type_Code; and rating;)

#Set seed value #2 
```{r}
set.seed(1927)
```

#Build Random Forest Model
```{r}
model_rf3 <-randomForest(bin ~ rating + Inspection_Type_Code, data=train_rf,replace=TRUE,ntree=100)
```

#Plot importance of variables
```{r}
imp_rf3 <- importance(model_rf3)
vars_rf3 <- dimnames(imp_rf3)[(1)]
imp_rf3 <- data.frame(vars=vars_rf3,imp=as.numeric(imp_rf3[,1]))
imp_rf3 <- imp_rf3[order(imp_rf3$imp,decreasing=TRUE),] 
```

```{r}
par(mfrow=c(1,2))
varImpPlot(model_rf3,main="Variable Importance Plot: Hypothesis Model")
```

#Plot Error vs. Tree
```{r}
plot(model_rf3,main="Error vs. No. of trees plot: Hypothesis Model")
```

################################################################
Test Random Forest Model #3 (Hypothesis Model)


#Run Prediction
```{r}
pred_rf3 <- predict(model_rf3, test_rf)
pred_rf3 <- as.matrix(pred_rf3)
actual_rf <- as.matrix(test_rf$bin)
```

#Plot actual vs. predicted
```{r}
plot(actual_rf,pred_rf3,xlab="Actual",ylab="Predicted",main="Critical Violation Category")
```

## plot the ROC curve for classification of y with p
```{r}
roc(p=pred_rf3, y=actual_rf, bty="n")
## our 1/5 rule cutoff
points(x= 1-mean((pred_rf3<.2)[actual_rf==0]), 
       y=mean((pred_rf3>.2)[actual_rf==1]), 
       cex=1.5, pch=20, col='red')
## a standard `max prob' (p=.5) rule
points(x= 1-mean((pred_rf3<.5)[actual_rf==0]), 
       y=mean((pred_rf3>.5)[actual_rf==1]), 
       cex=1.5, pch=20, col='blue') 
legend("topleft",fill=c("red","blue"),
       legend=c("p=1/5","p=1/2"),bty="n",title="cutoff")

```

#set decision point
```{r}
dp_rf3 <- 0.5
```

# Develop functions to sort results for confusion matrix
```{r}
false_neg_rf <- function(x,y,z) {
  if(x == 1 & y < z) 1 else 0
}
false_pos_rf <- function(x,y,z) {
  if(x == 0 & y >= z) 1 else 0
}
true_neg_rf <- function(x,y,z) {
  if(x == 0 & y < z) 1 else 0
}
true_pos_rf <- function(x,y,z) {
  if(x == 1 & y >= z) 1 else 0
}
```

#Sort results for confusion matrix
```{r}
true_pos_rf3<-rep(NA,length(pred_rf3))
true_neg_rf3<-rep(NA,length(pred_rf3))
false_pos_rf3<-rep(NA,length(pred_rf3))
false_neg_rf3<-rep(NA,length(pred_rf3))
for(i in 1:length(pred_rf3)) {
  true_pos_rf3[i] <- true_pos_rf(actual_rf[i],pred_rf3[i],---
title: "DCR_Random_Forest_RAS"
output: html_document
---

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document.

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

################################################################
Install Packages

#install package for plyr
```{r}
#install.packages("plyr")
library(plyr)
```

#install package for dplyr
```{r}
#install.packages("dplyr")
library(dplyr)
```

#install package for lubridate
```{r}
#install.packages("lubridate")
library(lubridate)
```

#knit 
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#Install package for trees
```{r}
#install.packages("rpart")
library(rpart)
```

#Install package for Random Forest
```{r}
#install.packages("randomForest")
library(randomForest)
```


################################################################
Load Dataset and Develop DC Restaurant Maps

#Check working directory
```{r}
getwd()
```

#Read in DC Restaurant dataset
```{r}
data_rf <-read.csv("data/data_merged_R.csv")
```

#Create map of Washington, DC
```{r}
# install.packages("ggplot2")
# install.packages("ggmap")
library(ggplot2)
library(ggmap)
dc_map <- get_map(location = "Washington, DC", maptype = "roadmap", zoom = 13)
```

# Create poit maps of inspections
```{r}
ggmap(dc_map, extent = "panel") + geom_point(aes(x = Longitude, y = Latitude), colour = "red", alpha = 0.3, size = 1, data =data_rf)
```

#Create heat map for inspections with 2 or more critical violations, filter for critical vioaltions
```{r}
dc_crit_viol<-data_rf[order(data_rf$Critical.Violations),]
dc_crit_viol<-subset(dc_crit_viol,dc_crit_viol[,20] > 2)
```

```{r}
ggmap(dc_map,extent="panel") + geom_density2d(data=dc_crit_viol, aes(x=Longitude,y=Latitude))+ 
  stat_density2d(data = dc_crit_viol, aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") + scale_alpha(range = c(0, 0.3), guide = FALSE)
```

################################################################
Data cleanup/prep

#Develop function to fix establishment categories
```{r}
fix_newcat <- function (x) {
  if(x=="vegetarian") "Vegan/vegeterian" else
    if (x=="Sea Food") "seafood" else
      if(x=="olittler_establishment") "Other_establishment" else
        if(x=="restaurants") "Other_establishment" else 
          if(x=="Indian/Packistani") "Indian/Pakistani" else
            if(x=="Delivery") "Other_establishment" else
              if(x=="falafel") "European" else
    if(x=="juicebars") "Other_establishment" else
      if(x=="Kosher") "Other_establishment" else
        if(x=="Russian") "Other_establishment" else
          if(x=="empanadas") "Other_establishment" else
            if(x=="Peruvian") "Latin" else
              if(x=="Argentine") "Latin" else
                if(x=="Dominican") "caribbean" else
                  if(x=="malaysian") "Asian" else
                    if(x=="Portuguese") "European" else
                      if (x=="Islander") "caribbean" else as.character(x)
}

```

#Add new column for fixed establishment category
```{r}
data_rf$RestCategory<-sapply(data_rf$NewCategory,fix_newcat)
```

#Develop new columns for year, month, and day of the week
```{r}
year_rf<-year(as.Date(as.character(data_rf$Date),"%m/%d/%Y"))
month_rf<-month(as.Date(as.character(data_rf$Date),"%m/%d/%Y"),label=TRUE)
dayofwk_rf<-wday(as.Date(as.character(data_rf$Date),"%m/%d/%Y"),label=TRUE)
```

#Develop new column for time of inspection (morning or afternoon)
```{r}
hour_rf<-hour(as.POSIXct(data_rf$Time,"%H:%M",tz="EST"))
daytime_rf<-function(x) {ifelse(x>12,"PM","AM")}
AM_PM_rf<-sapply(hour_rf,daytime_rf)
```

#Add new columns to dataset
```{r}
data_rf$Year<-year_rf
data_rf$Month<-month_rf
data_rf$Dayofwk<-dayofwk_rf
data_rf$AM_PM<-AM_PM_rf
```

#Add binary variable for predicted value: >= 2 = 1, and < 0 = 0
## Function to create binary variable
```{r}
crit_v <-function(x) {
  if (x>2) as.numeric(1) else as.numeric(0)
}
```

## Apply function to dataset
```{r}
data_rf$bin <- sapply(data_rf$Critical.Violations,crit_v)
```

#Remove missing values in dataset
```{r}
data_rf<-na.omit(data_rf)
```

#Format numeric variables
```{r}
mode(data_rf)
data_rf$Latitude  <-as.numeric(as.list(data_rf$Latitude))
data_rf$Longitude<-as.numeric(as.list(data_rf$Longitude))
data_rf$review_count<-as.numeric(as.list(data_rf$review_count))
data_rf$rating <-as.numeric(as.list(data_rf$rating))
data_rf$Rate_crimes_pt1_violent_2016<-as.numeric(as.list(data_rf$Rate_crimes_pt1_violent_2016))
data_rf$Rate_crimes_pt1_property_2016<-as.numeric(as.list(data_rf$Rate_crimes_pt1_property_2016))
data_rf$House_Median_price_per_sqft_2016<-as.numeric(as.list(data_rf$House_Median_price_per_sqft_2016))
data_rf$X.1.under..25.000<-as.numeric(as.list(data_rf$X.1.under..25.000))
data_rf$X.25.000.under..50.000<-as.numeric(as.list(data_rf$X.25.000.under..50.000))
data_rf$X.50.000.under..75.000<-as.numeric(as.list(data_rf$X.50.000.under..75.000))
data_rf$X.75.000.under..100.000<-as.numeric(as.list(data_rf$X.75.000.under..100.000))
data_rf$X.100.000.under..200.000<-as.numeric(as.list(data_rf$X.100.000.under..200.000))
data_rf$X.200.000.or.more<-as.numeric(as.list(data_rf$X.200.000.or.more))
data_rf$income_Sum <-as.numeric(as.list(data_rf$income_Sum))
data_rf$bin<-as.numeric(as.list(data_rf$bin))
```

#Normalize relevant columns data
```{r}
data_rf <- data_rf %>% mutate_each_(funs(scale(.) %>% as.vector), vars=c("Latitude","Longitude","review_count","rating","Rate_crimes_pt1_violent_2016","Rate_crimes_pt1_property_2016","House_Median_price_per_sqft_2016","X.1.under..25.000","X.25.000.under..50.000","X.50.000.under..75.000","X.75.000.under..100.000","X.100.000.under..200.000","X.200.000.or.more","income_Sum"))

```

#Format factor variables (so no characters)
```{r}
data_rf$Zipcode<-as.factor(data_rf$Zipcode)
data_rf$price <-as.factor(data_rf$price)
data_rf$RestCategory<-as.factor(data_rf$RestCategory)
data_rf$Month<-as.factor(data_rf$Month)
data_rf$Dayofwk<-as.factor(data_rf$Dayofwk)
data_rf$AM_PM<-as.factor(data_rf$AM_PM)
data_rf$Inspection_Type_Code<-as.factor(data_rf$Inspection_Type_Code)
```

################################################################
Create training and testing sets for Random Forest Model

#Set seed value #1
```{r}
set.seed(1467)
```

#Separate data into train and test datasets: split 60% and 40%, respectively
```{r}
rownames(data_rf)<-1:nrow(data_rf)
rows <- sample(x=1:nrow(data_rf),size=0.6*nrow(data_rf))

train_rf <- data_rf[rows,]
test_rf <- data_rf[! rownames(data_rf) %in% rows,]
```

################################################################
Build Correlation plot for Training Dataset

```{r}
drf_1<-data.frame(Latitude=train_rf$Latitude,Longitude=train_rf$Longitude,ReviewCount=train_rf$review_count,Rating=train_rf$rating,Income25K=train_rf$X.1.under..25.000,Income50K=train_rf$X.25.000.under..50.000,Income75K=train_rf$X.50.000.under..75.000,Income100K=train_rf$X.75.000.under..100.000,Income200K=train_rf$X.100.000.under..200.000,Income200KPlus=train_rf$X.200.000.or.more,Income=train_rf$income_Sum,PropertyCrime=train_rf$Rate_crimes_pt1_property_2016,ViolentCrime=train_rf$Rate_crimes_pt1_violent_2016,HousePrice=train_rf$House_Median_price_per_sqft_2016)
```

#Create correlation plot
```{r}
Mrf <- cor(drf_1)
```

```{r}
#install.packages("corrplot")
library(corrplot)
corrplot(Mrf,method="circle")
```

################################################################
Scatterplots 
 - Investigate relationships for variables that are highly correlated

#Plot Latitude vs. Longitude
```{r}
plot(train_rf$Latitude,train_rf$Longitude)
abline(lm(train_rf$Latitude ~ train_rf$Longitude), col="red")
```
There seems to be a few values that are significantly different from others.  Possible issue for model.

#Plot Income25K vs. Income50K
```{r}
plot(train_rf$X.1.under..25.000,train_rf$X.25.000.under..50.000)
abline(lm(train_rf$X.1.under..25.000 ~ train_rf$X.25.000.under..50.000), col="red")
```
Strong positve correlation.  Consider only including one of these variables.


#Plot Violent Crime Rate vs. Property Crime Rate
```{r}
plot(train_rf$Rate_crimes_pt1_violent_2016,train_rf$Rate_crimes_pt1_property_2016)
abline(lm(train_rf$Rate_crimes_pt1_violent_2016 ~ train_rf$Rate_crimes_pt1_property_2016), col="red")
```

#Plot Income vs. Violent Crime Rate
```{r}
plot(train_rf$income_Sum,train_rf$Rate_crimes_pt1_violent_2016)
abline(lm(train_rf$income_Sum ~ train_rf$Rate_crimes_pt1_violent_2016), col="red")
```

################################################################
Training Dataset Plots/Boxplots for Category Variables

#Plot Number of critical violations as a function of Month
```{r}
boxplot(Critical.Violations~Month,data=train_rf,ylab="Number of Critical Violations",xlab="Month",labels=TRUE,las=2)
```
Range of Second and Third Quantiles seem very similar for all Months except September and October, which are lower.
Medians seem very similar except for April, where it is higher.

#Plot Number of critical violations as a function of Day of the Week
```{r}
boxplot(Critical.Violations~Dayofwk,data=train_rf,ylab="Number of Critical Violations",xlab="Day of the Week",labels=TRUE,las=2)
```
The median seems to be the same for Mon - Sat.  There is little variation between Mon - Fri.  Saturday has more variation than Mon-Fri.  Sunday has a considerably different distribution than the other days.  In order to simplify model, consider changing factor variable from a level for each day to the following levels: "Weekday"", "Saturday", and "Sunday".

#Plot Number of critical violations as a function of Time of Day (morning or afternoon)
```{r}
boxplot(Critical.Violations~AM_PM,data=train_rf,ylab="Number of Critical Violations",xlab="Time of Day",labels=TRUE,las=2)
```
There seems to be variability for morning inspections than for afternoon inspections.

#Plot Number of critical violations as a function of Inspection Type
```{r}
boxplot(Critical.Violations~Inspection_Type_Code,data=train_rf,ylab="Number of Critical Violations",xlab="Inspection Type",lables=TRUE,las=2)
```

#Plot Number of critical violations as a function of Restaurant Categories
```{r}
boxplot(Critical.Violations~RestCategory,data=train_rf,ylab="Number of Critical Violations",xlab="Restaurant Category",labels=TRUE,las=2)
```

#Plot Yelp Rating vs. Number of Critical Violations
```{r}
(m=max((data_rf$rating)-min(data_rf$rating))/5)
```

```{r}
plot((train_rf$rating-min(data_rf$rating))/m,train_rf$Critical.Violations,xlab="Yelp Rating",ylab="Number of Critical Violations")
```

################################################################
Run Boruta Feature Selection

#Install Boruta package
```{r}
#install.packages("Boruta")
library(Boruta)
```

#Set Seed
```{r}
set.seed(1603)
```

#Check importance of variables
```{r}
model_bor1<-Boruta(bin ~ Zipcode + Latitude + Longitude + RestCategory + review_count + rating + price + Inspection_Type_Code + X.1.under..25.000 + X.25.000.under..50.000 + X.50.000.under..75.000 + X.75.000.under..100.000 + X.100.000.under..200.000+ X.200.000.or.more + income_Sum + Rate_crimes_pt1_violent_2016 + Rate_crimes_pt1_property_2016 + House_Median_price_per_sqft_2016 + Month + Dayofwk + AM_PM,data=train_rf,doTrace=2)
print(model_bor1)
```
(Takes a while to run--at least 7 min.)
All 21 variables deemed important.

```{r}
plot(model_bor1,xlab="",xaxt="n")
zbor1<-lapply(1:ncol(model_bor1$ImpHistory),function(i) model_bor1$ImpHistory[is.finite(model_bor1$ImpHistory[,i]),i])
names(zbor1)<-colnames(model_bor1$ImpHistory)
Labels<-sort(sapply(zbor1,median))
axis(side=1,las=2,labels=names(Labels),at = 1:ncol(model_bor1$ImpHistory),ces.axis = 0.7)
```

```{r}
model_bor1_df <- attStats(model_bor1)
class(model_bor1_df)
print(model_bor1_df)
```

```{r}
plotImpHistory(model_bor1,colCode=c("green","yellow","red","blue"),col = NULL,type="l",lty=1,pch=0)
```
################################################################
Construct Random Forest Model #1 (Base Model)

```{r}
#install.packages("lattice")
library(lattice)
library(caret)
library(randomForest)
```


#Set seed value #2 
```{r}
set.seed(1927)
```

#Build Random Forest Model
```{r}
model_rf1 <-randomForest(bin ~ Zipcode + Latitude + Longitude + RestCategory + review_count + rating + price + Inspection_Type_Code + Month + Dayofwk + AM_PM,data=train_rf,replace=TRUE,ntree=100)
```

#Plot importance of variables
```{r}
imp_rf1 <- importance(model_rf1)
vars_rf1 <- dimnames(imp_rf1)[(1)]
imp_rf1 <- data.frame(vars=vars_rf1,imp=as.numeric(imp_rf1[,1]))
imp_rf1 <- imp_rf1[order(imp_rf1$imp,decreasing=TRUE),] 
```

```{r}
par(mfrow=c(1,2))
varImpPlot(model_rf1,main="Variable Importance Plot: Base Model")
```
```{r}

```

#Plot Error vs. Tree
```{r}
plot(model_rf1,main="Error vs. No. of trees plot: Base Model")
```

################################################################
Test Random Forest Model #1 (Base Model)

#Run Prediction
```{r}
pred_rf1 <- predict(model_rf1, test_rf)
pred_rf1 <- as.matrix(pred_rf1)
actual_rf <- as.matrix(test_rf$bin)
```

#Plot actual vs. predicted
```{r}
plot(actual_rf,pred_rf1,xlab="Actual",ylab="Predicted",main="Critical Violation Category")
```

## plot the ROC curve for classification of y with p
```{r}
roc <- function(p,y, ...){
  y <- factor(y)
  n <- length(p)
  p <- as.vector(p)
  Q <- p > matrix(rep(seq(0,1,length=100),n),ncol=100,byrow=TRUE)
  specificity <- colMeans(!Q[y==levels(y)[1],])
  sensitivity <- colMeans(Q[y==levels(y)[2],])
  plot(1-specificity, sensitivity, type="l", ...)
  abline(a=0,b=1,lty=2,col=8)
}
```


```{r}
roc(p=pred_rf1, y=actual_rf, bty="n")
## our 1/5 rule cutoff
points(x= 1-mean((pred_rf1<.2)[actual_rf==0]), 
       y=mean((pred_rf1>.2)[actual_rf==1]), 
       cex=1.5, pch=20, col='red')
## a standard `max prob' (p=.5) rule
points(x= 1-mean((pred_rf1<.5)[actual_rf==0]), 
       y=mean((pred_rf1>.5)[actual_rf==1]), 
       cex=1.5, pch=20, col='blue') 
legend("topleft",fill=c("red","blue"),
       legend=c("p=1/5","p=1/2"),bty="n",title="cutoff")

```

#set decision point
```{r}
dp_rf1 <- 0.5
```

# Develop functions to sort results for confusion matrix
```{r}
false_neg_rf <- function(x,y,z) {
  if(x == 1 & y < z) 1 else 0
}
false_pos_rf <- function(x,y,z) {
  if(x == 0 & y >= z) 1 else 0
}
true_neg_rf <- function(x,y,z) {
  if(x == 0 & y < z) 1 else 0
}
true_pos_rf <- function(x,y,z) {
  if(x == 1 & y >= z) 1 else 0
}
```

#Sort results for confusion matrix
```{r}
true_pos_rf1<-rep(NA,length(pred_rf1))
true_neg_rf1<-rep(NA,length(pred_rf1))
false_pos_rf1<-rep(NA,length(pred_rf1))
false_neg_rf1<-rep(NA,length(pred_rf1))
for(i in 1:length(pred_rf1)) {
  true_pos_rf1[i] <- true_pos_rf(actual_rf[i],pred_rf1[i],dp_rf1)
  true_neg_rf1[i] <- true_neg_rf(actual_rf[i],pred_rf1[i],dp_rf1)
  false_pos_rf1[i] <- false_pos_rf(actual_rf[i],pred_rf1[i],dp_rf1)
  false_neg_rf1[i] <- false_neg_rf(actual_rf[i],pred_rf1[i],dp_rf1)
}
```

#Develop confuson matrix values
```{r}
con_rf1 <- c(true_pos_rf1, false_pos_rf1, false_neg_rf1, true_neg_rf1)
con_rf1 <- matrix(con_rf1,nrow = length(pred_rf1),ncol=4)
```

```{r}
TP_rf1<-sum(con_rf1[,1])
FP_rf1<-sum(con_rf1[,2])
FN_rf1<-sum(con_rf1[,3])
TN_rf1<-sum(con_rf1[,4])
Con_title_rf1<-c("TP","FP","FN","TN")
Confusion_rf1<-c(TP_rf1,FP_rf1,FN_rf1,TN_rf1)
(Confusion_rf1<-matrix(Confusion_rf1,nrow=2,ncol=2))
```

#Calculate Accuracy
```{r}
(Accuracy_rf1<-(TP_rf1+TN_rf1)/nrow(test_rf))
```

################################################################
Construct Random Forest Model #2 (Reduced Model)
(Drop 10 least important variables: X.1.under..25.000; X.25.000.under..50.000; X.50.000.under..75.000; X.75.000.under..100.000; X.100.000.under..200.000; X.200.000.or.more; income_Sum; Rate_crimes_pt1_violent_2016; Rate_crimes_pt1_property_2016; and House_Median_price_per_sqft_2016.)

#Set seed value #2 
```{r}
set.seed(1927)
```

#Build Random Forest Model
```{r}
model_rf2 <-randomForest(bin ~ Zipcode + Latitude + Longitude + RestCategory + review_count + rating + price + Inspection_Type_Code + Month + Dayofwk + AM_PM,data=train_rf,replace=TRUE,ntree=100)
```

#Plot importance of variables
```{r}
imp_rf2 <- importance(model_rf2)
vars_rf2 <- dimnames(imp_rf2)[(1)]
imp_rf2 <- data.frame(vars=vars_rf2,imp=as.numeric(imp_rf2[,1]))
imp_rf2 <- imp_rf2[order(imp_rf2$imp,decreasing=TRUE),] 
```

```{r}
par(mfrow=c(1,2))
varImpPlot(model_rf2,main="Variable Importance Plot: Reduced Model")
```



#Plot Error vs. Tree
```{r}
plot(model_rf2,main="Error vs. No. of trees plot: Reduced Model")
```

################################################################
Test Random Forest Model #2 (Reduced Model)


#Run Prediction
```{r}
pred_rf2 <- predict(model_rf2, test_rf)
pred_rf2 <- as.matrix(pred_rf2)
actual_rf <- as.matrix(test_rf$bin)
```

#Plot actual vs. predicted
```{r}
plot(actual_rf,pred_rf2,xlab="Actual",ylab="Predicted",main="Critical Violation Category")
```

## plot the ROC curve for classification of y with p
```{r}
roc(p=pred_rf2, y=actual_rf, bty="n")
## our 1/5 rule cutoff
points(x= 1-mean((pred_rf2<.2)[actual_rf==0]), 
       y=mean((pred_rf2>.2)[actual_rf==1]), 
       cex=1.5, pch=20, col='red')
## a standard `max prob' (p=.5) rule
points(x= 1-mean((pred_rf2<.5)[actual_rf==0]), 
       y=mean((pred_rf2>.5)[actual_rf==1]), 
       cex=1.5, pch=20, col='blue') 
legend("topleft",fill=c("red","blue"),
       legend=c("p=1/5","p=1/2"),bty="n",title="cutoff")

```

#set decision point
```{r}
dp_rf2 <- 0.5
```


#Sort results for confusion matrix
```{r}
true_pos_rf2<-rep(NA,length(pred_rf2))
true_neg_rf2<-rep(NA,length(pred_rf2))
false_pos_rf2<-rep(NA,length(pred_rf2))
false_neg_rf2<-rep(NA,length(pred_rf2))
for(i in 1:length(pred_rf2)) {
  true_pos_rf2[i] <- true_pos_rf(actual_rf[i],pred_rf2[i],dp_rf2)
  true_neg_rf2[i] <- true_neg_rf(actual_rf[i],pred_rf2[i],dp_rf2)
  false_pos_rf2[i] <- false_pos_rf(actual_rf[i],pred_rf2[i],dp_rf2)
  false_neg_rf2[i] <- false_neg_rf(actual_rf[i],pred_rf2[i],dp_rf2)
}
```

#Develop confuson matrix values
```{r}
con_rf2 <- c(true_pos_rf2, false_pos_rf2, false_neg_rf2, true_neg_rf2)
con_rf2 <- matrix(con_rf2,nrow = length(pred_rf2),ncol=4)
```

```{r}
TP_rf2<-sum(con_rf2[,1])
FP_rf2<-sum(con_rf2[,2])
FN_rf2<-sum(con_rf2[,3])
TN_rf2<-sum(con_rf2[,4])
Con_title_rf2<-c("TP","FP","FN","TN")
Confusion_rf2<-c(TP_rf2,FP_rf2,FN_rf2,TN_rf2)
(Confusion_rf2<-matrix(Confusion_rf2,nrow=2,ncol=2))
```

#Calculate Accuracy
```{r}
(Accuracy_rf2<-(TP_rf2+TN_rf2)/nrow(test_rf))
```

################################################################
Compare Random Forest Model 1 and 2 ROCs

```{r}
roc(p=pred_rf1, y=actual_rf, bty="n",col="red")
par(new=TRUE)
roc(p=pred_rf2, y=actual_rf, bty="n",col="blue",axes = FALSE, xlab = "", ylab = "")
legend("topleft",fill=c("red","blue"),
       legend=c("Base","Reduced"),bty="n",title="model")

```

So, the Reduced Model performed better than the Base Model.


################################################################
Construct Random Forest Model #3 (Hypothesis Model)
(Isolate hypothesis variables: Inspection_Type_Code; and rating;)

#Set seed value #2 
```{r}
set.seed(1927)
```

#Build Random Forest Model
```{r}
model_rf3 <-randomForest(bin ~ rating + Inspection_Type_Code, data=train_rf,replace=TRUE,ntree=500)
```

#Plot importance of variables
```{r}
imp_rf3 <- importance(model_rf3)
vars_rf3 <- dimnames(imp_rf3)[(1)]
imp_rf3 <- data.frame(vars=vars_rf3,imp=as.numeric(imp_rf3[,1]))
imp_rf3 <- imp_rf3[order(imp_rf3$imp,decreasing=TRUE),] 
```

```{r}
par(mfrow=c(1,2))
varImpPlot(model_rf3,main="Variable Importance Plot: Hypothesis Model")
```

#Plot Error vs. Tree
```{r}
plot(model_rf3,main="Error vs. No. of trees plot: Hypothesis Model")
```

################################################################
Test Random Forest Model #3 (Hypothesis Model)


#Run Prediction
```{r}
pred_rf3 <- predict(model_rf3, test_rf)
pred_rf3 <- as.matrix(pred_rf3)
actual_rf <- as.matrix(test_rf$bin)
```

#Plot actual vs. predicted
```{r}
plot(actual_rf,pred_rf3,xlab="Actual",ylab="Predicted",main="Critical Violation Category")
```

## plot the ROC curve for classification of y with p
```{r}
roc(p=pred_rf3, y=actual_rf, bty="n")
## our 1/5 rule cutoff
points(x= 1-mean((pred_rf3<.2)[actual_rf==0]), 
       y=mean((pred_rf3>.2)[actual_rf==1]), 
       cex=1.5, pch=20, col='red')
## a standard `max prob' (p=.5) rule
points(x= 1-mean((pred_rf3<.5)[actual_rf==0]), 
       y=mean((pred_rf3>.5)[actual_rf==1]), 
       cex=1.5, pch=20, col='blue') 
legend("topleft",fill=c("red","blue"),
       legend=c("p=1/5","p=1/2"),bty="n",title="cutoff")

```

#set decision point
```{r}
dp_rf3 <- 0.5
```


#Sort results for confusion matrix
```{r}
true_pos_rf3<-rep(NA,length(pred_rf3))
true_neg_rf3<-rep(NA,length(pred_rf3))
false_pos_rf3<-rep(NA,length(pred_rf3))
false_neg_rf3<-rep(NA,length(pred_rf3))
for(i in 1:length(pred_rf3)) {
  true_pos_rf3[i] <- true_pos_rf(actual_rf[i],pred_rf3[i],dp_rf3)
  true_neg_rf3[i] <- true_neg_rf(actual_rf[i],pred_rf3[i],dp_rf3)
  false_pos_rf3[i] <- false_pos_rf(actual_rf[i],pred_rf3[i],dp_rf3)
  false_neg_rf3[i] <- false_neg_rf(actual_rf[i],pred_rf3[i],dp_rf3)
}
```

#Develop confuson matrix values
```{r}
con_rf3 <- c(true_pos_rf3, false_pos_rf3, false_neg_rf3, true_neg_rf3)
con_rf3 <- matrix(con_rf3,nrow = length(pred_rf3),ncol=4)
```

```{r}
TP_rf3<-sum(con_rf3[,1])
FP_rf3<-sum(con_rf3[,2])
FN_rf3<-sum(con_rf3[,3])
TN_rf3<-sum(con_rf3[,4])
Con_title_rf3<-c("TP","FP","FN","TN")
Confusion_rf3<-c(TP_rf3,FP_rf3,FN_rf3,TN_rf3)
(Confusion_rf3<-matrix(Confusion_rf2,nrow=2,ncol=2))
```

#Calculate Accuracy
```{r}
(Accuracy_rf3<-(TP_rf3+TN_rf3)/nrow(test_rf))
```

################################################################
Conduct Cross-Validation on Random Forest Model

# Set Seed 4
```{r}
set.seed(997)
```

# Develop control function 
```{r}
control_rf <- rfeControl(functions=rfFuncs,method="cv",number=10)
```

```{r}
train_rf_control<-subset(train_rf,select = c(Zipcode, Latitude, Longitude, RestCategory, review_count, rating, price, Inspection_Type_Code, Month, Dayofwk,AM_PM,bin))
```


# Implement Recursive Feature Elimination (RFE) algorithm
```{r}
rfe.train <- rfe(train_rf_control[,2:11],train_rf_control[,12], sizes=1:11, rfeControl=control_rf)
```

```{r}
rfe.train
```

```{r}
plot(rfe.train, type=c("g","o"), cex=1.0, col=1:11)
```

```{r}
predictors(rfe.train)
```






