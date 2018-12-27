# Clearing Environment ----------------------------------------------------
rm(list=ls(all=TRUE))
setwd("C:\\Users\\Bharadwaj\\Desktop\\INSOFE\\Day43_44\\ML\\data")
getwd()

# Reading data ------------------------------------------------------------
cust <- read.csv('ExisitngCustomerDemographics.csv', header = TRUE, na.strings = c("", "  ","  ","   ","?","#",".","NA"),stringsAsFactors = TRUE)
tran <- read.csv('ExistingCustomersTransactionsData.csv', header = TRUE, na.strings = c("", "  ","  ","   ","?","#",".","NA"),stringsAsFactors = TRUE)
cust$DOB <- as.Date(cust$DOB)
tran$BillDate <-as.Date(tran$BillDate)

library(dplyr)
cust_tran <- left_join(cust, tran, by = "CustomerID" )
write.csv(cust_tran,"ExistingCustomerDetails.csv") #saving the merged file for convenience

# FEATURE ENGINEERING -----------------------------------------------------
library(eeptools)
cust_tran$age <- ceiling(eeptools::age_calc(cust_tran$DOB, enddate = Sys.Date(),units = "years",precise = TRUE)) # Age of the customer

library(lubridate)
cust_tran$tranYear <- lubridate::year(cust_tran$BillDate) # Year 
cust_tran$tranMonth <- lubridate::month(cust_tran$BillDate) # Month of the year
cust_tran$tranDate <- lubridate::day(cust_tran$BillDate) # day of the week
cust_tran$tranWeek <- lubridate::week(cust_tran$BillDate) # week of the Month
cust_tran$tranDay <- weekdays(cust_tran$BillDate) # Weekday names
cust_tran$revenue <- cust_tran$Sales*cust_tran$Quantity # Revenue generation
cust_tran$tranMonth <- as.integer(cust_tran$tranMonth) 

str(cust_tran)

write.csv(cust_tran,"ExistingCustomerDetailsFE.csv") # Feature derived file

#### Some more feature generations ###

dt <- tbl_df(cust_tran)

## Deriving frequency of the customer
whole <- group_by(dt, CustomerID)
frequency <- dplyr::summarize(whole, frequency = n())
frequency
cust <- left_join(cust,frequency, by = "CustomerID") # Left joining the freq column

## Deriving amount spent
whole1 <- group_by(dt, CustomerID,revenue)
amount <- dplyr::summarize(whole, Expenditure = sum(revenue)) ## renaming the amount to expenditure
amount
cust <- left_join(cust,amount, by = "CustomerID")## Left join the expenditure column

## Deriving year wise expenditures
##                  2017               ##
x <- dt %>% group_by(CustomerID,tranYear) %>% 
  select(CustomerID,revenue,tranYear) %>% 
  filter(tranYear == "2017") %>%
  dplyr::summarise(Exp_2017 = sum(revenue))  
x <- x[, c(1,3)] 
x <- data.frame(x)
str(x)
x$Exp_2017 <- as.numeric(x$Exp_2017)

cust <- left_join(cust,x, by = "CustomerID") ## joining Exp_2017
colSums(is.na(cust))
cust$Exp_2017[is.na(cust$Exp_2017)] <- 0

write.csv(cust,"Cust_Expend.csv")

##                 2016              ##
x <- dt %>% group_by(CustomerID,tranYear) %>% 
  select(CustomerID,revenue,tranYear) %>% 
  filter(tranYear == "2016") %>%
  dplyr::summarise(Exp_2016 = sum(revenue))  
x <- x[, c(1,3)]
x <- data.frame(x)
str(x)
x$Exp_2016 <- as.numeric(x$Exp_2016)

cust <- left_join(cust,x, by = "CustomerID") ## Left joining Exp_2016
colSums(is.na(cust))
cust$Exp_2016[is.na(cust$Exp_2016)] <- 0

write.csv(cust,"Cust_Expend.csv")

##                       2015                 ##
x <- dt %>% group_by(CustomerID,tranYear) %>% 
  select(CustomerID,revenue,tranYear) %>% 
  filter(tranYear == "2015") %>%
  dplyr::summarise(Exp_2015 = sum(revenue))  
x <- x[, c(1,3)]
x <- data.frame(x)
str(x)
x$Exp_2015 <- as.numeric(x$Exp_2015)

cust <- left_join(cust,x, by = "CustomerID") # left join Exp_2015
colSums(is.na(cust))
cust$Exp_2015[is.na(cust$Exp_2015)] <- 0

write.csv(cust,"Cust_Expend.csv")

##                2014                   ##
x <- dt %>% group_by(CustomerID,tranYear) %>% 
  select(CustomerID,revenue,tranYear) %>% 
  filter(tranYear == "2014") %>%
  dplyr::summarise(Exp_2014 = sum(revenue))  
x <- x[, c(1,3)]
x <- data.frame(x)
str(x)
x$Exp_2014 <- as.numeric(x$Exp_2014)

cust <- left_join(cust,x, by = "CustomerID")# Left join Exp_2014
colSums(is.na(cust))
cust$Exp_2014[is.na(cust$Exp_2014)] <- 0

write.csv(cust,"Cust_Expend.csv")

## Deriving Expenditure from each Product ctegory ##
x <- dt %>% group_by(CustomerID,ProductCategory) %>% 
  select(CustomerID,revenue,ProductCategory) %>% 
  filter(ProductCategory == "Office Supplies") %>%
  dplyr::summarise(Exp_Office = sum(revenue))  
x <- x[, c(1,3)]
x <- data.frame(x)
str(x)
x$Exp_Office <- as.numeric(x$Exp_Office)

cust <- left_join(cust,x, by = "CustomerID")# Left join Exp_Office column
colSums(is.na(cust))
cust$Exp_Office[is.na(cust$Exp_Office)] <- 0

write.csv(cust,"Cust_Expend.csv")
##                 Product Type -TEchnology    ##
x <- dt %>% group_by(CustomerID,ProductCategory) %>% 
  select(CustomerID,revenue,ProductCategory) %>% 
  filter(ProductCategory == "Technology") %>%
  dplyr::summarise(Exp_Technology = sum(revenue))  
x <- x[, c(1,3)]
x <- data.frame(x)
str(x)
x$Exp_Technology <- as.numeric(x$Exp_Technology)

cust <- left_join(cust,x, by = "CustomerID")## left join Exp_technology
colSums(is.na(cust))
cust$Exp_Technology[is.na(cust$Exp_Technology)] <- 0

write.csv(cust,"Cust_Expend.csv")

##              Product Type - Furniture    ##
x <- dt %>% group_by(CustomerID,ProductCategory) %>% 
  select(CustomerID,revenue,ProductCategory) %>% 
  filter(ProductCategory == "Furniture") %>%
  dplyr::summarise(Exp_Furniture = sum(revenue))  
x <- x[, c(1,3)]
x <- data.frame(x)
str(x)
x$Exp_Furniture <- as.numeric(x$Exp_Furniture)

cust <- left_join(cust,x, by = "CustomerID") # left join Exp-furniture
colSums(is.na(cust))
cust$Exp_Furniture[is.na(cust$Exp_Furniture)] <- 0

write.csv(cust,"Cust_Expend.csv")


## Feature derivation using sql 
library(sqldf)
## selecting Expenditure on fri ,sat, sun as weekend expenditures ##
x <- sqldf('select CustomerID,sum(revenue) as revenue from cust_tran where tranDay in("Friday","Saturday","Sunday") group by CustomerID')
x <- data.frame(x)
colnames(x)[2] <- "Exp_weekend" # Renaming the column
cust <- left_join(cust,x, by = "CustomerID") ## left join the column
colSums(is.na(cust))
cust$Exp_weekend[is.na(cust$Exp_weekend)] <- 0

write.csv(cust,"Cust_Expend.csv")

## Deriving expenditure on rest of the weekdays ##
x <- sqldf('select CustomerID,sum(revenue) as revenue from cust_tran where tranDay not in("Friday","Saturday","Sunday") group by CustomerID')
x <- data.frame(x)
colnames(x)[2] <- "Exp_weekdays" ## Rename the column
cust <- left_join(cust,x, by = "CustomerID")## Left join te column
colSums(is.na(cust))
cust$Exp_weekdays[is.na(cust$Exp_weekdays)] <- 0

write.csv(cust,"Cust_Expend.csv")


## Deriving the expenditure on starting of the month ##
x <- sqldf('select CustomerID,sum(revenue) as revenue from cust_tran where tranDate in(1,2,3,4,5,6,7) group by CustomerID')
x <- data.frame(x)
colnames(x)[2] <- "Exp_MoStart"# Rename the column
cust <- left_join(cust,x, by = "CustomerID")# left join the column
colSums(is.na(cust))
cust$Exp_MoStart[is.na(cust$Exp_MoStart)] <- 0

write.csv(cust,"Cust_Expend.csv")

# Deriving the expenditure on month ending ##
x <- sqldf('select CustomerID,sum(revenue) as revenue from cust_tran where tranDate in(31,30,29,28,25,26,27) group by CustomerID')
x <- data.frame(x)
colnames(x)[2] <- "Exp_MoEnd"# Rename the column
cust <- left_join(cust,x, by = "CustomerID")# left join the column
colSums(is.na(cust))
cust$Exp_MoEnd[is.na(cust$Exp_MoEnd)] <- 0

write.csv(cust,"Cust_Expend.csv")


library(zoo)
## Deriving the service age of a customer #
x <- sqldf('select CustomerID,MIN(BillDate) as minBillDate,MAX(BillDate) as maxBillDate from cust_tran group by CustomerID',method = "name__class")
x$minBillDate <- zoo::as.Date(x$minBillDate)# min bill date
x$maxBillDate <- zoo::as.Date(x$maxBillDate)# max bill date
x$serviceAge <- difftime(x$maxBillDate,x$minBillDate,units = "days") # Differencing the columns 
x <- x[,c(1,4)]
x <- data.frame(x)
cust <- left_join(cust,x, by = "CustomerID")## Left join the columns 
colSums(is.na(cust))
cust$serviceAge[is.na(cust$serviceAge)] <- 0

write.csv(cust,"Cust_Expend.csv")

cust$serviceAge <- as.numeric(cust$serviceAge)
cust$avgRevPerDay <- cust$Expenditure/cust$serviceAge # Avg Revenue generated  per day

write.csv(cust,"Cust_Expend.csv")

summary(tran$Sales)
## Deriving features based on expenditure 
cust_tran$ProductType <- ifelse(cust_tran$Sales>5000,"VExp",
                                ifelse(cust_tran$Sales>2500,"ModExp",
                                       ifelse(cust_tran$Sales>1000,"Exp",
                                              ifelse(cust_tran$Sales>500,"Nominal",
                                                     ifelse(cust_tran$Sales>100, "Economical","Budget")))))
## Exp > 5000 as very expensive
x <- sqldf('select CustomerID,sum(revenue) as revenue from cust_tran where ProductType="VExp" group by CustomerID')
x <- data.frame(x)
colnames(x)[2] <- "Exp_VExp" # rename the column
cust <- left_join(cust,x, by = "CustomerID") # left join the column
colSums(is.na(cust))
cust$Exp_VExp[is.na(cust$Exp_VExp)] <- 0

write.csv(cust,"Cust_Expend.csv")
## exp > 1000 and < 2500
x <- sqldf('select CustomerID,sum(revenue) as Exp_Exp from cust_tran where ProductType="Exp" group by CustomerID')
x <- data.frame(x)
x$Exp_Exp <- as.numeric(x$Exp_Exp) 
cust <- left_join(cust,x, by = "CustomerID") # left join the column
colSums(is.na(cust))
cust$Exp_Exp[is.na(cust$Exp_Exp)] <- 0

write.csv(cust,"Cust_Expend.csv")

cust$AvgPurchasePerVisit <- cust$Expenditure/cust$frequency # avg purchase per visit 
cust$AvgFrequencyInYear <- floor(cust$frequency/(cust$serviceAge/365)) # Freq in year 

## Exp <500 as budget
x <- sqldf('select CustomerID,sum(revenue) as Exp_Bud from cust_tran where ProductType="Budget" group by CustomerID')
x <- data.frame(x)
cust <- left_join(cust,x, by = "CustomerID")# left join the column
colSums(is.na(cust))
cust$Exp_Bud[is.na(cust$Exp_Bud)] <- 0

write.csv(cust,"Cust_Expend.csv")
# DATA CLEANING -----------------------------------------------------------

# Dealing with infinite values in columns 
sapply(cust, is.infinite)
is.na(cust)<-sapply(cust, is.infinite)
cust[is.na(cust)]<-0
#write.csv(cust,'Vdata.csv')
## Dummification of data ## 
df <- cust
str(df)
library(dummies)
rownames(df) <- df$CustomerID
df$CustomerID <- NULL
gender <- dummy(df$gender)
Mstat <- dummy(df$MaritalStatus)
df <- cbind(df, gender, Mstat)
df$gender <- NULL
df$MaritalStatus <- NULL
df$DOB <- NULL
#str(df)
## data Scale and split ##
set.seed(102188)
rdf <- data.frame(scale(df)) # scaling the data 

##                      CLUSTERING                 ##
# K-Means with Manhattan Distance and k=3 
d <- dist(rdf,method = "manhattan") # Distance 
# Hierarchical clustering using Ward's method
fit <- hclust(d, method="ward.D2")
plot(fit) # display dendogram
rect.hclust(fit, k=5, border="red") 

# K-means ,5 clusters
kfit<-kmeans(rdf,5)
aggregate(rdf,by=list(kfit$cluster),FUN=mean)
rdf <- data.frame(rdf,kfit$cluster)

# K-means:  Determine number of clusters by considering the withinness measure
wss <- 0
for (i in 1:15) {
  wss[i] <- sum(kmeans(rdf,centers=i)$withinss)
}

# Ploting the within sum of square error for different clusters
plot(1:15, wss, 
     type="b", 
     xlab="Number of Clusters",
     ylab="Within groups sum of squares") 

## K-means on whole 
kfit<-kmeans(rdf,5)
kfit1<-kmeans(rdf[1:2141,],5)
a<-kfit$cluster
b<-kfit1$cluster
table(a)
table(b)


## Checking clusters stability ##
library(fossil)
rand.index(a[1:2141],b)
library(mclust)
adjustedRandIndex(a[1:2141],b)

## Cluster plot 
library(factoextra)
fviz_cluster(kfit, data = rdf)



## Random-Forst  using caret just for further feature selection ##
#Splitting the data # 
set.seed(115288)
rows<-seq(1,nrow(rdf),1)
trainrows<-sample(rows,0.7*nrow(rdf))
train<-rdf[trainrows,]
test<-rdf[-trainrows,]
str(test)
str(train)

train$kfit.cluster<- as.factor(train$kfit.cluster)
test$kfit.cluster<- as.factor(test$kfit.cluster)

rfTC<- trainControl(method = 'repeatedcv',number = 10,repeats = 5)
mtry<- sqrt(ncol(train)-1)
rfgrid<- expand.grid(.mtry=mtry)
rftrain<- train(kfit.cluster~.,train,method='rf',trControl=rfTC,tuneGrid=rfgrid)
## Predictions on train and test ##
rfPreds<- predict(rftrain,train)
table(rfPreds)
table(train$kfit.cluster)
rfCM<- confusionMatrix(rfPreds,train$kfit.cluster)
rfCM

rfPredsT<- predict(rftrain,test)
rfCMT<- confusionMatrix(rfPredsT,test$kfit.cluster)
rfCMT

## Feature selection using importance ##
rfImp<-data.frame(rftrain[['finalModel']][['importance']])
rfImp$features<-rownames(rfImp)
rfImp<- rfImp[rev(order(rfImp$MeanDecreaseGini)),]

## selecting features from importance tble
ydata<-head(rfImp,n = 12)
rdf1<- rdf[,colnames(rdf)%in% ydata$features]

##     CLUSTERING ON FEATURE SELECTED DATA     ##

d1 <- dist(rdf1,method = "euclidean") # Distance 
# Hierarchical clustering using Ward's method
fit1 <- hclust(d1, method="ward.D2")
plot(fit1) # display dendogram
rect.hclust(fit1, k=3, border="red") 

# K-means ,5 clusters
featkfit<-kmeans(rdf1,3)
aggregate(rdf1,by=list(featkfit$cluster),FUN=mean)
rdf1 <- data.frame(rdf1,featkfit$cluster)

# K-means:  Determine number of clusters by considering the withinness measure
wss <- 0
for (i in 1:15) {
  wss[i] <- sum(kmeans(rdf1,centers=i)$withinss)
}

# Ploting the within sum of square error for different clusters
plot(1:15, wss, 
     type="b", 
     xlab="Number of Clusters",
     ylab="Within groups sum of squares") 

kfit<-kmeans(rdf1,3)
kfit1<-kmeans(rdf1[1:2141,],3)
a<-kfit$cluster
b<-kfit1$cluster
table(a)
table(b)


## Checking stability for feature selected data  ##
library(fossil)
rand.index(a[1:2141],b)
library(mclust)
adjustedRandIndex(a[1:2141],b)

library(factoextra)
fviz_cluster(featkfit, data = rdf1)
#write.csv(rdf1,'3Clusters917.csv')





##                Reading New Data             ##
## we perform the same operations as above on the new customers data 

newcust <- read.csv('NewCustomerDemographics_Segmentation.csv', header = TRUE, na.strings = c("", "  ","  ","   ","?","#",".","NA"),stringsAsFactors = TRUE)
newtran <- read.csv('NewCustomerTransactions_Segmentation.csv', header = TRUE, na.strings = c("", "  ","  ","   ","?","#",".","NA"),stringsAsFactors = TRUE)
newcust$DOB <- as.Date(newcust$DOB)
newtran$BillDate <-as.Date(newtran$BillDate)


new_cust_tran <- left_join(newcust, newtran, by = "CustomerID" )

write.csv(new_cust_tran,"newCustomerDetails.csv") #saving the merged file for convenience

new_cust_tran$age <- ceiling(eeptools::age_calc(new_cust_tran$DOB, enddate = Sys.Date(),units = "years",precise = TRUE)) # Age of the customer

library(lubridate)
new_cust_tran$tranYear <- lubridate::year(new_cust_tran$BillDate) # Year 
new_cust_tran$tranMonth <- lubridate::month(new_cust_tran$BillDate) # Month of the year
new_cust_tran$tranDate <- lubridate::day(new_cust_tran$BillDate) # day of the week
new_cust_tran$tranWeek <- lubridate::week(new_cust_tran$BillDate) # week of the Month
new_cust_tran$tranDay <- weekdays(new_cust_tran$BillDate) # Weekday names
new_cust_tran$revenue <- new_cust_tran$Sales*new_cust_tran$Quantity # Revenue generation
new_cust_tran$tranMonth <- as.integer(new_cust_tran$tranMonth) 

str(new_cust_tran)

write.csv(cust_tran,"newCustomerDetailsFE.csv") # Feature derived file

dt <- tbl_df(new_cust_tran)

## Deriving frequency of the customer
whole <- group_by(dt, CustomerID)
frequency <- dplyr::summarize(whole, frequency = n())
frequency
newcust <- left_join(newcust,frequency, by = "CustomerID") # Left joining the freq column

## Deriving amount spent
whole1 <- group_by(dt, CustomerID,revenue)
amount <- dplyr::summarize(whole, Expenditure = sum(revenue)) ## renaming the amount to expenditure
amount
newcust <- left_join(newcust,amount, by = "CustomerID")## Left join the expenditure column

x <- dt %>% group_by(CustomerID,tranYear) %>% 
  select(CustomerID,revenue,tranYear) %>% 
  filter(tranYear == "2017") %>%
  dplyr::summarise(Exp_2017 = sum(revenue))  
x <- x[, c(1,3)] 
x <- data.frame(x)
str(x)
x$Exp_2017 <- as.numeric(x$Exp_2017)

newcust <- left_join(newcust,x, by = "CustomerID") ## joining Exp_2017
colSums(is.na(newcust))
newcust$Exp_2017[is.na(newcust$Exp_2017)] <- 0

write.csv(cust,"NewCust_Expend.csv")

##                 2016              ##
x <- dt %>% group_by(CustomerID,tranYear) %>% 
  select(CustomerID,revenue,tranYear) %>% 
  filter(tranYear == "2016") %>%
  dplyr::summarise(Exp_2016 = sum(revenue))  
x <- x[, c(1,3)]
x <- data.frame(x)
str(x)
x$Exp_2016 <- as.numeric(x$Exp_2016)

newcust <- left_join(newcust,x, by = "CustomerID") ## Left joining Exp_2016
colSums(is.na(newcust))
newcust$Exp_2016[is.na(newcust$Exp_2016)] <- 0

write.csv(cust,"Cust_Expend.csv")

##                       2015                 ##
x <- dt %>% group_by(CustomerID,tranYear) %>% 
  select(CustomerID,revenue,tranYear) %>% 
  filter(tranYear == "2015") %>%
  dplyr::summarise(Exp_2015 = sum(revenue))  
x <- x[, c(1,3)]
x <- data.frame(x)
str(x)
x$Exp_2015 <- as.numeric(x$Exp_2015)

newcust <- left_join(newcust,x, by = "CustomerID") # left join Exp_2015
colSums(is.na(newcust))
newcust$Exp_2015[is.na(newcust$Exp_2015)] <- 0

write.csv(cust,"Cust_Expend.csv")

##                2014                   ##
x <- dt %>% group_by(CustomerID,tranYear) %>% 
  select(CustomerID,revenue,tranYear) %>% 
  filter(tranYear == "2014") %>%
  dplyr::summarise(Exp_2014 = sum(revenue))  
x <- x[, c(1,3)]
x <- data.frame(x)
str(x)
x$Exp_2014 <- as.numeric(x$Exp_2014)

newcust <- left_join(newcust,x, by = "CustomerID")# Left join Exp_2014
colSums(is.na(newcust))
newcust$Exp_2014[is.na(newcust$Exp_2014)] <- 0

write.csv(cust,"Cust_Expend.csv")


## Deriving Expenditure from each Product ctegory ##
x <- dt %>% group_by(CustomerID,ProductCategory) %>% 
  select(CustomerID,revenue,ProductCategory) %>% 
  filter(ProductCategory == "Office Supplies") %>%
  dplyr::summarise(Exp_Office = sum(revenue))  
x <- x[, c(1,3)]
x <- data.frame(x)
str(x)
x$Exp_Office <- as.numeric(x$Exp_Office)

newcust <- left_join(newcust,x, by = "CustomerID")# Left join Exp_Office column
colSums(is.na(newcust))
newcust$Exp_Office[is.na(newcust$Exp_Office)] <- 0

write.csv(cust,"Cust_Expend.csv")
##                 Product Type -TEchnology    ##
x <- dt %>% group_by(CustomerID,ProductCategory) %>% 
  select(CustomerID,revenue,ProductCategory) %>% 
  filter(ProductCategory == "Technology") %>%
  dplyr::summarise(Exp_Technology = sum(revenue))  
x <- x[, c(1,3)]
x <- data.frame(x)
str(x)
x$Exp_Technology <- as.numeric(x$Exp_Technology)

newcust <- left_join(newcust,x, by = "CustomerID")## left join Exp_technology
colSums(is.na(newcust))
newcust$Exp_Technology[is.na(newcust$Exp_Technology)] <- 0

write.csv(cust,"Cust_Expend.csv")

##              Product Type - Furniture    ##
x <- dt %>% group_by(CustomerID,ProductCategory) %>% 
  select(CustomerID,revenue,ProductCategory) %>% 
  filter(ProductCategory == "Furniture") %>%
  dplyr::summarise(Exp_Furniture = sum(revenue))  
x <- x[, c(1,3)]
x <- data.frame(x)
str(x)
x$Exp_Furniture <- as.numeric(x$Exp_Furniture)

newcust <- left_join(newcust,x, by = "CustomerID") # left join Exp-furniture
colSums(is.na(newcust))
newcust$Exp_Furniture[is.na(newcust$Exp_Furniture)] <- 0

write.csv(cust,"Cust_Expend.csv")

## Feature derivation using sql 
library(sqldf)
## selecting Expenditure on fri ,sat, sun as weekend expenditures ##
x <- sqldf('select CustomerID,sum(revenue) as revenue from new_cust_tran where tranDay in("Friday","Saturday","Sunday") group by CustomerID')
x <- data.frame(x)
colnames(x)[2] <- "Exp_weekend" # Renaming the column
newcust <- left_join(newcust,x, by = "CustomerID") ## left join the column
colSums(is.na(newcust))
newcust$Exp_weekend[is.na(newcust$Exp_weekend)] <- 0

write.csv(cust,"Cust_Expend.csv")

## Deriving expenditure on rest of the weekdays ##
x <- sqldf('select CustomerID,sum(revenue) as revenue from new_cust_tran where tranDay not in("Friday","Saturday","Sunday") group by CustomerID')
x <- data.frame(x)
colnames(x)[2] <- "Exp_weekdays" ## Rename the column
newcust <- left_join(newcust,x, by = "CustomerID")## Left join te column
colSums(is.na(newcust))
newcust$Exp_weekdays[is.na(newcust$Exp_weekdays)] <- 0

write.csv(cust,"Cust_Expend.csv")


## Deriving the expenditure on starting of the month ##
x <- sqldf('select CustomerID,sum(revenue) as revenue from new_cust_tran where tranDate in(1,2,3,4,5,6,7) group by CustomerID')
x <- data.frame(x)
colnames(x)[2] <- "Exp_MoStart"# Rename the column
newcust <- left_join(newcust,x, by = "CustomerID")# left join the column
colSums(is.na(newcust))
newcust$Exp_MoStart[is.na(newcust$Exp_MoStart)] <- 0

write.csv(cust,"Cust_Expend.csv")

# Deriving the expenditure on month ending ##
x <- sqldf('select CustomerID,sum(revenue) as revenue from new_cust_tran where tranDate in(31,30,29,28,25,26,27) group by CustomerID')
x <- data.frame(x)
colnames(x)[2] <- "Exp_MoEnd"# Rename the column
newcust <- left_join(newcust,x, by = "CustomerID")# left join the column
colSums(is.na(newcust))
newcust$Exp_MoEnd[is.na(newcust$Exp_MoEnd)] <- 0

write.csv(cust,"Cust_Expend.csv")


## Deriving the service age of a customer #
x <- sqldf('select CustomerID,MIN(BillDate) as minBillDate,MAX(BillDate) as maxBillDate from new_cust_tran group by CustomerID',method = "name__class")
x$minBillDate <- zoo::as.Date(x$minBillDate)# min bill date
x$maxBillDate <- zoo::as.Date(x$maxBillDate)# max bill date
x$serviceAge <- difftime(x$maxBillDate,x$minBillDate,units = "days") # Differencing the columns 
x <- x[,c(1,4)]
x <- data.frame(x)
newcust <- left_join(newcust,x, by = "CustomerID")## Left join the columns 
colSums(is.na(newcust))
newcust$serviceAge[is.na(newcust$serviceAge)] <- 0

write.csv(cust,"Cust_Expend.csv")

newcust$serviceAge <- as.numeric(newcust$serviceAge)
newcust$avgRevPerDay <- newcust$Expenditure/newcust$serviceAge # Avg Revenue generated  per day

write.csv(cust,"Cust_Expend.csv")

summary(newtran$Sales)
## Deriving features based on expenditure 
new_cust_tran$ProductType <- ifelse(new_cust_tran$Sales>5000,"VExp",
                                ifelse(new_cust_tran$Sales>2500,"ModExp",
                                       ifelse(new_cust_tran$Sales>1000,"Exp",
                                              ifelse(new_cust_tran$Sales>500,"Nominal",
                                                     ifelse(new_cust_tran$Sales>100, "Economical","Budget")))))
## Exp > 5000 as very expensive
x <- sqldf('select CustomerID,sum(revenue) as revenue from new_cust_tran where ProductType="VExp" group by CustomerID')
x <- data.frame(x)
colnames(x)[2] <- "Exp_VExp" # rename the column
newcust <- left_join(newcust,x, by = "CustomerID") # left join the column
colSums(is.na(newcust))
newcust$Exp_VExp[is.na(newcust$Exp_VExp)] <- 0

write.csv(cust,"Cust_Expend.csv")
## exp > 1000 and < 2500
x <- sqldf('select CustomerID,sum(revenue) as Exp_Exp from new_cust_tran where ProductType="Exp" group by CustomerID')
x <- data.frame(x)
x$Exp_Exp <- as.numeric(x$Exp_Exp) 
newcust <- left_join(newcust,x, by = "CustomerID") # left join the column
colSums(is.na(newcust))
newcust$Exp_Exp[is.na(newcust$Exp_Exp)] <- 0

write.csv(cust,"Cust_Expend.csv")

newcust$AvgPurchasePerVisit <- newcust$Expenditure/newcust$frequency # avg purchase per visit 
newcust$AvgFrequencyInYear <- floor(newcust$frequency/(newcust$serviceAge/365)) # Freq in year 

## Exp <500 as budget
x <- sqldf('select CustomerID,sum(revenue) as Exp_Bud from new_cust_tran where ProductType="Budget" group by CustomerID')
x <- data.frame(x)
newcust <- left_join(newcust,x, by = "CustomerID")# left join the column
colSums(is.na(newcust))
newcust$Exp_Bud[is.na(newcust$Exp_Bud)] <- 0

write.csv(cust,"Cust_Expend.csv")

sapply(newcust, is.infinite)
is.na(newcust)<-sapply(newcust, is.infinite)
newcust[is.na(newcust)]<-0

## Dummification of data ## 
ndf <- newcust
str(ndf)
library(dummies)
rownames(ndf) <- ndf$CustomerID
ndf$CustomerID <- NULL
ngender <- dummy(ndf$gender)
nMstat <- dummy(ndf$MaritalStatus)
ndf <- cbind(ndf, ngender, nMstat)
ndf$gender <- NULL
ndf$MaritalStatus <- NULL
ndf$DOB <- NULL
set.seed(142988)
sdf <- data.frame(scale(ndf)) # scaling the data 

## We predict the clusters based on best model on new data ##
a2<- colnames(rdf1)

a2 <- a2[1:12]
sdf_rf<- sdf[,a2]


library(flexclust)
a1 = as.kcca(featkfit, data = rdf1[,1:12])

pred <- predict(a1, sdf_rf)

sdf_rf <- data.frame(sdf_rf, pred)
#write.csv(sdf_rf,'NewCustData.csv')

