getwd()
setwd("D:\\R")
disease
d <- read.csv("Widget sales - Assignment 30_2.csv", na.strings = c(""," ","NA"))
d<-d[!duplicated(d),]
library(Amelia)
missmap(d)
sapply(d,function(x) sum(is.na(x)))
x=na.omit(d)
d<-d[!names(d) %in% c("X")]
d
prop.table(table(d$raw_timestamp_part_1))
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
is_outlier
dput(colnames(d))
meanvar=c( "gyros_forearm_x", "gyros_forearm_y", "gyros_forearm_x")
library(corrplot)

corrplot(cor(d[,names(d)%in% meanvar]),type="full",order="hclust",tl.cex = 1,tl.col = "Black",addrect = 8,method = "circle")
dput(colnames(disease))
SEvar=c( "accel_forearm_x", "accel_forearm_y", "accel_forearm_z")
library(corrplot)

corrplot(cor(d[,names(d)%in% SEvar]),type="full",order="hclust",tl.cex = 1,tl.col = "Black",addrect = 8,method = "circle")
worsevar=c(  "magnet_forearm_x", "magnet_forearm_y", "magnet_forearm_y")
library(corrplot)

corrplot(cor(d[,names(d)%in% worsevar]),type="full",order="AOE",tl.cex = 1,tl.col = "Black",addrect = 8,method = "color",addCoef.col = "gray")
library("caret")
library("dplyr")
#Remove Highly correlated 
d1=d %>% select(-findCorrelation(cor(d %>% select(-id,-raw_timestamp_part_1)),cutoff=.8))
d1

library("caret")
set.seed(123)
df3=cbind(raw_timestamp_part_1=d$raw_timestamp_part_1,d1)
index<-createDataPartition(df3$raw_timestamp_part_1,times = 1,p = .9,list=FALSE)
training=df3[index,]
testing=df3[-index,]

#Random Forest
library("rpart")
library("caret")
set.seed(1234)
df3=cbind(raw_timestamp_part_1=d$raw_timestamp_part_1,d1)
index<-createDataPartition(df3$raw_timestamp_part_1,times = 1,p = .9,list=FALSE)
training=df3[index,]
testing=df3[-index,]
library("caret")
model_dt=train(raw_timestamp_part_1 ~ .,data=training,method="rf")
model_dt_1 = predict(model_dt, data = training)
cm<-confusionMatrix(model_dt_1,testing$raw_timestamp_part_1,positive = "M")
cm
