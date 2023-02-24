library(corrplot)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(caret)
library(leaps)
library(MASS)
library(car)
#load the package we required
setwd("~/Desktop/module1")

data<-read.csv("BodyFat.csv",header = T)
summary(data)

pic1<-ggplot(data,aes(x=DENSITY,y=BODYFAT))+geom_point(shape=21)+xlab("DENSITY(gm/cm^3)")+ylab("BODYFAT(%)")+geom_point(aes(x=1.0991,y=17.3),colour="red")+geom_point(aes(x=1.0665,y=6.4),colour="red")+ 
  geom_point(aes(x=1.0666,y=18.3),colour="red")+ annotate("text",x=1.0991,y=18.5,label="96")+ annotate("text",x=1.0665,y=7.8,label="48")+annotate("text",x=1.0666,y=19.4,label="76")
print(pic1)

find_outlier<-function(a,b){
  dif<-((495/a-450)-b)^2
  return(order(dif,decreasing = T)[1:3])
}
find_outlier(data$DENSITY,data$BODYFAT)
data[c(96,48,76),]
data[48,2]<-14.14
data[76,2]<-14.09

data<-data[,-c(1,3)]#delete the index column and the density column, because we don't need them when building our model

#firstly,we build a linear model to check if there is any outliers which need to be eliminated
m1<-lm(BODYFAT~.,data)#firstly,we build a linear model to check if there is any outliers which need to be eliminated
which.max(cooks.distance(m1))#check the most influence point
d1<-data.frame(index=as.numeric(1:252),cooks=as.numeric(cooks.distance(m1)))
pic2<-ggplot(d1,aes(x=index,y=cooks))+geom_point()+annotate("text",x=42,y=2,label="42")+geom_point(x=42,y=1.910141,colour="red")+
  xlab("INDO")+ylab("cook's distance")
print(pic2)#we find 42 is a high influence poin
#we find 42 is a high influence point

data[42,]#shortest guy
pic3<-ggplot(data,aes(x=1:252,y=HEIGHT))+geom_point()+xlab("INDO")+ylab("HEIGHT(inches)")#we think the error is made 
#because a typo from the plot because it seperates so far from others
print(pic3)
data<-data[-42,]#delete the shortest guy
m2<-lm(BODYFAT~.,data)
which.max(cooks.distance(m2))
d2<-data.frame(index=as.numeric(1:251),cooks=as.numeric(cooks.distance(m2)))
pic4<-ggplot(d2,aes(x=index,y=cooks))+geom_point()+annotate("text",x=39,y=0.36,label="39")+geom_point(x=39,y=0.3956833,colour="red")+geom_hline(yintercept=4/(251-15),lty=2)
print(pic4)
data[39,]#this guy is really very fat
data<-data[-39,]#delete it becasue this man is too fat
m3<-lm(BODYFAT~.,data)
which.max(cooks.distance(m3))
d3<-data.frame(index=as.numeric(1:250),cooks=as.numeric(cooks.distance(m3)))
pic3<-ggplot(d3,aes(x=index,y=cooks))+geom_point()+annotate("text",x=219,y=0.17,label="219")+geom_point(x=219,y=0.1580923,colour="red")+geom_hline(yintercept=4/(250-15),lty=2)
print(pic3)
data[219,]#we reatin it, because his values are all in the range, not extreme value
#Then we want to draw the distribution of each covariates which may be used in our model
p1<-qplot(data$AGE,bins = 30)+xlab("Age(years)")
p2<-qplot(data$WEIGHT,bins = 30)+xlab("Weight(lbs)")
p3<-qplot(data$HEIGHT,bins = 30)+xlab("Height(inches)")
p4<-qplot(data$ADIPOSITY,bins = 30)+xlab("Adioposity(bmi)")
p5<-qplot(data$NECK,bins = 30)+xlab("Neck(cm)")
p6<-qplot(data$CHEST,bins = 30)+xlab("Chest(cm)")
p7<-qplot(data$ABDOMEN,bins = 30)+xlab("Abdomen(cm)")
p8<-qplot(data$HIP,bins = 30)+xlab("Hip(cm)")
p9<-qplot(data$THIGH,bins = 30)+xlab("Thigh(cm)")
p10<-qplot(data$KNEE,bins = 30)+xlab("Knee(cm)")
p11<-qplot(data$ANKLE,bins = 30)+xlab("Ankle(cm)")
p12<-qplot(data$BICEPS,bins = 30)+xlab("Biceps(cm) ")
p13<-qplot(data$FOREARM,bins = 30)+xlab("Forearm(cm) ")
p14<-qplot(data$WRIST,bins = 30)+xlab("Wristcm)")
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,ncol=4)
which.max(data$WEIGHT)
#for age
data[which.max(data$AGE),]
which.max(data$AGE)#too old, we may delete it
#for hip
data[which.max(data$HIP),]#he is also the fatest person, but his hip is too big, and this will gain the 
#weight of hip in our model, which will make our model not robust
which.max(data$HIP)
#for ankle
data[data$ANKLE>30,]#They are not very fat, but has a fat ankle, which is abnormal
which(data$ANKLE>30)
data<-data[-c(31,40,77,84),]
BMI <- (0.45*data$WEIGHT)/((0.025*data$HEIGHT)^2)
diff <- BMI - data$ADIPOSITY
d <- data.frame(cbind(index = seq(nrow(data)), diff))
pic6<-ggplot(d, aes(index,diff, label = index)) + geom_point() + xlab('INDEX') + ylab('BMI Difference') + theme(text = element_text(size = 7), element_line(size = 0.1), plot.title = element_text(hjust = 0.5))+ 
  geom_text(aes(label = ifelse(abs(diff) > 2, as.character(index),''), hjust = -0.5, vjust = 0.5), size = 3)
print(pic6)
data<-data[-c(157,215),]
dim(data)#after data cleaning process, w ehave 244 observations in total, and then we will divide them into 5 fold using 
#stratified sampling, and make the size of each fold as equal as possible
data1<-data%>%arrange(BODYFAT)
data1$index=1:244
d1<-data1[which(data1$index%%5==1),]#49*16
d2<-data1[which(data1$index%%5==2),]#49*16
d3<-data1[which(data1$index%%5==3),]#49*16
d4<-data1[which(data1$index%%5==4),]#49*16
d5<-data1[which(data1$index%%5==0),]#48*16
d1<-d1[,-16]
d2<-d2[,-16]
d3<-d3[,-16]#delete index column
d4<-d4[,-16]
d5<-d5[,-16]
model_data<-rbind(d1,d2,d3,d4,d5)#combine them into one data
model_data<-model_data[,-5]

str(model_data)
set.seed(999)
train.control <- trainControl(method = "cv", number = 10)
back.model <- train(BODYFAT ~., data = model_data,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control
)
back.model$results
summary(back.model$finalModel)#height,chest,abdomen,wrist
for.model <- train(BODYFAT ~., data = model_data,
                   method = "leapForward", 
                   tuneGrid = data.frame(nvmax = 1:5),
                   trControl = train.control
)
for.model$results
summary(for.model$finalModel)#weight,abdomen,biceps,wrist
both.model <- train(BODYFAT ~., data = model_data,
                    method = "leapSeq", 
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control
)
both.model$results
summary(both.model$finalModel)#weight,abdomen
corrplot(cor(data[,-1]),addCoef.col = "grey")
mse_back<-rep(0,5)
mse_for<-rep(0,5)
mse_both<-rep(0,5)
for(i in 1:5){
  if(i==5){
    test_data<-model_data[197:244,]
    train_data<-model_data[-(197:244),]
  }else{
    test_data<-model_data[((49*i-48):(49*i)),]
    train_data<-model_data[-((49*i-48):(49*i)),]
  }
  model_back<-lm(BODYFAT~WEIGHT+FOREARM+ABDOMEN+WRIST,data=train_data)
  model_for<-lm(BODYFAT~WEIGHT+ABDOMEN,data=train_data)
  model_both<-lm(BODYFAT~WEIGHT+ABDOMEN+WRIST,data=train_data)
  pre_back<-predict(model_back,newdata=test_data)
  pre_for<-predict(model_for,newdata=test_data)
  pre_both<-predict(model_both,newdata=test_data)
  mse_back[i]=sum((pre_back-test_data$BODYFAT)^2)/(nrow(test_data))
  mse_for[i]=sum((pre_for-test_data$BODYFAT)^2)/(nrow(test_data))
  mse_both[i]=sum((pre_both-test_data$BODYFAT)^2)/(nrow(test_data))
}
mean(mse_back)
mean(mse_for)
mean(mse_both)
#so we will choose the backward model with DENSITY~HEIGHT+CHEST+ABDOMEN+WRIST
final_model<-lm(BODYFAT~WEIGHT+FOREARM+ABDOMEN+WRIST,data=model_data)
par(mfrow=c(1,2))
plot(final_model,which=2)
plot(residuals(final_model))
par(mfrow=c(1,1))
summary(final_model)
vif(final_model)
