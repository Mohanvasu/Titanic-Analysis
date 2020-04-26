train=read.csv(file.choose(),sep=',',header = TRUE)
View(train)
test=read.csv(file.choose(),sep=',',header=TRUE)
View(test)

str(train)

test=data.frame(survived=rep("None",nrow(test)),test[,])
View(test)
str(test)
library(tidyr)
library(dplyr)
test=rename(test,"Survived"=survived)
datacomb=rbind(train,test)
View(datacomb)

datacomb$Survived=as.factor(datacomb$Survived)
str(datacomb)
datacomb$Pclass=as.factor(datacomb$Pclass)

?table
table(datacomb$Survived)
table(datacomb$Pclass)

nrow(subset(datacomb,Pclass=="1"))

train$Pclass=as.factor(train$Pclass)
str(train)

library(ggplot2)
ggplot(train,aes(x=Pclass))+geom_bar(aes(fill=factor(Survived)))

train1=train
train1$Pclass=as.integer(train1$Pclass)
ggplot(train1,aes(x=Pclass))+geom_freqpoly(aes())
train$Pclass=as.integer(train$Pclass)
str(train1)

head(train$Name)
length(unique(datacomb$Name))
nrow(datacomb)

dupnames=datacomb[which(duplicated(as.character(datacomb$Name))),"Name"]
datacomb[which(datacomb$Name %in% dupnames),]

dup=subset(datacomb,duplicated(Name),Name)
s1=dup$Name

#duplicated function over here simply returns the redundant names present in our dataset column Name
subset(datacomb,Name %in% s1,)


#detecting pattern in the names
library(stringr)
p1=str_detect(datacomb$Name,"Miss")
y1=(datacomb[which(str_detect(datacomb$Name,"Miss.")),])
head(y1)


t1=(subset(datacomb,str_detect(Name,"Mr."),))
head(t1)

head(subset(datacomb,Sex=="male",))

head(datacomb[which(str_detect(datacomb$Name,"Miss.")),])
head(subset(datacomb,Sex=="male",))


findtitle=function(name)
{
  as.character(name)
  if(length(grep("Miss",name))>0){
    return ("Miss.")
  }else if(length(grep("Mrs.",name)>0)){
    return ("Mrs.")
  }else if(length(grep("Master.",name))>0){
    return ("Master.")
  }else if(length(grep("Mr.",name))>0){
    return ("Mr.")
  }else{
    return ("Others")
  }
}

title=NULL;
for(i in 1:nrow(datacomb)){
  title=c(title,findtitle(datacomb[i,"Name"]))
}
length(title)
datacomb$title=as.factor(title)

View(datacomb)
str(datacomb)

library(plotly)
pl1=ggplot(data=datacomb[1:891,],aes(x=title))+geom_bar(aes(fill=Survived))+facet_wrap(~Pclass)
ggplotly(pl1)


pl2=ggplot(data=datacomb[1:891,],aes(x=Sex))+geom_bar(aes(fill=Survived))+facet_wrap(~Pclass)
ggplotly(pl2)


#Analysis of survival rate on the basis of pclass,sex,age

pl3=ggplot(datacomb[1:891,],aes(x=Age,fill=Survived))+facet_wrap(~Sex+Pclass)+geom_histogram(binwidth = 4)
ggplotly(pl3)

#finding the boys details
boys=(subset(datacomb,title=="Master."))
summary(boys$Age)

library(stringr)
#finding the girls 
girls=subset(datacomb,str_detect(Name,"Miss."),)
summary(girls$Age)

View(datacomb)
j1=subset(datacomb,title=="Master.",select = Age)
head(j1)


miss=datacomb[which(datacomb$title=="Miss."),]
summary(miss$Age)
miss$Age=as.integer(miss$Age)

ggplot(miss[miss$Survived !="None",],aes(x=Age,fill=Survived))+facet_wrap(~Pclass)+geom_histogram(binwidth = 5)

str(miss)


#we will now find the girls who are travelling alone

View(miss)
miss_alone=miss[which(miss$Parch==0 & miss$SibSp==0),]
View(miss_alone)

str(miss_alone)
#girl child who are travelling alone
gchild=miss_alone[which(miss_alone$Age<=14),]
gchild$Age

summary(miss_alone$Age)
nrow(gchild)

length(unique(datacomb$SibSp))
datacomb$SibSp=factor(datacomb$SibSp)

library(plotly)
pl4=ggplot(datacomb[1:891,],aes(x=SibSp))+facet_wrap(~title+Pclass)+geom_bar(aes(fill=Survived),width = 1)

pl5=pl4+ggtitle("Siblings on the ship")+xlab("Siblings/Spouses")+ylab("Count")

ggplotly(pl5)
#survival rates based upon the number of siblings or spouses
datacomb$Parch=as.factor(datacomb$Parch)
pl1=ggplot(datacomb[1:891,],aes(x=Parch))+facet_wrap(~Pclass+title)+geom_bar(aes(fill=Survived),width = 0.5)+ggtitle("Parents/Children")
ggplotly(pl1)


str(test)
sib=c(train$SibSp,test$SibSp)
parch=c(train$Parch,test$Parch)
View(datacomb)
datacomb$family=NULL
datacomb$family=as.factor((sib+1)+parch)
amily=(sib+1)+parch
length(amily)
View(datacomb)
datacomb$family=NULL
datacomb$family=amily
datacomb$SibSp=sib

train$SibSp
str(datacomb)
datacomb$family=as.factor(datacomb$family)

#survival rate based upon the family members
ggplot(datacomb[1:891,],aes(x=family))+facet_wrap(~Pclass+title)+geom_bar(aes(fill=Survived),width = 0.5)


#Analysing the ticket column

datacomb$Ticket=as.character(datacomb$Ticket)
str(datacomb)

datacomb$ticketinfo=ifelse(datacomb$Ticket==""," ",substr(datacomb$Ticket,1,1))
unique(datacomb$ticketinfo)
datacomb$ticketinfo=as.factor(datacomb$ticketinfo)

ggplot(datacomb[1:891,],aes(x=ticketinfo))+geom_bar(aes(fill=Survived),width=0.9)
ggplot(datacomb[1:891,],aes(x=ticketinfo))+geom_bar(aes(fill=Survived),width=1)+facet_wrap(~Pclass)

head(subset(datacomb,Pclass==3,Fare))
head(datacomb[which(datacomb$Pclass==1),"Fare"])

#Analysing the fares
ggplot(datacomb,aes(x=Fare))+geom_histogram(binwidth = 5)+ylim(0,200)

#Let's check if fare has predictive power
pl7=ggplot(datacomb[1:891,],aes(x=Fare))+facet_wrap(~Pclass+title)+geom_bar(aes(fill=Survived),width = 5)
ggplotly(pl7)

#random forest package can handle at most 32 levels 
#generally we convert the variables to their datatypes simply for those having large number of levels

subset(datacomb,Pclass==3,Cabin)

datacomb[is.na(datacomb$Cabin),"Cabin"]<-"U"
subset(datacomb,Cabin==" ",)
View(datacomb)
subset(datacomb,Cabin=="U",)

any(is.na(datacomb$Cabin))
str(datacomb)
datacomb$Cabin=as.character(datacomb$Cabin)

#Let's check for the cabin
datacomb$firstcharcabin=as.factor(substr(datacomb$Cabin,1,1))
library(ggplot2)
ggplot(datacomb[1:891,],aes(x=firstcharcabin))+geom_bar(aes(fill=Survived))

#checking the predictive power of the cabin
pl9=ggplot(datacomb[1:891,],aes(x=firstcharcabin))+geom_bar(aes(fill=Survived))+facet_wrap(~Pclass)
ggplotly(pl9)

ggplot(datacomb[1:891,],aes(x=firstcharcabin))+geom_bar(aes(fill=Survived))+facet_wrap(~Pclass+title)

#Checking whether people which have multiple cabins on the ship have more chances of surviving or not ?
datacomb$Cabin
datacomb$multicabin=as.factor(ifelse(str_detect(datacomb$Cabin," "),"Y","N"))
#in the above lines we are simply creating new column and feeding the value based upon the people having multiple columns

ggplot(datacomb[1:891,],aes(x=multicabin))+geom_bar(aes(fill=Survived))+facet_wrap(~Pclass+title)


#Let's check does survival rate depends on where we got onboard on the ship
str(datacomb)
ggplot(datacomb[1:891,],aes(x=Embarked))+geom_bar(aes(fill=Survived))+facet_wrap(~Pclass+title)

#S:Southempton
#Q:Queenstown
#C:Cheombourg
ggplot(datacomb[1:891,],aes(x=Embarked))+geom_bar(aes(fill=Survived))+facet_wrap(~Pclass)

#exploratory data analysis (EDA) is an approach to analyzing data sets to summarize their main characteristics, 
#often with visual methods.

#Logistic regression is also known as l2 regularization
#Feature selection:It is one of the method which help us to select the variables which are important for buliding 
#the models as predictive purpose


#In scenarios with lots of of features exploratory modeling helps us to idenitfy high value candidate features
#In scenarios where we have done data analysis, exploratory modeling helps us provide additional insights/confrimation 

#King of exploratory model :Random Forest


#--------------------------------------------------------------------------
                        #Exploratory Modelling 
#--------------------------------------------------------------------------

install.packages("randomForest")
library(randomForest)

rftrain=datacomb[1:891,c("Pclass","title")]

rflabel=as.factor(train$Survived)

set.seed(1234)
rf1=randomForest(x=rftrain,y=rflabel,importance = TRUE,ntree = 1000)
print(rf1)

table(train$Survived)

#let's do the feature selection based on the model, we make the graphical representation view 

varImpPlot(rf1)
#basically the graph that we have plotted here of the random forest model. We can see that there are some dots 
#some variable on the y-axis 
#Conclusion: The more farther the variable dot is towards the right the more important it is in terms of predcition


#Let's do the prediction using one more variable as we have seen Sibsp is somewhat more important

rftrain1=datacomb[1:891,c("Pclass","title","Parch","SibSp")]

rflabel1=as.factor(train$Survived)

set.seed(1234)
rf2=randomForest(x=rftrain1,y=rflabel1,importance = TRUE,ntree = 1000)
print(rf2)
varImpPlot(rf2)


#feature Selection :Selecting the important variable based upon their productive power is called feature selection
#Let's have a quick note on how the impotance of the variable is measured
#We have MeandecreaseAccuracy on the X-axis which tells about how much decrease that variable can led to the overall
#accuracy of the model. This shows how much important that variable is 

#Since from the graph we can see that title has the farmost greater value which tells that the variable is very important 
#If we'll loose the variable then we will loose our accuracy in our model

rftrain2=datacomb[1:891,c("Pclass","title","family")]

rflabel2=as.factor(train$Survived)
#best and accurate model
set.seed(1234)
rf3=randomForest(x=rftrain2,y=rflabel2,importance = TRUE,ntree = 1000)
print(rf3)
varImpPlot(rf3)


rftrain3=datacomb[1:891,c("Pclass","title","family","Parch","SibSp")]

rflabel3=as.factor(train$Survived)

set.seed(1234)
rf4=randomForest(x=rftrain3,y=rflabel3,importance = TRUE,ntree = 1000)
print(rf4)
varImpPlot(rf4)


#Let's move to the cross-validation part
test_data=datacomb[892:1309,c("Pclass","title","family")]
predn=predict(rf3,test_data)
table(predn)

resultdf=data.frame(Passengerid=rep(892:1309),Survived=predn)
table(resultdf$Survived)
write.csv(resultdf,file="prediction_rf.csv",row.names = FALSE)


#Cross- validation: It basically means that how good our predcitive function is going to work with the test dataset

browseVignettes("ggplot2")
install.packages("kernSmooth")


installed.packages("caret")
library(caret)
install.packages("doSNOW")
library(doSNOW)

#Caret package in R:
#Functionalities: Some perprocessing(cleaning)[preprocess]
#Data splitting :createDataPartition,createResample,createTimeslices
#Traning and testing functions: Train and test
#Model comparison: Confusion Matrix

set.seed(2348)
cv.10folds=createMultiFolds(rflabel2,k=10,times=10)

#checking stratification
table(rflabel2)
table(rflabel2[cv.10folds[[33]]])
