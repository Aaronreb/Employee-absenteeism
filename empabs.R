rm(list=ls()) 

setwd("C:/Users/ARON/Desktop/edwisor projects/employee absenteesim")
getwd()

Load Libraries
# x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
#       "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')
# 
# install.packages(x)
# lapply(x, require, character.only = TRUE)
# rm(x)

library(xlsx)
library(rlang)
library(ggplot2)


data_original=read.xlsx("Absenteeism_at_work_Project (6).xlsx",sheetIndex = 1, header = TRUE)
data=data_original

########analysing dataset################################################

#let us sse the structure
str(data)

colnames(data)
dim(data)

class(data)


#no of unique values in each variables


apply(data, 2,function(x) length(table(x)))

# we can see that the number if ID's are 36, which means we have data of 36 employees

###understanding the unique values and depending on which converting the categorical interger into factor
unique(data$ID)
data$ID=as.factor(as.integer( as.character(data$ID)))

unique(data$Reason.for.absence)
#we see that there are 28 categories so we convert this num into factor
data$Reason.for.absence=as.factor(as.integer( data$Reason.for.absence))
#data$Month.of.absence[data$Reason.for.absence %in%"0"]= NA



unique(data$Month.of.absence)
#we see that there are 12 months so it should be categorised
data$Month.of.absence=as.factor(as.integer(data$Month.of.absence))
#data$Month.of.absence[data$Month.of.absence %in%"0"]= NA

unique(data$Day.of.the.week)
#we have 5 days given in week so it should be categorised
data$Day.of.the.week=as.factor(as.character(data$Day.of.the.week))

unique(data$Seasons)
#we have been given 4 seasons so we need to convert it into factor
data$Seasons=as.factor(as.character(data$Seasons))


unique(data$Transportation.expense)

unique(data$Distance.from.Residence.to.Work)

unique(data$Disciplinary.failure)
#we have two categories, so we need to convert it into categorical
data$Disciplinary.failure=as.factor(as.character(data$Disciplinary.failure))


unique(data$Education)
#we have four categories, so we need to convert it into categorical
data$Education=as.factor(as.character( data$Education))

unique(data$Son)
data$Son=as.factor(as.character(data$Son))



unique(data$Social.drinker)
#we have two categories, so we need to convert it into categorical
data$Social.drinker  =as.factor(as.character(data$Social.drinker))

unique(data$Social.smoker)
#we have two categories, so we need to convert it into categorical
data$Social.smoker=as.factor(as.character(data$Social.smoker))

unique(data$Pet)
#we have categories, so we need to convert it into categorical
data$Pet=as.factor(as.character(data$Pet))

unique(data$Absenteeism.time.in.hours)


unique(data$Work.load.Average.day)


#now the dtype has been changed, so lets look at the structure of the data

str(data)


# #####analysing data (univariate)##########################
# 
round(prop.table(table(data$Absenteeism.time.in.hours))*100,2)
#we see that the percentage of employee remaining absenteesim is more between 0-24 hours



library(ggplot2)
ggplot(data = data,aes(x =Absenteeism.time.in.hours))+
geom_bar() +  labs(y='', title = ' ')





boxplot(data$Absenteeism.time.in.hours~data$ID,xlab="id",ylab="hours",mail="emplyess remainig absent")
#we see that employee id 9 has been absent for most of the time

boxplot(data$Absenteeism.time.in.hours~data$ID,xlab="id",ylab="hours",mail="emplyess remainig absent")


ggplot(data=data, aes(x=Reason.for.absence, y=Absenteeism.time.in.hours)) + geom_bar(stat="Identity")


#bivariate

#let us check if the reason of absense cause any effect on the absenteesim using boxplot

library(ggplot2)
ggplot(data = data,aes(x =Reason.for.absence))+
  geom_bar() +  labs(y='', title = ' ')
ggplot(data=data, aes(x=Reason.for.absence, y=Absenteeism.time.in.hours)) + geom_bar(stat="Identity")
#13,19,23,28 are the main reasons for absenteesim


library(ggplot2)
ggplot(data = data,aes(x =Month.of.absence))+
  geom_bar() +  labs(y='', title = ' ')
boxplot(data$Absenteeism.time.in.hours~data$Month.of.absence,xlab="month",ylab="hours",mail="hours vs month")
#january has less absentee

library(ggplot2)
ggplot(data = data,aes(x =Day.of.the.week))+
  geom_bar() +  labs(y='', title = ' ')
boxplot(data$Absenteeism.time.in.hours~data$Day.of.the.week,xlab="day",ylab="hours",mail="hours vs day")
#day doesnt explain beacuse it is uniformly distributed


ggplot(data = data,aes(x =Seasons))+
  geom_bar() +  labs(y='', title = ' ')
  boxplot(data$Absenteeism.time.in.hours~data$Seasons,xlab="Seasons",ylab="hours",mail="hours vs Seasons")
#uniformly distributed


ggplot(data = data,aes(x =Disciplinary.failure))+
  geom_bar() +  labs(y='', title = ' ')
boxplot(data$Absenteeism.time.in.hours~data$Disciplinary.failure,xlab="Disciplinary.failure",ylab="hours",mail="hours vs df")
#we see that the disciplinary failure can cause 0 absenteesim

boxplot(data$Absenteeism.time.in.hours~data$Education,xlab="Education",ylab="hours",mail="hours vs Education")

boxplot(data$Absenteeism.time.in.hours~data$Son,xlab="Son",ylab="hours",mail="hours vs Son")


 boxplot(data$Absenteeism.time.in.hours~data$Social.drinker,xlab="Social.drinker",ylab="hours",mail="hours vs Social.drinker")

boxplot(data$Absenteeism.time.in.hours~data$Social.smoker,xlab="Social.smoker",ylab="hours",mail="hours vs Social.smoker")

plot(data$Transportation.expense,data$Absenteeism.time.in.hours)

plot(data$Distance.from.Residence.to.Work,data$Absenteeism.time.in.hours)

plot(data$Age,data$Absenteeism.time.in.hours)

plot(data$Service.time,data$Absenteeism.time.in.hours)
#service time 5-20 hrs has more absenteesim

plot(data$Hit.target,data$Absenteeism.time.in.hours)
#more number of abseentism has hit more target. so the employee who hit thier target before time might stay absent 

plot(data$Body.mass.index,data$Absenteeism.time.in.hours)

plot(data$Pet,data$Absenteeism.time.in.hours)

plot(data$Body.mass.index,data$Absenteeism.time.in.hours)











######EDA

#getting all numeric varaibles together
num_index = sapply(data, is.numeric)
num_data = data[,num_index]
num_col = colnames(num_data) #storing all the column name

#getting all categorical variables together

cat_ind=sapply(data, is.factor)
cat_data=data[,cat_ind]
cat_col= colnames(cat_data)

str(data)

num_col
cat_col


####### missing value analysis and outlier analysis##############


#checking missing value
apply(data,2,function(x){sum(is.na(x))})

library(DMwR)
library(lattice)
library(grid)

# missing_val = data.frame(apply(data,2,function(x){sum(is.na(x))}))
# missing_val$Columns = row.names(missing_val)
# names(missing_val)[1] =  "Missing_percentage"
# missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(data)) * 100
# missing_val = missing_val[order(-missing_val$Missing_percentage),]
# row.names(missing_val) = NULL



#data=knnImputation(data,k=3)

#let us first check outliers 

library(ggplot2)

    for (i in 1:length(num_col))
{
  assign(paste0("gn",i),
         ggplot(aes_string(y = (num_col[i]), x = 'Absenteeism.time.in.hours'),data = data) +
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="blue", fill = "skyblue",
                        outlier.shape=18,outlier.size=1, notch=FALSE) +
           labs(y=num_col[i],x="Absenteeism.time.in.hours")+
           ggtitle(paste("Box plot of absenteesim for",num_col[i])))
}

#gn1-gn11 are all the numerical columns

## Plotting plots together


gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)


gridExtra::grid.arrange(gn4,gn5,gn6,ncol=3)


gridExtra::grid.arrange(gn7,gn8,gn9,ncol=3)

gridExtra::grid.arrange(gn10,ncol=1)







#we see that some variables has got outliers let us remove them

# #Removing oulier by replacing with NA and then impute
for(i in num_col){
  print(i)
  outv = data[,i][data[,i] %in% boxplot.stats(data[,i])$out]
  print(length(outv))
  data[,i][data[,i] %in% outv] = NA
}
#
# #checking all the missing values
 library(DMwR)
  sum(is.na(data))
data = knnImputation(data, k=3)  #as it gives error so we going via mean or median

# let us check missing values left  
apply(data,2,function(x){sum(is.na(x))})
dim(data)
  

#######feature selection
  library(corrgram)
  
  
  corrgram(data[,num_index],
           order = F,  #we don't want to reorder
           upper.panel=panel.pie,
           lower.panel=panel.shade,
           text.panel=panel.txt,
           main = 'CORRELATION PLOT')
  #We can see var the highly corr related var in plot marked dark blue. 
  #Dark blue color means highly positive cor related
  # We se that service.time is highly correlated with age so we remove service time
  # Also, weight is highly correlated to body mass index so we remove weight
  
  
  
  ##---------anova ----------------------------------
  
  colnames(cat_data)
 
  
  #Anova test
  library("lsr")
  anova_test=aov(Absenteeism.time.in.hours~ID+Reason.for.absence+Month.of.absence+Day.of.the.week+Seasons+
                   Disciplinary.failure+Education+Son+Social.drinker+Social.smoker+Pet,data = data)

  
  summary(anova_test)
  
   ##-----------------Removing Highly Corelated and Independent var----------------------
  data = subset(data, select = -c(Weight,Day.of.the.week,Seasons,
                                    Disciplinary.failure,Education,Son,Social.drinker,Social.smoker,Pet))
  
 colnames(data)
 str(data)
  
  #####feature scaling
  
  #Checking Data of Continuous Variable
 num_index = sapply(data, is.numeric)
 num_data = data[,num_index]
 num_col = colnames(num_data)
  
  ################  Histogram   ##################
 qqnorm(data$Transportation.expense)
 hist(data$Transportation.expense)
 
 #normalization
 
  for (i in num_col){
   print(i)
   data[,i]=(data[,i]-min(data[,i]))/(max(data[,i]-min(data[,i])))
 }
   
  
  #Most of the data is uniformally distributed
 
  #Using data Standardization/Z-Score here
  # for(i in num_col){
  #   print(i)
  #   data[,i] = (data[,i] - mean(data[,i]))/sd(data[,i])
  # }
  

str(data)

############model development####

####decision tree########

library(MASS)
library(rpart)

train_index= sample(1:nrow(data),0.6*nrow(data))
train= data[train_index,]
test= data[-train_index,]

regression=rpart(Absenteeism.time.in.hours ~.,data=train,method="anova")

summary(regression)

reg_predict=predict(regression,test[,-12])


#evaluate
View(test[,12])

#install.packages("DMwR")

library(DMwR)
regr.eval(test[,12],reg_predict,stats = c("mae","mape","rmse"))

# rmse=0.19

####random forest######

library(randomForest)

rf_model= randomForest(Absenteeism.time.in.hours~.,train,importance=TRUE,ntree=100)

summary(rf_model)

rf_predict=predict(rf_model,test[,-12])


regr.eval(test[,12],rf_predict,stats = c("mae","mape","rmse"))

#rmse=0.17

####linear regression#######


library(usdm)


lm_model= lm(Absenteeism.time.in.hours~.,data=train)

summary(lm_model)

lm_predict=predict(lm_model,test[,-12])


regr.eval(test[,19],lm_predict,stats = c("mae","mape","rmse"))

#rmse=3.26








# ###################################################################################################
# ###             MODEL BUILDING USING K_FOLD
# ###################################################################################################

##Uing k-fold


library(caret)
library(data.table)
##We will use k-fold cross validation method in all the models to be trained below.
train_control <- trainControl(method="cv", number=5)




#####-----DECISION TREE-----#####

dt_model <- train(Absenteeism.time.in.hours~.,data=data,method="rpart",trControl=train_control)
plot(dt_model)
summary(dt_model)
print(dt_model)
##RMSE is used to select the optimal model using the smallest model .i.e. when cp=0.0801
#RMSE=0.1958891  --  Rsquared=0.1455299  --  MAE=0.1422224

#####-----RANDOM FOREST-----#####

tgrid=expand.grid(.mtry=c(3:8),.splitrule ="variance",.min.node.size = c(5,10,15,20))
rf_model <- train(Absenteeism.time.in.hours~.,data=data,method="ranger",trControl=train_control,tuneGrid=tgrid,num.tree=200,importance="permutation")
plot(rf_model)
print(rf_model)
#From the plot above we could see that the optimal parameters are mtry=4, min.node.size=5
#RMSE=0.1783666  --  Rsquared=0.2814073  --  MAE=0.1299943

#####-----LINEAR REGRESSION-----#####

lr_model <- train(Absenteeism.time.in.hours~.,data=data,method="lm",trControl=train_control)
print(lr_model)
summary(lr_model)
##The accuracy of linear regression model is as follows
#RMSE=0.18  --  Rsquared=0.27239244357  --  MAE=0.12

####How much losses every month can we project in 2011 if same trend of absenteeism continues


#2nd PART PREDICTION OF LOSS FOR THE COMPANY IN EACH
#absenty monthwise

#to find loss we require month of absense service time absententy hours and work load

lossdata = subset(data, select = c(Month.of.absence, Service.time, Absenteeism.time.in.hours,
                              Work.load.Average.day.))

#Work loss = ((Work load per day/ service time)* Absenteeism hours)

lossdata["loss"]=with(lossdata,((lossdata[,4]*lossdata[,3])/lossdata[,2]))
for(i in 1:12)
{
  di=lossdata[which(lossdata["Month.of.absence"]==i),]
  print(sum(di$loss))
  
}


View(lossdata)
