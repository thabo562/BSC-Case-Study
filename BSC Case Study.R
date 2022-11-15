library(tidyverse)
library(fpc)
library(tree)
library(gbm)
library(ggplot2)
library(dplyr)
library(randomForest)
library(caret)
library(hrbrthemes)

#read in data #####
data<- read.table("Bike_Buyer_Data_edited.txt", header = T, sep = ',', stringsAsFactors = T)
data<-data[,-14]

summary(data) # identify outliers and missing observations 
which(data$Cars==-1)
which(data$Education=="NULL")
which(data$Gender=="NULL")
which(data$Region=="NULL")
which(data$Income==250000)
which(data$Children==12)


data<-data[-c(74,247,329,272,447,1000),] # remove outliers and missing observations
data<- droplevels(data) # remove unused levels as factors
data$Cars<- as.integer(data$Cars)

set.seed(2022)
sample_split <- sample(1:nrow(data), nrow(data)*0.8) #80/20 split for training and test set. 
train <- data[sample_split,]
test <- data[-sample_split,]

# Exploratory Data analysis ########

pie(table(data$Purchased.Bike), col = c("red","blue"))
legend("topright", c("No,Yes"), cex=0.8)


pie(x, labels = piepercent, main = "City pie chart",fill = rainbow(length(x)))
legend("topright", c("London","New York","Singapore","Mumbai"), cex = 0.8,
       fill = rainbow(length(x)))
ggplot(data, aes(x=Income , fill=Purchased.Bike)) + 
  geom_histogram(position = "Dodge", binwidth = 1) +
  scale_fill_brewer(palette = "Set1")+
  theme(legend.title = element_text(size = 30), 
        legend.text = element_text(size = 30),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20))+
  ylab("Count")

data %>%
  ggplot( aes(x=Income , fill=Purchased.Bike)) +
  geom_histogram(color="#e9ecef", alpha=0.8)

ggplot(data, aes(x=Age , fill=Purchased.Bike)) + 
  geom_histogram(position = "Dodge", binwidth = 1) +
  scale_fill_brewer(palette = "Set1")+
  theme(legend.title = element_text(size = 30), 
        legend.text = element_text(size = 30),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20))+
  ylab("Count")

ggplot(data, aes(x=Children , fill=Purchased.Bike)) + 
  geom_histogram(position = "Dodge", binwidth = 1) +
  scale_fill_brewer(palette = "Set1")+
  theme(legend.title = element_text(size = 30), 
        legend.text = element_text(size = 30),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20))+
  ylab("Count")



ggplot(data, aes(x=Cars , fill=Purchased.Bike)) + 
  geom_histogram(position = "Dodge", binwidth = 1) +
  scale_fill_brewer(palette = "Set1")+
  theme(legend.title = element_text(size = 30), 
        legend.text = element_text(size = 30),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20))+
  ylab("Count")



ggplot(data, aes(x=Purchased.Bike , fill=Gender)) + 
  geom_bar(position = "Dodge") +
  scale_fill_brewer(palette = "Set1")+
  theme(legend.title = element_text(size = 30), 
        legend.text = element_text(size = 30),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20))+
  ylab("Count")
  
ggplot(data, aes(x=Purchased.Bike , fill=Home.Owner)) + 
  geom_bar(position = "Dodge") +
  scale_fill_brewer(palette = "Set1")+
  theme(legend.title = element_text(size = 30), 
        legend.text = element_text(size = 30),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20))+
  ylab("Count")

ggplot(data, aes(x=Purchased.Bike , fill=Marital.Status)) + 
  geom_bar(position = "Dodge") +
  scale_fill_brewer(palette = "Set1")+
  theme(legend.title = element_text(size = 30), 
        legend.text = element_text(size = 30),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20))+
  ylab("Count")


ggplot(data, aes(x=Purchased.Bike , fill=Region)) + 
  geom_bar(position = "Dodge") +
  scale_fill_brewer(palette = "Set1")+
  theme(legend.title = element_text(size = 30), 
        legend.text = element_text(size = 30),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20))+
  ylab("Count")

ggplot(data, aes(x=Purchased.Bike , fill=Commute.Distance)) + 
  geom_bar(position = "Dodge") +
  scale_fill_brewer(palette = "Set1")+
  theme(legend.title = element_text(size = 30), 
        legend.text = element_text(size = 30),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20))+
  ylab("Count")

ggplot(data, aes(x=Purchased.Bike , fill=Education)) + 
  geom_bar(position = "Dodge") +
  scale_fill_brewer(palette = "Set1")+
  theme(legend.title = element_text(size = 30), 
        legend.text = element_text(size = 30),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20))+
  ylab("Count")

ggplot(data, aes(x=Purchased.Bike , fill=Occupation)) + 
  geom_bar(position = "Dodge") +
  scale_fill_brewer(palette = "Set1")+
  theme(legend.title = element_text(size = 30), 
        legend.text = element_text(size = 30),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20))+
  ylab("Count")

#### GLM##############

train_rescaled <- train[,-1] %>%
  mutate_if(is.numeric, funs(as.numeric(scale(.))))
test_rescaled <- test[,-1] %>%
  mutate_if(is.numeric, funs(as.numeric(scale(.))))

glm.model<- glm(formula= Purchased.Bike~., data = train_rescaled,family = "binomial")
summary(glm.model)

pred1<-predict(glm.model,train_rescaled, type="response")
(table(train_rescaled$Purchased.Bike, pred1 > 0.5)) #Misclassification of 33.08% on training data


pred2<- predict(glm.model,test_rescaled,type="response")
(table(test_rescaled$Purchased.Bike, pred2 > 0.5))#Misclassification of 35.18% on training data





## RF######
rf_bike<- randomForest(Purchased.Bike ~ .-ID, data=train, 
                           ntree = 200,
                           mtry= 4,
                           importance = TRUE, 
                           do.trace = 100) 

rf_bike # Misclassification of 25.91% on training set 

rf.pred <- predict(rf_bike, test, type='class')
table(rf.pred, test$Purchased.Bike)
100-sum(diag(table(rf.pred, test$Purchased.Bike)))/length(test$Purchased.Bike)*100  #Misclassification rate on test set
varImpPlot(rf_bike, type=2,main="Variable Importance with Random Forest") #type=2: Reduction in gini index

data1<- train[,-1]
library("party")
x <- ctree(Purchased.Bike ~ ., data=data1)
plot(x, type="simple")


