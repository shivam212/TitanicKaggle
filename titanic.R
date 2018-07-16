library(ggplot2)
library(dplyr)
library(Amelia)
train <- read.csv("titanic_train.csv")
test <- read.csv("titanic_test.csv")
#plot to visualise dead vs survived
print(ggplot(train,aes(Survived)) + geom_histogram() + labs(title= "SURVIVED VS DEAD"))
#plot to visualise the survived population by their sex i.e Male and Female
print(ggplot(train,aes(Sex)) + geom_bar(aes(fill=factor(Survived))) + labs(title= "SURVIVAL OF MALES VS FEMALES"))
#plot to visualise the survived ppopulation by the class they were travelling on
print(ggplot(train,aes(Pclass)) + geom_bar(aes(fill=factor(Survived))) + labs(title= "SURVIVAL BY CLASS TRAVELLED"))
#plot to visualise ages of passenger
print(ggplot(train,aes(Age)) + geom_histogram(fill='blue',alpha=0.5))
#plot to visualise ages of survived
print(ggplot(train,aes(Age)) + geom_histogram(aes(fill=factor(Survived)),alpha=0.5))
ac1 <- train %>% filter(Pclass == 1) %>% select(Age)
ac2 <- train %>% filter(Pclass == 2) %>% select(Age)
ac3 <- train %>% filter(Pclass == 3) %>% select(Age)
means <-c(mean(ac1$Age,na.rm = TRUE),mean(ac2$Age,na.rm = TRUE),mean(ac3$Age,na.rm = TRUE))
#Visualising the missing values in the data frame
missmap(train)
for (i in 1:891)
{
  if(train$Pclass[i]==1 & is.na(train$Age[i]))
      train$Age[i]=means[1]
  else if(train$Pclass[i]==2 & is.na(train$Age[i]))
    train$Age[i]=means[2]
  else if(train$Pclass[i]==3 & is.na(train$Age[i]))
    train$Age[i]=means[3]
}
for (k in 1:length(test$Pclass))
{
  if(test$Pclass[k]==1 & is.na(test$Age[k]))
    test$Age[k]=means[1]
  else if(test$Pclass[k]==2 & is.na(test$Age[k]))
    test$Age[k]=means[2]
  else if(test$Pclass[k]==3 & is.na(test$Age[k]))
    test$Age[k]=means[3]
}
#after cleaning the data
missmap(train)
#data for the model
modeltrain <- select(train,-Name,-PassengerId,-Ticket,-Fare,-Cabin,-Embarked)
modeltrain$Survived <- factor(modeltrain$Survived)
modeltrain$Pclass <- factor(modeltrain$Pclass)
modeltrain$SibSp <- factor(modeltrain$SibSp)
#training the model
log.model <- glm(formula=Survived ~ . , family = binomial(link='logit'),data = modeltrain)
print(summary(log.model))
#predicting
modeltest <- select(test,-Name,-PassengerId,-Ticket,-Fare ,-Cabin,-Embarked)
modeltest$Pclass <- factor(modeltest$Pclass)
modeltest$SibSp <- factor(modeltest$SibSp)
fitted.probabilities <- predict(log.model,newdata=modeltest,type='response')
results <- ifelse(fitted.probabilities >= 0.5,1,0)
answers <- data.frame(test$PassengerId,results)
colnames(answers)[1] <- 'PassengerId'
colnames(answers)[2] <- 'Survived'
write.csv(answers,'titanicanswers.csv',row.names = F)