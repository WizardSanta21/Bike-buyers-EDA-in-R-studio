library(ggplot2)
library(dplyr)
library(corrplot)
library(caTools)
library(rpart)
library(rpart.plot)
Bicycle <- read.csv("bike_buyers_clean.csv",stringsAsFactors = FALSE)
str(Bicycle)
head(Bicycle)
View(Bicycle)
summary(Bicycle)

#Histogram of occupation,income
ggplot(data = Bicycle, aes(x = Income)) + 
  geom_histogram(bins = 40,color = "black") +
  geom_vline(xintercept = mean(Bicycle$Income), linetype="dotted")

ggplot(data = Bicycle, aes(x = Occupation)) + 
  geom_bar(color = "black",alpha = 0.75,fill='green')

#Distribution of Occupation,income by Purchase.Bike
ggplot(data = Bicycle, aes(x = Income,fill = Purchased.Bike)) + 
  geom_histogram(bins = 40,color = "black") +
  geom_vline(xintercept = mean(Bicycle$Income), linetype="dotted")


ggplot(data = Bicycle, aes(x = Occupation, fill = Purchased.Bike,group = Purchased.Bike)) + 
  geom_bar(color = "black",alpha = 0.75) +
  geom_text(stat = "count", aes(label = ..count..),
            inherit.aes = TRUE,position = position_stack(), vjust = 1.5) +
  theme_bw() + labs(title = "Distribution of Occupation by Purchase.Bike")


#Dataset Preparation Income and bike purchase
p1 <- as.data.frame(prop.table(table(Bicycle$Income,Bicycle$Purchased.Bike),margin =1))
names(p1)<- c("Income","Bought.Bike","Frequency")
p1 <- p1 %>% filter(Bought.Bike == "Yes")

#Plotting frequency bar plot
ggplot(data = p1 , aes(x = Income, y = Frequency, group = 1)) +
  geom_point(color = "black") + geom_line() +
  scale_x_discrete(breaks = seq(10000 , 170000 , by = 40000)) +
  labs(title = "Precentage of bike buyers in each income category")


#Dataset Preparation for occupation and purchased bike
plot2 <- as.data.frame(prop.table(table(Bicycle$Occupation,Bicycle$Purchased.Bike),1))
names(plot2) <- c("Occupation","Purchased.Bike","Freq")

#Plotting Frequency Bike buyers by Occupation category
ggplot(data = plot2, aes(x = Occupation, y = Freq, fill = Purchased.Bike)) + 
  geom_bar(color = "black",alpha = 0.75,stat = "identity") +
  theme_bw() + 
  labs(title = "Frequency Bike buyers by Occupation category")

#Dataset Preparation
plot <- as.data.frame(prop.table(table(Bicycle$Education,Bicycle$Purchased.Bike),1))
names(plot) <- c("Education","Purchased.Bike","Freq")

#Plotting
ggplot(data = plot, aes(x = Education, y = Freq, fill = Purchased.Bike)) + 
  geom_bar(color = "black",alpha = 0.75,stat = "identity") +
  labs(title = "Frequency Bike buyers by Education category")

#Plotting Distribution of Occupation by education
ggplot(data = Bicycle, aes(x = Education, fill = Occupation)) + 
  geom_bar(color = "black",alpha = 0.75 ,position = "dodge") +
  theme_bw() +
  labs(title = "Distribution of Occupation by education")

ggplot(data = Bicycle, aes(x = Occupation, y = Income, color = Purchased.Bike)) +
  geom_point() + geom_jitter(width = .2)

ggplot(data = Bicycle, aes(x = Occupation, y = Income, color = Purchased.Bike)) +
  geom_jitter(width = .2)+ facet_wrap(~ Purchased.Bike)

ggplot(data = Bicycle, aes(x = Education, y = Occupation, color = Purchased.Bike)) +
  geom_jitter(width = .2)+ facet_wrap(~ Purchased.Bike)

ggplot(data = Bicycle, aes(x = Age,fill = Purchased.Bike)) + 
  geom_histogram(bins = 50,alpha = 0.5, color = "black") +
  theme_classic() +
  labs(title = "Age histogram")

ggplot(data = Bicycle, aes(x = Age, color = Purchased.Bike,fill = Purchased.Bike)) +
  geom_density(alpha = 0.35) +
  geom_vline(data = Bicycle %>% group_by(Purchased.Bike) %>% summarise(avg = mean(Age))
             , aes(xintercept = avg , color = Purchased.Bike), linetype = "dashed") +
  theme_classic() +
  labs(title = "Density of age in each category", subtitle = "Average age of each group is represented by vertical lines") +
  scale_x_continuous(breaks = seq(from = 25, to = 80 , by = 5))

df = Bicycle %>% mutate(Marital.Status = if_else(Marital.Status == "Married",1,0),
                          Home.Owner = if_else(Home.Owner == "Yes",1,0),
                          Purchased.Bike = if_else(Purchased.Bike == "Yes",1,0),
                          Gender = if_else(Gender == "Male",1,0),
                          Education = unclass(Education),
                          Commute.Distance = unclass(Bicycle$Commute.Distance)) %>%
  rename(Married = Marital.Status,
         Male = Gender)
head(df)
str(df)  
df=df[-1]
df
str(df)

cor_bicycle=cor(df[,c(1:4,7,8,11,12)])
cor_bicycle
corrplot(cor(df[,c(1:4,7,8,11,12)]),method = "number",order=)

df1=df[,c(1:4,7,8,11,12)]
#training and testing data set

df2=Bicycle[,c(4,5,9,12,13)]
View(df2)
library(caTools)
set.seed(123)
sample = sample.split(df2,SplitRatio = 0.8)
traindata=subset(df2,sample==T)
str(traindata)
dim(traindata)
head(traindata)
View(traindata)
testdata = subset(df2,sample==F)
str(testdata)
dim(testdata)
head(testdata)
View(testdata)

library(rpart)
model = rpart(formula=Purchased.Bike~.,data=traindata,method='class')
summary(model)

pdf(file='My assignment15.pdf',
    width = 4,
    height = 4)

library(rpart.plot)
rpart.plot(model)
dev.off()


pred.test=predict(model,newdata = testdata, type='class')
accuracy.test=mean(pred.test==testdata$Purchased.Bike)
accuracy.test
cat('Train Accuracy',accuracy.test)

library(caret)
testdata$Purchased.Bike=as.factor(testdata$Purchased)
confusion = confusionMatrix(pred.test,testdata$Purchased.Bike)
confusion
