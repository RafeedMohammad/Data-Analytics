library(corrplot)
library(mice)
library(VIM)
library(Boruta)
library(randomForest)
library(caret)
install.packages("adabag")
library(adabag)





df <- read.csv(file.choose(), header = TRUE)
str(df)
df <- as.data.frame(df)


##Missing Value Imputation:
sum(is.na(df$AGE))  #Checking for missing values in AGE attribute

md.pattern(df, rotate.names = FALSE)

plot <- aggr(df, col=c('navyblue','yellow'),
             numbers=TRUE, sortVars=TRUE,
             labels=names(df), cex.axis=.7,
             gap=3, ylab=c("Missing data","Pattern"))

imputed_Data <- mice(df, m=1, maxit = 50, method = 'pmm', seed = 500)

summary(imputed_Data)

sum(is.na(imputed_Data$AGE))  #Checking that, imputed_Data has any missing AGE value or not

##Feature Selction:
dim(imputed_Data)
library(mlbench)
library(caret)

completeData <- complete(imputed_Data,2)
pairs.panels(completeData)


set.seed(111)
imputed_Data <- as.data.frame(imputed_Data)
boruta <- Boruta(default.payment.next.month ~ ., data = completeData, doTrace = 2, maxRuns = 500)
where(is.mids(imputed_Data))

names(boruta)

imp <- completeData[c(2, 5:25)]  #Omitting unimportant features
names(imp)  #Checking the selected features in imp dataset

##Applying Models:
set.seed(3456)
trainIndex <- createDataPartition(imp$default.payment.next.month, p = .8, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)

#Data Splitting:
Train <- imp[ trainIndex,]
Test  <- imp[-trainIndex,]

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)


#Train The model
Fit1 <- train(default.payment.next.month ~ ., data = train, 
                 method = "nb", 
                 trControl = fitControl,
                 )
Fit1 ##Result

trellis.par.set(caretTheme())
plot(Fit1)  #Visualizing Accuracy after repeatation of cross validation


##Apply RF
library(randomForest)
set.seed(123)
rf <- randomForest(default.payment.next.month~., data = Train)
print(rf)

attributes(rf)

p1 <- predict(rf, Train)
confusionMatrix(p1, Train$default.payment.next.month)

p2 <- predict(rf, Test)
confusionMatrix(p2, Test$default.payment.next.month)


##Apply Adaboost

cvModel <- boosting.cv(default.payment.next.month ~ . , data = Train, boos = TRUE, mfinal = 10, v = 10)
x = cvModel[-1]
print(x)
print(data.frame(Train$default.payment.next.month, cvModel$class))


