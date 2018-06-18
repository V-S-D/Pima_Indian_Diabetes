# Load caret
if(!(require("caret"))) install.packages("caret")

# read data
data = read.csv("Pima.csv")

# Preprocessing
str(data)
sum(is.na(data)) 
class=as.factor(data$Class.variable..0.or.1.)
data$Class.variable..0.or.1. = NULL
preProcValues <- preProcess(data, method = c("center","scale"))  # Centring and scaling
data_processed <- predict(preProcValues, data)
data_processed$class = class
# Splitting data
set.seed(108)
index <- createDataPartition(data_processed$class, p=0.7, list=FALSE)
trainSet <- data_processed[ index,]
testSet <- data_processed[-index,]
str(trainSet)

# Feature selection using caret
control <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 3,
                   verbose = FALSE)
outcomeName<-'class'
predictors<-names(trainSet)[!names(trainSet) %in% outcomeName]
class_variable_Profile <- rfe(trainSet[,predictors], trainSet[,outcomeName],
                      rfeControl = control)
class_variable_Profile

# Taking the top predictors
predictors=c("Plasma.glucose.concentration.a.2.hours.in.an.oral.glucose.tolerance.test", "Body.mass.index..weight.in.kg..height.in.m..2.","Age..years.", "Number.of.times.pregnant", "X2.Hour.serum.insulin..mu.U.ml.")

# Modeling using caret
model_glm<-train(trainSet[,predictors],trainSet[,outcomeName],method='glm')
model_gbm<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm')
model_rf<-train(trainSet[,predictors],trainSet[,outcomeName],method='rf')
model_nnet<-train(trainSet[,predictors],trainSet[,outcomeName],method='nnet')

# Variable Importance
varImp(object=model_glm)
varImp(object=model_gbm)
varImp(object=model_rf)
varImp(object=model_nnet)

#Plotting Varianle importance for GLM
plot(varImp(object=model_glm),main="GLM - Variable Importance")
plot(varImp(object=model_gbm),main="GBM - Variable Importance")
plot(varImp(object=model_rf),main="RF - Variable Importance")
plot(varImp(object=model_nnet),main="NEURAL NET - Variable Importance")


## Predictions using caret
# GLM
predictions<-predict.train(object=model_glm,testSet[,predictors],type="raw")
table(predictions)
confusionMatrix(predictions,testSet[,outcomeName])

# GBM
predictions<-predict.train(object=model_gbm,testSet[,predictors],type="raw")
table(predictions)
confusionMatrix(predictions,testSet[,outcomeName])

# RF
predictions<-predict.train(object=model_rf,testSet[,predictors],type="raw")
table(predictions)
confusionMatrix(predictions,testSet[,outcomeName])

# NNET
predictions<-predict.train(object=model_nnet,testSet[,predictors],type="raw")
table(predictions)
confusionMatrix(predictions,testSet[,outcomeName])

