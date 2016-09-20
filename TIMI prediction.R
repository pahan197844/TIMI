
setwd("C:/Users/pdudin01/Desktop/Cardiac scores")
dataRaw=read.csv("EvaluationOfChestPai_DATA_2016-09-16_0931.csv")
columns=c(1,7,12:16,17,18,71,72,75,81:102,107:112,119,132,136,139:165,305:307,308:313,326)
data=subset(dataRaw, select = columns)
data=data[complete.cases(data[,71]),]
data=data[complete.cases(data[,3]),]
data=data[complete.cases(data[,5]),]
data=data[complete.cases(data[,4]),]
data=data[complete.cases(data[,11]),]
data=data[complete.cases(data[,13]),]
data=data[complete.cases(data[,14]),]
data=data[complete.cases(data[,15]),]
data=data[complete.cases(data[,20]),]
data=data[complete.cases(data[,41]),]
data=data[complete.cases(data[,12]),]

model<-glm(factor(data$return_visit_to_an_ed) ~ age_years+race+gender+timi_risk_score+ heart_score_number+ 
             history_mi_family+history_cad+smoker+prior_mi+bmi, family=binomial,data=data )



summary(model)
library(caTools) 
set.seed(123)
sample = sample.split(data$return_visit_to_an_ed, SplitRatio= 3/4 )
trainIdx = which(sample == T)
trainData = data[trainIdx,]
testIdx = which(sample == F)
testData = data[testIdx,]

#Display of distributed data
dim(trainData)
dim(testData)
modelTrain<-glm(return_visit_to_an_ed ~ age_years+race+gender+timi_risk_score+ 
                  history_mi_family+history_cad+smoker+prior_mi+bmi,data=trainData, family= binomial )


predictTrain=predict(modelTrain, type = "response")

str(predictTrain)
tapply(predictTrain,trainData$return_visit_to_an_ed, mean)

table(trainData$return_visit_to_an_ed, predictTrain> 0.2)

library(ROCR)

RPCRpred = prediction(predictTrain, trainData$return_visit_to_an_ed)
ROCRferf = performance(RPCRpred,"tpr", "fpr")

plot (ROCRferf, colorize=T, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))
predictTest = predict(modelTrain, type="response", newdata = testData)
 
   
library(pROC)
colAUC( predictTrain,trainData$return_visit_to_an_ed,plotROC=TRUE)

   predictTest = predict(modelTrain, type="response", newdata = testData)
tapply(predictTest,testData$return_visit_to_an_ed, mean)
table(testData$return_visit_to_an_ed, predictTest> 0.2)

library(ggplot2)
ggplot(data=data, aes(x=factor(timi_risk_score),fill=factor(return_visit_to_an_ed)))+geom_bar()
ggplot(data=data, aes(x=factor(heart_score_number),fill=factor(return_visit_to_an_ed)))+geom_bar()


library(e1071)
modelTrainCVM=svm(return_visit_to_an_ed ~ timi_risk_score,data=data,kernel = 'linear', cost=0.1, scale=FALSE)
predictTrainCVM=predict(modelTrainCVM, testData)

str(predictTrainCVM)
tapply(predictTrainCVM,trainData$return_visit_to_an_ed, mean)

library(SDMTools)
confusion.matrix(trainData$return_visit_to_an_ed, predictTrainCVM, threshold=0.6)
