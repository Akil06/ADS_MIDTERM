setwd ("C:/Users/dhruv/Desktop/ADS/Assignments/Midterm/AkilRFiles")

library(leaps)

library(ISLR)
library(forecast)
library(randomForest)
library(caret)
library(ROCR) 
library(e1071)
library(neuralnet)


#Q12005Hist<-Q200HistB
applyHeaders<- function(x){
  names(x) <- c ("LoanSequenceNumber","MonthlyReportingPeriod",
                "CurrentActualUPB","CurrentLoanDelinquencyStatus",
                "LoanAge","RemainingMonthsToLegalMaturity","RepurchaseFlag",
                "ModificationFLag","ZeroBalanceCode",
                "ZeroBalanceEffectiveDate","CurrentInterestRate",
                
                "CurrentDefferedUPB","DDLPI","MIRecoveries",
                "NetSalesProceeds","NonMIRecoveries","Expenses","LegalCosts","MaintanceCost"
                ,"Tax&Insurance","Misc","ActLoss","ModCost")  
  
}


cleanFile <- function(x) {
  columns<-c("CurrentActualUPB","CurrentLoanDelinquencyStatus",
             "LoanAge","RemainingMonthsToLegalMaturity","CurrentInterestRate",
             "CurrentDefferedUPB")
  
  x <-(x[,columns])
  
  
  x$CurrentLoanDelinquencyStatus[x$CurrentLoanDelinquencyStatus!=0]<-1    
  x$CurrentLoanDelinquencyStatus<-factor(x$CurrentLoanDelinquencyStatus,levels = c(0,1))
  return(x)
}
testDataFn<- function(x){
  size<- floor(0.01 * nrow(x))
  QLESS <- sample(seq_len(nrow(x)), size = size)
  QLESSDATA=x[QLESS,]
  return(QLESSDATA)
}


Q12005 <- read.table("historical_data1_time_Q12005.txt",sep = "|")
Q200HistB<-Q12005
names(Q12005)<-applyHeaders(Q12005)
Q12005<-cleanFile(Q12005)


head(Q12005)

Q22005 <- read.table("historical_data1_time_Q22005.txt",sep = "|")
names(Q22005)<-applyHeaders(Q22005)
Q22005<-cleanFile(Q22005)

######Logistic Regresion Begin######
lg.fit<- glm(CurrentLoanDelinquencyStatus ~ CurrentActualUPB+LoanAge+RemainingMonthsToLegalMaturity+
               CurrentInterestRate,data=Q1LESSDATA,family = binomial(link = "logit"))
#step=step(lg.fit,direction = "both")
summary(lg.fit)

test.p <- predict(lg.fit,Q22005Hist,type='response')
head(test.p)
pred<-rep(0,length(test.p))
pred[test.p >=0.5]<-1 

confusionMatrix(Q22005Hist$CurrentLoanDelinquencyStatus,pred)

library(ROCR)
prediction<- prediction(test.p,Q22005Hist$CurrentLoanDelinquencyStatus)
performance<-performance(prediction,measure="tpr",x.measure = "fpr")
plot(performance,main="ROC Curve",xlab="1-Specificity",ylab="Senssitivity")

#######Logistic Regresion End######

############ Random Forest Begin ###############
randomForestAlgo<- function(x){
  rf <-randomForest(CurrentLoanDelinquencyStatus ~ CurrentActualUPB+LoanAge+
                      RemainingMonthsToLegalMaturity+CurrentInterestRate,
                    ntree=10,na.action = na.exclude,family=binomial(link = "logit"),data=x,importance=T)
  return(rf)  
}
head(trainDataRF)
trainDataRF=testDataFn(Q12005)
rfModel=randomForestAlgo(trainDataRF)
#summary(rfModel)
print(rfModel)

test.RF <- predict(rfModel,Q22005,type='response')
head(test.RF)
predRF<-rep(0,length(test.RF))
predRF[test.RF!= 0]<- 1 

confusionMatrix(Q22005$CurrentLoanDelinquencyStatus,predRF)


predictionRF<- prediction(as.numeric(test.RF),as.numeric(Q22005Hist$CurrentLoanDelinquencyStatus))
performanceRF<-performance(predictionRF,measure="tpr",x.measure = "fpr")
plot(performanceRF,main="ROC Curve",xlab="1-Specificity",ylab="Senssitivity")

##Random Forest Alogritm End###

matrixOuput=function(x){
  
  table(x$CurrentLoanDelinquencyStatus,predRF)
  
  results<-table(x$CurrentLoanDelinquencyStatus,predRF)
  ActualDeli<-as.data.frame(x$CurrentLoanDelinquencyStatus[x$CurrentLoanDelinquencyStatus!=0])
  
  noActual<-nrow(ActualDeli)
  
  PredictedDeli<-as.data.frame(predRF[predRF!=0])
  
  noPredicted<-nrow(PredictedDeli)
  
  NoProperlyClassified <- (results[2,"1"])
  
  NoImProperlyClassified <- (results[1,"1"])
  
  NoTotalRowsDS<-nrow(x)
  resrows = cbind.data.frame(noActual,noPredicted,NoTotalRowsDS,NoProperlyClassified,NoImProperlyClassified)
  result = data.frame(resrows)
  
  names(result) <- c("Number of Actual Delinquents","Number of Predicted Delinquents","Number of records in the dataset",
                     "Number of Delinquents properly classified","Number of
                     non-delinquents improperly classified as delinquents")
  
  return(result)  
}

matrixOuput(Q22005)




#######Classicication Script#####
CScript<-function(){
yearList<- c(1999:2003)
QuarterList<-c("Q1","Q2","Q3","Q4")
resultRows<-NULL 
for (year in yearList){
  for (Quarter in QuarterList){
inYear<- year
inQuarter<- Quarter

trainYQ <- paste(inQuarter,inYear,sep = "")

if (inQuarter=="Q1"){
  testYQ <- paste("Q2",inYear,sep = "")
}
if (inQuarter=="Q2"){
  testYQ <- paste("Q3",inYear,sep = "")
}
if (inQuarter=="Q3"){
  testYQ <- paste("Q4",inYear,sep = "")
}
if (inQuarter=="Q4"){
  newYear=inYear + 1
  testYQ <- paste("Q1",newYear,sep = "")
}

if ((inYear=="2006") & (inQuarter=="Q1")){
  
  break;
}

resultRow=computeMatrix(trainYQ,testYQ) 
resultRows<- rbind(resultRows,resultRow)

}
}
return(resultRows)
}

computeMatrix= function(x1,x2){
  trainData = x1
  testData = x2
  filenameTrain=paste("historical_data1_time_",trainData,sep = "")
  filenameTrain=paste(filenameTrain,".txt",sep="")
  filenameTest=paste("historical_data1_time_",testData,sep = "")
  filenameTest=paste(filenameTest,".txt",sep="")
  cat(trainData)
  cat(filenameTrain)
  trainDataFrame <- read.table(filenameTrain,sep = "|")
  testDataFrame<- read.table(filenameTest,sep = "|")
  
  names(trainDataFrame)<-applyHeaders(trainDataFrame)
  names(testDataFrame)<-applyHeaders(testDataFrame)
  
  trainDataFrame<-cleanFile(trainDataFrame)
  testDataFrame<-cleanFile(testDataFrame)
  
  trainDataRF=testDataFn(trainDataFrame)
  rfModel=randomForestAlgo(trainDataRF)
  
  test.RF <- predict(rfModel,testDataFrame,type='response')
  head(test.RF)
  predRF<-rep(0,length(test.RF))
  predRF[test.RF!= 0]<- 1 
  
  
  results<- table(testDataFrame$CurrentLoanDelinquencyStatus,predRF)
  ActualDeli<-as.data.frame(testDataFrame$CurrentLoanDelinquencyStatus[testDataFrame$CurrentLoanDelinquencyStatus!=0])
  
  noActual<-nrow(ActualDeli)
  
  PredictedDeli<-as.data.frame(predRF[predRF!=0])
  
  noPredicted<-nrow(PredictedDeli)
  
  NoProperlyClassified <- (results[2,"1"])
  
  NoImProperlyClassified <- (results[1,"1"])
  
  NoTotalRowsDS<-nrow(testDataFrame)
  resrows = cbind.data.frame(trainData,noActual,noPredicted,NoTotalRowsDS,NoProperlyClassified,NoImProperlyClassified)
  resultRows = data.frame(resrows)
  
  }

result<-CScript()

#######Classicication Script#####


########Neural Network Algorithm ########


cols<- names(Q1LESSDATA)

#Q1LESSDATA

form <- cols[!cols%in%"OrignialInterestRate"]
form<-paste(form,collapse = '+')

form<-paste('CurrentLoanDelinquencyStatus~',form)
formulaused<-as.formula(form)

maxs <- apply(Q1LESSDATA[,c(1,3:5)],2,max)
mins <- apply(Q1LESSDATA[,c(1,3:5)],2,min)

# typeof(Q1LESSDATA$OriginalLoanTerm)

scaled.data <- as.data.frame(scale(Q1LESSDATA[,c(1,3:5)],center = mins,scale = maxs-mins))
CurrentLoanDelinquencyStatus<- as.numeric(Q1LESSDATA$CurrentLoanDelinquencyStatus)
scaled.data=cbind(CurrentLoanDelinquencyStatus,scaled.data)
print(head(scaled.data,10))

# unique(Q1LESSDATA$SuperConfirmingFlag)

nn <- neuralnet(CurrentLoanDelinquencyStatus ~ CurrentActualUPB+LoanAge+
                  RemainingMonthsToLegalMaturity+CurrentInterestRate,scaled.data,
                hidden=c(1,1),stepmax = 1e3,threshold = 0.01,linear.output = T)

help("neuralnet")