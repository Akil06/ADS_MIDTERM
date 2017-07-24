setwd ("C:/Users/dhruv/Desktop/ADS/Assignments/Midterm/AkilRFiles")

library(leaps)
library(MASS)
library(ISLR)
library(forecast)
library(randomForest)
library(caret)

Q1 <- read.table("historical_data1_Q12005.txt",sep = "|")

applyHeaders<- function(x){
  names(x) <- c("CreditScore","FirstPaymentDate","FirstTimeHomeBuyerFlag","MaturityDate","MSA"
                ,"MI","NoofUnits","OccupancyStatus","CLTV","DTI","UPB","LTV","OrignialInterestRate","Channel","PPMFlag"
                ,"ProductType","PropertyState",
                "PropertyType","PostalCode","LoanSequenceNumber",
                "LoanPurpose","OriginalLoanTerm","NoofBorrowers",
                "SellerName","ServiceName","SuperConfirmingFlag")  

  }
names(Q1)<-applyHeaders(Q1)

#head(Q1)

cleanFile <- function(x) {
  # Code for Cleaning the Columns by removing the NA#
  
  # NoofUnits #
  NoofUnits<-as.data.frame(x$NoofUnits)
  NoofUnits[is.na(NoofUnits)] <- 1
  x[,"NoofUnits"]<-NoofUnits
  
  # MI Filling all NAs with 56 as anything above 55 is regarded as NA in File#
  MI<-as.data.frame(x$MI)
  MI[is.na(MI)] <- 56
  x[,"MI"]<-MI
  
  # DTI Filling all NAs with 56 as anything above 66 is regarded as NA in File#
  DTI<-as.data.frame(x$DTI)
  DTI[is.na(DTI)] <- 66
  x[,"DTI"]<-DTI
  
  
  # PostalCode Filling all NAs with 56 as anything above 55 is regarded as NA in File#
  PostalCode<-as.data.frame(x$PostalCode)
  PostalCode[is.na(PostalCode)] <- 0
  x[,"PostalCode"]<-PostalCode
  
  # LTV Filling all NAs with 1 as anything below 1 is regarded as NA in File#
  LTV<-as.data.frame(x$LTV)
  LTV[is.na(LTV)] <- 0
  x[,"LTV"]<-LTV
  
  # CLTV Filling all NAs with 1 as anything below 1 is regarded as NA in File#
  CLTV<-as.data.frame(x$CLTV)
  CLTV[is.na(CLTV)] <- 0
  x[,"CLTV"]<-CLTV
  
  # Credit Score Filling NAs with 900 as anything above 850 is regarded as NA in File# #
  CreditScore<-as.data.frame(x$CreditScore)
  CreditScore[is.na(CreditScore)] <- 900
  x[,"CreditScore"]<-CreditScore
  
  # MSA Filling all NAs with 56 as anything above 55 is regarded as NA in File#
  MSA<-as.data.frame(x$MSA)
  MSA[is.na(MSA)] <- 0
  x[,"MSA"]<-MSA
  
  
  # NoofBorrowers Filling all NAs with 56 as anything above 55 is regarded as NA in File#
  NoofBorrowers<-as.data.frame(x$NoofBorrowers)
  NoofBorrowers[is.na(NoofBorrowers)] <- 1
  x[,"NoofBorrowers"]<-NoofBorrowers
  
  # NoofBorSuperConfirmingFlagrowers Filling all NAs with 56 as anything above 55 is regarded as NA in File#
  SuperConfirmingFlag<-as.data.frame(x$SuperConfirmingFlag)
  SuperConfirmingFlag[is.na(SuperConfirmingFlag)] <- "N"
  x[,"SuperConfirmingFlag"]<-SuperConfirmingFlag
  x[,"SuperConfirmingFlag"] <- ifelse(x$SuperConfirmingFlag=="Y", 0, 1)
  
  # FirstTimeHomeBuyerFlag Filling all NAs with 56 as anything above 55 is regarded as NA in File#
  
  x[,"FirstTimeHomeBuyerFlag"] <- ifelse(x$FirstTimeHomeBuyerFlag=="Y", 0, 1)
  
  
  # PPMFlag Filling all NAs with 56 as anything above 55 is regarded as NA in File#
  x$PPMFlag [x$PPMFlag ==""] <- "N"
  x[,"PPMFlag"] <- ifelse(x$PPMFlag=="Y", 0, 1)
  

  
  x[,"ChannelB"]<-ifelse(x$Channel=="B", 1, 0)
  x[,"ChannelC"]<-ifelse(x$Channel=="C", 1, 0)
  x[,"ChannelR"]<-ifelse(x$Channel=="R", 1, 0)
  x[,"ChannelT"]<-ifelse(x$Channel=="T", 1, 0)
  
  
  x[,"PropertyCO"]<-ifelse(x$PropertyType=="CO", 1, 0)
  x[,"PropertySF"]<-ifelse(x$PropertyType=="SF", 1, 0)
  x[,"PropertyPU"]<-ifelse(x$PropertyType=="PU", 1, 0)
  x[,"PropertyMH"]<-ifelse(x$PropertyType=="MH", 1, 0)
  x[,"PropertyLH"]<-ifelse(x$PropertyType=="LH", 1, 0)
  x[,"PropertyCP"]<-ifelse(x$PropertyType=="CP", 1, 0)
  
  x[,"LoanPurposeC"]<-ifelse(x$LoanPurpose=="C", 1, 0)
  x[,"LoanPurposeN"]<-ifelse(x$LoanPurpose=="N", 1, 0)
  x[,"LoanPurposeP"]<-ifelse(x$LoanPurpose=="P", 1, 0)
  
  x[,"OccupancyStatusO"]<-ifelse(x$OccupancyStatus=="O", 1, 0)
  x[,"OccupancyStatusS"]<-ifelse(x$OccupancyStatus=="S", 1, 0)
  x[,"OccupancyStatusI"]<-ifelse(x$OccupancyStatus=="I", 1, 0)
  
  
  x <- x[,c("OrignialInterestRate","CreditScore","FirstTimeHomeBuyerFlag","MSA","MI","NoofUnits","DTI"
                  ,"UPB","CLTV","LTV","PPMFlag","PostalCode","OriginalLoanTerm","NoofBorrowers"
                  ,"SuperConfirmingFlag","ChannelB","ChannelC","ChannelR","ChannelT","PropertyCO"
                  ,"PropertySF","PropertyPU","PropertyMH","PropertyLH","PropertyCP","LoanPurposeC"
                  ,"LoanPurposeN","LoanPurposeP","OccupancyStatusO","OccupancyStatusS","OccupancyStatusI")]
  return(x)
  }
Q12005=cleanFile(Q1)



# unique(Q1$CreditScore)
# # Q12005= Q1[]
# # unique(Q1$OccupancyStatus)
# # summary(Q1$PPMFlag)
# # help("as.Date")
# #
# # head(Q1)


# head(Q12005)

linearRegression<- function(x){
  

lm.fit <-lm(OrignialInterestRate~.,data=x)
#step=step(lm.fit,direction = "both")
lm.fit <-lm( OrignialInterestRate ~ CreditScore  + 
               MSA + MI + NoofUnits + DTI + UPB + CLTV + PPMFlag + PostalCode + 
               OriginalLoanTerm + NoofBorrowers + ChannelB + ChannelC + 
               ChannelR + 
               LoanPurposeC + LoanPurposeN + OccupancyStatusO + OccupancyStatusS,data=x)
# summary (lm.fit)
return(lm.fit)
}
lm.fit<-linearRegression(Q12005)
summary(lm.fit)

Q22005 <- read.table("historical_data1_Q22005.txt",sep = "|")
names(Q22005)<- applyHeaders(Q22005)
testData <- cleanFile(Q22005)
head(testData)
names(testData)
predVariable = predict(lm.fit, testData)
#predVariable
plot(predVariable,testData$OrignialInterestRate, col='blue',main='Real vs predicted Linear',xlab="Predicted",ylab="Actual")
abline(0,1)

accuracy(predVariable,testData$OrignialInterestRate)


size<- floor(0.30 * nrow(Q12005))
Q1LESS <- sample(seq_len(nrow(Q12005)), size = size)
Q1LESSDATA=Q12005[Q1LESS,]

randomForestAlgo<- function(x){
  rf <-randomForest(OrignialInterestRate ~ CreditScore  + 
                      MSA + MI + NoofUnits + DTI + UPB + CLTV + PPMFlag  + 
                      OriginalLoanTerm + NoofBorrowers + ChannelB + ChannelC + 
                      ChannelR + 
                      LoanPurposeC + LoanPurposeN + OccupancyStatusO + OccupancyStatusS,ntree=10,na.action = na.exclude,data=x,importance=T)
return(rf)  
}

rfModel=randomForestAlgo(Q1LESSDATA)
#summary(rfModel)
print(rfModel)
#attributes(rfModel)
#rfModel$importance
predictedRF<-predict(rfModel,newdata =testData)
accuracy(predictedRF,testData$OrignialInterestRate)

  
# roundedPredicted<-rfModel$predicted
# reoundedtestData<-testData$OrignialInterestRate

# confusionMatrix(data=roundedPredicted,
#                 reference=reoundedtestData,
#                 positive='yes')
# rfModel=randomForestAlgo(Q1LESSDATA)
# help (accuracy)
# table(predictedRF,testData$OrignialInterestRate)
plot (predictedRF,testData$OrignialInterestRate,col='red',main='Real vs predicted Random Forest',xlab="Predicted",ylab="Actual")

########Neural Network Algorithm ########
library(neuralnet)
cols<- names(Q1LESSDATA)

head(Q1LESSDATA)

form <- cols[!cols%in%"OrignialInterestRate"]
form<-paste(form,collapse = '+')

form<-paste('OrignialInterestRate~',form)
formulaused<-as.formula(form)

maxs <- apply(Q1LESSDATA[,2:31],2,max)
mins <- apply(Q1LESSDATA[,2:31],2,min)

# typeof(Q1LESSDATA$OriginalLoanTerm)

scaled.data <- as.data.frame(scale(Q1LESSDATA[,2:31],center = mins,scale = maxs-mins))
OrignialInterestRate<- as.numeric(Q1LESSDATA$OrignialInterestRate)
scaled.data=cbind(OrignialInterestRate,scaled.data)
print(head(scaled.data,10))

unique(Q1LESSDATA$SuperConfirmingFlag)

nn <- neuralnet(OrignialInterestRate ~ CreditScore  + 
                   MSA + MI + NoofUnits + DTI + UPB + CLTV + PPMFlag + PostalCode + 
                   OriginalLoanTerm + NoofBorrowers + ChannelB + ChannelC + 
                   ChannelR + 
                   LoanPurposeC + LoanPurposeN + OccupancyStatusO + OccupancyStatusS,
                scaled.data,hidden=c(2,1),stepmax = 1e6,threshold = 0.01,linear.output = T)
plot(nn)
print(nn)
names(testData)
testData[,2:31]
names(nn$data)

maxs <- apply(testData[,2:31],2,max)
mins <- apply(testData[,2:31],2,min)

# typeof(Q1LESSDATA$OriginalLoanTerm)
OrignialInterestRate<- as.numeric(testData$OrignialInterestRate)
testData.scaled <- as.data.frame(scale(testData[,2:31],center = mins,scale = maxs-mins))
testData.scaled <- cbind(OrignialInterestRate,testData.scaled)

head(testData.scaled[,2:31])
head(nn$data[,2:31])
usedColumns<-c("CreditScore" ,"MSA","MI","NoofUnits","DTI",
               "UPB","CLTV","PPMFlag","PostalCode","OriginalLoanTerm","NoofBorrowers",
               "ChannelB","ChannelC", 
               "ChannelR", 
               "LoanPurposeC", "LoanPurposeN", "OccupancyStatusO", "OccupancyStatusS")

predictednn <- compute(nn,testData.scaled[,usedColumns])

#predictedNNA <- predict(nn,as.numeric(unlist(testData.scaled$OrignialInterestRate)))
accuracy(predictednn,testData.scaled$OrignialInterestRate)

print(head(predictednn$net.result))
predictednn<-predictednn$net.result*(max(testData.scaled$OrignialInterestRate)-min(testData.scaled$OrignialInterestRate))+min(testData.scaled$OrignialInterestRate)
testData.r<-(testData.scaled$OrignialInterestRate)*(max(testData.scaled$OrignialInterestRate)-min(testData.scaled$OrignialInterestRate))+min(testData.scaled$OrignialInterestRate)
MSE.nn <- sum ((testData.r-predictednn)^2)/nrow(testData.scaled)
predictednn
plot(testData.scaled$OrignialInterestRate,predictednn,
     col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2,untf = T)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')




Q12007<- read.table("historical_data1_Q12007.txt",sep = "|")
Q22007<- read.table("historical_data1_Q22007.txt",sep = "|")
Q32007<- read.table("historical_data1_Q32007.txt",sep = "|")
Q42007<- read.table("historical_data1_Q42007.txt",sep = "|")
Q12008<- read.table("historical_data1_Q12009.txt",sep = "|")

Q12009<- read.table("historical_data1_Q12009.txt",sep = "|")
Q22009<- read.table("historical_data1_Q22009.txt",sep = "|")
Q32009<- read.table("historical_data1_Q32009.txt",sep = "|")
Q42009<- read.table("historical_data1_Q42009.txt",sep = "|") 

Q11999<- read.delim("historical_data1_Q11999.txt",sep = "|")


Q21999<- read.table("historical_data1_Q21999.txt",sep = "|")
Q31999<- read.table("historical_data1_Q31999.txt",sep = "|")
Q41999<- read.table("historical_data1_Q41999.txt",sep = "|")
Q12013<- read.table("historical_data1_Q12013.txt",sep = "|")
Q22013<- read.table("historical_data1_Q22013.txt",sep = "|")
Q32013<- read.table("historical_data1_Q32013.txt",sep = "|")
Q42013<- read.table("historical_data1_Q42013.txt",sep = "|")


names(Q12007)<- applyHeaders(Q12007)
names(Q22007)<- applyHeaders(Q22007)
names(Q32007)<- applyHeaders(Q32007)
names(Q42007)<- applyHeaders(Q42007)
names(Q12008)<- applyHeaders(Q12008)
names(Q12009)<- applyHeaders(Q12009)
names(Q22009)<- applyHeaders(Q22009)
names(Q32009)<- applyHeaders(Q32009)
names(Q42009)<- applyHeaders(Q42009)
colnames(Q11999)<- applyHeaders(Q11999)
names(Q21999)<- applyHeaders(Q21999)
names(Q31999)<- applyHeaders(Q31999)
names(Q41999)<- applyHeaders(Q41999)

names(Q12013)<- applyHeaders(Q12013)
names(Q22013)<- applyHeaders(Q22013)
names(Q32013)<- applyHeaders(Q32013)
names(Q42013)<- applyHeaders(Q42013)


Q12007 <- cleanFile(Q12007)

testDataFn<- function(x){
size<- floor(0.30 * nrow(x))
QLESS <- sample(seq_len(nrow(x)), size = size)
QLESSDATA=x[QLESS,]
return(QLESSDATA)
}

trainDataRF=testDataFn(Q12007)
rfModel=randomForestAlgo(testDataRF)
testData=cleanFile(Q22007)
predictedRF<-predict(rfModel,newdata =testData)
accuracy(predictedRF,testData$OrignialInterestRate)

trainDataRF = testDataFn(Q22007)
trainDataRF<-cleanFile(trainDataRF)
rfModel= randomForestAlgo(trainDataRF)
testData=cleanFile(Q32007)
predictedRF<-predict(rfModel,newdata =testData)
accuracy(predictedRF,testData$OrignialInterestRate)

trainDataRF=testDataFn(Q32007)
trainDataRF<-cleanFile(trainDataRF)
rfModel=randomForestAlgo(trainDataRF)
testData=cleanFile(Q42007)
predictedRF<-predict(rfModel,newdata =testData)
accuracy(predictedRF,testData$OrignialInterestRate)

trainDataRF=testDataFn(Q42007)
trainDataRF<-cleanFile(trainDataRF)
rfModel=randomForestAlgo(trainDataRF)
testData=cleanFile(Q12008)
predictedRF<-predict(rfModel,newdata =testData)
accuracy(predictedRF,testData$OrignialInterestRate)

#####Running for 2009 Year#######

trainDataRF=testDataFn(Q12009)
trainDataRF<-cleanFile(trainDataRF)
rfModel=randomForestAlgo(trainDataRF)
testData=cleanFile(Q22009)
predictedRF<-predict(rfModel,newdata =testData)
accuracy(predictedRF,testData$OrignialInterestRate)

trainDataRF = testDataFn(Q22009)
trainDataRF<-cleanFile(trainDataRF)
rfModel= randomForestAlgo(trainDataRF)
testData=cleanFile(Q32009)
predictedRF<-predict(rfModel,newdata =testData)
accuracy(predictedRF,testData$OrignialInterestRate)

trainDataRF = testDataFn(Q32009)
trainDataRF<-cleanFile(trainDataRF)
rfModel= randomForestAlgo(trainDataRF)
testData=cleanFile(Q42009)
predictedRF<-predict(rfModel,newdata =testData)
accuracy(predictedRF,testData$OrignialInterestRate)

#####Running for 1999 Year#######
trainDataRF=testDataFn(Q11999)
trainDataRF<-cleanFile(trainDataRF)
rfModel=randomForestAlgo(trainDataRF)
testData=cleanFile(Q21999)
predictedRF<-predict(rfModel,newdata =testData)
accuracy(predictedRF,testData$OrignialInterestRate)

trainDataRF = testDataFn(Q21999)
trainDataRF<-cleanFile(trainDataRF)
rfModel= randomForestAlgo(trainDataRF)
testData=cleanFile(Q31999)
predictedRF<-predict(rfModel,newdata =testData)
accuracy(predictedRF,testData$OrignialInterestRate)

trainDataRF = testDataFn(Q31999)
trainDataRF<-cleanFile(trainDataRF)
rfModel= randomForestAlgo(trainDataRF)
testData=cleanFile(Q41999)
predictedRF<-predict(rfModel,newdata =testData)
accuracy(predictedRF,testData$OrignialInterestRate)



#####Running for 2013 Year#######
trainDataRF=testDataFn(Q12013)
trainDataRF<-cleanFile(trainDataRF)
rfModel=randomForestAlgo(trainDataRF)
testData=cleanFile(Q22013)
predictedRF<-predict(rfModel,newdata =testData)
accuracy(predictedRF,testData$OrignialInterestRate)

trainDataRF = testDataFn(Q22013)
trainDataRF<-cleanFile(trainDataRF)
rfModel= randomForestAlgo(trainDataRF)
testData=cleanFile(Q32013)
predictedRF<-predict(rfModel,newdata =testData)
accuracy(predictedRF,testData$OrignialInterestRate)

trainDataRF = testDataFn(Q32013)
trainDataRF<-cleanFile(trainDataRF)
rfModel= randomForestAlgo(trainDataRF)
testData=cleanFile(Q42013)
predictedRF<-predict(rfModel,newdata =testData)
accuracy(predictedRF,testData$OrignialInterestRate)

