#Library
{
  usePackage <- function(p) 
  {
    if (!is.element(p, installed.packages()[,1]))
      install.packages(p, dep = TRUE)
    require(p, character.only = TRUE)
  }
  
  usePackage("ggplot2")
  usePackage("dplyr")
  usePackage("caret")
  usePackage("readr")
  

}
#Function
{
  Signal_cleaning <- function(Table_used)
  {   #Create the initial new table
      clean_table<-Table_used
      #Number of column tested
      col_n<-ncol(clean_table)
      #Create the signal/nonSignal table
      SignalOnOff<-Table_used[,1:col_n]> 0
      SignalOnOff<-SignalOnOff*1
      #Initialize the number of column selected
      cpt<-0
      #Loop of test and removing
      for(i in 1:col_n){
        Signal_test<-max(SignalOnOff[,i])
        if(Signal_test==0){
          clean_table[,i-cpt]<-NULL
          cpt<-cpt+1
        }
        if(any(is.na(Signal_test))==TRUE){
          print("NA in the Data")
        }
      }
    print(paste0("Number of non signal WPA : ", cpt," Number Signal WPA : ",ncol(clean_table)))
    return(clean_table)
    
  }
  Signal_cleaning_abs <- function(Table_used)
  {   #Create the initial new table
    clean_table<-Table_used
    #Number of column tested
    col_n<-ncol(clean_table)
    #Create the signal/nonSignal table
    SignalOnOff<-Table_used[,1:col_n]> -110
    SignalOnOff<-SignalOnOff*1
    #Initialize the number of column selected
    cpt<-0
    #Loop of test and removing
    for(i in 1:col_n){
      Signal_test<-max(SignalOnOff[,i])
      if(Signal_test== 0){
        clean_table[,i-cpt]<-NULL
        cpt<-cpt+1
      }
      if(any(is.na(Signal_test))==TRUE){
        print("NA in the Data")
      }
    }
    print(paste0("Number of non signal WPA : ", cpt," Number Signal WPA : ",ncol(clean_table)))
    return(clean_table)
    
  }
  Signal_Binary <- function(Table_used)
  {   #Create the initial new table
    clean_table<-Table_used
    #Number of column tested
    col_n<-ncol(clean_table)
    #Create the signal/nonSignal table
    SignalOnOff<-Table_used[,1:col_n]<=0
    SignalOnOff<-SignalOnOff*1
    return(SignalOnOff)
  }
}
#Data Set 
{
  InitialTrainningData <- read_csv("~/Desktop/UJIndoorLoc/trainingData.csv")
  #Validation data
  validationData <- read_csv("~/Desktop/UJIndoorLoc/validationData.csv")
}

#MODELING 
  #GENERAL PREPROCCESS
  {
  trainingData<-InitialTrainningData
  trainingData[trainingData==100]<- -110
  }

  Signal<-trainingData[,1:520]
  Info<-trainingData[,521:529]
  
  dfNorm<-data.frame(t(apply(Signal, 1, function(x)(x-min(x))/(max(x)-min(x)))))
  dfLog<-dfNorm+1
  dfLog<-log(dfLog)
  dfLog<-data.frame(dfLog,Info)
  Signal_NA<-dfLog[,1:520]
  dfLog<-data.frame(dfLog[complete.cases(dfLog), ])
  
  #STEP 1 BUILDING
  {
    #Data Cleaning & Preprocessing BUILDING
    {
      #Create Data set
      
      Signal<-dfLog[,1:520]
      Info<-dfLog[,521:529]
      
      
      #Delete the non signal WPA column 
      #this step will reduce the data quantity
      Clean_table<-Signal_cleaning(Signal)
      BUILDINGID<-Info$BUILDINGID
      Building_Table<-data.frame(Clean_table,BUILDINGID)
      Building_Table$BUILDINGID<-as.factor(Building_Table$BUILDINGID)
      set.seed(1000)
      BT<-sample_n(Building_Table, 1000)
  
    }
    #Run Model BUILDING
    {
      #KNN Model
      fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)#summaryFunction = twoClassSummary classProbs = TRUE
      knnfit <- train(BUILDINGID ~ .,data = BT, method = "knn",trControl=fitControl,tuneLength = 10,preProcess=c("center","scale"))
      knnfit
      knnfit.Buiding<-knnfit
      knnfit.Buiding
      saveRDS(knnfit.Buiding,"KNN B TASK 4")
      #svm.model
      fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)#summaryFunction = twoClassSummary classProbs = TRUE
      svm.model<-train(BUILDINGID ~ .,data = BT, method = "svmLinear3",trControl=fitControl,preProcess = c("center", "scale"),tuneLength = 10)
      svm.model.Building<-svm.model
      svm.model.Building
      saveRDS(svm.model.Building,"SVM B TASK 4")
      #Random forest
      fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)#summaryFunction = twoClassSummary classProbs = TRUE
      rf_model<-train(BUILDINGID~.,data=BT,method="Rborist",
                      trControl=fitControl,
                      prox=TRUE,allowParallel=TRUE)
      rf_model.Building<-rf_model
      rf_model
      saveRDS(rf_model.Building,"Random Forest B TASK 4")
      
      #GBM
      #fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)#summaryFunction = twoClassSummary classProbs = TRUE
      #GBM <- train(BUILDINGID ~ .,data = BT, method = "gbm",trControl=fitControl,tuneLength = 10)
      #GBM.Building<-GBM
      #GBM.Building
      #saveRDS(GBM.Building,"GBM B TASK 4")
    }
    #Compare Model BUILDING
    {

      results <- resamples(list(KNN=knnfit,RF=rf_model,SVM=svm.model))
      summary(results)

      # boxplots of results
      bwplot(results,scales = list(relation = "free"),xlim = list(c(1, 1.2), c(1, 1.2),c(0, 1)))
      # dot plots of results
      dotplot(results,scales = list(relation = "free"),xlim = list(c(0, 1000), c(0, 1000),c(0, 1)))
    }
    #Predict BUILDING
    {

      svm.pred  <- predict(svm.model, Signal_NA)
      #rf.pred  <- predict(rf_model, Clean_table)
      #knn.pred  <- predict(knnfit, Signal_NA)
      #GBM.pred<-predict(GBM, Clean_table)
      #Prediction_Table<-data.frame(Info,round(svm.pred,0),round(rf.pred,0),round(knn.pred,0))#GBM.pred
      #KNN Model Test
      TEST<-Info$BUILDINGID==svm.pred
      Prediction_Table<-data.frame(Signal,Info,svm.pred,TEST)
      Prediction_Table.false<-Prediction_Table[which(Prediction_Table$TEST==FALSE),]

      ggplot()+geom_point(data=Prediction_Table,aes(x =LATITUDE ,y=LONGITUDE,colour = factor(TEST)))      #GBM.pred
      
    }
  }
  #STEP 2 LAT/LONG/FLOOR
  {
    #Use the predictive 
    NEW.trainingData<-trainingData
    
    #Removing non signal 
    NR<-nrow(NEW.trainingData)
    line<-NEW.trainingData[1,1:520]
    NoNS<-vector(mode = "logical", length = NR)
    for(i in 1:NR){
      line<-NEW.trainingData[i,1:520]
      if(max(line)==min(line)){NoNS[i]<-FALSE}
      else{NoNS[i]<-TRUE}
    }
    NoNS<-as.data.frame(NoNS)
    NEW.trainingData<-data.frame(NEW.trainingData,NoNS)
    NEW.trainingData<-NEW.trainingData[which(NEW.trainingData$NoNS==TRUE),]
    NEW.trainingData$BUILDINGID<-svm.pred

    #Data Cleaning & Preprocessing LAT/LONG
    {
      B<-NEW.trainingData
      #Create Data set
      Signal<-B[,1:520]
      Info<-B[,521:529]
      #Delete the non signal WPA column 
      #this step will reduce the data quantity
      Clean_table<-Signal_cleaning_abs(Signal)
      BUILDINGID<-as.numeric(Info$BUILDINGID)
      summary(BUILDINGID)
      LATITUDE<-Info$LATITUDE
      LONGITUDE<-Info$LONGITUDE
      FLOOR<-Info$FLOOR
      
    }
    #Start with Latitude
    Building_Table<-data.frame(Clean_table,BUILDINGID,LATITUDE)
    set.seed(1000)
    BT<-sample_n(Building_Table, 1000)
    #Run Model LAT
    {
      #KNN Model
      fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)#summaryFunction = twoClassSummary classProbs = TRUE
      knnfit.LAT <- train(LATITUDE ~ .,data = BT, method = "knn",trControl=fitControl,tuneLength = 10,preProcess=c("center","scale"))
      knnfit.LAT
      saveRDS(knnfit.LAT,"KNN LAT TASK 4")
      
      #Random forest
      fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)#summaryFunction = twoClassSummary classProbs = TRUE
      rf_model<-train(LATITUDE~.,data=BT,method="Rborist",
                      trControl=fitControl,
                      prox=TRUE,allowParallel=TRUE)
      rf_model.LAT<-rf_model
      rf_model.LAT
      saveRDS(rf_model.LAT,"Random Forest LAT TASK 4")
      
      #svm.model
      fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)#summaryFunction = twoClassSummary classProbs = TRUE
      svm.model<-train(LATITUDE~ .,data =BT, method = "svmLinear3",trControl=fitControl,preProcess = c("center", "scale"),tuneLength = 10)
      svm.model.LAT<-svm.model
      print(svm.model.LAT)
      saveRDS(svm.model.LAT,"SVM LAT TASK 4")
      
      #comparing
      results <- resamples(list(KNN=knnfit.LAT,RF=rf_model.LAT,SVM=svm.model.LAT))
      summary(results)
      kappa(knn.pred.LAT)
      knn.pred.LAT
      
    }

    #Then Longitude
    knn.pred.LAT  <- predict(knnfit.LAT, data.frame(Clean_table,BUILDINGID))#HHHHHHHHHHHHHHHHHHHHHHHH
    Building_Table<-data.frame(Clean_table,BUILDINGID,knn.pred.LAT,LONGITUDE)
    set.seed(1000)
    BT<-sample_n(Building_Table, 1000)
      
    #Run Model LONG
    {
      #KNN Model
      fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)#summaryFunction = twoClassSummary classProbs = TRUE
      knnfit.LON <- train(LONGITUDE ~ .,data = BT, method = "knn",trControl=fitControl,tuneLength = 10,preProcess=c("center","scale"))
      knnfit.LON
      saveRDS(knnfit.LON,"KNN LONG TASK 4")
      #Random forest
      fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)#summaryFunction = twoClassSummary classProbs = TRUE
      rf_model<-train(LONGITUDE~.,data=BT,method="Rborist",
                      trControl=fitControl,
                      prox=TRUE,allowParallel=TRUE)
      rf_model.LON<-rf_model
      rf_model.LON
      saveRDS(rf_model.LAT,"Random Forest LAT TASK 4")
      
      #comparing
      results <- resamples(list(KNN=knnfit.LON,RF=rf_model.LON))
      summary(results)
      kappa(knn.pred.LAT)
      knn.pred.LAT
      
    }
    #Then FLOOR
    knn.pred.LON  <- predict(knnfit.LON, data.frame(Clean_table,BUILDINGID,knn.pred.LAT))
    Building_Table<-data.frame(Clean_table,FLOOR,BUILDINGID)
    set.seed(1000)
    BT<-sample_n(Building_Table, 1000)
    
    #Run Model FLOOR
    {
      #KNN Model
      fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)#summaryFunction = twoClassSummary classProbs = TRUE
      knnfit.FLO <- train(FLOOR ~ .,data = BT, method = "knn",trControl=fitControl,tuneLength = 10,preProcess=c("center","scale"))
      knnfit.FLO
      saveRDS(knnfit.FLO,"KNN FLO TASK 4")
      #Random forest
      fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)#summaryFunction = twoClassSummary classProbs = TRUE
      rf_model<-train(FLOOR~.,data=BT,method="Rborist",
                      trControl=fitControl,
                      prox=TRUE,allowParallel=TRUE)
      rf_model.FLO<-rf_model
      rf_model.FLO
      saveRDS(rf_model.FLO,"Random Forest FLO TASK 4")
      
      #svm.model
      fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)#summaryFunction = twoClassSummary classProbs = TRUE
      svm.model<-train(FLOOR~ .,data =BT, method = "svmLinear3",trControl=fitControl,preProcess = c("center", "scale"),tuneLength = 10)
      svm.model.FLO<-svm.model
      print(svm.model.FLO)
      saveRDS(svm.model.LAT,"SVM FLO TASK 4")
      
      #GB.model
      fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)#summaryFunction = twoClassSummary classProbs = TRUE
      GBM <- train(FLOOR ~ .,data = BT, method = "gbm",trControl=fitControl,tuneLength = 10)
      
      summary(GBM)
      
      
      #comparing
      results <- resamples(list(KNN=knnfit.FLO,RF=rf_model.FLO,GBM=GBM,SVM=svm.model.FLO))
      summary(results)
      kappa(knn.pred.LAT)
      knn.pred.LAT
      
    }
    rf.pred.FLO  <- predict(rf_model.FLO, data.frame(Clean_table,BUILDINGID,knn.pred.LAT,knn.pred.LON))
    rf.pred.FLO<-round(rf.pred.FLO,digits = 0)
  }
  

#First plot 
#Creer un tableau clean pour comparer les prediction du reste
prediction_Table_final<-data.frame(NEW.trainingData,TEST,knn.pred.LAT,knn.pred.LON,rf.pred.FLO)

ggplot()+geom_point(data=prediction_Table_final,aes(x =knn.pred.LAT ,y=knn.pred.LON,colour = factor(TEST)))      #GBM.pred

Error.LON<-abs(prediction_Table_final$LONGITUDE-prediction_Table_final$knn.pred.LON)
Error.LAT<-abs(prediction_Table_final$LATITUDE-prediction_Table_final$knn.pred.LAT)
Error.Floor<-prediction_Table_final$FLOOR==prediction_Table_final$rf.pred.FLO
Real.LON<-prediction_Table_final$LONGITUDE
Real.LAT<-prediction_Table_final$LATITUDE
error_in_meter<-sqrt(Error.LAT^2+Error.LON^2)
error_in_meter<-error_in_meter<10

ERROR_TABLE<-data.frame(Real.LON,Real.LAT,TEST,Error.LON,Error.LAT,Error.Floor,TEST,knn.pred.LAT,knn.pred.LON,rf.pred.FLO,error_in_meter)
summary(ERROR_TABLE)



ggplot()+geom_point(data=ERROR_TABLE,aes(x =Real.LAT ,y=Real.LON))
ggplot()+geom_point(data=ERROR_TABLE,aes(x =knn.pred.LAT ,y=knn.pred.LON,colour = factor(error_in_meter)))    #GBM.pred



