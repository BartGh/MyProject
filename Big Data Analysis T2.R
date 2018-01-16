#Library
#More expanation
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
    usePackage("corrplot")
  
}
#Function
{
  Delete.EmptyCol <- function(Table_used)
  {
    #Create the initial new table
    clean_table<-Table_used
    #Number of column tested
    col_n<-ncol(Table_used)
    #Initialize the number of column selected
    cpt<- 0
    #Loop of test and removing
    for(i in 1:col_n){
      SUM.col<-sum(Table_used[,i])
      if(SUM.col==0){
        clean_table[,i-cpt]<-NULL
        cpt<-cpt+1
        print(i)
        print(SUM.col)
      }
    }
    print(paste0("Number of deleted column : ", cpt))
    return(clean_table)
  }
}
#Data Upload
{
  iPhoneLargeMatrix <- read_delim("~/Desktop/MyOutputTest2/iPhoneLargeMatrix.csv",";", escape_double = FALSE, trim_ws = TRUE)
  GalaxyLargeMatrix <- read_delim("~/Desktop/MyOutputTest2/GalaxyLargeMatrix.csv",";", escape_double = FALSE, trim_ws = TRUE)
  Source.Data<-GalaxyLargeMatrix
}
#Data Analysis
{
  #Basic Analysis
  {
    str(Source.Data)
    summary(Source.Data)
    #The attributes are all integer 
    summary(Source.Data$iphoneSentiment)
    #The iphone Sentiment goes from -233 to 576 the Mean is slightly positive 
    sum(is.na(Source.Data))
  }
  #Preprocess
  {
    New.Table<-Source.Data
    New.Table$id<-NULL
    New.Table<-Delete.EmptyCol(New.Table)
  }
  #Correlation Matrix
  {

    M.cor<-cor(New.Table)
    Phone.cor<-colnames(colnames(M.cor))
    Phone.cor<-data.frame(M.cor[,1])
    corrplot(M.cor, order="hclust")
  }
  #Deteremine the 
  {
  
    fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)#summaryFunction = twoClassSummary classProbs = TRUE
    rf_model<-train(galaxySentiment~.,data=New.Table,method="Rborist",
                  trControl=fitControl,
                  prox=TRUE,allowParallel=TRUE)
    predictors(rf_model)
    }
 
  
}


