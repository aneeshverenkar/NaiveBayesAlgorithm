
NaiveBayesLearn <- function(Attributes,YesData,NoData,m){
  
  i <- 1
  attributeNames <- colnames(YesData)
  
  for (attribute in YesData) {
    tb <- matrix(rep(NA, length(unique(Attributes[,i]))),ncol=1,byrow=TRUE)
    
    colnames(tb) <- c(attributeNames[i])
    rName <- unique(Attributes[,i])
    
    rownames(tb)[1:length(rName)] <- paste(rName[1:length(rName)])
    
    tbRow <- 1
    for (value in rName) {
      p <- 1/length(rName)
      n <- nrow(YesData)
      nc <- sum(attribute == value)
      tb[tbRow,1] <- (nc + m*p)/(n + m)
      tbRow <- tbRow + 1
    }
    
    YesAttributesList[[i]] <<- tb
    i <- i +1 
    
  }
  
  
  i <- 1
  for (attribute in NoData) {
    tb <- matrix(rep(NA, length(unique(Attributes[,i]))),ncol=1,byrow=TRUE)
    
    colnames(tb) <- c(attributeNames[i])
    rName <- unique(Attributes[,i])
    rownames(tb)[1:length(rName)] <- paste(rName[1:length(rName)])
    tbRow <- 1
    for (value in rName) {
      p <- 1/length(rName)  # prior estimate p set to 1/number of possible values for attribute 
      n <- nrow(NoData)
      nc <- sum(attribute == value)
      tb[tbRow,1] <- (nc + m*p)/(n + m)
      tbRow <- tbRow + 1
    }
    
    NoAttributesList[[i]] <<- tb
    i <- i +1 
    
  }
  
}


testNaiveBayes <- function(testData){
  correct <-0
  for(row in 1:nrow(testData)) {  # for each row in the data set
    # instance is the current row that's being looked at
    instance <- testData[row,]
    probYes <- calculateProbYes(instance)
    probNo <- calculateProbNo(instance)
    
    
    if (probYes>=probNo) {
      #print("1")
      if (instance[ncol(instance)]=='1') {
        #print("correctly classified!")
        correct <- correct + 1
      }else{
        #print("wrong!")
      }
    }else{
      #print("0")
      if (instance[ncol(instance)]=='0') {
        #print("correctly classified!")
        correct <- correct + 1
      }else{
        #print("wrong!")
      }
    }
    
  }
  
  success <- (correct/nrow(testData))*100
  print(paste0("Accuracy:  ",success,"%"))
  return(success)
  
}


calculateProbYes <- function(instance){
  
  probYes <- PYes
  for (i in 1:(ncol(instance)-1)) {
    attProbTable <- YesAttributesList[[i]]
    iVal = instance[1,i]
    probYes <- probYes*attProbTable[[which(rownames(attProbTable)== iVal)]]  
    
  }
  return(probYes)
}

calculateProbNo <- function(instance){
  
  probNo <- PNo
  for (i in 1:(ncol(instance)-1)) {
    attProbTable <- NoAttributesList[[i]]
    iVal = instance[1,i]
    probNo <- probNo*attProbTable[[which(rownames(attProbTable)== iVal)]]  
  }
  return(probNo)
}

makeTestSet <- function(df, size) {
  len <- 1:length(df[,1])
  randRows <- sample(len, size, replace=F)
  return(randRows)
}


RunNaiveBayes <- function(csv,m){
  Data <- read.csv(file = csv, head = TRUE, sep = ",")
  randRows <- makeTestSet(Data,30) 
  testData <- Data[randRows,]
  trainingData <- Data[-randRows,]
  
  class <- trainingData[,ncol(trainingData)] #target values vector
  targetValues <- unique(class)
  numberOfTargetValues <- length(targetValues) #number of target values
  
  
  target <- 'class'
  Attributes <- trainingData
  Attributes[target] <- NULL
  
  a <- table(class)
  No <- a[which(names(a)=="0")]
  Yes <- a[which(names(a)=="1")]
  
  PNo <<- No/length(class)
  PYes <<- Yes/length(class)
  
  YesData <- trainingData[trainingData[target]=='1',]
  NoData <- trainingData[trainingData[target]=='0',]
  
  drops <- c(target)
  YesData <- YesData[ , !(names(YesData) %in% drops)]
  NoData <- NoData[ , !(names(NoData) %in% drops)]
  
  YesAttributesList <<- vector(mode = "list", length =  ncol(YesData))
  NoAttributesList <<- vector(mode = "list", length =  ncol(NoData))
  
  NaiveBayesLearn(Attributes,YesData,NoData,m)
  accuracy <- testNaiveBayes(testData)
  return(accuracy)
}

run <- function(csv,m,iterations){
  accuracySum <- 0
  for (i in 1:iterations) {
    accuracy <- RunNaiveBayes(csv,m)
    accuracySum <- accuracySum +accuracy
  }
  percentage <- accuracySum/iterations
  print(paste0("Average accuracy:  ",percentage,"%"))
}
