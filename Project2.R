#211805048-Recep Sami Özdemir, 211805054-Cemre Polat
#Please change a directory of setwd
#Also please use given DatasetNA.txt inside of .zip file
setwd("C://Users//recep//Documents//Ders Notları//4. Dönem//Statistical Programming//Project_2_211805048_211805054")
datas <- data.frame(read.table("DatasetNA.txt"))
colNames <- c("IdNo", "Group", "Gender", "Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8")
colnames(datas) <- colNames

#Number of Observations
observationFun <- function(x) {
  observation <- length(x)
  return(observation)
}

#Minimum
minFun <- function(x) {
  minVar <- x[1]
  for (i in 2:length(x)) {
    if (!is.na(x[i]) && x[i] < minVar) {
      minVar <- x[i]
    }
  }
  return(minVar)
}

#Maximum
maxFun <- function(x) {
  maxVar <- x[1]
  for (i in 2:length(x)) {
    if (!is.na(x[i]) && x[i] > maxVar) {
      maxVar <- x[i]
    }
  }
  return(maxVar)
}

#Range
rangeFun <- function(x) {
  return(c(minFun(x), maxFun(x)))
}

#Sum
sumFun <- function(x) {
  vars <- 0
  for (i in 1:length(x)) {
    if(!(is.na(x[i]))) {
      vars <- vars + x[i]
    }
  }
  return(vars)
}

#Count NA variables
countNa <- function(x) {
  countNa <- 0
  for(i in 1:length(x)) {
    if(is.na(x[i])) {
      countNa <- countNa + 1
    }
  }
  return(countNa)
}

#Mean
meanFun <- function(x) {
  mean <- sumFun(x) / (length(x) - countNa(x))
  return(mean)
}

#Median
medianFun <- function(x) {
  variables <- sort(x)
  rank <- 0
  median <- 0
  if (length(variables) %% 2 == 0) {
    rank <- (length(variables) / 2) + 1
    median <- (variables [rank] + variables[rank - 1]) / 2 
  } else {
    rank <- (length(variables) + 1) / 2
    median <- variables[rank]
  }
  return(median)
}

#Sum of Squares
sumOfSquaresFun <- function(x) {
  vars <- 0
  for (i in 1:length(x)) {
    if(!(is.na(x[i]))) {
      vars <- vars + (x[i]^2)
    }
  }
  return(vars)
}

#Variance
varianceFun <- function(x) {
  variance <- 0
  for(i in 1:length(x)) {
    if(!is.na(x[i])) { 
      variance <- variance + ((x[i] - meanFun(x))^2)  
    }
  }
  variance <- variance / ((length(x) - countNa(x)) - 1)
  return(variance)
}

#Standard Deviation
standardDeviationFun <- function(x) {
  return(sqrt(varianceFun(x)))
}

#Cross Product
crossProductFun <- function(x,y) {
  
  matrixX <- 0
  matrixY <- 0
  
  lengthX <- length(x) - countNa(x)
  lengthY <- length(y) - countNa(y)
  
  varX <- c(NULL)
  varY <- c(NULL)
  
  indexX <- 1
  indexY <- 1
  
  for(i in 1:length(x)) {
    if(!is.na(x[i])) {
      varX[indexX] <- x[i]
      indexX <- indexX + 1
    }
  }
  for(i in 1:length(y)) {
    if(!is.na(y[i])) {
      varY[indexY] <- y[i]
      indexY <- indexY + 1
    }
  }
  matrixX <- matrix(data=varX, nrow = lengthX, ncol=1)
  matrixY <- matrix(data=varY, nrow = lengthY, ncol=1)
  
  if(length(x) != length(y) || lengthX != lengthY) {
    return ("Cross Product is incalculable.")
  } else {
    return(t(matrixX) %*% matrixY)
  }
}

#Covariance
covarianceFun <- function(x,y) {
  meanX <- meanFun(x)
  meanY <- meanFun(y)
  
  lengthX <- length(x) - countNa(x)
  lengthY <- length(y) - countNa(y)
  
  if(length(x) == length(y) && lengthX == lengthY) {
      return (sumFun((na.omit(x) - meanX) * (na.omit(y) - meanY)) / (lengthX - 1))
  } else {
    return ("Covariance is incalculable.")
  }
}

#Corraletions
correlationFun <- function(x,y) {
  
  lengthX <- length(x) - countNa(x)
  lengthY <- length(y) - countNa(y)
  
  if (length(x) == length(y) && lengthX == lengthY) {
      return(covarianceFun(x,y) / (standardDeviationFun(x) * standardDeviationFun(y)))
  } else {
    return("Correlation is incalculable.")
  }
}

#These operations group by group

observationGroup <- function(x) {
  group1Count <- 0
  group2Count <- 0
  group3Count <- 0
  group4Count <- 0
  for(i in 1:length(x)) {
      if(datas$Group[i] == "Group1") {
          group1Count <- group1Count + 1
      } else if (datas$Group[i] == "Group2") {
          group2Count <- group2Count + 1
      } else if (datas$Group[i] == "Group3") {
          group3Count <- group3Count + 1
      } else {
          group4Count <- group4Count + 1
      }
  }
  return(list(Group1 = group1Count, 
              Group2 = group2Count, 
              Group3 = group3Count, 
              Group4 = group4Count))
}

minGroup <- function(x) {
  group1Values <- c(NULL)
  group2Values <- c(NULL)
  group3Values <- c(NULL)
  group4Values <- c(NULL)
  
  index1 <- 1
  index2 <- 1
  index3 <- 1
  index4 <- 1
  
  minGroup1 <- 0
  minGroup2 <- 0
  minGroup3 <- 0
  minGroup4 <- 0
  
  for(i in 1:length(x)) {
    if (!is.na(x[i])) {
      if (datas$Group[i] == "Group1"){
        group1Values[index1] <- x[i]
        index1 <- index1 + 1
      } else if (datas$Group[i] == "Group2") {
        group2Values[index2] <- x[i]
        index2 <- index2 + 1
      } else if (datas$Group[i] == "Group3") {
        group3Values[index3] <- x[i]
        index3 <- index3 + 1
      } else {
        group4Values[index4] <- x[i]
        index4 <- index4 + 1
      }
    }  
  }
  
  minGroup1 <- minFun(group1Values)
  minGroup2 <- minFun(group2Values)
  minGroup3 <- minFun(group3Values)
  minGroup4 <- minFun(group4Values)
  
  return(list(Group1 = minGroup1, 
              Group2 = minGroup2, 
              Group3 = minGroup3, 
              Group4 = minGroup4))
}

maxGroup <- function(x) {
  group1Values <- c(NULL)
  group2Values <- c(NULL)
  group3Values <- c(NULL)
  group4Values <- c(NULL)
  
  index1 <- 1
  index2 <- 1
  index3 <- 1
  index4 <- 1
  
  maxGroup1 <- 0
  maxGroup2 <- 0
  maxGroup3 <- 0
  maxGroup4 <- 0
  
  for(i in 1:length(x)) {
    if (!is.na(x[i])) {
      if (datas$Group[i] == "Group1"){
        group1Values[index1] <- x[i]
        index1 <- index1 + 1
      } else if (datas$Group[i] == "Group2") {
        group2Values[index2] <- x[i]
        index2 <- index2 + 1
      } else if (datas$Group[i] == "Group3") {
        group3Values[index3] <- x[i]
        index3 <- index3 + 1
      } else {
        group4Values[index4] <- x[i]
        index4 <- index4 + 1
      }
    }  
  }
  
  maxGroup1 <- maxFun(group1Values)
  maxGroup2 <- maxFun(group2Values)
  maxGroup3 <- maxFun(group3Values)
  maxGroup4 <- maxFun(group4Values)
  
  return(list(Group1 = maxGroup1, 
              Group2 = maxGroup2,
              Group3 = maxGroup3, 
              Group4 = maxGroup4))
}

rangeGroup <- function(x) {
  group1Values <- c(NULL)
  group2Values <- c(NULL)
  group3Values <- c(NULL)
  group4Values <- c(NULL)
  
  index1 <- 1
  index2 <- 1
  index3 <- 1
  index4 <- 1
  
  minGroup1 <- 0
  minGroup2 <- 0
  minGroup3 <- 0
  minGroup4 <- 0
  
  maxGroup1 <- 0
  maxGroup2 <- 0
  maxGroup3 <- 0
  maxGroup4 <- 0
  
  for(i in 1:length(x)) {
    if (!is.na(x[i])) {
      if (datas$Group[i] == "Group1"){
        group1Values[index1] <- x[i]
        index1 <- index1 + 1
      } else if (datas$Group[i] == "Group2") {
        group2Values[index2] <- x[i]
        index2 <- index2 + 1
      } else if (datas$Group[i] == "Group3") {
        group3Values[index3] <- x[i]
        index3 <- index3 + 1
      } else {
        group4Values[index4] <- x[i]
        index4 <- index4 + 1
      }
    }  
  }
  
  minGroup1 <- minFun(group1Values)
  minGroup2 <- minFun(group2Values)
  minGroup3 <- minFun(group3Values)
  minGroup4 <- minFun(group4Values)
  
  maxGroup1 <- maxFun(group1Values)
  maxGroup2 <- maxFun(group2Values)
  maxGroup3 <- maxFun(group3Values)
  maxGroup4 <- maxFun(group4Values)
  
  return(list(Group1 = c(minGroup1, maxGroup1), 
              Group2 = c(minGroup2, maxGroup2), 
              Group3 = c(minGroup3, maxGroup3), 
              Group4 = c(minGroup4, maxGroup4)))
}

sumGroup <- function(x) {
  group1Sum <- 0
  group2Sum <- 0
  group3Sum <- 0
  group4Sum <- 0
  
  for(i in 1:length(x)) {
    if(!is.na(x[i])) {
      if (datas$Group[i] == "Group1") {
        group1Sum <- group1Sum + x[i]
      } else if(datas$Group[i] == "Group2") {
        group2Sum <- group2Sum + x[i]
      } else if (datas$Group[i] == "Group3") {
        group3Sum <- group3Sum + x[i]
      } else {
        group4Sum <- group4Sum + x[i]
      }
    }
  }
  return (list(Group1 = group1Sum, 
               Group2 = group2Sum, 
               Group3 = group3Sum, 
               Group4 = group4Sum))
}

meanGroup <- function(x) {
  meanGroup1 <- 0
  meanGroup2 <- 0
  meanGroup3 <- 0
  meanGroup4 <- 0
  
  group1Count <- 0
  group2Count <- 0
  group3Count <- 0
  group4Count <- 0
  
  group1Sum <- 0
  group2Sum <- 0
  group3Sum <- 0
  group4Sum <- 0
  
  for(i in 1:length(x)) {
    if(!is.na(x[i])) {
      if (datas$Group[i] == "Group1") {
        group1Count <- group1Count + 1
        group1Sum <- group1Sum + x[i]
        meanGroup1 <- group1Sum / group1Count
      } else if (datas$Group[i] == "Group2") {
        group2Count <- group2Count + 1
        group2Sum <- group2Sum + x[i]
        meanGroup2 <- group2Sum / group2Count
      } else if (datas$Group[i] == "Group3") {
        group3Count <- group3Count + 1
        group3Sum <- group3Sum + x[i]
        meanGroup3 <- group3Sum / group3Count
      } else {
        group4Count <- group4Count + 1
        group4Sum <- group4Sum + x[i]
        meanGroup4 <- group4Sum / group4Count
      }
    } 
  }
  return (list(Group1 = meanGroup1, 
               Group2 = meanGroup2, 
               Group3 = meanGroup3, 
               Group4 = meanGroup4))
}

medianGroup <- function(x) {
  group1Values <- c(NULL)
  group2Values <- c(NULL)
  group3Values <- c(NULL)
  group4Values <- c(NULL)
  
  index1 <- 1
  index2 <- 1
  index3 <- 1
  index4 <- 1
  
  medianGroup1 <- 0
  medianGroup2 <- 0
  medianGroup3 <- 0
  medianGroup4 <- 0
  
  for(i in 1:length(x)) {
    if (!is.na(x[i])) {
      if (datas$Group[i] == "Group1"){
        group1Values[index1] <- x[i]
        index1 <- index1 + 1
      } else if (datas$Group[i] == "Group2") {
        group2Values[index2] <- x[i]
        index2 <- index2 + 1
      } else if (datas$Group[i] == "Group3") {
        group3Values[index3] <- x[i]
        index3 <- index3 + 1
      } else {
        group4Values[index4] <- x[i]
        index4 <- index4 + 1
      }
    }  
  }
  medianGroup1 <- medianFun(group1Values)
  medianGroup2 <- medianFun(group2Values)
  medianGroup3 <- medianFun(group3Values)
  medianGroup4 <- medianFun(group4Values)
  
  return (list(Group1 = medianGroup1, 
               Group2 = medianGroup2, 
               Group3 = medianGroup3, 
               Group4 = medianGroup4))
}

sumOfSquaresGroup <- function(x) {
  group1Values <- c(NULL)
  group2Values <- c(NULL)
  group3Values <- c(NULL)
  group4Values <- c(NULL)
  
  index1 <- 1
  index2 <- 1
  index3 <- 1
  index4 <- 1
  
  sumOfSquaresGroup1 <- 0
  sumOfSquaresGroup2 <- 0
  sumOfSquaresGroup3 <- 0
  sumOfSquaresGroup4 <- 0
  
  for(i in 1:length(x)) {
    if (!is.na(x[i])) {
      if (datas$Group[i] == "Group1"){
        group1Values[index1] <- x[i]
        index1 <- index1 + 1
      } else if (datas$Group[i] == "Group2") {
        group2Values[index2] <- x[i]
        index2 <- index2 + 1
      } else if (datas$Group[i] == "Group3") {
        group3Values[index3] <- x[i]
        index3 <- index3 + 1
      } else {
        group4Values[index4] <- x[i]
        index4 <- index4 + 1
      }
    }  
  }
  
  sumOfSquaresGroup1 <- sumOfSquaresFun(group1Values)
  sumOfSquaresGroup2 <- sumOfSquaresFun(group2Values)
  sumOfSquaresGroup3 <- sumOfSquaresFun(group3Values)
  sumOfSquaresGroup4 <- sumOfSquaresFun(group4Values)
  
  return (list(Group1 = sumOfSquaresGroup1, 
               Group2 = sumOfSquaresGroup2, 
               Group3 = sumOfSquaresGroup3, 
               Group4 = sumOfSquaresGroup4))
}

varianceGroup <- function (x) {
  group1Values <- c(NULL)
  group2Values <- c(NULL)
  group3Values <- c(NULL)
  group4Values <- c(NULL)
  
  index1 <- 1
  index2 <- 1
  index3 <- 1
  index4 <- 1
  
  varianceGroup1 <- 0
  varianceGroup2 <- 0
  varianceGroup3 <- 0
  varianceGroup4 <- 0
  
  for(i in 1:length(x)) {
    if (!is.na(x[i])) {
      if (datas$Group[i] == "Group1"){
        group1Values[index1] <- x[i]
        index1 <- index1 + 1
      } else if (datas$Group[i] == "Group2") {
        group2Values[index2] <- x[i]
        index2 <- index2 + 1
      } else if (datas$Group[i] == "Group3") {
        group3Values[index3] <- x[i]
        index3 <- index3 + 1
      } else {
        group4Values[index4] <- x[i]
        index4 <- index4 + 1
      }
    }  
  }
  varianceGroup1 <- varianceFun(group1Values)
  varianceGroup2 <- varianceFun(group2Values)
  varianceGroup3 <- varianceFun(group3Values)
  varianceGroup4 <- varianceFun(group4Values)
  
  return (list(Group1 = varianceGroup1, 
               Group2 = varianceGroup2, 
               Group3 = varianceGroup3, 
               Group4 = varianceGroup4))
}

standardDeviationGroup <- function (x) {
  group1Values <- c(NULL)
  group2Values <- c(NULL)
  group3Values <- c(NULL)
  group4Values <- c(NULL)
  
  index1 <- 1
  index2 <- 1
  index3 <- 1
  index4 <- 1
  
  standardDaviationGroup1 <- 0
  standardDaviationGroup2 <- 0
  standardDaviationGroup3 <- 0
  standardDaviationGroup4 <- 0
  
  for(i in 1:length(x)) {
    if (!is.na(x[i])) {
      if (datas$Group[i] == "Group1"){
        group1Values[index1] <- x[i]
        index1 <- index1 + 1
      } else if (datas$Group[i] == "Group2") {
        group2Values[index2] <- x[i]
        index2 <- index2 + 1
      } else if (datas$Group[i] == "Group3") {
        group3Values[index3] <- x[i]
        index3 <- index3 + 1
      } else {
        group4Values[index4] <- x[i]
        index4 <- index4 + 1
      }
    }  
  }
  standardDaviationGroup1 <- standardDeviationFun(group1Values)
  standardDaviationGroup2 <- standardDeviationFun(group2Values)
  standardDaviationGroup3 <- standardDeviationFun(group3Values)
  standardDaviationGroup4 <- standardDeviationFun(group4Values)
  
  return (list(Group1 = standardDaviationGroup1, 
               Group2 = standardDaviationGroup2, 
               Group3 = standardDaviationGroup3, 
               Group4 = standardDaviationGroup4))
}

crossProductGroup <- function(x,y) {
  
  varXGroup1 <- c(NULL)
  varXGroup2 <- c(NULL)
  varXGroup3 <- c(NULL)
  varXGroup4 <- c(NULL)
  
  varYGroup1 <- c(NULL)
  varYGroup2 <- c(NULL)
  varYGroup3 <- c(NULL)
  varYGroup4 <- c(NULL)
  
  
  indexX1 <- 1
  indexX2 <- 1
  indexX3 <- 1
  indexX4 <- 1
  
  indexY1 <- 1
  indexY2 <- 1
  indexY3 <- 1
  indexY4 <- 1
  
  crossProduct1 <- 0
  crossProduct2 <- 0
  crossProduct3 <- 0
  crossProduct4 <- 0
  
  for(i in 1:length(x)) {
    if (!is.na(x[i])) {
      if (datas$Group[i] == "Group1"){
        varXGroup1[indexX1] <- x[i]
        indexX1 <- indexX1 + 1
      } else if (datas$Group[i] == "Group2") {
        varXGroup2[indexX2] <- x[i]
        indexX2 <- indexX2 + 1
      } else if (datas$Group[i] == "Group3") {
        varXGroup3[indexX3] <- x[i]
        indexX3 <- indexX3 + 1
      } else {
        varXGroup4[indexX4] <- x[i]
        indexX4 <- indexX4 + 1
      }
    }  
  }
  
  for(i in 1:length(y)) {
    if (!is.na(y[i])) {
      if (datas$Group[i] == "Group1"){
        varYGroup1[indexY1] <- y[i]
        indexY1 <- indexY1 + 1
      } else if (datas$Group[i] == "Group2") {
        varYGroup2[indexY2] <- y[i]
        indexY2 <- indexY2 + 1
      } else if (datas$Group[i] == "Group3") {
        varYGroup3[indexY3] <- y[i]
        indexY3 <- indexY3 + 1
      } else {
        varYGroup4[indexY4] <- y[i]
        indexY4 <- indexY4 + 1
      }
    }  
  }
  
  crossProduct1 <- crossProductFun(varXGroup1,varYGroup1)
  crossProduct2 <- crossProductFun(varXGroup2,varYGroup2)
  crossProduct3 <- crossProductFun(varXGroup3,varYGroup3)
  crossProduct4 <- crossProductFun(varXGroup4,varYGroup4)
  
  return(list(Group1 = crossProduct1, Group2 = crossProduct2, 
              Group3 = crossProduct3, Group4 = crossProduct4))
}

covarianceGroup <- function (x,y) {
  group1XValues <- c(NULL)
  group2XValues <- c(NULL)
  group3XValues <- c(NULL)
  group4XValues <- c(NULL)
  
  group1YValues <- c(NULL)
  group2YValues <- c(NULL)
  group3YValues <- c(NULL)
  group4YValues <- c(NULL)
  
  index1X <- 1
  index2X <- 1
  index3X <- 1
  index4X <- 1
  
  index1Y <- 1
  index2Y <- 1
  index3Y <- 1
  index4Y <- 1
  
  covarianceGroup1 <- 0
  covarianceGroup2 <- 0
  covarianceGroup3 <- 0
  covarianceGroup4 <- 0
  
  for(i in 1:length(x)) {
    if (!is.na(x[i])) {
      if (datas$Group[i] == "Group1"){
        group1XValues[index1X] <- x[i]
        index1X <- index1X + 1
      } else if (datas$Group[i] == "Group2") {
        group2XValues[index2X] <- x[i]
        index2X <- index2X + 1
      } else if (datas$Group[i] == "Group3") {
        group3XValues[index3X] <- x[i]
        index3X <- index3X + 1
      } else {
        group4XValues[index4X] <- x[i]
        index4X <- index4X + 1
      }
    }  
  }
  
  for(i in 1:length(y)) {
    if (!is.na(y[i])) {
      if (datas$Group[i] == "Group1"){
        group1YValues[index1Y] <- y[i]
        index1Y <- index1Y + 1
      } else if (datas$Group[i] == "Group2") {
        group2YValues[index2Y] <- y[i]
        index2Y <- index2Y + 1
      } else if (datas$Group[i] == "Group3") {
        group3YValues[index3Y] <- y[i]
        index3Y <- index3Y + 1
      } else {
        group4YValues[index4Y] <- y[i]
        index4Y <- index4Y + 1
      }
    }  
  }
  
  covarianceGroup1 <- covarianceFun(group1XValues, group1YValues)
  covarianceGroup2 <- covarianceFun(group2XValues, group2YValues)
  covarianceGroup3 <- covarianceFun(group3XValues, group3YValues)
  covarianceGroup4 <- covarianceFun(group4XValues, group4YValues)
  
  return (list(Group1 = covarianceGroup1, Group2 = covarianceGroup2, 
               Group3 = covarianceGroup3, Group4 = covarianceGroup4))
}

correlationGroup <- function(x,y) {
  
  group1XValues <- c(NULL)
  group2XValues <- c(NULL)
  group3XValues <- c(NULL)
  group4XValues <- c(NULL)
  
  group1YValues <- c(NULL)
  group2YValues <- c(NULL)
  group3YValues <- c(NULL)
  group4YValues <- c(NULL)
  
  indexX1 <- 1
  indexX2 <- 1
  indexX3 <- 1
  indexX4 <- 1
  
  indexY1 <- 1
  indexY2 <- 1
  indexY3 <- 1
  indexY4 <- 1
  
  correlationGroup1 <- 0
  correlationGroup2 <- 0
  correlationGroup3 <- 0
  correlationGroup4 <- 0
  
  for(i in 1:length(x)) {
    if(!is.na(x[i])) {
      if (datas$Group[i] == "Group1") {
        group1XValues[indexX1] <- x[i]
        indexX1 <- indexX1 + 1
      } else if (datas$Group[i] == "Group2") {
        group2XValues[indexX2] <- x[i]
        indexX2 <- indexX2 + 1
      } else if (datas$Group[i] == "Group3") {
        group3XValues[indexX3] <- x[i]
        indexX3 <- indexX3 + 1
      } else {
        group4XValues[indexX4] <- x[i]
        indexX4 <- indexX4 + 1
      }
    }
  }
  
  for(i in 1:length(y)) {
    if(!is.na(y[i])) {
      if (datas$Group[i] == "Group1") {
        group1YValues[indexY1] <- y[i]
        indexY1 <- indexY1 + 1
      } else if (datas$Group[i] == "Group2") {
        group2YValues[indexY2] <- y[i]
        indexY2 <- indexY2 + 1
      } else if (datas$Group[i] == "Group3") {
        group3YValues[indexY3] <- y[i]
        indexY3 <- indexY3 + 1
      } else {
        group4YValues[indexY4] <- y[i]
        indexY4 <- indexY4 + 1
      }
    }
  }
  
  correlationGroup1 <- correlationFun(group1XValues, group1YValues)
  correlationGroup2 <- correlationFun(group2XValues, group2YValues)
  correlationGroup3 <- correlationFun(group3XValues, group3YValues)
  correlationGroup4 <- correlationFun(group4XValues, group4YValues)
  
  return (list(Group1 = correlationGroup1, Group2 = correlationGroup2, 
               Group3 = correlationGroup3, Group4 = correlationGroup4))
}

#These operations gender by gender

observationGender <- function(x) {
  FemaleCount <- 0
  MaleCount <- 0
  for(i in 1:length(x)) {
      if(datas$Gender[i] == "Female") {
        FemaleCount <- FemaleCount + 1
      } else {
        MaleCount <- MaleCount + 1
      }
  }
  return(list(Female = FemaleCount, Male = MaleCount))
}

countNaByGender <- function(x){
  countNaFemale <- 0
  countNaMale <- 0
  
  for(i in 1:length(x)){
    if(is.na(x[i])){
      if(datas$Gender[i] == "Female"){
        countNaFemale <- countNaFemale + 1
      }else {
        countNaMale <- countNaMale + 1
      }
    }  
  }
  
  return (list(Female=countNaFemale, Male=countNaMale))
}

minGender <- function(x) {
  FemaleValues <- c(NULL) 
  MaleValues <- c(NULL) 
  
  index1 <- 1
  index2 <- 1
  
  minFemale <- 0
  minMale <- 0
  
  for(i in 1:length(x)) {
    if (!is.na(x[i])) {
      if (datas$Gender[i] == "Female"){
        FemaleValues[index1] <- x[i]
        index1 <- index1 + 1
      } else {
        MaleValues[index2] <- x[i]
        index2 <- index2 + 1
      }
    }  
  }
  
  minFemale <- minFun(FemaleValues)
  minMale <- minFun(MaleValues)
  
  return(list(Female = minFemale ,  Male = minMale))
}

maxGender <- function(x) {
  FemaleValues <- c(NULL) 
  MaleValues <- c(NULL)
  
  index1 <- 1
  index2 <- 1
  
  maxFemale <- 0
  maxMale <- 0
  
  for(i in 1:length(x)) {
    if (!is.na(x[i])) {
      if (datas$Gender[i] == "Female"){
        FemaleValues[index1] <- x[i]
        index1 <- index1 + 1
      } else {
        MaleValues[index2] <- x[i]
        index2 <- index2 + 1
      }
    }  
  }
  
  maxFemale <- maxFun(FemaleValues)
  maxMale <- maxFun(MaleValues)
  
  return(list(Female = maxFemale ,  Male = maxMale))
}

rangeGender <- function(x){
  FemaleValues <- c(NULL)
  MaleValues <- c(NULL)
  
  index1 <- 1
  index2 <- 1
  
  minFemale <- 0
  minMale <- 0
  
  maxFemale <- 0
  maxMale <- 0
  
  for(i in 1:length(x)) {
    if (!is.na(x[i])) {
      if (datas$Gender[i] == "Female"){
        FemaleValues[index1] <- x[i]
        index1 <- index1 + 1
      } else {
        MaleValues[index2] <- x[i]
        index2 <- index2 + 1
      }
    }  
  }
  
  minFemale<- minFun(FemaleValues)
  minMale <- minFun(MaleValues)
  
  maxFemale <- maxFun(FemaleValues)
  maxMale <- maxFun(MaleValues)
  
  return(list(Female = c(minFemale, maxFemale),  Male = c(minMale, maxMale)))
}

sumGender <- function(x) {
  FemaleSum <- 0
  MaleSum <- 0
  
  for(i in 1:length(x)) {
    if(!is.na(x[i])) {
      if (datas$Gender[i] == "Female") {
        FemaleSum <- FemaleSum + x[i]
      } else {
        MaleSum <-  MaleSum + x[i]
      }
    }
  }
  
  return (list(Female=FemaleSum, Male=MaleSum))
}

meanGender <- function(x) {
  meanFemale <- 0
  meanMale <- 0
  
  FemaleCount <- 0
  MaleCount <- 0
  
  FemaleSum <- 0
  MaleSum <- 0
  
  for(i in 1:length(x)) {
    if(!is.na(x[i])) {
      if (datas$Gender[i] == "Female") {
        FemaleCount <- FemaleCount + 1
        FemaleSum <- FemaleSum + x[i]
        meanFemale <- FemaleSum / FemaleCount
      }  else {
        MaleCount <- MaleCount + 1
        MaleSum <- MaleSum + x[i]
        meanMale <- MaleSum / MaleCount
      }
    } 
  }
  
  return (list(Female = meanFemale,  Male = meanMale))
}

medianGender <- function(x) {
  FemaleValues <- c(NULL)
  MaleValues <- c(NULL)
  
  index1 <- 1
  index2 <- 1
  
  medianFemale <- 0
  medianMale <- 0
  
  for(i in 1:length(x)) {
    if (!is.na(x[i])) {
      if (datas$Gender[i] == "Female"){
        FemaleValues[index1] <- x[i]
        index1 <- index1 + 1
      } else {
        MaleValues[index2] <- x[i]
        index2 <- index2 + 1
      }
    }  
  }
  
  medianFemale <- medianFun(FemaleValues)
  medianMale <- medianFun(MaleValues)
  
  return (list(Female = medianFemale, Male = medianMale))
}

sumOfSquaresGender <- function(x) {
  FemaleValues <- c(NULL)
  MaleValues <- c(NULL)
  
  index1 <- 1
  index2 <- 1
  
  sumOfSquaresFemale <- 0
  sumOfSquaresMale <- 0
  
  for(i in 1:length(x)) {
    if (!is.na(x[i])) {
      if (datas$Gender[i] == "Female"){
        FemaleValues[index1] <- x[i]
        index1 <- index1 + 1
      } else {
        MaleValues[index2] <- x[i]
        index2 <- index2 + 1
      }
    }  
  }
  
  sumOfSquaresFemale <- sumOfSquaresFun(FemaleValues)  
  sumOfSquaresMale <- sumOfSquaresFun(MaleValues)
  
  return (list(Female = sumOfSquaresFemale, Male = sumOfSquaresMale))
}

varianceGender <- function (x) {
  FemaleValues <- c(NULL)
  MaleValues <- c(NULL)
  
  index1 <- 1
  index2 <- 1
  
  varianceMale <- 0
  varianceFemale <- 0
  
  for(i in 1:length(x)) {
    if (!is.na(x[i])) {
      if (datas$Gender[i] == "Female"){
        FemaleValues[index1] <- x[i]
        index1 <- index1 + 1
      }  else {
        MaleValues[index2] <- x[i]
        index2 <- index2 + 1
      }
    }  
  }
  
  varianceFemale <- varianceFun(FemaleValues)
  varianceMale <- varianceFun(MaleValues)
  
  return (list(Female = varianceFemale, Male = varianceMale))
}

standardDeviationGender <- function (x) {
  FemaleValues <- c(NULL)
  MaleValues <- c(NULL)
  
  index1 <- 1
  index2 <- 1
  
  standardDaviationFemale <- 0
  standardDaviationMale <- 0
  
  for(i in 1:length(x)) {
    if (!is.na(x[i])) {
      if (datas$Gender[i] == "Female"){
        FemaleValues[index1] <- x[i]
        index1 <- index1 + 1
      } else {
        MaleValues[index2] <- x[i]
        index2 <- index2 + 1
      }
    }  
  }
  
  standardDaviationFemale <- standardDeviationFun(FemaleValues)
  standardDaviationMale <- standardDeviationFun(MaleValues)
  
  return (list(Female = standardDaviationFemale,  Male = standardDaviationMale))
}

covarianceGender <- function (x,y) {
  FemaleXValues <- c(NULL)
  MaleXValues <- c(NULL)
  
  FemaleYValues <- c(NULL)
  MaleYValues <- c(NULL)
  
  index1X <- 1
  index2X <- 1
  
  index1Y <- 1
  index2Y <- 1
  
  covarianceFemale <- 0
  covarianceMale <- 0
  
  for(i in 1:length(x)) {
    if (!is.na(x[i])) {
      if (datas$Gender[i] == "Female"){
        FemaleXValues[index1X] <- x[i]
        index1X <- index1X + 1
      }  else {
        MaleXValues[index2X] <- x[i]
        index2X <- index2X + 1
      }
    }  
  }
  
  for(i in 1:length(y)) {
    if (!is.na(y[i])) {
      if (datas$Gender[i] == "Female"){
        FemaleYValues[index1Y] <- y[i]
        index1Y <- index1Y + 1
      } else {
        MaleYValues[index2Y] <- y[i]
        index2Y <- index2Y + 1
      }
    }  
  }
  
  covarianceFemale <- covarianceFun(FemaleXValues, FemaleYValues)
  covarianceMale <- covarianceFun(MaleXValues, MaleYValues)
  
  return (list(Female = covarianceFemale, Male = covarianceMale))
}

crossProductGender <- function(x,y) {
  maleVarsX <- c(NULL)
  femaleVarsX <- c(NULL)
  
  maleVarsY <- c(NULL)
  femaleVarsY <- c(NULL)
  
  maleIndexX <- 1
  femaleIndexX <- 1
  
  maleIndexY <- 1
  femaleIndexY <- 1
  
  crossProductMale <- 0
  crossProductFemale <- 0
  
  for (i in 1:length(x)) {
    if (!is.na(x[i])) {
      if (datas$Gender[i] == "Male") {
        maleVarsX[maleIndexX] <- x[i]
        maleIndexX <- maleIndexX + 1
      } else {
        femaleVarsX[femaleIndexX] <- x[i]
        femaleIndexX <- femaleIndexX + 1
      }
    }
  }
  
  for(i in 1:length(y)) {
    if (!is.na(y[i])) {
      if (datas$Gender[i] == "Male") {
        maleVarsY[maleIndexY] <- y[i]
        maleIndexY <- maleIndexY + 1
      } else {
        femaleVarsY[femaleIndexY] <- y[i]
        femaleIndexY <- femaleIndexY + 1
      }
    }
  }
  
  crossProductMale <- crossProductFun(maleVarsX, maleVarsY)
  crossProductFemale <- crossProductFun(femaleVarsX, femaleVarsY)
  
  return (list(Male = crossProductMale, Female = crossProductFemale))
}

correlationGender <- function(x,y) {
  maleVarsX <- c(NULL)
  femaleVarsX <- c(NULL)
  
  maleVarsY <- c(NULL)
  femaleVarsY <- c(NULL)
  
  maleIndexX <- 1
  femaleIndexX <- 1
  
  maleIndexY <- 1
  femaleIndexY <- 1
  
  correlationMale <- 0
  correlationFemale <- 0
  
  for (i in 1:length(x)) {
    if (!is.na(x[i])) {
      if (datas$Gender[i] == "Male") {
        maleVarsX[maleIndexX] <- x[i]
        maleIndexX <- maleIndexX + 1
      } else {
        femaleVarsX[femaleIndexX] <- x[i]
        femaleIndexX <- femaleIndexX + 1
      }
    }
  }
  
  for(i in 1:length(y)) {
    if (!is.na(y[i])) {
      if (datas$Gender[i] == "Male") {
        maleVarsY[maleIndexY] <- y[i]
        maleIndexY <- maleIndexY + 1
      } else {
        femaleVarsY[femaleIndexY] <- y[i]
        femaleIndexY <- femaleIndexY + 1
      }
    }
  }
  
  correlationMale <- correlationFun(maleVarsX, maleVarsY)
  correlationFemale <- correlationFun(femaleVarsX, femaleVarsY)
  
  return (list(Male = correlationMale, Female = correlationFemale))
}

#These operations by gender and group

observationGroupbyGender <- function(x) {
  group1FemaleCount <- 0
  group2FemaleCount <- 0
  group3FemaleCount <- 0
  group4FemaleCount <- 0
  group1MaleCount <- 0
  group2MaleCount <- 0
  group3MaleCount <- 0
  group4MaleCount <- 0
  
  for(i in 1:length(x)) {
      if(datas$Gender[i] == "Female") {
        if(datas$Group[i] == "Group1") {
          group1FemaleCount <- group1FemaleCount + 1
        } else if (datas$Group[i] == "Group2") {
          group2FemaleCount <- group2FemaleCount + 1
        } else if (datas$Group[i] == "Group3") {
          group3FemaleCount <- group3FemaleCount + 1
        } else {
          group4FemaleCount <- group4FemaleCount + 1
        }
      } else {
        if(datas$Group[i] == "Group1") {
          group1MaleCount <- group1MaleCount + 1
        } else if (datas$Group[i] == "Group2") {
          group2MaleCount <- group2MaleCount + 1
        } else if (datas$Group[i] == "Group3") {
          group3MaleCount <- group3MaleCount + 1
        } else {
          group4MaleCount <- group4MaleCount + 1
        }
      }
  }
  
  return(list(Group1Female = group1FemaleCount, Group2Female = group2FemaleCount, 
              Group3Female = group3FemaleCount, Group4Female = group4FemaleCount, 
              Group1Male = group1MaleCount, Group2Male = group2MaleCount, 
              Group3Male = group3MaleCount, Group4Male = group4MaleCount))
}

minByGenderAndGroup <- function(x){
  minGroup1Female <-0
  minGroup1Male <-0
  minGroup2Female <-0
  minGroup2Male <-0
  minGroup3Female <-0
  minGroup3Male <-0
  minGroup4Female <-0
  minGroup4Male <-0
  
  group1FemaleValues <- c(NULL)
  group2FemaleValues <- c(NULL)
  group3FemaleValues <- c(NULL)
  group4FemaleValues <- c(NULL)
  group1MaleValues <- c(NULL)
  group2MaleValues <- c(NULL)
  group3MaleValues <- c(NULL)
  group4MaleValues <- c(NULL)
  
  index1 <- 1
  index2 <- 1
  index3 <- 1
  index4 <- 1
  index5 <- 1
  index6 <- 1
  index7 <- 1
  index8 <- 1
  
  for(i in 1:length(x)){
    if (!is.na(x[i])){
      if (datas$Group[i] == "Group1"){
        if(datas$Gender[i] == "Female"){
          group1FemaleValues[index1] <- x[i]
          index1 <- index1 + 1
        }else{
          group1MaleValues[index2] <- x[i]
          index2 <- index2 + 1
        }
      }else if(datas$Group[i] == "Group2"){
        if(datas$Gender[i] == "Female"){
          group2FemaleValues[index3] <- x[i]
          index3 <- index3 + 1
        }else{
          group2MaleValues[index4] <- x[i]
          index4 <- index4 + 1
        }
      }else if(datas$Group[i] == "Group3"){
        if(datas$Gender[i] == "Female"){
          group3FemaleValues[index5] <- x[i]
          index5 <- index5 + 1
        }else{
          group3MaleValues[index6] <- x[i]
          index6 <- index6 + 1
        }
      }else if(datas$Group[i] == "Group4"){
        if(datas$Gender[i] == "Female"){
          group4FemaleValues[index7] <- x[i]
          index7 <- index7 + 1
        }else{
          group4MaleValues[index8] <- x[i]
          index8 <- index8 + 1
        }
      }
    }
  }
  
  minGroup1Female <- minFun(group1FemaleValues)
  minGroup1Male <- minFun(group1MaleValues)
  minGroup2Female <- minFun(group2FemaleValues)
  minGroup2Male <- minFun(group2MaleValues)
  minGroup3Female <- minFun(group3FemaleValues)
  minGroup3Male <- minFun(group3MaleValues)
  minGroup4Female <- minFun(group4FemaleValues)
  minGroup4Male <- minFun(group4MaleValues)
  
  return(list(Group1=list(Female=minGroup1Female, Male=minGroup1Male),
              Group2=list(Female=minGroup2Female, Male=minGroup2Male),
              Group3=list(Female=minGroup3Female, Male=minGroup3Male),
              Group4=list(Female=minGroup4Female, Male=minGroup4Male)
  ))
  
}

maxByGenderAndGroup <- function(x){
  maxGroup1Female <-0
  maxGroup1Male <-0
  maxGroup2Female <-0
  maxGroup2Male <-0
  maxGroup3Female <-0
  maxGroup3Male <-0
  maxGroup4Female <-0
  maxGroup4Male <-0
  
  group1FemaleValues <- c(NULL)
  group2FemaleValues <- c(NULL)
  group3FemaleValues <- c(NULL)
  group4FemaleValues <- c(NULL)
  group1MaleValues <- c(NULL)
  group2MaleValues <- c(NULL)
  group3MaleValues <- c(NULL)
  group4MaleValues <- c(NULL)
  
  index1 <- 1
  index2 <- 1
  index3 <- 1
  index4 <- 1
  index5 <- 1
  index6 <- 1
  index7 <- 1
  index8 <- 1
  
  for(i in 1:length(x)){
    if (!is.na(x[i])){
      if (datas$Group[i] == "Group1"){
        if(datas$Gender[i] == "Female"){
          group1FemaleValues[index1] <- x[i]
          index1 <- index1 + 1
        }else{
          group1MaleValues[index2] <- x[i]
          index2 <- index2 + 1
        }
      }else if(datas$Group[i] == "Group2"){
        if(datas$Gender[i] == "Female"){
          group2FemaleValues[index3] <- x[i]
          index3 <- index3 + 1
        }else{
          group2MaleValues[index4] <- x[i]
          index4 <- index4 + 1
        }
      }else if(datas$Group[i] == "Group3"){
        if(datas$Gender[i] == "Female"){
          group3FemaleValues[index5] <- x[i]
          index5 <- index5 + 1
        }else{
          group3MaleValues[index6] <- x[i]
          index6 <- index6 + 1
        }
      }else if(datas$Group[i] == "Group4"){
        if(datas$Gender[i] == "Female"){
          group4FemaleValues[index7] <- x[i]
          index7 <- index7 + 1
        }else{
          group4MaleValues[index8] <- x[i]
          index8 <- index8 + 1
        }
      }
    }
  }
  
  maxGroup1Female <- maxFun(group1FemaleValues)
  maxGroup1Male <- maxFun(group1MaleValues)
  maxGroup2Female <- maxFun(group2FemaleValues)
  maxGroup2Male <- maxFun(group2MaleValues)
  maxGroup3Female <- maxFun(group3FemaleValues)
  maxGroup3Male <- maxFun(group3MaleValues)
  maxGroup4Female <- maxFun(group4FemaleValues)
  maxGroup4Male <- maxFun(group4MaleValues)
  
  return(list(Group1=list(Female=maxGroup1Female, Male=maxGroup1Male),
              Group2=list(Female=maxGroup2Female, Male=maxGroup2Male),
              Group3=list(Female=maxGroup3Female, Male=maxGroup3Male),
              Group4=list(Female=maxGroup4Female, Male=maxGroup4Male)
  ))
  
}

rangeByGenderAndGroup <- function(x) {
  group1Values <- c(NULL)
  group1Values2 <- c(NULL)
  group2Values <- c(NULL)
  group2Values2 <- c(NULL)
  group3Values <- c(NULL)
  group3Values2 <- c(NULL)
  group4Values <- c(NULL)
  group4Values2 <- c(NULL)
  
  index1 <- 1
  index2 <- 1
  index3 <- 1
  index4 <- 1
  index5 <- 1
  index6 <- 1
  index7 <- 1
  index8 <- 1
  
  minGroup1 <- 0
  minGroup2 <- 0
  minGroup3 <- 0
  minGroup4 <- 0
  minGroup5 <- 0
  minGroup6<- 0
  minGroup7 <- 0
  minGroup8 <- 0
  
  
  maxGroup1 <- 0
  maxGroup2 <- 0
  maxGroup3 <- 0
  maxGroup4 <- 0
  maxGroup5 <- 0
  maxGroup6 <- 0
  maxGroup7 <- 0
  maxGroup8 <- 0
  
  for(i in 1:length(x)) {
    if (!is.na(x[i])) {
      if (datas$Group[i] == "Group1"){
        if(datas$Gender[i] == "Female"){
          group1Values[index1] <- x[i]
          index1 <- index1 + 1
        }else{
          group1Values2[index2] <- x[i]
          index2 <- index2 + 1
        }
      } else if (datas$Group[i] == "Group2") {
        if(datas$Gender[i] == "Female"){
          group2Values[index3] <- x[i]
          index3 <- index3 + 1
        }else{
          group2Values2[index4] <- x[i]
          index4 <- index4 + 1
        }
      } else if (datas$Group[i] == "Group3") {
        if(datas$Gender[i] == "Female"){
          group3Values[index5] <- x[i]
          index5 <- index5 + 1
        }else{
          group3Values2[index6] <- x[i]
          index6 <- index6 + 1
        }
      } else {
        if(datas$Gender[i] == "Female"){
          group4Values[index7] <- x[i]
          index7 <- index7 + 1
        }else{
          group4Values2[index8] <- x[i]
          index8 <- index8 + 1
        }
      }
    }  
  }
  
  minFemaleGroup1 <- minFun(group1Values)
  minFemaleGroup2 <- minFun(group2Values)
  minFemaleGroup3 <- minFun(group3Values)
  minFemaleGroup4 <- minFun(group4Values)
  minMaleGroup1 <- minFun(group1Values2)
  minMaleGroup2 <- minFun(group2Values2)
  minMaleGroup3 <- minFun(group3Values2)
  minMaleGroup4 <- minFun(group4Values2)
  
  maxFemaleGroup1 <- maxFun(group1Values)
  maxFemaleGroup2 <- maxFun(group2Values)
  maxFemaleGroup3 <- maxFun(group3Values)
  maxFemaleGroup4 <- maxFun(group4Values)
  maxMaleGroup1 <- maxFun(group1Values2)
  maxMaleGroup2 <- maxFun(group2Values2)
  maxMaleGroup3 <- maxFun(group3Values2)
  maxMaleGroup4 <- maxFun(group4Values2)
  
  return(list(Group1= list(Female=c(minFemaleGroup1, maxFemaleGroup1), Male= c(minMaleGroup1,maxMaleGroup1)),
              Group2= list(Female=c(minFemaleGroup2, maxFemaleGroup2), Male= c(minMaleGroup2,maxMaleGroup2)),
              Group3=list(Female=c(minFemaleGroup3, maxFemaleGroup3), Male= c(minMaleGroup3,maxMaleGroup3)),
              Group4= list(Female=c(minFemaleGroup4, maxFemaleGroup4), Male= c(minMaleGroup4,maxMaleGroup4))))
}

countNaByGenderGroup <- function(x){
  countNaFemale1 <- 0
  countNaMale1 <- 0
  
  countNaFemale2 <- 0
  countNaMale2 <- 0
  
  countNaFemale3 <- 0
  countNaMale3 <- 0
  
  countNaFemale4 <- 0
  countNaMale4 <- 0
  
  for(i in 1:length(x)){
    if(is.na(x[i])){
      if(datas$Group[i] == "Group1"){
        if(datas$Gender[i] == "Female"){
          countNaFemale1 <- countNaFemale1 + 1
        }else {
          countNaMale1 <- countNaMale1 + 1
        }
      }else if(datas$Group[i] == "Group2"){
        if(datas$Gender[i] == "Female"){
          countNaFemale2 <- countNaFemale2 + 1
        }else {
          countNaMale2 <- countNaMale2 + 1
        }
      }else if(datas$Group[i] == "Group3"){
        if(datas$Gender[i] == "Female"){
          countNaFemale3 <- countNaFemale3 + 1
        }else {
          countNaMale3 <- countNaMale3 + 1
        }
      }else{
        if(datas$Gender[i] == "Female"){
          countNaFemale4 <- countNaFemale4 + 1
        }else {
          countNaMale4 <- countNaMale4 + 1
        }
      }
    }
  }
  return(list(Group1Female=countNaFemale1 ,
              Group1Male=countNaMale1, 
              Group2Female=countNaFemale2, 
              Group2Male=countNaMale2,
              Group3Female=countNaFemale3, 
              Group3Male= countNaMale3, 
              Group4Female=countNaFemale4, 
              Group4Male=countNaMale4))
}

sumByGroupAndGender <- function(x) {
  group1FemaleSum <- 0
  group1MaleSum <- 0
  group2FemaleSum <- 0
  group2MaleSum <- 0
  group3FemaleSum <- 0
  group3MaleSum <- 0
  group4FemaleSum <- 0
  group4MaleSum <- 0
  
  for (i in 1:length(x)) {
    if (!is.na(x[i])) {
      if (datas$Group[i] == "Group1") {
        if (datas$Gender[i] == "Female") {
          group1FemaleSum <- group1FemaleSum + x[i]
        } else {
          group1MaleSum <- group1MaleSum + x[i]
        }
      } else if (datas$Group[i] == "Group2") {
        if (datas$Gender[i] == "Female") {
          group2FemaleSum <- group2FemaleSum + x[i]
        } else {
          group2MaleSum <- group2MaleSum + x[i]
        }
      } else if (datas$Group[i] == "Group3") {
        if (datas$Gender[i] == "Female") {
          group3FemaleSum <- group3FemaleSum + x[i]
        } else {
          group3MaleSum <- group3MaleSum + x[i]
        }
      } else {
        if (datas$Gender[i] == "Female") {
          group4FemaleSum <- group4FemaleSum + x[i]
        }}}}
  
  return (list(Group1 = list(Female = group1FemaleSum, Male = group1MaleSum),
               Group2 = list(Female = group2FemaleSum, Male = group2MaleSum),
               Group3 = list(Female = group3FemaleSum, Male = group3MaleSum),
               Group4 = list(Female = group4FemaleSum, Male = group4MaleSum)))
}

meanByGenderAndGroup <- function(x){
  meanFemale1 <- 0
  meanMale1 <- 0
  meanFemale2 <- 0
  meanMale2 <- 0
  meanFemale3 <- 0
  meanMale3 <- 0
  meanFemale4 <- 0
  meanMale4 <- 0
  
  FemaleCount1 <- 0
  MaleCount1 <- 0
  FemaleCount2 <- 0
  MaleCount2 <- 0
  FemaleCount3 <- 0
  MaleCount3 <- 0
  FemaleCount4 <- 0
  MaleCount4 <- 0
  
  FemaleSum1 <- 0
  MaleSum1 <- 0
  FemaleSum2 <- 0
  MaleSum2 <- 0
  FemaleSum3 <- 0
  MaleSum3 <- 0
  FemaleSum4 <- 0
  MaleSum4 <- 0
  
  for(i in 1:length(x)){
    if(!is.na(x[i])){
      if(datas$Group[i] == "Group1"){
        if (datas$Gender[i] == "Female") {
          FemaleCount1 <- FemaleCount1 + 1
          FemaleSum1 <- FemaleSum1 + x[i]
          meanFemale1 <- FemaleSum1 / FemaleCount1
        }  else {
          MaleCount1 <- MaleCount1 + 1
          MaleSum1 <- MaleSum1 + x[i]
          meanMale1 <- MaleSum1 / MaleCount1
        }
      }else if(datas$Group[i] == "Group2"){
        if (datas$Gender[i] == "Female") {
          FemaleCount2 <- FemaleCount2 + 1
          FemaleSum2 <- FemaleSum2 + x[i]
          meanFemale2 <- FemaleSum2 / FemaleCount2
        }  else {
          MaleCount2 <- MaleCount2 + 1
          MaleSum2 <- MaleSum2 + x[i]
          meanMale2 <- MaleSum2 / MaleCount2
        }
      }else if(datas$Group[i] == "Group3"){
        if (datas$Gender[i] == "Female") {
          FemaleCount3 <- FemaleCount3 + 1
          FemaleSum3 <- FemaleSum3 + x[i]
          meanFemale3 <- FemaleSum3 / FemaleCount3
        }  else {
          MaleCount3 <- MaleCount3 + 1
          MaleSum3 <- MaleSum3 + x[i]
          meanMale3 <- MaleSum3 / MaleCount3
        }
      }else{
        if (datas$Gender[i] == "Female") {
          FemaleCount4 <- FemaleCount4 + 1
          FemaleSum4 <- FemaleSum4 + x[i]
          meanFemale4 <- FemaleSum4 / FemaleCount4
        }  else {
          MaleCount4 <- MaleCount4 + 1
          MaleSum4 <- MaleSum4 + x[i]
          meanMale4 <- MaleSum4 / MaleCount4
        }
      }
    }
  }
  return(list(Group1=list(Female=meanFemale1, Male=meanMale1),
              Group2=list(Female=meanFemale2, Male=meanMale2),
              Group3=list(Female=meanFemale3, Male=meanMale3),
              Group4=list(Female=meanFemale4, Male=meanMale4)
  ))
}

medianByGenderAndGroup <- function(x) {
  group1Values <- c(NULL)
  group2Values <- c(NULL)
  group3Values <- c(NULL)
  group4Values <- c(NULL)
  group1Values2 <- c(NULL)
  group2Values2 <- c(NULL)
  group3Values2 <- c(NULL)
  group4Values2 <- c(NULL)
  
  index1 <- 1
  index2 <- 1
  index3 <- 1
  index4 <- 1
  index5 <- 1
  index6 <- 1
  index7 <- 1
  index8 <- 1
  
  medianFemaleGroup1 <- 0
  medianFemaleGroup2 <- 0
  medianFemaleGroup3 <- 0
  medianFemaleGroup4 <- 0
  medianMaleGroup1 <- 0
  medianMaleGroup2 <- 0
  medianMaleGroup3 <- 0
  medianMaleGroup4 <- 0
  
  for(i in 1:length(x)) {
    if (!is.na(x[i])) {
      if (datas$Group[i] == "Group1"){
        if(datas$Gender[i] == "Female"){
          group1Values[index1] <- x[i]
          index1 <- index1 + 1
        }else{
          group1Values2[index2] <- x[i]
          index2 <- index2 + 1
        }
        
      } else if (datas$Group[i] == "Group2") {
        if(datas$Gender[i] == "Female"){
          group2Values[index3] <- x[i]
          index3 <- index3 + 1
        }else{
          group2Values2[index4] <- x[i]
          index4 <- index4 + 1
        }
      } else if (datas$Group[i] == "Group3") {
        if(datas$Gender[i] == "Female"){
          group3Values[index5] <- x[i]
          index5 <- index5 + 1
        }else{
          group3Values2[index6] <- x[i]
          index6 <- index6 + 1
        }
      } else {
        if(datas$Gender[i] == "Female"){
          group4Values[index7] <- x[i]
          index7 <- index7 + 1
        }else{
          group4Values2[index8] <- x[i]
          index8 <- index8 + 1
        }
      }
    }  
  }
  medianFemaleGroup1 <- medianFun(group1Values)
  medianFemaleGroup2 <- medianFun(group2Values)
  medianFemaleGroup3 <- medianFun(group3Values)
  medianFemaleGroup4 <- medianFun(group4Values)
  medianMaleGroup1 <- medianFun(group1Values2)
  medianMaleGroup2 <- medianFun(group2Values2)
  medianMaleGroup3 <- medianFun(group3Values2)
  medianMaleGroup4 <- medianFun(group4Values2)
  
  return (list(Group1 = list(Female=medianFemaleGroup1, Male = medianMaleGroup1),
               Group2 = list(Female=medianFemaleGroup2, Male = medianMaleGroup2), 
               Group3 = list(Female=medianFemaleGroup3, Male = medianMaleGroup3), 
               Group4 = list(Female=medianFemaleGroup4, Male = medianMaleGroup4)))
}

sumOfSquaresByGenderAndGroup <- function(x) {
  group1Values <- c(NULL)
  group2Values <- c(NULL)
  group3Values <- c(NULL)
  group4Values <- c(NULL)
  group1Values2 <- c(NULL)
  group2Values2 <- c(NULL)
  group3Values2 <- c(NULL)
  group4Values2 <- c(NULL)
  
  index1 <- 1
  index2 <- 1
  index3 <- 1
  index4 <- 1
  index5 <- 1
  index6 <- 1
  index7 <- 1
  index8 <- 1
  
  sumOfSquaresFemaleGroup1 <- 0
  sumOfSquaresFemaleGroup2 <- 0
  sumOfSquaresFemaleGroup3 <- 0
  sumOfSquaresFemaleGroup4 <- 0
  sumOfSquaresMaleGroup1 <- 0
  sumOfSquaresMaleGroup2 <- 0
  sumOfSquaresMaleGroup3 <- 0
  sumOfSquaresMaleGroup4 <- 0
  
  for(i in 1:length(x)) {
    if (!is.na(x[i])) {
      if (datas$Group[i] == "Group1"){
        if(datas$Gender[i] == "Female"){
          group1Values[index1] <- x[i]
          index1 <- index1 + 1
        }else {
          group1Values2[index2] <- x[i]
          index2 <- index2 + 1
        }
      } else if (datas$Group[i] == "Group2") {
        if(datas$Gender[i] == "Female"){
          group2Values[index3] <- x[i]
          index3 <- index3 + 1
        }else {
          group2Values2[index4] <- x[i]
          index4 <- index4 + 1
        }
      } else if (datas$Group[i] == "Group3") {
        if(datas$Gender[i] == "Female"){
          group3Values[index5] <- x[i]
          index5 <- index5 + 1
        }else {
          group3Values2[index6] <- x[i]
          index6 <- index6 + 1
        }
      } else {
        if(datas$Gender[i] == "Female"){
          group4Values[index7] <- x[i]
          index7 <- index7 + 1
        }else {
          group4Values2[index8] <- x[i]
          index8 <- index8 + 1
        }
      }
    }  
  }
  
  sumOfSquaresFemaleGroup1 <- sumOfSquaresFun(group1Values)
  sumOfSquaresFemaleGroup2 <- sumOfSquaresFun(group2Values)
  sumOfSquaresFemaleGroup3 <- sumOfSquaresFun(group3Values)
  sumOfSquaresFemaleGroup4 <- sumOfSquaresFun(group4Values)
  sumOfSquaresMaleGroup1 <- sumOfSquaresFun(group1Values2)
  sumOfSquaresMaleGroup2 <- sumOfSquaresFun(group2Values2)
  sumOfSquaresMaleGroup3 <- sumOfSquaresFun(group3Values2)
  sumOfSquaresMaleGroup4 <- sumOfSquaresFun(group4Values2)
  
  return (list(Group1 = list(Female= sumOfSquaresFemaleGroup1, Male= sumOfSquaresMaleGroup1), 
               Group2 = list(Female= sumOfSquaresFemaleGroup2, Male= sumOfSquaresMaleGroup2), 
               Group3 = list(Female= sumOfSquaresFemaleGroup3, Male= sumOfSquaresMaleGroup3), 
               Group4 = list(Female= sumOfSquaresFemaleGroup4, Male= sumOfSquaresMaleGroup4)))
}

varianceByGenderAndGroup <- function (x) {
  group1Values <- c(NULL)
  group2Values <- c(NULL)
  group3Values <- c(NULL)
  group4Values <- c(NULL)
  group1Values2 <- c(NULL)
  group2Values2 <- c(NULL)
  group3Values2 <- c(NULL)
  group4Values2 <- c(NULL)
  
  index1 <- 1
  index2 <- 1
  index3 <- 1
  index4 <- 1
  index5 <- 1
  index6 <- 1
  index7 <- 1
  index8 <- 1
  
  varianceFemaleGroup1 <- 0
  varianceFemaleGroup2 <- 0
  varianceFemaleGroup3 <- 0
  varianceFemaleGroup4 <- 0
  varianceMaleGroup1 <- 0
  varianceMaleGroup2 <- 0
  varianceMaleGroup3 <- 0
  varianceMaleGroup4 <- 0
  
  for(i in 1:length(x)) {
    if (!is.na(x[i])) {
      if (datas$Group[i] == "Group1"){
        if(datas$Gender[i] == "Female"){
          group1Values[index1] <- x[i]
          index1 <- index1 + 1
        }else{
          group1Values2[index2] <- x[i]
          index2 <- index2 + 1
        }
      } else if (datas$Group[i] == "Group2") {
        if(datas$Gender[i] == "Female"){
          group2Values[index3] <- x[i]
          index3 <- index3 + 1
        }else{
          group2Values2[index4] <- x[i]
          index4 <- index4 + 1
        }
      } else if (datas$Group[i] == "Group3") {
        if(datas$Gender[i] == "Female"){
          group3Values[index5] <- x[i]
          index5 <- index5 + 1
        }else{
          group3Values2[index6] <- x[i]
          index6 <- index6 + 1
        }
      } else {
        if(datas$Gender[i] == "Female"){
          group4Values[index7] <- x[i]
          index7 <- index7 + 1
        }else{
          group4Values2[index8] <- x[i]
          index8 <- index8 + 1
        }
      }
    }  
  }
  varianceFemaleGroup1 <- varianceFun(group1Values)
  varianceFemaleGroup2 <- varianceFun(group2Values)
  varianceFemaleGroup3 <- varianceFun(group3Values)
  varianceFemaleGroup4 <- varianceFun(group4Values)
  varianceMaleGroup1 <- varianceFun(group1Values2)
  varianceMaleGroup2 <- varianceFun(group2Values2)
  varianceMaleGroup3 <- varianceFun(group3Values2)
  varianceMaleGroup4 <- varianceFun(group4Values2)
  
  
  return (list(Group1 = list(Female = varianceFemaleGroup1,varianceMaleGroup1),
               Group2 = list(Female = varianceFemaleGroup2,varianceMaleGroup2),
               Group3 = list(Female = varianceFemaleGroup3,varianceMaleGroup3),
               Group4 = list(Female = varianceFemaleGroup4,varianceMaleGroup4)))
}

standardDeviationByGroupAndGender <- function (x) {
  group1Values <- c(NULL)
  group2Values <- c(NULL)
  group3Values <- c(NULL)
  group4Values <- c(NULL)
  group1Values2 <- c(NULL)
  group2Values2 <- c(NULL)
  group3Values2 <- c(NULL)
  group4Values2 <- c(NULL)
  
  index1 <- 1
  index2 <- 1
  index3 <- 1
  index4 <- 1
  index5 <- 1
  index6 <- 1
  index7 <- 1
  index8 <- 1
  
  standardDaviationFemaleGroup1 <- 0
  standardDaviationFemaleGroup2 <- 0
  standardDaviationFemaleGroup3 <- 0
  standardDaviationFemaleGroup4 <- 0
  standardDaviationMaleGroup1 <- 0
  standardDaviationMaleGroup2 <- 0
  standardDaviationMaleGroup3 <- 0
  standardDaviationMaleGroup4 <- 0
  
  for(i in 1:length(x)) {
    if (!is.na(x[i])) {
      if (datas$Group[i] == "Group1"){
        if(datas$Gender[i] == "Female"){
          group1Values[index1] <- x[i]
          index1 <- index1 + 1
        }else {
          group1Values2[index2] <- x[i]
          index2 <- index2 + 1
        }
      } else if (datas$Group[i] == "Group2") {
        if(datas$Gender[i] == "Female"){
          group2Values[index3] <- x[i]
          index3 <- index3 + 1
        }else {
          group2Values2[index4] <- x[i]
          index4 <- index4 + 1
        }
      } else if (datas$Group[i] == "Group3") {
        if(datas$Gender[i] == "Female"){
          group3Values[index5] <- x[i]
          index5 <- index5 + 1
        }else {
          group3Values2[index6] <- x[i]
          index6 <- index6 + 1
        }
      } else {
        if(datas$Gender[i] == "Female"){
          group4Values[index7] <- x[i]
          index7 <- index7 + 1
        }else {
          group4Values2[index8] <- x[i]
          index8 <- index8 + 1
        }
      }
    }  
  }
  standardDaviationFemaleGroup1 <- standardDeviationFun(group1Values)
  standardDaviationFemaleGroup2 <- standardDeviationFun(group2Values)
  standardDaviationFemaleGroup3 <- standardDeviationFun(group3Values)
  standardDaviationFemaleGroup4 <- standardDeviationFun(group4Values)
  standardDaviationMaleGroup1 <- standardDeviationFun(group1Values2)
  standardDaviationMaleGroup2 <- standardDeviationFun(group2Values2)
  standardDaviationMaleGroup3 <- standardDeviationFun(group3Values2)
  standardDaviationMaleGroup4 <- standardDeviationFun(group4Values2)
  
  return (list(Group1 =list(Female = standardDaviationFemaleGroup1, Male = standardDaviationMaleGroup1),
               Group2 = list(Female = standardDaviationFemaleGroup2, Male = standardDaviationMaleGroup2), 
               Group3 = list(Female = standardDaviationFemaleGroup3, Male = standardDaviationMaleGroup3),
               Group4 = list(Female = standardDaviationFemaleGroup4, Male = standardDaviationMaleGroup4)))
}

crossProductByGenderAndGroup <- function (x,y) {
  group1XValues <- c(NULL)
  group2XValues <- c(NULL)
  group3XValues <- c(NULL)
  group4XValues <- c(NULL)
  group1XValues2 <- c(NULL)
  group2XValues2 <- c(NULL)
  group3XValues2 <- c(NULL)
  group4XValues2 <- c(NULL)
  
  group1YValues <- c(NULL)
  group2YValues <- c(NULL)
  group3YValues <- c(NULL)
  group4YValues <- c(NULL)
  group1YValues2 <- c(NULL)
  group2YValues2 <- c(NULL)
  group3YValues2 <- c(NULL)
  group4YValues2 <- c(NULL)
  
  index1X <- 1
  index2X <- 1
  index3X <- 1
  index4X <- 1
  index5X <- 1
  index6X <- 1
  index7X <- 1
  index8X <- 1
  
  index1Y <- 1
  index2Y <- 1
  index3Y <- 1
  index4Y <- 1
  index5Y <- 1
  index6Y <- 1
  index7Y <- 1
  index8Y <- 1
  
  crossFemaleGroup1 <- 0
  crossFemaleGroup2 <- 0
  crossFemaleGroup3 <- 0
  crossFemaleGroup4 <- 0
  crossMaleGroup1 <- 0
  crossMaleGroup2 <- 0
  crossMaleGroup3 <- 0
  crossMaleGroup4 <- 0
  
  for(i in 1:length(x)) {
    if (!is.na(x[i])) {
      if (datas$Group[i] == "Group1"){
        if(datas$Gender[i] == "Female"){
          group1XValues[index1X] <- x[i]
          index1X <- index1X + 1
        }else{
          group1XValues2[index2X] <- x[i]
          index2X <- index2X + 1
        }
        
      } else if (datas$Group[i] == "Group2") {
        if(datas$Gender[i] == "Female"){
          group2XValues[index3X] <- x[i]
          index3X <- index3X + 1
        }else{
          group2XValues2[index4X] <- x[i]
          index4X <- index4X + 1
        }
      } else if (datas$Group[i] == "Group3") {
        if(datas$Gender[i] == "Female"){
          group3XValues[index5X] <- x[i]
          index5 <- index5X + 1
        }else{
          group3XValues2[index6X] <- x[i]
          index6X <- index6X + 1
        }
      } else {
        if(datas$Gender[i] == "Female"){
          group4XValues[index7X] <- x[i]
          index7X <- index7X + 1
        }else{
          group4XValues2[index8X] <- x[i]
          index8X <- index8X + 1
        }
      }
    }  
  }
  
  for(i in 1:length(y)) {
    if (!is.na(y[i])) {
      if (datas$Group[i] == "Group1"){
        if(datas$Gender[i] == "Female"){
          group1YValues[index1Y] <- y[i]
          index1Y <- index1Y + 1
        }else{
          group1YValues2[index2Y] <- y[i]
          index2Y <- index2Y + 1
        }
        
      } else if (datas$Group[i] == "Group2") {
        if(datas$Gender[i] == "Female"){
          group2YValues[index3Y] <- y[i]
          index3Y <- index3Y + 1
        }else{
          group2YValues2[index4Y] <- y[i]
          index4Y <- index4Y + 1
        }
      } else if (datas$Group[i] == "Group3") {
        if(datas$Gender[i] == "Female"){
          group3YValues[index5Y] <- y[i]
          index5Y <- index5Y + 1
        }else{
          group3YValues2[index6Y] <- y[i]
          index6Y <- index6Y + 1
        }
      } else {
        if(datas$Gender[i] == "Female"){
          group4YValues[index7Y] <- y[i]
          index7Y <- index7Y + 1
        }else{
          group4YValues2[index8Y] <- x[i]
          index8Y <- index8Y + 1
        }
      }
    }  
  }
  
  crossFemaleGroup1 <- crossProductFun(group1XValues, group1YValues)
  crossFemaleGroup2 <- crossProductFun(group2XValues, group2YValues)
  crossFemaleGroup3 <- crossProductFun(group3XValues, group3YValues)
  crossFemaleGroup4 <- crossProductFun(group4XValues, group4YValues)
  crossMaleGroup1 <- crossProductFun(group1XValues2, group1YValues2)
  crossMaleGroup2 <- crossProductFun(group2XValues2, group2YValues2)
  crossMaleGroup3 <- crossProductFun(group3XValues2, group3YValues2)
  crossMaleGroup4 <- crossProductFun(group4XValues2, group4YValues2)
  
  return (list(Group1 = list(Female = crossFemaleGroup1, Male = crossMaleGroup1), 
               Group2 = list(Female = crossFemaleGroup2, Male = crossMaleGroup2), 
               Group3 = list(Female = crossFemaleGroup3, Male = crossMaleGroup3), 
               Group4 = list(Female = crossFemaleGroup4, Male = crossMaleGroup4)))
}

covarianceByGenderAndGroup <- function (x,y) {
  group1XValues <- c(NULL)
  group2XValues <- c(NULL)
  group3XValues <- c(NULL)
  group4XValues <- c(NULL)
  group1XValues2 <- c(NULL)
  group2XValues2 <- c(NULL)
  group3XValues2 <- c(NULL)
  group4XValues2 <- c(NULL)
  
  group1YValues <- c(NULL)
  group2YValues <- c(NULL)
  group3YValues <- c(NULL)
  group4YValues <- c(NULL)
  group1YValues2 <- c(NULL)
  group2YValues2 <- c(NULL)
  group3YValues2 <- c(NULL)
  group4YValues2 <- c(NULL)
  
  index1X <- 1
  index2X <- 1
  index3X <- 1
  index4X <- 1
  index5X <- 1
  index6X <- 1
  index7X <- 1
  index8X <- 1
  
  index1Y <- 1
  index2Y <- 1
  index3Y <- 1
  index4Y <- 1
  index5Y <- 1
  index6Y <- 1
  index7Y <- 1
  index8Y <- 1
  
  covarianceFemaleGroup1 <- 0
  covarianceFemaleGroup2 <- 0
  covarianceFemaleGroup3 <- 0
  covarianceFemaleGroup4 <- 0
  covarianceMaleGroup1 <- 0
  covarianceMaleGroup2 <- 0
  covarianceMaleGroup3 <- 0
  covarianceMaleGroup4 <- 0
  
  for(i in 1:length(x)) {
    if (!is.na(x[i])) {
      if (datas$Group[i] == "Group1"){
        if(datas$Gender[i] == "Female"){
          group1XValues[index1X] <- x[i]
          index1X <- index1X + 1
        }else{
          group1XValues2[index2X] <- x[i]
          index2X <- index2X + 1
        }
        
      } else if (datas$Group[i] == "Group2") {
        if(datas$Gender[i] == "Female"){
          group2XValues[index3X] <- x[i]
          index3X <- index3X + 1
        }else{
          group2XValues2[index4X] <- x[i]
          index4X <- index4X + 1
        }
      } else if (datas$Group[i] == "Group3") {
        if(datas$Gender[i] == "Female"){
          group3XValues[index5X] <- x[i]
          index5 <- index5X + 1
        }else{
          group3XValues2[index6X] <- x[i]
          index6X <- index6X + 1
        }
      } else {
        if(datas$Gender[i] == "Female"){
          group4XValues[index7X] <- x[i]
          index7X <- index7X + 1
        }else{
          group4XValues2[index8X] <- x[i]
          index8X <- index8X + 1
        }
      }
    }  
  }
  
  for(i in 1:length(y)) {
    if (!is.na(y[i])) {
      if (datas$Group[i] == "Group1"){
        if(datas$Gender[i] == "Female"){
          group1YValues[index1Y] <- y[i]
          index1Y <- index1Y + 1
        }else{
          group1YValues2[index2Y] <- y[i]
          index2Y <- index2Y + 1
        }
        
      } else if (datas$Group[i] == "Group2") {
        if(datas$Gender[i] == "Female"){
          group2YValues[index3Y] <- y[i]
          index3Y <- index3Y + 1
        }else{
          group2YValues2[index4Y] <- y[i]
          index4Y <- index4Y + 1
        }
      } else if (datas$Group[i] == "Group3") {
        if(datas$Gender[i] == "Female"){
          group3YValues[index5Y] <- y[i]
          index5Y <- index5Y + 1
        }else{
          group3YValues2[index6Y] <- y[i]
          index6Y <- index6Y + 1
        }
      } else {
        if(datas$Gender[i] == "Female"){
          group4YValues[index7Y] <- y[i]
          index7Y <- index7Y + 1
        }else{
          group4YValues2[index8Y] <- x[i]
          index8Y <- index8Y + 1
        }
      }
    }  
  }
  
  covarianceFemaleGroup1 <- covarianceFun(group1XValues, group1YValues)
  covarianceFemaleGroup2 <- covarianceFun(group2XValues, group2YValues)
  covarianceFemaleGroup3 <- covarianceFun(group3XValues, group3YValues)
  covarianceFemaleGroup4 <- covarianceFun(group4XValues, group4YValues)
  covarianceMaleGroup1 <- covarianceFun(group1XValues2, group1YValues2)
  covarianceMaleGroup2 <- covarianceFun(group2XValues2, group2YValues2)
  covarianceMaleGroup3 <- covarianceFun(group3XValues2, group3YValues2)
  covarianceMaleGroup4 <- covarianceFun(group4XValues2, group4YValues2)
  
  return (list(Group1 = list(Female = covarianceFemaleGroup1, Male = covarianceMaleGroup1), 
               Group2 = list(Female = covarianceFemaleGroup2, Male = covarianceMaleGroup2), 
               Group3 = list(Female = covarianceFemaleGroup3, Male = covarianceMaleGroup3), 
               Group4 = list(Female = covarianceFemaleGroup4, Male = covarianceMaleGroup4)))
}

correlationByGenderAndGroup <- function (x,y) {
  group1XValues <- c(NULL)
  group2XValues <- c(NULL)
  group3XValues <- c(NULL)
  group4XValues <- c(NULL)
  group1XValues2 <- c(NULL)
  group2XValues2 <- c(NULL)
  group3XValues2 <- c(NULL)
  group4XValues2 <- c(NULL)
  
  group1YValues <- c(NULL)
  group2YValues <- c(NULL)
  group3YValues <- c(NULL)
  group4YValues <- c(NULL)
  group1YValues2 <- c(NULL)
  group2YValues2 <- c(NULL)
  group3YValues2 <- c(NULL)
  group4YValues2 <- c(NULL)
  
  index1X <- 1
  index2X <- 1
  index3X <- 1
  index4X <- 1
  index5X <- 1
  index6X <- 1
  index7X <- 1
  index8X <- 1
  
  index1Y <- 1
  index2Y <- 1
  index3Y <- 1
  index4Y <- 1
  index5Y <- 1
  index6Y <- 1
  index7Y <- 1
  index8Y <- 1
  
  correlationFemaleGroup1 <- 0
  correlationFemaleGroup2 <- 0
  correlationFemaleGroup3 <- 0
  correlationFemaleGroup4 <- 0
  correlationMaleGroup1 <- 0
  correlationMaleGroup2 <- 0
  correlationMaleGroup3 <- 0
  correlationMaleGroup4 <- 0
  
  for(i in 1:length(x)) {
    if (!is.na(x[i])) {
      if (datas$Group[i] == "Group1"){
        if(datas$Gender[i] == "Female"){
          group1XValues[index1X] <- x[i]
          index1X <- index1X + 1
        }else{
          group1XValues2[index2X] <- x[i]
          index2X <- index2X + 1
        }
        
      } else if (datas$Group[i] == "Group2") {
        if(datas$Gender[i] == "Female"){
          group2XValues[index3X] <- x[i]
          index3X <- index3X + 1
        }else{
          group2XValues2[index4X] <- x[i]
          index4X <- index4X + 1
        }
      } else if (datas$Group[i] == "Group3") {
        if(datas$Gender[i] == "Female"){
          group3XValues[index5X] <- x[i]
          index5 <- index5X + 1
        }else{
          group3XValues2[index6X] <- x[i]
          index6X <- index6X + 1
        }
      } else {
        if(datas$Gender[i] == "Female"){
          group4XValues[index7X] <- x[i]
          index7X <- index7X + 1
        }else{
          group4XValues2[index8X] <- x[i]
          index8X <- index8X + 1
        }
      }
    }  
  }
  
  for(i in 1:length(y)) {
    if (!is.na(y[i])) {
      if (datas$Group[i] == "Group1"){
        if(datas$Gender[i] == "Female"){
          group1YValues[index1Y] <- y[i]
          index1Y <- index1Y + 1
        }else{
          group1YValues2[index2Y] <- y[i]
          index2Y <- index2Y + 1
        }
        
      } else if (datas$Group[i] == "Group2") {
        if(datas$Gender[i] == "Female"){
          group2YValues[index3Y] <- y[i]
          index3Y <- index3Y + 1
        }else{
          group2YValues2[index4Y] <- y[i]
          index4Y <- index4Y + 1
        }
      } else if (datas$Group[i] == "Group3") {
        if(datas$Gender[i] == "Female"){
          group3YValues[index5Y] <- y[i]
          index5Y <- index5Y + 1
        }else{
          group3YValues2[index6Y] <- y[i]
          index6Y <- index6Y + 1
        }
      } else {
        if(datas$Gender[i] == "Female"){
          group4YValues[index7Y] <- y[i]
          index7Y <- index7Y + 1
        }else{
          group4YValues2[index8Y] <- x[i]
          index8Y <- index8Y + 1
        }
      }
    }  
  }
  
  correlationFemaleGroup1 <- correlationFun(group1XValues, group1YValues)
  correlationFemaleGroup2 <- correlationFun(group2XValues, group2YValues)
  correlationFemaleGroup3 <- correlationFun(group3XValues, group3YValues)
  correlationFemaleGroup4 <- correlationFun(group4XValues, group4YValues)
  correlationMaleGroup1 <- correlationFun(group1XValues2, group1YValues2)
  correlationMaleGroup2 <- correlationFun(group2XValues2, group2YValues2)
  correlationMaleGroup3 <- correlationFun(group3XValues2, group3YValues2)
  correlationMaleGroup4 <- correlationFun(group4XValues2, group4YValues2)
  
  return (list(Group1 = list(Female = correlationFemaleGroup1, Male = correlationMaleGroup1), 
               Group2 = list(Female = correlationFemaleGroup2, Male = correlationMaleGroup2), 
               Group3 = list(Female = correlationFemaleGroup3, Male = correlationMaleGroup3), 
               Group4 = list(Female = correlationFemaleGroup4, Male = correlationMaleGroup4)))
}
#Scatter Plot function
scatterPlotFun <- function(x,y){
  vecX <- c(NULL)
  vecY <- c(NULL)
  data_name_x <- lapply(substitute(x)[-1], deparse)
  data_name_y <- lapply(substitute(y)[-1], deparse)
  
  for (i in 1:length(x)) {
    if(is.na(x[i])| is.na(y[i])){
      vecX <- vecX[-i]
      vecY <- vecY[-i]
    }else{
      vecX[i] <- x[i]
      vecY[i] <- y[i]
    }
  }
  
  all_colors <- c("aquamarine4", "chartreuse4", "chocolate4", "blue3", "palevioletred3","darkgreen","firebrick4","darkorchid4")
  color <- adjustcolor(sample(all_colors, 1))
  if (length(vecX) != length(vecY)) {
    stop("Vectors' length are not same.")
  } else {
    plot(vecX, vecY, xlim = rangeFun(vecX), ylim = rangeFun(vecY), col=color, pch=19, 
         xlab = data_name_x[2], ylab = data_name_y[2])
  }
}
#Scatter Plot Matrix Function
scatterPlotMatrixFun <- function(...) {
  dataList <- list(...)
  dataLength <- length(dataList)
  par(mfrow = c(dataLength, dataLength), mar = c(2, 2, 1, 1))
  data_names <- lapply(substitute(list(...))[-1], deparse)
  column_names <- sapply(data_names, function(x) tail(strsplit(x, "\\$")[[1]], 1))
  
  for (i in 1:dataLength) {
    for (j in 1:dataLength) {
      if (i == j) {
        plot(0, type="n", xlab="", ylab="", frame.plot=TRUE, col="black",axes=FALSE )
        text(x =1, y = 0, labels = column_names[i], cex=2)
      } else {
        scatterPlotFun(dataList[[i]], dataList[[j]])
      }
    }
  }
}
#Scale function
scaleFun <- function(x) {
  vars <- c(NULL)
  
  index <- 1
  
  mean <- meanFun(x)
  standardDeviation <- standardDeviationFun(x)
  
  for(i in 1:length(x)) {
    if(!is.na(x[i])) {
      vars[index] <- (x[i] - mean) / standardDeviation
      index <- index + 1
    }
  }
  return (list(Scaled = vars))
}