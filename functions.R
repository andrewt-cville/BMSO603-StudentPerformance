subset_data = function (df, trainPct)
{
  set.seed(4566)
  inTrain <- sample(nrow(df), 0.5*nrow(df))
  
  dftrain <- data.frame(df[inTrain,])
  dftemp <- data.frame(df[-inTrain,])
  inVal <- sample(nrow(dftemp),trainPct*nrow(dftemp))
  dfvalidation <- data.frame(dftemp[inVal,])
  dftest <- data.frame(dftemp[-inVal,])
  dftemp <- NULL
  
  return(list(dftrain, dfvalidation, dftest))
}


best_k = function (kmax, dftrain, dfvalidation, dftest, col_index)
{
  set.seed(1879)
  
  ER1 <- rep(0,kmax)
  ER2 <- rep(0,kmax)
  
  for (i in 1:kmax) {
    prediction <- knn(dftrain[,-col_index], dftrain[,-col_index],dftrain[,col_index], k=i)
    prediction2 <- knn(dftrain[,-col_index], dfvalidation[,-col_index],dftrain[,col_index], k=i)
    prediction3 <- knn(dftrain[,-col_index], dftest[,-col_index],dftrain[,col_index], k=i)
    #
    # The confusion matrix for training data is:
    CM1 <- table(prediction, dftrain[,4])
    # The training error rate is:
    ER1[i] <- (CM1[1,2]+CM1[2,1])/sum(CM1)
    # The confusion matrix for validation data is: 
    CM2 <- table(prediction2, dfvalidation[,4])
    ER2[i] <- (CM2[1,2]+CM2[2,1])/sum(CM2)
  }
  plot(c(1,kmax),c(0,.6),type="n", xlab="k",ylab="Error Rate")
  lines(ER1,col="red")
  lines(ER2,col="blue")
  legend(7, 0.1, c("Training","Validation"),lty=c(1,1), col=c("red","blue"))
  z <- which.min(ER2)
  cat("Minimum Validation Error k:", z)
  points(z,ER2[z],col="red",cex=2,pch=20)
  
  return (z)
}

calculate_error_rate = function (df, prediction, col_index)
{
  confusion = table(df[,col_index],prediction)
  error_rate = ((confusion[1,2] + confusion[2,1]) / (sum(confusion)))
  return (error_rate)
}

calculate_rmse = function (error_rate)
{
  return (sqrt(mean(error_rate^2)))
}