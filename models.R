setwd("C:/Users/Nefeli/Desktop/Lab/Διδακτορικό/Upper Lower Epiph/Lower Limbs")

# Library loading
library(readr)
library(Metrics)
library(dplyr)
library(caret)
library(e1071)

bone <- "femur"
alg <- "SVM"

# Load the bone data
dataset <- read.csv("Femur 20-80 vars.csv")

dataset <- dataset[,-c(1,2)] # Delete the index column

y <- 0
x <- 0

for (i in 1:8)
{
  y[i] <- paste0("Y",i)
  x[i] <- paste0("X",i)
}

colnames(dataset) <- c(y,x)
remove(y,x,i)

set.seed(1993)
partition = createDataPartition(dataset$Y1, times = 1, p=0.8)

idx <- partition[[1]]

##### LR models
data <- dataset[idx,-c(5,8,13,16)]
unsc_test <- dataset[-idx,-c(5,8,13,16)]
test <- unsc_test

data <- scale(data)

scaleList <- list(scale = attr(data, "scaled:scale"),
                  center = attr(data, "scaled:center"))

data <- as.data.frame(data)

# 20% variable
mlm1 <- lm(cbind(Y1, Y2, Y3, Y4, Y6, Y7) ~ X1 + X2 + X3 + X4 + X6 + X7, 
           data = data)
saveRDS(mlm1, paste0(bone, "_20_LR.rds"))



test[,7] <- (test[,7]-scaleList$center["X1"])/scaleList$scale["X1"]
test[,8] <- (test[,8]-scaleList$center["X2"])/scaleList$scale["X2"]
test[,9] <- (test[,9]-scaleList$center["X3"])/scaleList$scale["X3"]
test[,10] <- (test[,10]-scaleList$center["X4"])/scaleList$scale["X4"]
test[,11] <- (test[,11]-scaleList$center["X6"])/scaleList$scale["X6"]
test[,12] <- (test[,12]-scaleList$center["X7"])/scaleList$scale["X7"]

predY <- predict(mlm1, newdata = test[, c(7:12)])

predY[,1] <- (predY[,1]*scaleList$scale["Y1"]) + scaleList$center["Y1"]
predY[,2] <- (predY[,2]*scaleList$scale["Y2"])+ scaleList$center["Y2"]
predY[,3] <- (predY[,3]*scaleList$scale["Y3"])+ scaleList$center["Y3"]
predY[,4] <- (predY[,4]*scaleList$scale["Y4"])+ scaleList$center["Y4"]
predY[,5] <- (predY[,5]*scaleList$scale["Y6"])+ scaleList$center["Y6"]
predY[,6] <- (predY[,6]*scaleList$scale["Y7"])+ scaleList$center["Y7"]

pred_20 <- predY

remove(predY)

# 80% variable
mlm2 <- lm(cbind(X1, X2, X3, X4, X6, X7) ~ Y1 + Y2 + Y3 + Y4 + Y6 + Y7, 
           data = data)
saveRDS(mlm2, paste0(bone, "_80_LR.rds"))

test[,1] <- (test[,1]-scaleList$center["Y1"])/scaleList$scale["Y1"]
test[,2] <- (test[,2]-scaleList$center["Y2"])/scaleList$scale["Y2"]
test[,3] <- (test[,3]-scaleList$center["Y3"])/scaleList$scale["Y3"]
test[,4] <- (test[,4]-scaleList$center["Y4"])/scaleList$scale["Y4"]
test[,5] <- (test[,5]-scaleList$center["Y6"])/scaleList$scale["Y6"]
test[,6] <- (test[,6]-scaleList$center["Y7"])/scaleList$scale["Y7"]

predY <- predict(mlm2, newdata = test[, c(1:6)])

predY[,1] <- (predY[,1]*scaleList$scale["X1"]) + scaleList$center["X1"]
predY[,2] <- (predY[,2]*scaleList$scale["X2"])+ scaleList$center["X2"]
predY[,3] <- (predY[,3]*scaleList$scale["X3"])+ scaleList$center["X3"]
predY[,4] <- (predY[,4]*scaleList$scale["X4"])+ scaleList$center["X4"]
predY[,5] <- (predY[,5]*scaleList$scale["X6"])+ scaleList$center["X6"]
predY[,6] <- (predY[,6]*scaleList$scale["X7"])+ scaleList$center["X7"]

pred_80 <- predY

  ##### SVM models
  data <- dataset[idx,-c(5,8,13,16)]
  test <- dataset[-idx,-c(5,8,13,16)]
  
  vars <- c("Y1", "Y2", "Y3", "Y4", "Y6", "Y7")
  predY <- 0
  
  
  for (i in 1:6)
  {
    form <- as.formula(paste(vars[i], " ~ X1 + X2 + X3 + X4 + X6 + X7"))
    nam <- paste("mlm20", i, sep="_")
    assign(nam, svm(formula = form, 
                     data = data, kernel = "linear", cost=10,
                     scale = TRUE))
    
    mod <- svm(formula = form, 
               data = data, kernel = "linear", cost=10,
               scale = TRUE)
    pred <- predict(mod, newdata = test[, c(7:12)])
    predY <- cbind(predY, pred)
  }
  
  predY <- predY[,-1]
  colnames(predY) <- vars
  
  pred_20 <- predY
  remove(predY, vars)
  
  svm_20 <- list(mlm20_1, mlm20_2, mlm20_3, mlm20_4, mlm20_5, mlm20_6)
  saveRDS(svm_20, paste0(bone, "_20_SVM.rds"))
  
  # Predicting the 80% variables
  
  vars <- c("X1", "X2", "X3", "X4", "X6", "X7")
  predY <- 0
  
  for (i in 1:6)
  {
    form <- as.formula(paste(vars[i], " ~ Y1 + Y2 + Y3 + Y4 + Y6 + Y7"))
    nam <- paste("mlm80", i, sep="_")
    assign(nam, svm(formula = form, 
                    data = data, kernel = "linear", cost=10,
                    scale = TRUE))
    
    mod <- svm(formula = form, 
               data = data, kernel = "linear", cost=10,
               scale = TRUE)
    pred <- predict(mod, newdata = test[, c(1:6)])
    predY <- cbind(predY, pred)
  }
  
  predY <- predY[,-1]
  colnames(predY) <- vars
  
  pred_80 <- predY
  remove(predY, vars)
  
  svm_80 <- list(mlm80_1, mlm80_2, mlm80_3, mlm80_4, mlm80_5, mlm80_6)
  saveRDS(svm_80, paste0(bone, "_80_svm.rds"))
  
  ##### Threshold calculation
  
  # Threshold for 20% predictions
  true_difs <- matrix(0, nrow = nrow(test), ncol= 6)
  colnames(true_difs) <- names(test)[1:6]
  for (i in 1:nrow(test))
  {
    true_difs[i,] <- abs(as.numeric(unsc_test[i,1:6] - pred_20[i,]))
  }
  
  u_thr_20 <- 0
  l_thr_20 <- 0
  
  for (i in 1:6)
  {
    u_thr_20[i] <- mean(true_difs[,i]) + 2*sd(true_difs[,i])
    l_thr_20[i] <- mean(true_difs[,i]) - 2*sd(true_difs[,i])
  }
  
  remove(true_difs)
  
  # Threshold for 80% predictions
  true_difs <- matrix(0, nrow = nrow(test), ncol= 6)
  colnames(true_difs) <- names(test)[7:12]
  for (i in 1:nrow(test))
  {
    true_difs[i,] <- abs(as.numeric(unsc_test[i,7:12] - pred_80[i,]))
  }
  
  u_thr_80 <- 0
  l_thr_80 <- 0
  
  for (i in 1:6)
  {
    u_thr_80[i] <- mean(true_difs[,i]) + 2*sd(true_difs[,i])
    l_thr_80[i] <- mean(true_difs[,i]) - 2*sd(true_difs[,i])
  }
  
  remove(true_difs)
  
  
  thresholds <- rbind(u_thr_20, l_thr_20, u_thr_80, l_thr_80)
  write.csv(thresholds, paste0(bone, "_thr_", alg, ".csv"))
