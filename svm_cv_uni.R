setwd("C:/Users/Nefeli/Desktop/Lab/Διδακτορικό/Upper Lower Epiph/Lower Limbs")

# Library loading
library(readr)
library(caret) # for the partition of the dataset
library(e1071)
library(Metrics)
library(dplyr)   

# Functions
which.mins <- function(x, mins=6) {
  head(order(x), mins)
}


# Load the Femur data
dataset <- read.csv("Femur 20-80 vars.csv")

sample_ids <- dataset[,2]
dataset <- dataset[,-c(1,2)] # Delete the index column

vars_full <- names(dataset)
vars_full <- vars_full[-c(5,8,13,16)]

y <- 0
x <- 0

## Predicting the 20% variables
y <- 0
x <- 0

for (i in 1:8)
{
  y[i] <- paste0("Y",i)
  x[i] <- paste0("X",i)
}

colnames(dataset) <- c(y,x)
remove(y,x,i)

set.seed(1997)
partition = createDataPartition(dataset$Y1, times = 20, p=0.8)
meth <- c("euclidean", "maximum", "manhattan", "canberra",
          "minkowski")

for (f in 1:20)
{
  
idx <- partition[[f]]
data <- dataset[idx, -c(5,8,13,16)]

test <- dataset[-idx,-c(5,8,13,16)]

# Predicting the 20% variables
vars <- c("Y1", "Y2", "Y3", "Y4", "Y6", "Y7")
predY <- 0
for (i in 1:6)
{
  form <- as.formula(paste(vars[i], " ~ X1 + X2 + X3 + X4 + X6 + X7"))
  mlm1 <- svm(formula = form, 
           data = data, kernel = "linear", cost=10,
           scale = TRUE)

  pred <- predict(mlm1, newdata = test[, c(7:12)])
  predY <- cbind(predY, pred)
}

predY <- predY[,-1]
colnames(predY) <- vars

mae_20 <- 0
for (i in 1:6)
{
  mae_20[i] <- mae(test[,i], predY[,i])
}

pred_20 <- predY
remove(predY, vars)

# Predicting the 80% variables
vars <- c("X1", "X2", "X3", "X4", "X6", "X7")
predY <- 0
for (i in 1:6)
{
  form <- as.formula(paste(vars[i], " ~ Y1 + Y2 + Y3 + Y4 + Y6 + Y7"))
  mlm2 <- svm(formula = form, data = data,
              kernel = "linear", cost=10, 
              scale = TRUE)
  
  pred <- predict(mlm2, newdata = test[, c(1:6)])
  predY <- cbind(predY, pred)
}

predY <- predY[,-1]
colnames(predY) <- vars

mae_80 <- 0
for (i in 1:6)
{
  mae_80[i] <- mae(test[,6+i], predY[,i])
}

pred_80 <- predY
remove(predY, vars)

mae_vars <- cbind(mae_20, mae_80)
colnames(mae_vars) <- c("Y: 20%", "Y: 80%")
rownames(mae_vars) <- c("Area", "Perimeter", "Ix", "Iy", "Imin", "Imax")

write.csv(mae_vars, paste0("./Femur/CV_SVM_80/Fold_", f, "_MAE.csv"))

meth_acc <- matrix(0, nrow=5, ncol=12)
meth_tnr <- matrix(0, nrow=5, ncol=12)

for (m in 1:5)
{
  
#Sorting

results_20 <- matrix(0, nrow=5, ncol=6)

m_dist_20 <- round(mean(dist),2)

u_thr_20 <- 0
l_thr_20 <- 0

mism_20 <- matrix(0, nrow=nrow(test), ncol=6)

for (v in 1:6)
{
  # Threshold for 20% predictions
  true_difs <- matrix(0, nrow = nrow(test), ncol=1)
  colnames(true_difs) <- vars_full[v]
  dist <- 0
  for (i in 1:nrow(test))
  {
    y <- rbind(test[i,v],pred_20[i,v])
    true_difs[i,] <- abs(as.numeric(test[i,v] - pred_20[i,v]))
    dist[i] <- dist(y, method=meth[m], p=1.5)
  }
  
  u_thr_20[v] <- mean(true_difs) + 2*sd(true_difs)
  l_thr_20[v] <- mean(true_difs) - 2*sd(true_difs)
  
  
  remove(true_difs, dist, y)
  
  
# 20% - Minimum five
pr_label<-0
pr_idx <-0

five_pr<-matrix(0, nrow=nrow(test), ncol=6)

for (i in 1:nrow(test))
{
  true <- test[i, v]
  name <- rownames(test[i,])
  
  vec <- c(true, pred_20[,v])
  y <- as.matrix(dist(vec, method = meth[m], p=1.5))
  rownames(y)[[1]] <- "true"
  
  ## Boolean indexing for out of boundaries
  dif <- abs(true-pred_20[,v])
  mism_ind <- ifelse(between(dif, l_thr_20[v], u_thr_20[v]), 0, 1)
  
  if (rownames(y)[i] != name)
  {
    mism_ind <- mism_ind[-i] 
  }
  
  mism_20[i,v] <- sum(mism_ind)
  
  g <- which.mins(y[,1])
  
  pr_idx <- as.numeric(names(y[g,1]))
  
  five_pr[i,] <-sample_ids[pr_idx]
  name <- c(name, rownames(y[-1,]))
  
  remove(true, vec, y)
}

five_pr<-cbind(sample_ids[-idx], five_pr)


  
five_pr <- five_pr[,-2]
colnames(five_pr)<-c("Observation #", "1st Choice", "2nd Choice", 
                     "3rd Choice", "4th Choice", "5th Choice")

results_20[,v] <-c(sum(five_pr[,1]==five_pr[,2]),
                   sum(five_pr[,1]==five_pr[,3]),
                   sum(five_pr[,1]==five_pr[,4]),
                   sum(five_pr[,1]==five_pr[,5]),
                   sum(five_pr[,1]==five_pr[,6]))

}

# 80 % - Minimum five

results_80 <- matrix(0, nrow=5, ncol=6)

m_dist_80 <- round(mean(dist),2)

u_thr_80 <- 0
l_thr_80 <- 0

mism_80 <- matrix(0, nrow=nrow(test), ncol=6)

for (v in 1:6)
{
  # Threshold for 80% predictions
  true_difs <- matrix(0, nrow = nrow(test), ncol=1)
  colnames(true_difs) <- vars_full[v+6]
  dist <- 0
  for (i in 1:nrow(test))
  {
    y <- rbind(test[i,v+6],pred_80[i,v])
    true_difs[i,] <- abs(as.numeric(test[i,v+6] - pred_80[i,v]))
    dist[i] <- dist(y, method=meth[m], p=1.5)
  }
  
  u_thr_80[v] <- mean(true_difs) + 2*sd(true_difs)
  l_thr_80[v] <- mean(true_difs) - 2*sd(true_difs)
  
  remove(true_difs, dist, y)
  
pr_label<-0
pr_idx <-0

five_pr<-matrix(0, nrow=nrow(test), ncol=6)

for (i in 1:nrow(test))
{
  true <- test[i, v+6]
  name <- rownames(test[i,])
  
  vec <- c(true, pred_80[,v])
  y <- as.matrix(dist(vec, method = meth[m], p=1.5))
  rownames(y)[[1]] <- "true"
  
  ## Boolean indexing for out of boundaries
  dif <- abs(true-pred_80[,v])
  mism_ind <- ifelse(between(dif, l_thr_80[v], u_thr_80[v]), 0, 1)
  
  if (rownames(y)[i] != name)
  {
    mism_ind <- mism_ind[-i] 
  }
  
  mism_80[i,v] <- sum(mism_ind)
  
  g <- which.mins(y[,1])
  
  pr_idx <- as.numeric(names(y[g,1]))
  
  five_pr[i,] <-sample_ids[pr_idx]
  name <- c(name, rownames(y[-1,]))
  
  remove(true, vec, y)
}

five_pr<-cbind(sample_ids[-idx], five_pr)

five_pr <- five_pr[,-2]


colnames(five_pr)<-c("Observation #", "1st Choice", "2nd Choice", 
                     "3rd Choice", "4th Choice", "5th Choice")

results_80[,v] <-c(sum(five_pr[,1]==five_pr[,2]),
                   sum(five_pr[,1]==five_pr[,3]),
                   sum(five_pr[,1]==five_pr[,4]),
                   sum(five_pr[,1]==five_pr[,5]),
                   sum(five_pr[,1]==five_pr[,6]))

}

results <- cbind(results_20, results_80)

results <-rbind(results, c(round(sum(results[,1])/nrow(test)*100,2),
                           round(sum(results[,2])/nrow(test)*100,2),
                           round(sum(results[,3])/nrow(test)*100,2),
                           round(sum(results[,4])/nrow(test)*100,2),
                           round(sum(results[,5])/nrow(test)*100,2),
                           round(sum(results[,6])/nrow(test)*100,2),
                           round(sum(results[,7])/nrow(test)*100,2),
                           round(sum(results[,8])/nrow(test)*100,2),
                           round(sum(results[,9])/nrow(test)*100,2),
                           round(sum(results[,10])/nrow(test)*100,2),
                           round(sum(results[,11])/nrow(test)*100,2),
                           round(sum(results[,12])/nrow(test)*100,2)))


rownames(results) <- c("1st Choice", "2nd Choice", "3rd Choice",
                       "4th Choice", "5th Choice", "Total")

colnames(results) <- vars_full

write.csv(results,
         paste0("./Femur/Univariate/CV_SVM_80/Fold_", f, "_PerVar_Accuracy_",
                meth[m], ".csv"))

mm <- cbind(mism_20, mism_80)
total_mm <- apply(mm, 2, "sum")

tnr <- total_mm/(nrow(test)*(nrow(test)-1))*100

meth_acc[m,] <- results[6,]

meth_tnr[m,] <- tnr
  
}

rownames(meth_acc) <- meth
colnames(meth_acc) <- vars_full
write.csv(meth_acc,
         paste0("./Femur/Univariate/CV_SVM_80/Fold_", f, "_Total_PerVar_Accuracy.csv"))

rownames(meth_tnr) <- meth
colnames(meth_tnr) <- vars_full
write.csv(meth_tnr,
         paste0("./Femur/Univariate/CV_SVM_80/Fold_", f, "_PerVar_TNR.csv"))
}


