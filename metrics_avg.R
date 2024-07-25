library(readr)

alg <- c("LR_80", "SVM_80")

meth <- c("euclidean", "maximum", "manhattan", "canberra",
          "minkowski")

path <- "C:/Users/Nefeli/Desktop/Lab/Διδακτορικό/Upper Lower Epiph/Lower Limbs/Tibia/"
path_uni <- "C:/Users/Nefeli/Desktop/Lab/Διδακτορικό/Upper Lower Epiph/Lower Limbs/Femur/Univariate/"
path_combo <- "C:/Users/Nefeli/Desktop/Lab/Διδακτορικό/Upper Lower Epiph/Lower Limbs/Tibia_combo/"


## Accuracy files

for (k in 1:2)
{
  setwd(paste0(path, "CV_", alg[k]))

  for (j in 1:5)
  {
    first <- rep(0, times=6)
    second <- rep(0, times=6) 
    third <- rep(0, times=6)
    fourth <- rep(0, times=6)
    fifth <- rep(0, times=6)
    total <-rep(0, times=6)
    for (i in 1:20)
    {
      data <- read.csv(paste0("Fold_", i, "_Accuracy_", meth[j], ".csv"))
      
      first <- rbind(first, data[1,2:7])
      second <- rbind(second, data[2,2:7]) 
      third <- rbind(third, data[3,2:7])
      fourth <- rbind(fourth, data[4,2:7])
      fifth <- rbind(fifth, data[5,2:7])
      total <-rbind(total, data[6,2:7])
    }
    
    results_mean <- matrix(0, nrow=6, ncol=7)
    results_mean[,1] <- data[,1]
    results_mean[1,2:7] <- apply(first[2:21,], 2, FUN="mean")
    results_mean[2,2:7] <- apply(second[2:21,], 2, FUN="mean")
    results_mean[3,2:7] <- apply(third[2:21,], 2, FUN="mean")
    results_mean[4,2:7] <- apply(fourth[2:21,], 2, FUN="mean")
    results_mean[5,2:7] <- apply(fifth[2:21,], 2, FUN="mean")
    results_mean[6,2:7] <- apply(total[2:21,], 2, FUN="mean")
    
    colnames(results_mean) <- c("Choice", "# - 20", "% Accuracy - 20", "# - 80", 
                                "% Accuracy - 80", 
                                "# - Combo", "% Accuracy - Combo")
    
    write.csv(results_mean, 
              paste0(path, "CV_Avg/Mean_Accuracy_", alg[k], "_", meth[j], ".csv"))
    
    results_sd <- matrix(0, nrow=6, ncol=7)
    results_sd[,1] <- data[,1]
    results_sd[1,2:7] <- apply(first[2:21,], 2, FUN="sd")
    results_sd[2,2:7] <- apply(second[2:21,], 2, FUN="sd")
    results_sd[3,2:7] <- apply(third[2:21,], 2, FUN="sd")
    results_sd[4,2:7] <- apply(fourth[2:21,], 2, FUN="sd")
    results_sd[5,2:7] <- apply(fifth[2:21,], 2, FUN="sd")
    results_sd[6,2:7] <- apply(total[2:21,], 2, FUN="sd")
    
    colnames(results_sd) <- c("Choice", "# - 20", "% Accuracy - 20", "# - 80", 
                                "% Accuracy - 80", 
                                "# - Combo", "% Accuracy - Combo")
    write.csv(results_sd, 
              paste0(path, "CV_Avg/SD_Accuracy_", alg[k], "_", meth[j], ".csv"))
  }
}

remove(data, first, second, third, fourth, fifth, total, 
       results_mean, results_sd, i, j, k)

## Mean Distance true files

for (k in 1:2)
{
  setwd(paste0(path, "CV_", alg[k]))
  results_mean <- matrix(0, nrow=5, ncol=3)
  results_sd <- matrix(0, nrow=5, ncol=3)
  
  for (j in 1:5)
  {
    pred_20 <- 0
    pred_80 <- 0
    pred_combo <- 0
    
    for (i in 1:20)
    {
      data <- read.csv(paste0("Fold_", i, "_MD_true_", meth[j], ".csv"))
      
      pred_20[i] <- data[1,2]
      pred_80[i] <- data[2,2] 
      pred_combo[i] <- data[3,2]
      
    }
    
    results_mean[j,1] <- round(mean(pred_20),2)
    results_mean[j,2] <- round(mean(pred_80),2)
    results_mean[j,3] <- round(mean(pred_combo),2)
    
    rownames(results_mean) <- meth
    colnames(results_mean) <- c("Pred 20", "Pred 80", "Pred Combo")
    
    write.csv(results_mean, 
              paste0(path, "CV_Avg/Mean_MD_true_", alg[k], ".csv"))
    
    results_sd[j,1] <- round(sd(pred_20),2)
    results_sd[j,2] <- round(sd(pred_80),2)
    results_sd[j,3] <- round(sd(pred_combo),2)
    
    rownames(results_sd) <- meth
    colnames(results_sd) <- c("Pred 20", "Pred 80", "Pred Combo")
    
    write.csv(results_sd, 
              paste0(path, "CV_Avg/SD_MD_true_", alg[k],  ".csv"))
  }
}


remove(data, pred_20, pred_80, pred_combo,
       results_mean, results_sd, i, k)


## MAE

for (k in 1:2)
{
  setwd(paste0(path, "CV_", alg[k]))
  results_mean <- matrix(0, nrow=6, ncol=2)
  results_sd <- matrix(0, nrow=6, ncol=2)
  
  pred_20 <- rep(0, times=6)
  pred_80 <- rep(0, times=6)
    
    for (i in 1:20)
    {
      data <- read.csv(paste0("Fold_", i, "_MAE.csv"))
      
      pred_20 <- cbind(pred_20, data[,2])
      pred_80 <- cbind(pred_20, data[,3])
      
    }
    
  pred_20 <- pred_20[,-1]
  pred_80 <- pred_80[,-1]
  
  results_mean[,1] <- apply(pred_20, 1, FUN="mean")
  results_mean[,2] <- apply(pred_80, 1, FUN="mean")
  
  rownames(results_mean) <- data[,1]
  colnames(results_mean) <- c("MAE Pred 20", "MAE Pred 80")
    
  write.csv(results_mean, 
            paste0(path, "CV_Avg/Mean_MAE_", alg[k], ".csv"))
    
  results_sd[,1] <- apply(pred_20, 1, FUN="sd")
  results_sd[,2] <- apply(pred_80, 1, FUN="sd")
    
  rownames(results_mean) <- data[,1]
  colnames(results_mean) <- c("MAE Pred 20", "MAE Pred 80")
  
  write.csv(results_sd, 
            paste0(path, "CV_Avg/SD_MAE_", alg[k],  ".csv"))

}

remove(data, pred_20, pred_80, 
       results_mean, results_sd, i,  k)

## Definitive pairs

results_mean <- matrix(0, nrow=5, ncol=2)
results_sd <- matrix(0, nrow=5, ncol=2)

for (k in 1:2)
{
  setwd(paste0(path, "CV_", alg[k]))
  def_m <- 0
  
  for (j in 1:5)
  {
    sorted <- 0
    
    for (i in 1:20)
    {
      data <- read.csv(paste0("Fold_", i, "_stats_", meth[j], ".csv"))
      
      sorted[i] <- data[2,2]
      
    }
    
    results_mean[j,k] <- mean(sorted)
    results_sd[j,k] <- sd(sorted)
  }
  
}

rownames(results_mean) <- meth
colnames(results_mean) <- alg
write.csv(results_mean, 
          paste0(path, "CV_Avg/Mean_stats.csv"))


rownames(results_sd) <- meth
colnames(results_sd) <- alg

write.csv(results_sd, 
          paste0(path, "CV_Avg/SD_stats.csv"))

remove(data, pred_20, pred_80, pred_combo,
       results_mean, results_sd, i, k)

## Definitive mismatches

results_mean <- matrix(0, nrow=5, ncol=2)
results_sd <- matrix(0, nrow=5, ncol=2)

for (k in 1:2)
{
  setwd(paste0(path, "CV_", alg[k]))
  
  for (j in 1:5)
  {
    tnr <- 0
    
    for (i in 1:20)
    {
      data <- read.csv(paste0("Fold_", i, "_stats_", meth[j], ".csv"))
      
      tnr[i] <- data[4,2]
      
    }
    
    results_mean[j,k] <- mean(tnr)
    results_sd[j,k] <- sd(tnr)
  }
  
}

rownames(results_mean) <- meth
colnames(results_mean) <- alg
write.csv(results_mean, 
          paste0(path, "CV_Avg/Mean_mism.csv"))


rownames(results_sd) <- meth
colnames(results_sd) <- alg

write.csv(results_sd, 
          paste0(path, "CV_Avg/SD_mism.csv"))

remove(data, results_mean, results_sd, i, k)

## False negatives

results_mean <- matrix(0, nrow=5, ncol=2)
results_sd <- matrix(0, nrow=5, ncol=2)

for (k in 1:2)
{
  setwd(paste0(path, "CV_", alg[k]))
  
  for (j in 1:5)
  {
    fn <- 0
    
    for (i in 1:20)
    {
      data <- read.csv(paste0("Fold_", i, "_stats_", meth[j], ".csv"))
      
      fn[i] <- data[5,2]
      
    }
    
    results_mean[j,k] <- mean(fn)
    results_sd[j,k] <- sd(fn)
  }
  
}

rownames(results_mean) <- meth
colnames(results_mean) <- alg
write.csv(results_mean, 
          paste0(path, "CV_Avg/Mean_fn.csv"))


rownames(results_sd) <- meth
colnames(results_sd) <- alg

write.csv(results_sd, 
          paste0(path, "CV_Avg/SD_fn.csv"))

remove(data, results_mean, results_sd, i, k)

#### Univariate stuff

# Total Per Var Accuracy
for (k in 1:2)
{
  setwd(paste0(path_uni, "CV_", alg_uni[k]))
  
  euc <- rep(0, times=12)
  maximum <- rep(0, times=12) 
  man <- rep(0, times=12)
  cranb <- rep(0, times=12)
  mink <- rep(0, times=12)
    
  for (i in 1:20)
    {
      data <- read.csv(paste0("Fold_", i, "_Total_PerVar_Accuracy.csv"))
      
      euc <- rbind(euc, data[1, 2:13])
      maximum <- rbind(maximum, data[2, 2:13]) 
      man <- rbind(man, data[3, 2:13])
      cranb <- rbind(cranb, data[4, 2:13])
      mink <- rbind(mink, data[5, 2:13])
    }
    
  results_mean <- matrix(0, nrow=5, ncol=13)
  results_mean[,1] <- data[,1]
  results_mean[1,2:13] <- apply(euc[2:21,], 2, FUN="mean")
  results_mean[2,2:13] <- apply(maximum[2:21,], 2, FUN="mean")
  results_mean[3,2:13] <- apply(man[2:21,], 2, FUN="mean")
  results_mean[4,2:13] <- apply(cranb[2:21,], 2, FUN="mean")
  results_mean[5,2:13] <- apply(mink[2:21,], 2, FUN="mean")
    
  colnames(results_mean) <- c("Distance", names(data)[2:13])
    
  write.csv(results_mean, 
            paste0(path_uni, "CV_Avg/Mean_Total_PerVar_Accuracy_", alg_uni[k], ".csv"))
    
  results_mean <- matrix(0, nrow=5, ncol=13)
  results_mean[,1] <- data[,1]
  results_mean[1,2:13] <- apply(euc[2:21,], 2, FUN="sd")
  results_mean[2,2:13] <- apply(maximum[2:21,], 2, FUN="sd")
  results_mean[3,2:13] <- apply(man[2:21,], 2, FUN="sd")
  results_mean[4,2:13] <- apply(cranb[2:21,], 2, FUN="sd")
  results_mean[5,2:13] <- apply(mink[2:21,], 2, FUN="sd")
  
  colnames(results_mean) <- c("Distance", names(data)[2:13])
  
  write.csv(results_mean, 
            paste0(path_uni, "CV_Avg/SD_Total_PerVar_Accuracy_", alg_uni[k], ".csv"))
  
}

remove(data, euc, maximum, man, cranb, mink, 
       results_mean, results_sd, i, k)

# Total Per Var TNR
for (k in 1:2)
{
  setwd(paste0(path_uni, "CV_", alg_uni[k]))
  
  tnr <- rep(0, times=12)
  
  for (i in 1:20)
  {
    data <- read.csv(paste0("Fold_", i, "_PerVar_TNR.csv"))
    
    tnr <- rbind(tnr, data[1, 2:13])

  }
  
  results <- matrix(0, nrow=2, ncol=13)
  results[,1] <- c("Mean TNR", "SD TNR")
  
  colnames(results) <- c("Metric", names(data)[2:13])
  
  results[1,2:13] <- apply(tnr[2:21,], 2, FUN="mean")
  results[2,2:13] <- apply(tnr[2:21,], 2, FUN="sd")
  
  write.csv(results, 
            paste0(path_uni, "CV_Avg/TNR_", alg_uni[k], ".csv"))
  
}

remove(data, tnr, 
       results, i, k)


### NOT ALL VARS COMBO

## Accuracy files for combo

for (k in 1:2)
{
  setwd(paste0(path_combo, "CV_", alg_uni[k]))
  
  for (j in 1:5)
  {
    first <- rep(0, times=6)
    second <- rep(0, times=6) 
    third <- rep(0, times=6)
    fourth <- rep(0, times=6)
    fifth <- rep(0, times=6)
    total <-rep(0, times=6)
    for (i in 1:20)
    {
      data <- read.csv(paste0("Fold_", i, "_Accuracy_", meth[j], ".csv"))
      
      first <- rbind(first, data[1,2:7])
      second <- rbind(second, data[2,2:7]) 
      third <- rbind(third, data[3,2:7])
      fourth <- rbind(fourth, data[4,2:7])
      fifth <- rbind(fifth, data[5,2:7])
      total <-rbind(total, data[6,2:7])
    }
    
    results_mean <- matrix(0, nrow=6, ncol=7)
    results_mean[,1] <- data[,1]
    results_mean[1,2:7] <- apply(first[2:21,], 2, FUN="mean")
    results_mean[2,2:7] <- apply(second[2:21,], 2, FUN="mean")
    results_mean[3,2:7] <- apply(third[2:21,], 2, FUN="mean")
    results_mean[4,2:7] <- apply(fourth[2:21,], 2, FUN="mean")
    results_mean[5,2:7] <- apply(fifth[2:21,], 2, FUN="mean")
    results_mean[6,2:7] <- apply(total[2:21,], 2, FUN="mean")
    
    colnames(results_mean) <- c("Choice", "# - 20", "% Accuracy - 20", "# - 80", 
                                "% Accuracy - 80", 
                                "# - Combo", "% Accuracy - Combo")
    
    write.csv(results_mean, 
              paste0(path_combo, "CV_Avg/Mean_Accuracy_", alg_uni[k], "_", meth[j], ".csv"))
    
    results_sd <- matrix(0, nrow=6, ncol=7)
    results_sd[,1] <- data[,1]
    results_sd[1,2:7] <- apply(first[2:21,], 2, FUN="sd")
    results_sd[2,2:7] <- apply(second[2:21,], 2, FUN="sd")
    results_sd[3,2:7] <- apply(third[2:21,], 2, FUN="sd")
    results_sd[4,2:7] <- apply(fourth[2:21,], 2, FUN="sd")
    results_sd[5,2:7] <- apply(fifth[2:21,], 2, FUN="sd")
    results_sd[6,2:7] <- apply(total[2:21,], 2, FUN="sd")
    
    colnames(results_sd) <- c("Choice", "# - 20", "% Accuracy - 20", "# - 80", 
                              "% Accuracy - 80", 
                              "# - Combo", "% Accuracy - Combo")
    write.csv(results_sd, 
              paste0(path_combo, "CV_Avg/SD_Accuracy_", alg_uni[k], "_", meth[j], ".csv"))
  }
}

remove(data, first, second, third, fourth, fifth, total, 
       results_mean, results_sd, i, j, k)

## Definitive mismatches

results_mean <- matrix(0, nrow=5, ncol=2)
results_sd <- matrix(0, nrow=5, ncol=2)

for (k in 1:2)
{
  setwd(paste0(path_combo, "CV_", alg_uni[k]))
  
  for (j in 1:5)
  {
    tnr <- 0
    
    for (i in 1:20)
    {
      data <- read.csv(paste0("Fold_", i, "_stats_", meth[j], ".csv"))
      
      tnr[i] <- data[4,2]
      
    }
    
    results_mean[j,k] <- mean(tnr)
    results_sd[j,k] <- sd(tnr)
  }
  
}

rownames(results_mean) <- meth
colnames(results_mean) <- alg_uni
write.csv(results_mean, 
          paste0(path_combo, "CV_Avg/Mean_mism.csv"))


rownames(results_sd) <- meth
colnames(results_sd) <- alg_uni

write.csv(results_sd, 
          paste0(path_combo, "CV_Avg/SD_mism.csv"))

remove(data, results_mean, results_sd, i, k)

## False negatives

results_mean <- matrix(0, nrow=5, ncol=2)
results_sd <- matrix(0, nrow=5, ncol=2)

for (k in 1:2)
{
  setwd(paste0(path_combo, "CV_", alg_uni[k]))
  
  for (j in 1:5)
  {
    fn <- 0
    
    for (i in 1:20)
    {
      data <- read.csv(paste0("Fold_", i, "_stats_", meth[j], ".csv"))
      
      fn[i] <- data[5,2]
      
    }
    
    results_mean[j,k] <- mean(fn)
    results_sd[j,k] <- sd(fn)
  }
  
}

rownames(results_mean) <- meth
colnames(results_mean) <- alg_uni
write.csv(results_mean, 
          paste0(path_combo, "CV_Avg/Mean_fn.csv"))


rownames(results_sd) <- meth
colnames(results_sd) <- alg_uni

write.csv(results_sd, 
          paste0(path_combo, "CV_Avg/SD_fn.csv"))

remove(data, results_mean, results_sd, i, k)