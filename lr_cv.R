setwd("C:/Users/Nefeli/Desktop/Lab/Διδακτορικό/Upper Lower Epiph/Lower Limbs")

# Library loading
library(readr)
library(Metrics)
library(dplyr)
library(caret)

# Functions
which.mins <- function(x, mins=6) {
  head(order(x), mins)
}

# Load the Tibia data
dataset <- read.csv("Tibia 20-80 vars.csv")

sample_ids <- dataset[,2]

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

set.seed(1997)
partition = createDataPartition(dataset$Y1, times = 20, p=0.8)
meth <- c("euclidean", "maximum", "manhattan", "canberra",
          "minkowski")

for (f in 1:20)
{

idx <- partition[[f]]

data <- dataset[idx,-c(5,8,13,16)]

data <- scale(data)
scaleList <- list(scale = attr(data, "scaled:scale"),
                  center = attr(data, "scaled:center"))

data <- as.data.frame(data)

test <- dataset[-idx,-c(5,8,13,16)]

test[,7] <- (test[,7]-scaleList$center["X1"])/scaleList$scale["X1"]
test[,8] <- (test[,8]-scaleList$center["X2"])/scaleList$scale["X2"]
test[,9] <- (test[,9]-scaleList$center["X3"])/scaleList$scale["X3"]
test[,10] <- (test[,10]-scaleList$center["X4"])/scaleList$scale["X4"]
test[,11] <- (test[,11]-scaleList$center["X6"])/scaleList$scale["X6"]
test[,12] <- (test[,12]-scaleList$center["X7"])/scaleList$scale["X7"]

# Predicting the 20% variables
mlm1 <- lm(cbind(Y1, Y2, Y3, Y4, Y6, Y7) ~ X1 + X2 + X3 + X4 + X6 + X7, 
           data = data)

predY <- predict(mlm1, newdata = test[, c(7:12)])

predY[,1] <- (predY[,1]*scaleList$scale["Y1"]) + scaleList$center["Y1"]
predY[,2] <- (predY[,2]*scaleList$scale["Y2"])+ scaleList$center["Y2"]
predY[,3] <- (predY[,3]*scaleList$scale["Y3"])+ scaleList$center["Y3"]
predY[,4] <- (predY[,4]*scaleList$scale["Y4"])+ scaleList$center["Y4"]
predY[,5] <- (predY[,5]*scaleList$scale["Y6"])+ scaleList$center["Y6"]
predY[,6] <- (predY[,6]*scaleList$scale["Y7"])+ scaleList$center["Y7"]

mae_20 <- 0
for (i in 1:6)
{
  mae_20[i] <- mae(test[,i], predY[,i])
}

pred_20 <- predY

remove(predY)

# Predicting the 80% variables
mlm2 <- lm(cbind(X1, X2, X3, X4, X6, X7) ~ Y1 + Y2 + Y3 + Y4 + Y6 + Y7, 
           data = data)

test <- dataset[-idx,-c(5,8,13,16)]
unsc_test <- test

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

mae_80 <- 0
for (i in 1:6)
{
  mae_80[i] <- mae(test[,6+i], predY[,i])
}

pred_80 <- predY
remove(predY)


mae_vars <- cbind(mae_20, mae_80)
colnames(mae_vars) <- c("Y: 20%", "Y: 80%")
rownames(mae_vars) <- c("Area", "Perimeter", "Ix", "Iy", "Imin", "Imax")

write.csv(mae_vars, paste0("./Tibia/CV_LR_80/Fold_", f, "_MAE.csv"))

for (m in 1:5)
{
#Sorting

# 20% - Minimum five
pr_label<-0
pr_idx <-0

five_pr<-matrix(0, nrow=nrow(test), ncol=6)
five_pr_dis_20<-matrix(0, nrow=nrow(test)+1, ncol=1)

for (i in 1:nrow(test))
{
  true <- unsc_test[i, 1:6]
  name <- rownames(true)
  rownames(true) <- c("true")
  
  vec <- rbind(true, pred_20)
  y <- as.matrix(dist(vec, method = meth[m], p=1.5))
  
  g <- which.mins(y[,1])
  
  pr_idx <- as.numeric(names(y[g,1]))
  
  five_pr[i,] <-sample_ids[pr_idx]
  name <- c(name, rownames(y[-1,]))
  five_pr_dis_20 <- cbind(five_pr_dis_20, name, y[,1])
  
  remove(true, vec, y)
}

five_pr<-cbind(sample_ids[-idx], five_pr)
five_pr_dis_20 <- five_pr_dis_20[,-1]

five_pr <- five_pr[,-2]
colnames(five_pr)<-c("Observation #", "1st Choice", "2nd Choice", 
                     "3rd Choice", "4th Choice", "5th Choice")

results<-rbind(sum(five_pr[,1]==five_pr[,2]),
               sum(five_pr[,1]==five_pr[,3]),
               sum(five_pr[,1]==five_pr[,4]),
               sum(five_pr[,1]==five_pr[,5]),
               sum(five_pr[,1]==five_pr[,6]))

results <- cbind(results, c(round(sum(five_pr[,1]==five_pr[,2])/nrow(test)*100,2),
                            round(sum(five_pr[,1]==five_pr[,3])/nrow(test)*100,2),
                            round(sum(five_pr[,1]==five_pr[,4])/nrow(test)*100,2),
                            round(sum(five_pr[,1]==five_pr[,5])/nrow(test)*100,2),
                            round(sum(five_pr[,1]==five_pr[,6])/nrow(test)*100,2)))

five_pr_20 <- five_pr

# 80 % - Minimum five

pr_label<-0
pr_idx <-0

five_pr<-matrix(0, nrow=nrow(test), ncol=6)
five_pr_dis_80<-matrix(0, nrow=nrow(test)+1, ncol=1)

for (i in 1:nrow(test))
{
  true <- unsc_test[i, 7:12]
  name <- rownames(true)
  rownames(true) <- c("true")
  
  vec <- rbind(true, pred_80)
  y <- as.matrix(dist(vec, method = meth[m], p=1.5))
  
  g <- which.mins(y[,1])
  
  pr_idx <- as.numeric(names(y[g,1]))
  
  five_pr[i,] <-sample_ids[pr_idx]
  name <- c(name, rownames(y[-1,]))
  five_pr_dis_80 <- cbind(five_pr_dis_80, name, y[,1])
  
  remove(true, vec, y)
}

five_pr<-cbind(sample_ids[-idx], five_pr)
five_pr_dis_80 <- five_pr_dis_80[,-1]

five_pr <- five_pr[,-2]
colnames(five_pr)<-c("Observation #", "1st Choice", "2nd Choice", 
                     "3rd Choice", "4th Choice", "5th Choice")

results<-cbind(results, c(sum(five_pr[,1]==five_pr[,2]),
                          sum(five_pr[,1]==five_pr[,3]),
                          sum(five_pr[,1]==five_pr[,4]),
                          sum(five_pr[,1]==five_pr[,5]),
                          sum(five_pr[,1]==five_pr[,6])))

results <- cbind(results, c(round(sum(five_pr[,1]==five_pr[,2])/nrow(test)*100,2),
                            round(sum(five_pr[,1]==five_pr[,3])/nrow(test)*100,2),
                            round(sum(five_pr[,1]==five_pr[,4])/nrow(test)*100,2),
                            round(sum(five_pr[,1]==five_pr[,5])/nrow(test)*100,2),
                            round(sum(five_pr[,1]==five_pr[,6])/nrow(test)*100,2)))

five_pr_80 <- five_pr

# Threshold for 20% predictions
true_difs <- matrix(0, nrow = nrow(test), ncol= 6)
colnames(true_difs) <- names(test)[1:6]
dist <- 0
for (i in 1:nrow(test))
{
  y <- rbind(unsc_test[i,1:6],pred_20[i,])
  true_difs[i,] <- abs(as.numeric(unsc_test[i,1:6] - pred_20[i,]))
  dist[i] <- dist(y, method=meth[m], p=1.5)
}

m_dist_20 <- round(mean(dist),2)
u_thr_20 <- 0
l_thr_20 <- 0

for (i in 1:6)
{
  u_thr_20[i] <- mean(true_difs[,i]) + 2*sd(true_difs[,i])
  l_thr_20[i] <- mean(true_difs[,i]) - 2*sd(true_difs[,i])
}

remove(true_difs, dist, y)

# Threshold for 80% predictions
true_difs <- matrix(0, nrow = nrow(test), ncol= 6)
colnames(true_difs) <- names(test)[7:12]
dist <- 0
for (i in 1:nrow(test))
{
  y <- rbind(unsc_test[i,7:12],pred_80[i,])
  true_difs[i,] <- abs(as.numeric(unsc_test[i,7:12] - pred_80[i,]))
  dist[i] <- dist(y, method=meth[m], p=1.5)
}

# Threshold for 80% predictions
m_dist_80 <- round(mean(dist),2)
u_thr_80 <- 0
l_thr_80 <- 0

for (i in 1:6)
{
  u_thr_80[i] <- mean(true_difs[,i]) + 2*sd(true_difs[,i])
  l_thr_80[i] <- mean(true_difs[,i]) - 2*sd(true_difs[,i])
}

remove(true_difs, dist, y)


# Mean Distance for true pairs combo
true_difs <- matrix(0, nrow = nrow(test), ncol= 12)
colnames(true_difs) <- names(test)[1:12]
dist <- 0
pred_combo <- cbind(pred_20, pred_80)
for (i in 1:nrow(test))
{
  y <- rbind(unsc_test[i,1:12],pred_combo[i,])
  true_difs[i,] <- abs(as.numeric(unsc_test[i,1:12] - pred_combo[i,]))
  dist[i] <- dist(y, method=meth[m], p=1.5)
}

# Threshold for 80% predictions
m_dist_combo <- round(mean(dist),2)


# Find matches AFTER checking for threshold

five_pr<-matrix(0, nrow=nrow(test), ncol=6)
five_pr_dis<-matrix(0, nrow=nrow(test)+1, ncol=1)

excluded <- 0
plausible <- matrix(0, nrow=nrow(test), ncol=nrow(test)-5)

for (i in 1:nrow(test))
{
  pr_sample <- 0
  el <- 0
  mism <- 0
  fn <- 0
  print(i)
  
  dif_20 <- abs(sweep(unsc_test[,1:6], 2, pred_20[i,], FUN = "-"))
  dif_80 <- abs(sweep(unsc_test[,7:12], 2, pred_80[i,], FUN = "-"))
  dif <- cbind(dif_20, dif_80)
  for (k in 1:nrow(dif))
  {
    y_idx <-0
    #print(k)
    for (j in 1:6)
    {
      if (between(dif_20[k,j], l_thr_20[j], u_thr_20[j]) & between(dif_80[k,j], 
                                                                 l_thr_80[j], 
                                                                 u_thr_80[j]))
      {
        y_idx <- y_idx + 1
      }
    }
    if (y_idx == 6)
    {
      pr_idx <- as.numeric(row.names(dif[k,]))
      pr_sample[k] <- sample_ids[pr_idx]
      el[k] <- k
    } else { if (sample_ids[i] %in% pr_sample)
    {
      fn <- fn + 1
    } else{mism <- mism + 1}
    }
  }
  pr_sample <- pr_sample[!is.na(pr_sample)]
  el <- el[!is.na(el)]
  
  el_pred_20 <- pred_20[el, ]
  el_pred_80 <- pred_80[el, ]
  
  if (length(pr_sample) == 1 || (length(pr_sample) ==2 & pr_sample[1] == 0))
  {
     el_pred <- c(el_pred_20, el_pred_80)
  }  else {el_pred <- cbind(el_pred_20, el_pred_80)}
  
  true <- unsc_test[i, 1:12]
  name <- rownames(true)
  rownames(true) <- c("true")
  
  vec <- rbind(true, el_pred)
  y <- as.matrix(dist(vec, method = meth[m], p=1.5))
  
  g <- which.mins(y[,1])
  
  suppressWarnings(md_idx <- as.numeric(names(y[g,1])))
  
  excluded[i] <- mism
  if (length(sample_ids[md_idx]) > 5)
  {
    five_pr[i,] <- sample_ids[md_idx]
    
    name <- c(name, rownames(y[-1,]))
    if (nrow(y) == nrow(five_pr_dis))
    {
      five_pr_dis <- cbind(five_pr_dis, name, y[,1])
    } else { 
      name <- c(name, rep(NA, nrow(five_pr_dis) - length(name)))
      dis_1 <- c(y[,1], rep(NA, nrow(five_pr_dis) - nrow(y)))
      five_pr_dis <- cbind(five_pr_dis, name, dis_1) 
    }
  } else {five_pr[i,] <- c(sample_ids[md_idx], 
                           rep(0, times=6-length(sample_ids[md_idx])))}
  remove(true, vec, y, el_pred, el_pred_20, el_pred_80, g, pr_sample)
}

five_pr<-cbind(sample_ids[-idx], five_pr)
five_pr_dis <- five_pr_dis[,-1]

five_pr <- five_pr[,-2]
colnames(five_pr)<-c("Observation #", "1st Choice", "2nd Choice", 
                     "3rd Choice", "4th Choice", "5th Choice")


write.csv(five_pr, paste0("./Tibia/CV_LR_80/Fold_", f, "_combo_pred_", meth[m], ".csv"))
write.csv(five_pr_20, paste0("./Tibia/CV_LR_80/Fold_", f, "_20_pred_", meth[m], ".csv"))
write.csv(five_pr_80, paste0("./Tibia/CV_LR_80/Fold_", f, "_80_pred_", meth[m], ".csv"))

results<-cbind(results, c(sum(five_pr[,1]==five_pr[,2]),
                          sum(five_pr[,1]==five_pr[,3]),
                          sum(five_pr[,1]==five_pr[,4]),
                          sum(five_pr[,1]==five_pr[,5]),
                          sum(five_pr[,1]==five_pr[,6])))

results <- cbind(results, c(round(sum(five_pr[,1]==five_pr[,2])/nrow(test)*100,2),
                            round(sum(five_pr[,1]==five_pr[,3])/nrow(test)*100,2),
                            round(sum(five_pr[,1]==five_pr[,4])/nrow(test)*100,2),
                            round(sum(five_pr[,1]==five_pr[,5])/nrow(test)*100,2),
                            round(sum(five_pr[,1]==five_pr[,6])/nrow(test)*100,2)))

results <-rbind(results, c(sum(results[,1]),
                           round(sum(results[,1])/nrow(test)*100, 2),
                           sum(results[,3]),
                           round(sum(results[,3])/nrow(test)*100, 2),
                           sum(results[,5]),
                           round(sum(results[,5])/nrow(test)*100, 2)))

rownames(results) <- c("1st Choice", "2nd Choice", "3rd Choice",
                       "4th Choice", "5th Choice", "Total")

colnames(results) <- c("# - 20", "% Accuracy - 20", "# - 80", "% Accuracy - 80",
                       "# - Combo", "% Accuracy - Combo")



#A joke
def_m <- 0
sorted <- c("Sample ID", "Matching")
for (i in 1:nrow(five_pr))
{
  if (five_pr[i,1] %in% five_pr[i,2:6] & five_pr_80[i,1] %in% five_pr_80[i,2:6]
      & five_pr_20[i,1] %in% five_pr_20[i,2:6])
  {
    def_m <- def_m + 1
    print(paste0(c(five_pr[i,1]), " and ",
          five_pr[i,(which(five_pr[i,1] == five_pr[i,2:6])) + 1]))
    sorted <- rbind(sorted, five_pr[i,1],
                          five_pr[i,(which(five_pr[i,1] == five_pr[i,2:6])) + 1][[1]])
  }
}

sorted <- unique(sorted)

stats <- c(nrow(test), def_m, sum(excluded),
           sum(excluded)/(nrow(test)*(nrow(test)-1))*100, sum(fn))
names(stats) <- c("Sample size", "Definite Matches", "# of Excluded",
                     "TNR", "# of False Negatives")

write.csv(stats, paste0("./Tibia/CV_LR_80/Fold_", f, "_stats_", meth[m], ".csv"))
write.csv(sorted, paste0("./Tibia/CV_LR_80/Fold_", f, "_sorted_", meth[m], ".csv"))


md_true <- rbind(m_dist_20, m_dist_80, m_dist_combo)
rownames(md_true) <- c("Pred 20", "Pred 80",
                       "Pred combo")
colnames(md_true) <- "Mean Distance (True Pairs)"

write.csv(md_true, paste0("./Tibia/CV_LR_80/Fold_", f, "_MD_true_", meth[m], ".csv"))
write.csv(results, paste0("./Tibia/CV_LR_80/Fold_", f, "_Accuracy_", meth[m], ".csv"))

write.csv(five_pr_dis,
         paste0("./Tibia/CV_LR_80/Fold_", f, "_combo_pred_dis_", meth[m], ".csv"))
write.csv(five_pr_dis_20,
         paste0("./Tibia/CV_LR_80/Fold_", f, "_20_pred_dis_", meth[m], ".csv"))
write.csv(five_pr_dis_80,
         paste0("./Tibia/CV_LR_80/Fold_", f, "_80_pred_dis_", meth[m], ".csv"))

}
}