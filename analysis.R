setwd("C:/Users/Nefeli/Desktop/Lab/Διδακτορικό/Upper Lower Epiph/Lower Limbs")

# Library loading
library(readr)
library(corrplot)
library(RColorBrewer)
library(xtable)
library(Hmisc)
library(caret) # for the partition of the dataset
library(e1071)

# corstars function 
# source: http://www.sthda.com/english/wiki/elegant-correlation-table-using-xtable-r-package
corstars <-function(x, method=c("pearson", "spearman"), 
                    removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 


# Set the bones vector
bones <- c("Femur", "Tibia")

# Keep only the 20-80% variables from the files
for (i in 1:2)
{
  data_og <- read.csv(paste0(bones[i], " Complete Data.csv"))
  data <- data_og[, c(1,8:15,40:47)]
  write.csv(data, paste0(bones[i], " 20-80 vars.csv"))
}

# Correlation plots and tables for the two bones between the 20% and 80% vars
for (i in 1:2)
{
  data <- read.csv(paste0(bones[i], " 20-80 vars.csv"))
  data <- data[,-c(1,2)]
  data_20 <- data[,1:8]
  data_80 <- data[,9:16]
  # Correlation plot
  cc <- cor(data_20, data_80, method = "spearman")
  
  png(paste0("Correlation plot - ", bones[i], ".png"), 
      width=1500, height=1500, res=300)
  corrplot(cc, method="circle",type="upper",
           title=paste0("Correlation - ", bones[i]), mar=c(1,1,2,1),
           col=COL2('PuOr', 10), tl.col="black",
           cl.ratio = 0.15, tl.srt = 45)
  dev.off()
  
  # Using the corstars function, save the correlation matrix with ss levels
  cc_tab <- corstars(data)
  write.csv(cc_tab, paste0("Correlation table -", bones[i], ".csv"))
}
