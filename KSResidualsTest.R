##################################################################
# Function for appltying the two-sample Kolmogorov-Smirnov test  #
# for testing the equality of residual distributions             #
# in different environments                                      #
##################################################################
KSResidualsTest <- function(Residuals, Environment, Boot){
  
  ### Load required packages ###
  require(Matching)
  
  ### Throw errors for bad inputs ###
  if (grepl("Yes|No", Boot) == FALSE) {stop("Boot must be either \"Yes\" or \"No\"")}
  
  ### Extract training residuals ###
  R <- data.frame(Residuals, Environment)
  R <- subset(R, R$Type == "Training")[,c(1,3)]
  
  ### Perform the two-sample Kolmogorov-Smirnov test ###
  if (Boot == "No"){
    p <- ks.test(subset(R, R$Environment == 1, 1)[,1],subset(R, R$Environment == 2, 1)[,1])$p.value
  }
  if (Boot == "Yes"){
    p <- ks.boot(subset(R, R$Environment == 1, 1)[,1],subset(R, R$Environment == 2, 1)[,1])$ks.boot.pvalue
  }
  p <- ifelse(p == 0, 0.00000000001, p)
  p <- as.numeric(p)
  
  ### Return values ###
  result <- list()
  result$p <- p
  return(result)
}