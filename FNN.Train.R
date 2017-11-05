##################################################################
# Function for training a feed-forward neural network            #
##################################################################
FNN.Train <- function(x, y, Architecture, Optimizer, Ini.Seed, Shuffle.Seed, Split.Ratio,
                      Minibatch.Size, Max.Epochs, Regularization, Var.Imp){
  
  ### Load required packages ###
  require(tensorflow)
  require(tcltk)
  
  #install_tensorflow(version = "default")
  #install_tensorflow(version = "gpu")

  ### Throw errors for bad inputs ###
  if (dim(as.matrix(x))[1] != dim(as.matrix(y))[1]) {stop("Number of samples in x.train and y.train must be equal")}
  if (length(Architecture) > 10) {stop("Maximum of 10 hidden layers are supported")}
  if (length(Architecture) < 1) {stop("Must have at least one hidden layer")}
  if (min(Architecture) < 1) {stop("Every hidden layer must have at least one hidden unit")}
  if (grepl("SGD|SGD (momentum)|RMSProp|RMSProp (momentum)|Adam", Optimizer) == FALSE) {stop("Optimizer must be either \"SGD\", \"SGD (momentum)\", \"RMSProp\", \"RMSProp (momentum)\" or \"Adam\"")}
  if (Minibatch.Size <= 0) {stop("Minibatch.Size must be a whole number greater than 0")}
  if (Max.Epochs <= 0) {stop("Max.Epochs must be a whole number greater than 0")}
  if (Regularization < 0) {stop("Regularization must be greater than or equal to 0")}
  if (grepl("Yes|No", Var.Imp) == FALSE) {stop("Var.Imp must be either \"Yes\" or \"No\"")}
  
  ### Shuffle data ###
  set.seed(Shuffle.Seed)
  x <- as.matrix(x); y <- as.matrix(y)
  order <- sample(dim(x)[1])
  x <- as.matrix(x[order,,drop = FALSE]); y <- as.matrix(y[order,,drop = FALSE])
  
  ### Split data into training set and validation set ###
  x.train <- x[1:(dim(x)[1]*Split.Ratio),,drop = FALSE]
  y.train <- y[1:(dim(y)[1]*Split.Ratio),,drop = FALSE]
  m.train <- dim(x.train)[1] 
  d.x <- dim(x.train)[2]
  
  x.val <- x[(m.train + 1):dim(x)[1],,drop = FALSE]
  y.val <- y[(m.train + 1):dim(y)[1],,drop = FALSE]
  m.val <- dim(x.val)[1]
  
  ### Convert to matrix ###
  x.train <- as.matrix(x.train); y.train <- as.matrix(y.train)
  x.val <- as.matrix(x.val); y.val <- as.matrix(y.val)

  ### Standardize data ###
  means <- colMeans(x.train)
  sds <- apply(x.train, 2, sd)
  if(sum(sds == 0) == 0){
    for (i in 1:d.x){
      x[,i] <- (x[,i] - means[i])/sds[i]
      x.train[,i] <- (x.train[,i] - means[i])/sds[i]
      x.val[,i] <- (x.val[,i] - means[i])/sds[i]
    }
  }
  
  ### Placeholders for data ###
  x_ <- tf$placeholder(tf$float64, shape(NULL, d.x))
  y_ <- tf$placeholder(tf$float64, shape(NULL, 1))
  
  ### Setup FNN ###
    ## First hidden layer ##
    W1 <- tf$cast(tf$Variable(tf$random_uniform(shape(d.x, Architecture[1]), minval = -1/sqrt(d.x), maxval = 1/sqrt(d.x), seed = Ini.Seed)), tf$float64)
    b1 <- tf$cast(tf$Variable(rep(0, Architecture[1])), tf$float64)
    z1 <- tf$nn$relu(tf$matmul(x_, W1) + b1)
    
    ## Other hidden layers ##
    if (length(Architecture) > 1){
      for (i in 2:length(Architecture)){
        assign(paste("W", i, sep = ""), tf$cast(tf$Variable(tf$random_uniform(shape(Architecture[i-1], Architecture[i]), minval = -1/sqrt(Architecture[i-1]), maxval = 1/sqrt(Architecture[i-1]), seed = Ini.Seed)), tf$float64))
        assign(paste("b", i, sep = ""), tf$cast(tf$Variable(rep(0, Architecture[i])), tf$float64))
        assign(paste("z", i, sep = ""), tf$nn$relu(tf$matmul(get(paste("z", i-1, sep = "")), get(paste("W", i, sep = ""))) + get(paste("b", i, sep = ""))))
        W1 <- tf$cast(W1, tf$float64) 
        b1 <- tf$cast(b1, tf$float64)
      }
    }
    
    ## Output layer ##
    W.Out <- tf$cast(tf$Variable(tf$random_uniform(shape(tail(Architecture, n = 1), 1), minval = -1/sqrt(tail(Architecture, n = 1)), maxval = 1/sqrt(tail(Architecture, n = 1)), seed = Ini.Seed)), tf$float64)
    b.Out <- tf$cast(tf$Variable(tf$zeros(shape(1))), tf$float64)
    y_hat <- tf$matmul(get(paste("z", length(Architecture), sep = "")), W.Out) +  b.Out
    
    ### Loss function with L2-regularization ###
    WeightDecay <- function(Architecture){
      Weights.L2 <- tf$reduce_sum(tf$square(W.Out))
      for (i in 1:length(Architecture)){
        Weights.L2 <- Weights.L2 + tf$reduce_sum(tf$square(get(paste("W", i, sep = ""))))
      }
      return(tf$cast(Weights.L2,tf$float64))
    }
    
    Regularization <- tf$cast(Regularization, tf$float64)
    loss <- tf$reduce_mean(tf$squared_difference(y_hat, y_) + Regularization*WeightDecay(Architecture))

  ### Methods for training ###
  if (Optimizer == "SGD"){optimizer <- tf$train$GradientDescentOptimizer(learning_rate = 0.01)}
  if (Optimizer == "RMSProp"){optimizer <- tf$train$RMSPropOptimizer(learning_rate = 0.001)}
  if (Optimizer == "Adam"){optimizer <- tf$train$AdamOptimizer(learning_rate = 0.001)}
  train_step <- optimizer$minimize(loss)
  
  ### Initialize variables and run session ###
  init <- tf$global_variables_initializer()
  sess <- tf$InteractiveSession()
  sess$run(init)
  
  with(tf$device("/gpu:0"),{
  #with(tf$device("/cpu:0"),{

  ## Initial errors ##
    error.train.ini <- loss$eval(dict(x_ = as.matrix(x.train), y_ = as.matrix(y.train)))
    error.val.ini <- loss$eval(dict(x_ = as.matrix(x.val), y_ = as.matrix(y.val)))

    ## Train model  ##
    iterations <- m.train/Minibatch.Size
    error.train <- rep(NA,Max.Epochs)
    error.val <- rep(NA,Max.Epochs)
    pb <- tkProgressBar(title = paste("progress bar"), min = 0, max = Max.Epochs, width = 300)
    
    for (j in 1:Max.Epochs){
      set.seed(j)
      order1 <- sample(m.train)
      x.train.order <- as.matrix(x.train[order1,]); y.train.order <- as.matrix(y.train[order1,])
    
      for (i in 1:iterations){
        train_step$run(feed_dict = dict(x_ = as.matrix(x.train.order[((i-1)*Minibatch.Size + 1):(i*Minibatch.Size),]), 
                                        y_ = as.matrix(y.train.order[((i-1)*Minibatch.Size + 1):(i*Minibatch.Size),])))
      }
        
      setTkProgressBar(pb, j, label = paste(round(j/Max.Epochs*100, 0),"% done"))
      error.train[j] <- loss$eval(dict(x_ = as.matrix(x.train), y_ = as.matrix(y.train)))
      error.val[j] <- loss$eval(dict(x_ = as.matrix(x.val), y_ = as.matrix(y.val)))

    }
    
    close(pb)

    ## Retrieve quantities of interest ##
    for (i in 1:length(Architecture)){
      assign(paste("W_", i, sep = ""), get(paste("W", i, sep = ""))$eval())
      assign(paste("b_", i, sep = ""), get(paste("b", i, sep = ""))$eval())
    }
    W_Out <- W.Out$eval()
    b_Out <- b.Out$eval()
    y_Out <- y_hat$eval(dict(x_ = as.matrix(x)))
    residuals <- y - y_Out
    
    type <- c(rep("Training",m.train), rep("Validation",m.val))
    y_Out <- data.frame(y_Out, type, order)
    y_Out <- y_Out[order(y_Out$order),1:2]
    residuals <- data.frame(residuals, type, order)
    residuals <- residuals[order(residuals$order),1:2]
    colnames(residuals) <- c("Residuals","Type")
    MSE <- data.frame(error.train[Max.Epochs],error.val[Max.Epochs])
    rownames(MSE) <- c("MSE"); colnames(MSE) <- c("Training", "Validation")

    ### Variable importance ###
    if (Var.Imp == "Yes"){
      Var.Imp.Train <- matrix(NA, nrow = 100, ncol = d.x)
      Var.Imp.Val <- matrix(NA, nrow = 100, ncol = d.x)

      if (d.x == 1){
        for (j in 1:100){
          set.seed(j)
          x.train.imp <- x.train[sample(m.train),1]
          x.val.imp <- x.val[sample(m.val),1]
          Var.Imp.Train[j] <- loss$eval(dict(x_ = as.matrix(x.train.imp), y_ = as.matrix(y.train)))
          Var.Imp.Val[j] <- loss$eval(dict(x_ = as.matrix(x.val.imp), y_ = as.matrix(y.val)))
        }
      }

      if (d.x > 1){
        for (j in 1:100){
          set.seed(j)
          for (i in 1:d.x){
            if (i == 1){
              x.train.imp <- cbind(x.train[sample(m.train),1],x.train[,2:d.x])
              x.val.imp <- cbind(x.val[sample(m.val),1],x.val[,2:d.x])
            }
            else if (i == d.x){
              x.train.imp <- cbind(x.train[,1:(d.x-1)],x.train[sample(m.train),d.x])
              x.val.imp <- cbind(x.val[,1:(d.x-1)],x.val[sample(m.val),d.x])
            }
            else{
              x.train.imp <- cbind(x.train[,1:(i-1)],x.train[sample(m.train),i],x.train[,(i+1):d.x])
              x.val.imp <- cbind(x.val[,1:(i-1)],x.val[sample(m.val),i],x.val[,(i+1):d.x])
            }
            Var.Imp.Train[j,i] <- loss$eval(dict(x_ = as.matrix(x.train.imp), y_ = as.matrix(y.train)))
            Var.Imp.Val[j,i] <- loss$eval(dict(x_ = as.matrix(x.val.imp), y_ = as.matrix(y.val)))
          }
        }
      }
      Var.Imp1 <- data.frame(t(cbind(colMeans(Var.Imp.Train) - MSE[1,1],colMeans(Var.Imp.Val)-MSE[1,2])))
      rownames(Var.Imp1) <- c("Change in MSE (training)", "Change in MSE (validation)")
      colnames(Var.Imp1) <- colnames(x.train)
    }
  })  
  ### Close the session ###
  sess$close()
  
  ### Return ###
  result <- list()
  
  for (i in 1:length(Architecture)){
    result[[i]] <- get(paste("W_", i, sep = ""))
    result[[i + length(Architecture)]] <- get(paste("b_", i, sep = ""))
  }
  names(result) <- c(paste("Weight.",1:length(Architecture), sep = ""),
                     paste("Bias.",1:length(Architecture), sep = ""))
 
  result$Weight.Output <- W_Out
  result$Bias.Output <- b_Out
  result$y.Output <- y_Out
  result$Residuals <- residuals
  
  result$MSE <- MSE
  result$MSE.Train <- c(error.train.ini,error.train)
  result$MSE.Val <- c(error.val.ini,error.val)

  result$Architecture <- Architecture
  result$d.x <- d.x
  
  result$Means.Train <- unname(means)
  result$Stddev.Train <- unname(sds)
  
  if (Var.Imp == "Yes"){
    result$Var.Imp <- Var.Imp1
  }
  return(result)
}