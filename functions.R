#------------
# FUNCTIONS
#------------ 
# reset.df
# tgrey; tred; etc.
# col2t
# rmse
# shadebands
# midrange
# llreg
# llcv
# plot.llcv
# glmcv
# density.plot
# elasticnetcv
# lrm.cv        linear regularized model cross-validation
# gamcv
# tree.cv



df <- read.csv("C:/df_backup_4april2016.csv", row.names=1)
reset.df <- function(){
  x <<- df$dayofyear
  y <<- df$Trump
}


tgrey  <- function(alpha) rgb(0,0,0,alpha)
tred   <- function(alpha) rgb(1,0,0,alpha)
tgreen <- function(alpha) rgb(0,1,0,alpha)
tblue  <- function(alpha) rgb(0,0,1,alpha)
col2t <- function(num, alpha=50){
  return(rgb(col2rgb(num)[1], col2rgb(num)[2], col2rgb(num)[3], alpha = alpha, maxColorValue = 255))
}


rmse <- function(fitted,observed) sqrt(mean((observed-fitted)^2))


shadebands <- function(x,lo,hi,col) polygon(c(x,rev(x)),y=c(lo,rev(hi)),col=col,border=NA)


midrange <- function(num.vector, percentage){
  if(percentage>1) percentage <- percentage/100
  gap <- (1 - percentage)/2
  return(list(min=quantile(num.vector, gap)
              ,max=quantile(num.vector, 1-gap))
  )
}


library(KernSmooth)
llreg <- function(x,y, bandwidth, degree=1, gridsize = max(x)-min(x)+1, ...){
  output <- locpoly(x=x,y=y,degree=degree,bandwidth=bandwidth, gridsize = gridsize, ...)
  return(output)
}



llcv <- function(x,y,bandwidths,loo=FALSE, n.folds=NULL){
  
  # check conditions
  if (!loo & is.null(n.folds)) stop("Have to specify either n.folds or loo")
  if (loo & !is.null(n.folds)) stop("Cannot be both k-fold and LOO")
  
  # Leave-one-out
  if(loo) n.folds <- length(x)
  
  
  # Create data frame
  data <- data.frame(x,y, id=1:length(x))
  
  #Randomly shuffle the data
  data<-data[sample(nrow(data)),]
  
  #Assign folds
  fold.assign <- cut(seq(1,nrow(data)),breaks=n.folds,labels=FALSE)
  data$fold.assign <- fold.assign
  
  
  #Set some parameters
  n.bw <- length(bandwidths)
  n.obs <- length(x)
  cv <- list(bandwidths=bandwidths,rmse=rep(NA,n.bw))
  
  
  for (b in 1:n.bw) {	
    temp.fitted <- numeric()
    
    for (k in 1:n.folds){
      
      #Fit regression on data NOT in k
      tempreg <- llreg(
        x=data$x[data$fold.assign != k]
        , y=data$y[data$fold.assign != k]
        , bandwidth = bandwidths[b]
        # to make sure that we get fitted values on the whole range
        # of the dates in the original data set
        , gridsize= max(data$x)-min(data$x)+1
        , range.x=c(min(data$x), max(data$x))
      )
      
      #Use model to score x values in k, then store the fitted values in temp.fitted
      score.needed <- data$x[data$fold.assign == k]
      
      for (poll in 1:length(score.needed)){
        x.value <- score.needed[poll]
        y.fitted <- tempreg$y[which(tempreg$x == x.value)]
        
        temp.fitted <- c(temp.fitted, y.fitted)
        
      }
      
      
    }
    
    #save fitted values in data frame "data"
    data$temp.fitted <- temp.fitted
    colnames(data)[ncol(data)] <- paste('fitted.bw', bandwidths[b], sep="")
    
    #save RMSE
    cv$rmse[b] <- rmse(temp.fitted,data$y)
  }
  cv$best.bandwidth <- bandwidths[which.min(cv$rmse)]
  cv$n.folds <- n.folds
  output <- list(data = data[order(data$id),] #data in same order of original dataset
                 , cv = cv)  
  class(output) <- 'llcv'

  return(output)	
}



plot.llcv <- function(llcv.reg, ...){
  
  plot(llcv.reg$cv$bandwidths,llcv.reg$cv$rmse,
       xlab="Bandwidth",
       ylab="RMSE",
       main=paste("Cross-Validation: k=", llcv.reg$cv$n.folds, sep=""),
       type="b",
       ...)
  points(llcv.reg$bw,llcv.reg$cv$rmse[which(llcv.reg$bw == llcv.reg$cv$bandwidths)],pch=16)
  mtext(paste("Bandwidth with minimum RMSE is", llcv.reg$cv$best.bandwidth))
  abline(v = llcv.reg$cv$best.bandwidth, col=2)
}




glmcv <- function(x,y, bandwidths = 1:30, n.folds=10, loo=FALSE){
  
  # check conditions
  if (!loo & is.null(n.folds)) stop("Have to specify either n.folds or loo")
  if (loo & !is.null(n.folds)) stop("Cannot be both k-fold and LOO")
  
  # Leave-one-out
  if(loo) n.folds <- length(y)
  
  # level-0
  reg <- llcv(x,y, bandwidths = bandwidths, n.folds = n.folds)
  
  # Prepare level-1
  X <- as.matrix(reg$data[, 6:ncol(reg$data)])
  
  
  #Create and shuffle
  fold.assign <- sample(cut(seq(1,length(y)),breaks=n.folds,labels=FALSE) ) 
  
  # Create data frame
  data <- data.frame(id=1:length(y), fold.assign, y, X )
  data <- data[order(data$fold.assign, data$id), ]
  
  #to store coefficients at every fold
  fold.coefficients <- matrix(nrow=length(bandwidths)-1+1 , ncol=n.folds)
  row.names(fold.coefficients) <- c("intercept", colnames(data[,4:ncol(data)]))
  
  #Set some parameters
  n.obs <- length(y)
  
  temp.fitted <- numeric()
  
  for (k in 1:n.folds){
    
    #Fit regression on data NOT in k
    tempreg <- glm(y~. - id - fold.assign, data=data[data$fold.assign != k,])
    
    #Use model to score x values in k, then store the fitted values in temp.fitted
    y.fitted <- predict(tempreg, newdata = data[data$fold.assign == k, ])
    temp.fitted <- c(temp.fitted, y.fitted)
    
    #save coefficients
    fold.coefficients[,k] <- tempreg$coefficients
  }
  
  #save fitted values in data frame "data"
  data$y.fitted <- temp.fitted
  
  
  #save RMSE
  rmse <- rmse(temp.fitted,data$y)
  
  #save a glm
  output.glm <- glm(y~. - id - fold.assign - y.fitted, data=data)  
  
  output <- list(data = data[order(data$id),] #return data in same order of original dataset
                 , rmse = rmse  
                 , output.glm = output.glm
                 , fold.coefficients = fold.coefficients)  
  class(output) <- 'glmcv'
  
  return(output)	
  
}




density.plot <- function(list.of.arrays, legend.position="topright", legend.cex=1,legend.horiz=FALSE, legend.ncol=1, col=1:length(list.of.arrays), ...){
  
  if(is.null(names(list.of.arrays))) stop("Arrays must have names (for legend)")  
  
  ### lines
  
  plot(density(unlist(list.of.arrays[1])), col=col[1], lwd=2 , lty=1, ...)
  
  i <- 1
  for(array in list.of.arrays[2:length(list.of.arrays)]){
    i <- i+1
    lines(density(unlist(array)), col=col[i], lwd=2)
  }
  
  ### shadebands
  
  b <- 0
  for(array in list.of.arrays){
    b <- b+1
    
    shadebands( x  = density(unlist(array))$x
                ,lo = rep(0, length(density(unlist(array))$x))
                ,hi = density(unlist(array))$y
                ,col = col2t(col[b],alpha=50)  
    )
  }
  
  
  ### legend
  legend(legend.position
         , legend = names(list.of.arrays)
         , fill = col
         #, lwd=2
         #, lty=1:length(list.of.arrays)
         #, col=1:length(list.of.arrays)
         , cex=legend.cex
         , horiz = legend.horiz
         , ncol = legend.ncol
  )
}


library(glmnet)

elasticnetcv <- function(x,y, bandwidths=1:30, loo=FALSE, n.folds=10, alpha=0){
  
  # check conditions
  if (!loo & is.null(n.folds)) stop("Have to specify either n.folds or loo")
  if (loo & !is.null(n.folds)) stop("Cannot be both k-fold and LOO")
  
  # Leave-one-out
  if(loo) n.folds <- length(y)
  
  
  # level-0
  reg <- llcv(x,y, bandwidths = bandwidths, n.folds = n.folds)
  X <- reg$data[, 6:ncol(reg$data)]
  
  #Create and shuffle
  fold.assign <- sample(cut(seq(1,length(y)),breaks=n.folds,labels=FALSE) ) 
  
  
  # Create data frame
  data <- data.frame(id=1:length(y), fold.assign, y, X )
  X <- X[order(data$fold.assign, data$id),]
  y <- y[order(data$fold.assign, data$id)]
  data <- data[order(data$fold.assign, data$id), ]
  
  #to store coefficients at every fold
  fold.coefficients <- matrix(nrow=length(bandwidths)-1+1 , ncol=n.folds)
  row.names(fold.coefficients) <- c("intercept", colnames(data[,4:ncol(data)]))
  
  
  temp.fitted <- numeric()
  lambda.min <- cv.glmnet(x= as.matrix(X), y=as.matrix(y))$lambda.min
  
  for (k in 1:n.folds){
    
    #Fit regression on data NOT in k
    tempreg   <- glmnet(    x = as.matrix(X[data$fold.assign != k,])
                            , y = as.matrix(y[data$fold.assign != k])
                            , lambda=lambda.min
                            , alpha=alpha) #if =0 then ridge only. if =1 then lasso only
    
    # save coefficients
    fold.coefficients[,k] <- as.array(coef(tempreg))
    
    #Use model to score x values in k, then store the fitted values in temp.fitted
    
    y.fitted <- predict(tempreg, newx = as.matrix(X[data$fold.assign == k,]) )
    temp.fitted <- c(temp.fitted, y.fitted)
  }
  
  #save fitted values in data frame "data"
  data$y.fitted <- temp.fitted
  
  #output
  output <- list(data = data[order(data$id),] #return data in same order of original dataset
                 , rmse = rmse(temp.fitted,data$y)  
                 , fold.coefficients = fold.coefficients
                 , lambda = lambda.min
  )  
  class(output) <- 'elasticnetcv'
  
  return(output)	
  
}




library(limSolve)
lrm.cv <- function(x,y, bandwidths = 1:30, loo=FALSE, n.folds=10, non.neg = TRUE, sum.to.one=TRUE, type=2){
  
  # check conditions
  if (!loo & is.null(n.folds)) stop("Have to specify either n.folds or loo")
  if (loo & !is.null(n.folds)) stop("Cannot be both k-fold and LOO")
  
  # Leave-one-out
  if(loo) n.folds <- length(y)
  
  
  # level-0
  reg <- llcv(x,y, bandwidths = bandwidths, n.folds = n.folds)
  
  # Prepare level-1
  X <- as.matrix(reg$data[, 6:ncol(reg$data)])
  
  #Create and shuffle
  fold.assign <- sample(cut(seq(1,length(y)),breaks=n.folds,labels=FALSE) ) 
  
  # Create data frame
  data <- data.frame(id=1:length(y), fold.assign, y, X )
  X <- X[order(data$fold.assign, data$id),]
  y <- y[order(data$fold.assign, data$id)]
  data <- data[order(data$fold.assign, data$id), ]
  
  #to store fitted values
  temp.fitted <- numeric()
  
  #to store coefficients at every fold
  fold.coefficients <- matrix(nrow=length(bandwidths)-1 , ncol=n.folds)
  row.names(fold.coefficients) <- colnames(data[,4:ncol(data)])

  
  for (k in 1:n.folds){
    
    #Fit regression on data NOT in k and save coefficients
    temp.reg   <- lsei(A=as.matrix(X[data$fold.assign != k, ])
                       , B= as.matrix(y[data$fold.assign != k ])
                       , E= if(sum.to.one) matrix(rep(1, ncol(X)), 1) else NULL
                       , F= 1
                       , G= diag(ncol(X))
                       , H= matrix(rep(0, ncol(X)), byrow = TRUE)
                       , type = type
    )
    
    if(temp.reg$IsError) warning(paste("Error at fold ", k)) 
    
    fold.coefficients[,k] <- temp.reg$X 
    
    #Use model to score x values in k, then store the fitted values in temp.fitted
    need.fitted <- X[data$fold.assign == k, ]
    y.fitted <- as.matrix(need.fitted, nrow=1) %*%  fold.coefficients[,k]
    
    temp.fitted <- c(temp.fitted, y.fitted)
  } 
  
  #save fitted values in data frame "data"
  data$y.fitted <- temp.fitted
  
  #save RMSE
  rmse <- rmse(temp.fitted,y)
  
  output <- list(data = data[order(data$id),] #return data in same order of original dataset
                 , rmse = rmse  
                 , fold.coefficients = fold.coefficients 
  )  
  class(output) <- 'lrm.cv'
  
  return(output)	
}



library(mgcv)
gamcv <- function(x,y, bandwidths = 1:30, n.folds=10, loo=FALSE){
  
  # check conditions
  if (!loo & is.null(n.folds)) stop("Have to specify either n.folds or loo")
  if (loo & !is.null(n.folds)) stop("Cannot be both k-fold and LOO")
  
  # Leave-one-out
  if(loo) n.folds <- length(y)
  
  # level-0
  reg <- llcv(x,y, bandwidths = bandwidths, n.folds = n.folds)
  
  # Prepare level-1
  X <- as.matrix(reg$data[, 6:ncol(reg$data)])
  
  
  #Create and shuffle
  fold.assign <- sample(cut(seq(1,length(y)),breaks=n.folds,labels=FALSE) ) 
  
  # Create data frame
  data <- data.frame(id=1:length(y), fold.assign, y, X )
  data <- data[order(data$fold.assign, data$id), ]
  
  #Set some parameters
  n.obs <- length(y)
  
  temp.fitted <- numeric()
  
  for (k in 1:n.folds){
    
    #Fit regression on data NOT in k
    tempreg <- gam( y~s(fitted.bw3, by=fitted.bw15) #+ s(fitted.bw30) 
                    , data=data[data$fold.assign != k,])
    
    #Use model to score x values in k, then store the fitted values in temp.fitted
    y.fitted <- predict(tempreg, newdata = data[data$fold.assign == k, ])
    temp.fitted <- c(temp.fitted, y.fitted)
    
  }
  
  #save fitted values in data frame "data"
  data$y.fitted <- temp.fitted
  
  
  #save RMSE
  rmse <- rmse(temp.fitted,data$y)
  
  output <- list(data = data[order(data$id),] #return data in same order of original dataset
                 , rmse = rmse)  
  class(output) <- 'gamcv'
  
  return(output)	
  
}



library(rpart)
library(rpart.plot)

tree.cv <- function(x,y, bandwidths = 1:30, n.folds=10, loo=FALSE){
  
  # check conditions
  if (!loo & is.null(n.folds)) stop("Have to specify either n.folds or loo")
  if (loo & !is.null(n.folds)) stop("Cannot be both k-fold and LOO")
  
  # Leave-one-out
  if(loo) n.folds <- length(y)
  
  # level-0
  reg <- llcv(x,y, bandwidths = bandwidths, n.folds = n.folds)
  
  # Prepare level-1
  X <- as.matrix(reg$data[, 6:ncol(reg$data)])
  
  
  #Create and shuffle
  fold.assign <- sample(cut(seq(1,length(y)),breaks=n.folds,labels=FALSE) ) 
  
  # Create data frame
  data <- data.frame(id=1:length(y), fold.assign, y, X )
  data <- data[order(data$fold.assign, data$id), ]
  
  #Set some parameters
  n.obs <- length(y)
  
  temp.fitted <- numeric()
  
  for (k in 1:n.folds){
    
    #Fit regression on data NOT in k
    tree <- rpart(y~. - id - y - fold.assign
                  , data=data[data$fold.assign != k,]
                  , method="anova"
                  #,control=
    ) 
    
    tempreg <- prune(tree, cp= tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])
    
    #Use model to score x values in k, then store the fitted values in temp.fitted
    y.fitted <- predict(tempreg, newdata = data[data$fold.assign == k, ], type="matrix")
    temp.fitted <- c(temp.fitted, y.fitted)
    
  }
  
  #save fitted values in data frame "data"
  data$y.fitted <- temp.fitted
  
  
  #save RMSE
  rmse <- rmse(temp.fitted,data$y)
  
  output <- list(data = data[order(data$id),] #return data in same order of original dataset
                 , rmse = rmse)  
  class(output) <- 'tree.cv'
  
  return(output)	
  
}

