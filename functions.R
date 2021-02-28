fit_logistic_lasso<- function(x, y, lambda, beta0 = NULL, eps = 0.0001, iter_max = 100) {
  ## fit_logistic_lasso is used to fit a logistic lasso regression model 
  ## for the given data. The algorithm used the coordinate descent method. 
  ## The function will give a list of logistic lasso information, 
  ## including the intercept of the model, coefficient for the predictors, 
  ## and also lambda, iteration time, and whether the result converged or
  ## not.
  ##
  ## Input:
  ## - x:        A matrix of data information for the predictors.
  ## - y:        Vector of data.
  ## - lambda:   Penalty term for the lasso algorithm.
  ## - beta0:    Initial guess of the coefficients before each  predictors 
  ##          and also the value for intercept.
  ## - eps:      Parameter for stopping criterion for checking convergence.
  ## - iter_max: Maximum number of iterations used in the algorithm.
  ##
  ## Output:
  ## -          The result from the logistic lasso regression for the given data, 
  ##            in the form of a list containing the intercept, coefficient for 
  ##            predictors, and lambda.
  ##
  ## Example:
  ## library(tidyverse)
  ## 
  ## n = 1000
  ## dat <- tibble(x1 = seq(-3,3, length.out = n),
  ##               x2 = seq(-2,2, length.out = n),
  ##               w = 3*cos(3*seq(-pi,pi, length.out = n)),
  ##               y = rbinom(n,size = 1, prob = 1/(1 + exp(-w+2*x)) )%>% as.numeric %>% factor,
  ##               cat = sample(c("a","b","c"), n, replace = TRUE))
  ##
  ## x <- as.matrix(dat[, 1:2])
  ## 
  ## fit_logistic_lasso(x, dat$y, 5, eps = 0.0001, iter_max = 100)
  
  
  n <- dim(x)[1]
  p <- dim(x)[2] + 1
  
  
  if (is.null(beta0)) {
    beta0 <- rep(0, p)
  }
  
  fct_levels <- levels(y)
  y <- as.numeric(y) - 1
  
  intercept <- c(rep(1, n))
  x <- cbind(intercept, x)
  x <- as.matrix(x)
  
  beta <- beta0
  x_beta0 <- (x %*% beta0) %>% as.numeric
  prob <- 1/(1 + exp(-x_beta0))
  
  for(iter in 1:iter_max) {
    w <- prob * (1 - prob)
    z <- x_beta0 + (y - prob)/w
    
    for(j in 1:p) {
      rj = z - x[,-j] %*% beta[-j]
      beta[j] <- sign(sum(w*rj*x[,j])) * max(abs(sum(w*rj*x[,j]))-lambda, 0) / sum(w*x[,j]^2)
    }
    
    
    x_beta0 <- (x %*% beta) %>% as.numeric
    names(beta) <- colnames(x)
    
    prob <- 1/(1 + exp(-x_beta0))
    # grad <- t(x) %*% (y - p) /n
    
    
    if (max(abs(beta - beta0))/max(abs(beta)) < eps) { 
      return( list(intercept=beta[1], beta = beta[-1], fct_levels = fct_levels,
                   iter = iter, converged = TRUE))
    }
    
    beta0 = beta
    
  }
  
  warning(paste("Method did not converge in", iter_max, "iterations", sep = " ")) 
  
  return(list(intercept=beta[1], beta = beta[-1], fct_levels = fct_levels,
              iter = iter, converged = FALSE))
}



predict_logistic_lasso<- function(object, new_x){
  ## predict_logistic_lasso converts the result get from 
  ## fit_logistic_lasso back to the correct factor levels.
  ## 
  ## Input:
  ## - object: The output from fit_logistic_lasso function,
  ##           a list of information getting from the logistic
  ##           lasso. 
  ## - new_x:  Data to predict at. It is possible that it 
  ##           contains more than one point.
  ##
  ## Output:
  ## - A list of information from the logistic lasso regreession
  ##   model is returned. It contained the  
  ##
  ## Example:
  ##
  ## 
  
  new_x <- as.matrix(new_x)
  intercept <- c(rep(1, nrow(new_x)))
  new_x <- cbind(intercept, new_x)
  
  beta  <- c(object$intercept, object$beta)
  numeric_pred <- (new_x %*% beta >= 0) %>% as.numeric ##
  return( object$fct_levels[numeric_pred + 1] %>% factor)
  
}



