library(caret)
library(cre)

## this script ceates a new method for compressed rule ensemble classifiation in the caret-package


# make list for compressed rule ensemble classifier
cre <- list(type = "Classification",
              library = "cre",
              loop = NULL) 

#hyperparameters of cre
hyperparams <- data.frame(parameter = c("eta", "k"),
                  class = c("numeric", "integer"),
                  label = c("Eta", "Max-Clusters"))

cre$parameters <- hyperparams

#grid element for hyperparam tuning

creGrid <- function(x, y, len = NULL, search = "grid") {
  # if(len < 4){
  #   stop("length must be larger or equal than 4")
  # }
  etas <- seq(0.1,0.9, by = 1/len)
  k_values <- c(4)
  
  ## To use grid search:
  if(search == "grid") {
    out <- expand.grid(eta = etas, 
                       k = k_values)
  } else {
    ## For random search, define ranges for the parameters then
    ## generate random values for them
    out <- data.frame(etas = runif(len,0,1),
                      k_values = c(4))#sample(k_values, size = len, replace = TRUE) )
  }
  out
}

cre$grid <- creGrid

# fit element for model fitting
creFit <- function(x, y, wts, param, lev, last, weights, classProbs, ...){
  cre::cre(x,
           y,
           task = "class",
           eta = param$eta,
           k = param$k,
           ...)   
}

cre$fit <- creFit

# predict element
crePred <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
cre$predict <- crePred

creProb <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "probabilities")
cre$prob <- creProb


