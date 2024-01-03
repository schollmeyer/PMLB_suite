library(devtools)
devtools::install_git("https://github.com/maltenlz/Compressed-Rule-Ensembles")
library(cre)



x1 = c(rnorm(100,-1, 1), rnorm(100, 1, 1))
x2 = c(rnorm(100,-1, 1), rnorm(100, 1, 1))
x = cbind(x1, x2)
y = c(rep(1, times = 100), rep(0, times = 100))
#Run the CRE model with default settings:
cre_mod = cre(x, as.factor(y), task = "class")
#Predict in-sample:
predict(cre_mod, x)
#Look at the most important rules:
important_rules(cre_mod)
#Also look the distribution of split points:
visualise_clusters(cre_mod)

cre(x,y,task = "class", eta = 0.1, k=2)

#try integration in caret
source("make_method_cre.R")

#check runtime
system.time(
model <- caret::train(x=x,y=as.factor(y),method = cre)
)

#compare to ranger
system.time(
model <- caret::train(x=x,y=as.factor(y),method = "ranger")
)

#compare to knn
system.time(
  model <- caret::train(x=x,y=as.factor(y),method = "knn")
)

  
  
  
  