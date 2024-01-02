library(devtools)
devtools::install_git("https://github.com/maltenlz/Compressed-Rule-Ensembles")
library(cre)



x1 = c(rnorm(100,-1, 1), rnorm(100, 1, 1))
x2 = c(rnorm(100,-1, 1), rnorm(100, 1, 1))
x = cbind(x1, x2)
y = c(rep(1, times = 100), rep(0, times = 100))
#Run the CRE model with default settings:
cre_mod = cre(x, y, task = "class")
#Predict in-sample:
predict(cre_mod, x)
#Look at the most important rules:
important_rules(cre_mod)
#Also look the distribution of split points:
visualise_clusters(cre_mod)



for(k in seq_len(length(datasets))){
  dat <- datasets[[k]]
  n_row <- nrow(dat)
  n_col <- ncol(dat)
  x <- dat[,-n_col]
  y <- dat[,n_col]
  #cre_mod = cre(x, y, task = "class",eta=0.5,k=4,model_type="glmnet")
  #y_perturbated <- perturbate_y(y)
  #x_perturbated <- perturbate_x(x)
  folds <- compute_k_folds(n_row,k=10)
  accuracy_tree <- rep(0,10)
  for(l in (1:10)){
    indexs <- which(folds==l)
    x_train <- x[-indexs,]
    y_train <- y[-indexs]
    x_test <- x[indexs,]
    y_test <- y[indexs]
    
    #
    svm_model <- train(x=x_train,y=y_train,method = 'svmLinear')
    tree_model <- train(x=x_train,y=as.factor(y_train),method = 'J48')
    accuracy_tree[l] <- compute_accuracy(predict(tree_model,x_test),y_test)
    
  }
  
  
  
  n_test <-round(metadat$n_instances/2)
  
  i_test <- sample((1:metadat$n_instances),size=n_test)
  
  x_test <- x[i_test,]
  y_test <- y[i_test]
  
  x_train <- x[- i_test,]
  y_train <- y[- i_test]
  
  
  m <- nrow(dat)
  
  p <-ncol(dat)
  #####
  #####
  #      SVM
  #probs_svm <- probs_rpart <- rep(0,m)
  
  for(l in (1:m)){
    
    models_svm <- tune(svm, train.x=dat[-l,-p],train.y=dat$target[-l],
                       ranges = list(gamma = 2^(-1:1), cost = 2^(2:4)),
                       tunecontrol = tune.control(sampling = "fix"),best.model=TRUE,probability=TRUE,kernel="radial"
    )
    
    probs_svm[l] <- (predict(models_svm$best.model,dat[l,-p],probability=TRUE))#$probabilities[,2]
    
    
    
    
    #ROC_svm <- roc(response = y_test,predictor = prediction_svm)
    
    
    #####
    #####
    #      RPART
    
    
    models_rpart <- tune.rpart(target~.,data=dat[-l,], minsplit = c(5,10,15))
    
    
    
    
    
    
    probs_rpart[l] <- predict(models_rpart$best.model,dat[l,])
  }
  
  ROC_svm <- roc(response = dat$target,predictor = probs_svm)
  ROC_rpart <- roc(response = dat$target,predictor = probs_rpart)
  
  plot(ROC_svm)
  lines(ROC_rpart,col="blue")
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ####
  ####
  #          RANDOMFOREST
  
  x <- dat[,which(colnames(dat)!="target")]
  y <- as.factor(dat[,which(colnames(dat)=="target")])
  
  model_rf <- tuneRF( x = x ,y = y ,doBest=TRUE,stepFactor=1.5,plot=FALSE,improve=0.01,probability=TRUE)
  
  #M <- randomForest(x = x,y=y)
  
  prediction_rf <- model_rf$votes[,2]#/M$votes[,1]#model_rf$votes[,2]
  
  ROC_rf <- roc(response = dat$target ,predictor = prediction_rf)#,levels=c("0","1"))
  
  
  #####
  #####
  #####
  # KNN
  
  #models_knn <- tune.knn(x=x,y=y,data=data,k= (1:5), tunecontrol = tune.control(sampling = "boot"))
  #models_knn$best.parameters$k
  
  #model_knn <- gknn(target~.,data=dat,k= models_knn$best.parameters$k)#, l = 0, prob = TRUE, use.all = TRUE)
  
  
  #obj <- tune(svm, Species~., data = iris,
  #             ranges = list(gamma = 2^(-1:1), cost = 2^(2:4)),
  #            tunecontrol = tune.control(sampling = "fix")
  #          )
  
  
  ###plots
  
  ROC_svm <- roc(dat$target,probs_svm)
  ROC_rpart <- roc(dat$target,probs_rpart)
  
  plot(ROC_svm)
  lines(ROC_rpart,col="blue")
  lines(ROC_rf,col="cyan")
  
  
  
  
}