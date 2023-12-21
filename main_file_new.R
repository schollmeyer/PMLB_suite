## needed own functions:


compute_k_folds <- function(n_row,k=10,set.seed=TRUE,seed=1234567){
  result <- rep((1:k),ceiling(n_row/k))
  result <- result[(1:n_row)]
  if(set.seed){set.seed(seed)}
  return(sample(n_row))
 }

########



#Welche Methoden:


#SVM
#RPART
#Random Forest
#ELASTIC NET
#KNN

#LOGISTIC REGRESSION ? bzw. GLM

# knn



#install.packages("pmlbr")
library(caret)
library(pmlbr)
library(e1071)
library(pROC)
library(randomForest)
library(kernlab)

t <- 1
datasets <- list()
for(k in seq_len(length(classification_dataset_names))){
  index <- which(summary_stats[,1]==classification_dataset_names[k])
  metadat <- summary_stats[index,]
  if( metadat$n_instances %in% c(10:10000) & metadat$n_classes==2 & metadat$task== "classification"){
      dat <- fetch_data(classification_dataset_names[k])
      datasets[[t]] <- dat
      print(t)
      t <- t+1
  }
}

saveRDS(datasets,"datasets.RDS")

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
  for(l in (1:10)){
    indexs <- which(folds==l)
    x_train <- x[-indexs,]
	y_train <- y[-indexs]
	x_test <- x[indexs,]
	y_test <- y[indexs]
	
	
	svm_model <- train(x=x_train,y=y_train,method = 'svmLinear')

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
}
