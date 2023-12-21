#Welche Methoden:


#SVM
#RPART
#LOGISTIC REGRESSION ? bzw. GLM

#Random Forest

# knn



#install.packages("pmlbr")
library(pmlbr)
library(e1071)
library(pROC)
library(randomForest)
t <-0

for(k in (78:78)){
  
  
  
  
  
  index <- which(summary_stats[,1]==classification_dataset_names[k])
  
  metadat <- summary_stats[index,]
  
  
  
  metadat$n_instances %in% c(10:2000) & metadat$n_classes==2 & metadat$task== "classification"
  
  metadat
  
  
  {
    
    
    if(
      print(k)
      dat <- fetch_data(classification_dataset_names[k])
      t <- t+1
      print(c("t",t))
      
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