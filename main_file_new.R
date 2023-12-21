## needed own functions:


compute_k_folds <- function(n_row,k=10,set.seed=TRUE,seed=1234567){
  result <- rep((1:k),ceiling(n_row/k))
  result <- result[(1:n_row)]
  if(set.seed){set.seed(seed)}
  return(sample(result))
 }


compute_accuracy <- function(predictions,true_labels){
  result <- mean(predictions==true_labels)
  return(result)
}


perturbate_y <- function(y,p_percent=5,set.seed=TRUE,seed=1234567){
  if(set.seed){set.seed(seed)}
  indexs <- which( sample(c(1,0),size=length(y),prob=c(p_percent/100,(100-p_percent)/100),replace=TRUE)==1)
  length_indexs <- length(indexs)
  y[indexs] <- sample(y, size=length_indexs)
  return(y)
}


perturbate_x <- function(x,p_percent=5,set.seed=TRUE,seed=1234567){
  if(set.seed){set.seed(seed)}
  
  for(k in seq_len(ncol(x))){
    indexs <- which( sample(c(1,0),size=nrow(x),prob=c(p_percent/100,(100-p_percent)/100),replace=TRUE)==1)
    length_indexs <- length(indexs)
    x[indexs,k] <- sample(x[,k], size=length_indexs)
   }
  return(x)
}
########



#Welche Methoden:


#SVM
#RPART
#Random Forest
#ELASTIC NET
#KNN

#LOGISTIC REGRESSION ? bzw. GLM : Im Moment nicht





#install.packages("pmlbr")
library(caret)
library(Rweka)
library(ranger)
library(kknn)
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


methods <- c("svmLinear","svmRadial","J48","ranger","knn","glmnet","cre")
for(k in seq_len(length(datasets))){
  dat <- datasets[[k]]
  n_row <- nrow(dat)
  n_col <- ncol(dat)
  x <- dat[,-n_col]
  y <- dat[,n_col]
  #cre_mod = cre(x, y, task = "class",eta=0.5,k=4,model_type="glmnet")
  y_perturbated <- perturbate_y(y)
  x_perturbated <- perturbate_x(x)
  folds <- compute_k_folds(n_row,k=10)
  accuracy_svm <- accuracy_tree <- rep(0,10)
  
  clean_accuracies <- array(0,c(10,7))
  colnames(clean_accuracies) <- methods
  accuracies_noisy_y <- accuracies
  accuracies_noisy_x <- accuracies
  
  for(l in (1:10)){
    indexs <- which(folds==l)
    x_train <- x[-indexs,]
	y_train <- y[-indexs]
	x_test <- x[indexs,]
	y_test <- y[indexs]
	
	x_train_perturbated <- x_perturbated[-indexs,]
	y_train_perturbated <- y_perturbated[-indexs]
	x_test_perturbated <- x_perturbated[indexs,]
	y_test_perturbated <- y_perturbated[indexs]
	
	
	
	### clean data
	
	
	
	svm_model <- train(x=x_train,y=as.factor(y_train),method = 'svmLinear')
	accuracy_svm[l] <- compute_accuracy(predict(svm_model,x_test),y_test)
	
	tree_model <- train(x=x_train,y=as.factor(y_train),method = 'J48')
	accuracy_tree[l] <- compute_accuracy(predict(tree_model,x_test),y_test)
	
	
	rf_model <- train(x=x_train,y=y_train,method = 'ranger')
	accuracy_rf[l]compute_accuracy(predict(rf_model,x_test),y_test)
	
	knn_model <- train(x=x_train,y=y_train,method ='kknn')
	accuracy_knn[l] <- compute_accuracy(predict(knn_model,x_test),y_test)
	
	
	glmnet_model <- train(x=x_train,y=y_train,method ='glmnet')
	
	### perturbation in y
	
	svm_model <- train(x=x_train,y=as.factor(y_train_perturbated),method = 'svmLinear')
	accuracy_svm_perturbated[l] <- compute_accuracy(predict(svm_model,x_test),y_test_perturbated)
	
	tree_model <- train(x=x_train,y=as.factor(y_train_perturbated),method = 'J48')
	accuracy_tree_perturbated_y[l] <- compute_accuracy(predict(tree_model,x_test),y_test)
	
	
	rf_model <- train(x=x_train,y=y_train_perturbated,method = 'ranger')
	accuracy_rf_perturbated_y[l]compute_accuracy(predict(rf_model,x_test),y_test)
	
	knn_model <- train(x=x_train,y=y_train_perturbated,method ='kknn')
	accuracy_knn_perturbated_y[l] <- compute_accuracy(predict(knn_model,x_test),y_test_perturbated)

}


