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
  y_noisy <- perturbate_y(y)
  x_noisy <- perturbate_x(x)
  folds <- compute_k_folds(n_row,k=10)
  
  
  clean_accuracies <- array(0,c(10,7))
  colnames(clean_accuracies) <- methods
  accuracies_noisy_y <- clean_accuracies
  accuracies_noisy_x <- clean_accuracies
  
  for(l in (1:10)){
    indexs <- which(folds==l)
    x_train <- x[-indexs,]
	y_train <- y[-indexs]
	x_test <- x[indexs,]
	y_test <- y[indexs]
	
	x_train_noisy <- x_noisy[-indexs,]
	y_train_noisy <- y_noisy[-indexs]
	x_test_noisy <- x_noisy[indexs,]
	y_test_noisy <- y_noisy[indexs]
	
	
	
	### clean data
	
	
	for(i in (1:6)){
	   method <- methods[i]
	   model <- train(x=x_train,y=as.factor(y_train),method = method)
	   accuracy <- compute_accuracy(predict(model,x_test),y_test)
	   clean_accuracies[l,i] <- accuracy
	   print(c(method,accuracy))
	 }
	 
	### perturbation in y
	for(i in (1:6)){
	   method <- methods[i]
	   model <- train(x=x_train,y=as.factor(y_train_noisy),method = method)
	   accuracy <- compute_accuracy(predict(model,x_test),y_test_noisy)
	   accuracies_noisy_y[l,i] <- accuracy
	   print(c(method,accuracy))
	}
	
	### perturbation in x
	for(i in (1:6)){
	   method <- methods[i]
	   model <- train(x=x_train_noisy,y=as.factor(y_train),method = method)
	   accuracy <- compute_accuracy(predict(model,x_test_noisy),y_test)
	   accuracies_noisy_x[l,i] <- accuracy
	   print(c(method,accuracy))
	}


}
}	
	

