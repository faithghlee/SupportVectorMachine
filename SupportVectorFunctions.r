#LIBRARIES 
setwd('/users/faithlee/Desktop/SupportVectorMachine')

#LIBRARIES 
library(quadprog) 
library(ggplot2)
library(R.matlab)


#LOAD DATA 
#NOTE THE DATA IS IN .MAT FORM; 
# the features are encoded in p by n; the responses are in n by 1 
#1. LINEAR
linear<- readMat('linear_new.mat')
linear_train<-data.frame(X = t(linear$X), class = linear$y)
linear_test<-data.frame(X = t(linear$Xtest), class = linear$ytest)



#2. Noisy Linear
noisy<-readMat('noisylinear_new.mat')
noisy_train<-data.frame(X = t(noisy$X), class = noisy$y)
noisy_test<-data.frame(X = t(noisy$Xtest), class = noisy$ytest)

#3. Quadratic 
quadratic<-readMat('quadratic_new.mat')
quadratic_train<-data.frame(X = t(quadratic$X), class = quadratic$y)
quadratic_test<-data.frame(X = t(quadratic$Xtest), class = quadratic$ytest)

########EDA###############

#' EDA USING GGPLOT2 
#' @param data either of the data.frame 
#' @param type string of either "Linear" or "Noisy" or "Quadratic"
#' @param label string of either "Training" or "Test" 
#' @return plot showing the separability of our data 
#' 

generate_plot<-function(data, type, label){

  p<-ggplot(data = data, aes(x = X.1, y = X.2, color = factor(class))) + geom_point() + 
    theme_bw()+ggtitle(paste(type, label, "Data Set")) +theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                 panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 
  p 
}
  
############################################
## ---Hard Margin Classifier for SVM --- ###
############################################


#' Hard Margin Classifier using quadprog
#' In the documentation of quadprog, it requires matrix to be positive definite. 
#' Our matrix is positive semi-definite. To go around this, we need to tweak our matrix to be 
#' positive definite by adding in a very small epsilon term 
#' @param eps (optional; default = 5e-4)
#' @param X is an n by p vector of features 
#' @param y is an n by 1 vector of classes {-1, 1}
#' @return \item{\beta} and \item{\beta_0}

HardMarg<-function(X, y, eps){
  if (missing(eps)){
    eps <- 5e-4
  }
n <- length(y)
Q <- as.matrix(y*X)# n by 2 matrix 
S <- Q%*%t(Q)#n by n matrix 
S_star <- S + eps*diag(n) #add epsilon to all the diagonals so it becomes positive def. 
dvec <- matrix(1, nrow=n)

b0 <- rbind( matrix(0, nrow=1, ncol = 1), matrix(0, nrow = n, ncol = 1))
A <- t(rbind(matrix(y, nrow=1, ncol=n), diag(nrow=n)))

#From the A matrix (which is the part of A^Tb >= b_0, the first row is equality, rest are inequality)
sol <- solve.QP(S_star, dvec, A, b0, meq=1, factorized=FALSE) 
qpsol <- matrix(sol$solution, nrow=n) 

#Compute beta_0 and beta 
W <- as.matrix(colSums(qpsol*y*X)) #1 by p 
b <- -0.5*(max(as.matrix(X[y == -1,])%*%W) + min(as.matrix(X[y==1,])%*%W))
list(b0 = b, B = W)

}

############################################
##---Soft Margin Classifier for SVM --- #### 
############################################
#' Soft Margin Classifier 
#'
#' @param epsilon (optional; default = 5e-4)
#' @param gamma (sum of slack variables <= gamma)
#' @param X is an n by p vector of features 
#' @param y is an n by 1 vector of classes {-1, 1}
#' @return \item{\beta} \item{\beta_0} 


SoftMarg<-function(X,y, gamma, eps){
  if (missing(eps)){
    eps <- 5e-4
  }
  n <- length(y)
  Q <- as.matrix(y*X)# n by 2 matrix 
  S <- Q%*%t(Q)#n by n matrix 
  S_star <- S + eps*diag(n)
  dvec <- matrix(1, nrow=n)
  
  #Here the constraints need to satisfy three things 
  # 1. 0<= \alpha_i 
  # 2. \alpha_i <= gamma 
  # 3. \sum_{i=1}^{n} \alpha_iy_i = 0 
  
  b0 <- rbind( matrix(0, nrow=1, ncol = 1), matrix(-gamma, nrow = n, ncol = 1), matrix(0, nrow = n, ncol = 1)) #alpha_i <= C , alpha_i > = 0 
  A<- t(rbind(matrix(y, nrow=1, ncol=n), -diag(nrow=n), diag(nrow = n)))
  
  # call the QP solver:
  sol <- solve.QP(S_star, dvec, A, b0, meq=1, factorized=FALSE)
  qpsol <- matrix(sol$solution, nrow=n)
 
  #Compute beta_0 and beta 
  W <- as.matrix(colSums(qpsol*y*X)) #1 by p 
  b <- -0.5*(max(as.matrix(X[y == -1,])%*%W) + min(as.matrix(X[y==1,])%*%W))
  list(b0 = b, B = W)
}

############################################
###Using output for classification----- #### 
############################################

#' Use the outputs from hardmarg /softmarg to see performance on test and train dataset 
#' @param Xtest n_test by p matrix 
#' @param b vector of coefficients separating hyperplane (from above functions)
#' @param b0 scalar (from above functions)
#' @return y n_test by 1 matrix of predicted outputs.  


Classify<-function(Xtest, b, b0){
  Xtest<-as.matrix(Xtest)
  b<-as.matrix(b)
  estimate<- Xtest%*%b + b0 
  yhat<-t(ifelse(estimate<0, -1, 1))
  return(yhat)
}


## PLOT BOUNDARIES AND MISCLASSIFICATION ######
#' Use the functions above to plot the hyperplane, the training accuracies/misclassification, 
#' Plot out the actual boundaries. Plot the SVM margins as well 
#' 
#' @param X matrix n by p (or n_test by p if using test set)
#' @param gamma scalar (for soft margin SVM). if none provided defaults to hard margin SVM 
#' @param eps scalar (make our matrix positive def. again default to 5e-4 if not provided)
#' @param type string of either "Linear" or "Noisy" or "Quadratic"
#' @param label string of either "Training" or "Test" 
#' @param y vector of n by 1 (or n_test by 1 if using test set) this will simply be used for accurary measures 
#' @return accuracy and plot of data points 
#' 

results <- function(X, y, gamma, eps, type, label){
  if (missing(eps)){
    eps<- 5e-4
  }
  
  if (missing(gamma)){
    B <- HardMarg(X,y, eps)$B
    b0 <-HardMarg(X,y,eps)$b0
  } else { 
    B <- SoftMarg(X,y, gamma, eps)$B
    b0 <-SoftMarg(X,y,gamma, eps)$b0
  }
  
  y_predict<-t(Classify(X, B, b0)) #transpose because it gives us 1 by n 
  accuracy <- round(sum(y_predict == y)/length(y_predict), digits = 3)
  
  # PLOT TIME 
  #' Separating hyperplane 
  #' Recall the equation of separating hyperplane is defined by 
  #' x^T\beta + \beta_0 = 0 
  #' Use X2 as vertical axis, X1 as horizotal axes. then Y = mX + C equation becomes 
  #' \begin{equation} X_2 = -b_0/\beta_2 - \beta_1/-\beta_2X_1 \end{equation}
  #' 
  
  #' Margins 
  #' Recall the margin is defined by  x^T\beta + \beta_0 = {-1, 1}
  #' Equation for margins is simply \begin{equation} X_2 = -b_0/\beta_2 - \beta_1/-\beta_2X_1 \pm 1/\beta_2 \end{equation}
  #' 
  
  m = -B[1]/B[2] 
  intercept_svm = -b0/B[2] 
  width <- 1/sqrt(B[1]^2 + B[2]^2)
  
  data<-data.frame(X, y, y_predict)
  data$plot_class<-as.factor(ifelse(data$y == data$y_predict, data$y, "misspecified"))
  p<-ggplot(data = data, aes(x = X.1, y = X.2, color = plot_class)) + geom_point()  + theme_bw() +ggtitle(paste(type, label, "Data Set")) +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ 
    geom_abline(intercept = intercept_svm, slope = m)+ geom_abline(intercept = intercept_svm + 1/B[2], slope = m, linetype = "dashed", color = "red")+ 
    geom_abline(intercept = intercept_svm - 1/B[2], slope = m, linetype = "dashed", color = "red") +annotate("text", x = 0.7, y = 0.7, label = paste("Accuracy is", accuracy))
  p 
  
}


