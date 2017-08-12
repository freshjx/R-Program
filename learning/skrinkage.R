skrinkage <- function(fit,k=10) {
  require(bootstrap)
  
  theta.fit <- function(x,y){lsfit(x,y)}
  theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef}
  
  x <- fit$model[,2:ncol(fit$model)]#存放预测变量(自变量)
  y <- fit$model[,1]#存放响应变量(因变量)
  #k个样本的交叉验证
  results <- crossval(x,y,theta.fit,theta.predict,ngroup = k)
  r2 <- cor(y,fit$fitted.values)^2 #R^2=真是响应变量和模型估算响应变量之间关系系数的平方
  r2cv <- cor(y,results$cv.fit)^2 #交叉验证的R^2
  cat('Original R-square =',r2,'\n')
  cat(k,'Fold Cross-validated R-square',r2cv,'\n')
  cat('Change =',r2-r2cv,'\n')
}