skrinkage <- function(fit,k=10) {
  require(bootstrap)
  
  theta.fit <- function(x,y){lsfit(x,y)}
  theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef}
  
  x <- fit$model[,2:ncol(fit$model)]#���Ԥ�����(�Ա���)
  y <- fit$model[,1]#�����Ӧ����(�����)
  #k�������Ľ�����֤
  results <- crossval(x,y,theta.fit,theta.predict,ngroup = k)
  r2 <- cor(y,fit$fitted.values)^2 #R^2=������Ӧ������ģ�͹�����Ӧ����֮���ϵϵ����ƽ��
  r2cv <- cor(y,results$cv.fit)^2 #������֤��R^2
  cat('Original R-square =',r2,'\n')
  cat(k,'Fold Cross-validated R-square',r2cv,'\n')
  cat('Change =',r2-r2cv,'\n')
}