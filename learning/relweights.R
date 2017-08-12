#计算模型中各个预测变量的权重，但只支持一元一维的线性回归模型
relweights <- function(fit,...){
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar,2:nvar]
  rxy <- R[2:nvar,1]
  svd <- eigen(rxx)#Computes eigenvalues and eigenvectors of numeric (double, integer, logical) or complex matrices.
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))#Extract or replace the diagonal of a matrix, or construct a diagonal matrix.
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda^2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta^2)
  rawwgt <- lambdasq %*% beta^2
  import <- (rawwgt / rsquare) * 100
  import <- as.data.frame(import)
  row.names(import) <- names(fit$model[2:nvar])
  names(import) <- 'Weights'
  import <- import[order(import),1,drop=FALSE]
  dotchart(import$Weights, labels = row.names(import),
     xlab = '% of R-Square', pch = 19,
     main = 'Relative Importance of Predictor Variabels',
     sub = paste('Total R-Square=',round(rsquare,digits = 3)),
     ...)
  return(import)
}