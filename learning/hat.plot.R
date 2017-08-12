hat.plot <- function(fit){
  p <- length(coefficients(fit))#模型的参数数目
  n <- length(fitted(fit))#预测值的数目跟样本量相等
  plot(hatvalues(fit),main = 'Index Plot of Hat Values')#绘制预测点对应的hat statics点
  abline(h=c(2,3)*p/n,lty=2)
  identify(1:n,hatvalues(fit),names(hatvalues(fit)))
}