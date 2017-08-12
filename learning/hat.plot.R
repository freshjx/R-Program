hat.plot <- function(fit){
  p <- length(coefficients(fit))#ģ�͵Ĳ�����Ŀ
  n <- length(fitted(fit))#Ԥ��ֵ����Ŀ�����������
  plot(hatvalues(fit),main = 'Index Plot of Hat Values')#����Ԥ����Ӧ��hat statics��
  abline(h=c(2,3)*p/n,lty=2)
  identify(1:n,hatvalues(fit),names(hatvalues(fit)))
}