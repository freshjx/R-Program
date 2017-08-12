residplot <- function(fit,nbreaks=10){
  z<- rstudent(fit)
  hist(z,breaks = nbreaks,freq = F,
       xlab = 'Studentized Residual',main='Distribustion of Errors')
  rug(jitter(z),col='brown')
  curve(dnorm(x,mean = mean(z),sd=sd(z)),
        add = TRUE,col='blue',lwd=2)
  lines(density(z),col='red',lwd=2,lty=2)
  legend('topright',
         legend = c('Normal Curve','Kernel Density Curve'),
         lty = 1:2,col=c('blue','red'),cex = .7)
}