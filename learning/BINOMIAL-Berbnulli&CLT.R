#二项分布-->中心极限定理
x1<-rbinom(100,1,prob = .7)
xn<-replicate(100,rbinom(100,1,prob = .7))
xn_m <- mean(xn)
xn_sd <- sd(xn)
xn_l_m <- apply(xn,2,mean)
xn_l_sd <- apply(xn,2,sd)
hist(xn_l_m)
plot(density(xn_l_m))