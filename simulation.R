
## Parameters
trueL<-4
x<-c(1:6)

## Function
feas<-function(lambda,dat){
  sum((dat$x1<=lambda)&(dat$x2>=lambda))/100
}




## Without noise
x1<-runif(100,1,trueL)
x2<-runif(100,trueL,6)

plot(cbind(x1,x2),xlim=c(1,6),ylim=c(1,6),
     xlab=expression(x[1]),ylab=expression(x[2]),cex.lab=1.5)
abline(v=4)
abline(h=4)

unlist(lapply(c(1:6),feas,dat=data.frame(x1=x1,x2=x2))) #result in Table 1


## With noise

x1n<-runif(100,1,trueL)+rnorm(100,0,1)
x2n<-runif(100,trueL,6)+rnorm(100,0,1)

x1n[x1n>6]<-6
x1n[x1n<1]<-1
x2n[x2n>6]<-6
x2n[x2n<1]<-1


plot(cbind(x1n,x2n),xlim=c(1,6),ylim=c(1,6),
     xlab=expression(x[1]),ylab=expression(x[2]),cex.lab=1.5)
abline(v=4)
abline(h=4)

unlist(lapply(c(1:6),feas,dat=data.frame(x1=x1n,x2=x2n)))  #result in Table 1


 
