% NORMAL SAMPLES USING BOX-MUELLER METHOD
% DRAW SAMPLES FROM PROPOSAL DISTRIBUTION
u = rand(2,100000);
r = sqrt(-2*log(u(1,:)));
theta = 2*pi*u(2,:);
x = r.*cos(theta);
y = r.*sin(theta);

% DISPLAY BOX-MULLER SAMPLES
figure
% X SAMPLES
subplot(121);
hist(x,100);
colormap hot;axis square
title(sprintf('Box-Muller Samples Y\n Mean = %1.2f\n Variance = %1.2f\n Kurtosis = %1.2f',mean(x),var(x),3-kurtosis(x)))
xlim([-6 6])

% Y SAMPLES
subplot(122);
hist(y,100);
colormap hot;axis square
title(sprintf('Box-Muller Samples X\n Mean = %1.2f\n Variance = %1.2f\n Kurtosis = %1.2f',mean(y),var(y),3-kurtosis(y)))
xlim([-6 6])

library(nortest)
boxmuller<-function(){
  size = 1000000
  
  u = runif(size)
  v = runif(size)
  
  x=rep(0,size)
  y=rep(0,size)
  
  for (i in 1:size){
    x[i] = sqrt(-2*log(u[i]))*cos(2*pi*v[i])
    y[i] = sqrt(-2*log(u[i]))*sin(2*pi*v[i])
  }
  
  #a test for normality
  lillie.test(c(x,y))
  
  #plot the estimation of the density
  plot(density(c(x,y)))
}

ptm <- proc.time()
boxmuller()
proc.time() - ptm


myziggurat <- function(total,N) {
  x <- xtable
  y <- ytable
  A <- Atable
  R0 <- x[1]*y[1]
  data <- numeric(total)
  i = 1
  while (i <= total) {
    #index for rectangular
    k <- sample(0:N-1,1)           
    u1<-runif(1)
    if (k>0){
      #generating from rectangular S[k]
      xi<-u1*x[k]
      if(xi<(x[k+1])){
        #xi in R[k], accept
        data[i] = xi
        i = i+1
      }else{
        #generating yi
        u2<-runif(1)
        yi <- y[k]+u2*(y[k+1]-y[k])
        if(yi <=  dnorm(xi)) {
          data[i] = xi
          i = i + 1
        } else{
        }
      }
    }
    else{
      #generating from S0
      w<-runif(1,max = A)
      if (w<= R0) {
        data[i] = w/y[1]
        i = i + 1
      } else {
        data[i] =GenFromTail(x[1])
        j = j+1
        i = i+1
      }
    }
  }
  return(data)
}

ptm <- proc.time()
zdata <- myziggurat(1000000,256)
proc.time() - ptm


par(mfrow = c(3,1))
hist(zdata,breaks=1000, main = "Histagram of Ziggurat 10000 Sampling")

plot(density(zdata), main = "Density plot")

qqplot(zdata,rnorm(100000), main = "qq-plot of Ziggurat 10000 Sampling")