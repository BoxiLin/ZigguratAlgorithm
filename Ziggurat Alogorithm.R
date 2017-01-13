# Finding A and x[0]~x[N-1], y[0]~y[N-1]
# using the standard norm
dnorminv<-function(y) sqrt(-2*log(sqrt(2*pi)*y))


findAxy <- function (N) {
  x1 <- numeric (N)
  y1 <- numeric (N)
  flag1 = 0
  A1 = 0
  xcap = 5
  xflo = 0
  xcur = (xcap+xflo)/2
  flag1= findyN(N,xcur) [[1]]
  
  while (flag1 !=1) {
    if (flag1 == 100) {
      xflo = xcur
    } else if (flag1 == -100)  {
      xcap = xcur
    }
    xcur = (xcap+xflo)/2
    flag1= findyN(N,xcur) [[1]]
  }
  
  A1= round(findyN(N,xcur) [[2]],digits = 6)
  x1 = round(findyN(N,xcur) [[3]],digits = 6)
  y1 = round(findyN(N,xcur) [[4]],digits = 6)
  list (A = A1 , x = x1 , y = y1)
}


findyN <- function (N,xinit) {
  x <- numeric (N)
  y <- numeric (N)
  
  flag = 0
  
  x[1] <- xinit
  tail <- 1-pnorm(x[1])
  y[1] <- dnorm(x[1])
  A <- x[1] * y[1] + tail
  
  x[2]= x[1]
  y[2] = y[1]+A/x[2]
  
  for (i in 2:N-1) {
    if ((y[i] > dnorm(0))) {
      flag = -1
      break
    }
    
    x[i+1] = dnorminv(y[i])
    y[i+1] = y[i] + A/x[i]
    
  }
  
  if (flag == -1) {flag = 100}
  else if(abs(y[N] - dnorm(0))<=1e-6) {
    flag =1   #FOUND
  } else if (y[N] > dnorm(0)) {
    flag = 100
  } else if (y[N] < dnorm(0)) {
    flag = -100
  }
  return (list (flag,A,x,y))
}

#ziggurat

#Genearating from standard normal
GenFromTail<-function(a) {
  u1<-runif(1)
  u2<-runif(1)
  while (u2 > a*sqrt((a^2-2*log(u1)))){
    u1<-runif(1)
    u2<-runif(1)
  }
  x = sqrt((a^2-2*log(u1)))
  return(x)
}

table <- findAxy(256)
#x[N+1] <- 0
xtable <-table$x
ytable <-table$y
Atable <-table$A

myziggurat <- function(total,N) {
  x <- xtable
  y <- ytable
  A <- Atable
  immediate = 0
  rejection = 0
  R0 <- x[1]*y[1]
  data <- numeric(total)
  signpn <- (2*rbinom(total,1,0.5)-1)
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
        data[i] = signpn[i]*xi
        immediate = immediate + 1
        i = i+1
      }else{
        #generating yi
        u2<-runif(1)
        yi <- y[k]+u2*(y[k+1]-y[k])
        if(yi <=  dnorm(xi)) {
          data[i] = signpn[i]*xi
          i = i + 1
        } else{
          rejection = rejection + 1
        }
      }
    }
    else{
      #generating from S0
      w<-runif(1,max = A)
      if (w<= R0) {
        data[i] = signpn[i]*w/y[1]
        immediate = immediate + 1
        i = i + 1
      } else {
        data[i] = signpn[i]*GenFromTail(x[1])
        i = i+1
      }
    }
  }
  print(noquote(paste("rejection rate:",rejection/total)))
  print(noquote(paste("immediate accept rate:",immediate/total)))
  return(data)
}
    
    
    
    
  #      xi <- x[i]                   # x[i]
  #  if (i == N-1) {xi_1 = 0}
  #  else {xi_1 <- x[i+1]   }              # x[i+1]
  #  yi <- y[i]                   # y[i]
  #  if (i==0) {yi_1 = 0}
  #  else {yi_1 <- y[i-1]}                   # y[i-1]
  #  u1 <- runif(1)                 # ramdom u
  #  x1 <- u1 * xi                  # random generated data
  #  if (x1<xi_1) {
  #    data[k] = x1
  #    k = k+1
  #  } else {
  #    y1 <- dnorm(x1)
  #    y1.1 <- y1 - yi_1
  #    u2 <- runif(1)
  #    y2 <- u2 * (yi - yi_1)
  #    if (y2 < y1.1) {
  #      data [k] = x1
  #      k = k+1
  #    }
 #   }
#  }
  #return (data)
#}


zdata <- myziggurat(10000,256)

hist(zdata,breaks=1000, main = "Histagram of Ziggurat 10000 Sampling")

qqplot(zdata,rnorm(10000), main = "qq-plot of Ziggurat 10000 Sampling")
