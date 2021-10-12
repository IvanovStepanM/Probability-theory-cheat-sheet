# формулы для контрольной №1
# probability of a uniform variable = 1.6 on the interval [1, 2]
punif(1.6, 1, 2) #0.6
c = seq(1, 2, 0.2)
punif(c, min =1, max =2) # перевести вектор с в вероятности от 1 до 2

# success prob.0.3
# (Simulate Binomial or Bernoulli trials)
rbinom(100, 25, 0.3) #100 trials, 25 per each, rate of smth is 0.3
#gives a list of how many whatever per day

dbinom(1, size =25, prob =0.3) #prob of getting exactly 1 of 25

#cdf, not pmf
pbinom(3, 25, 0.3) # 25 trials and x is 3 or less (x<4)


#To get f(x) of normal distribution: 
  dnorm(x, mean, sd, log=FALSE)

#To get CDF I.e F(x) of normal distribution:
  pnorm(x, mean, sd)

#To get the value of x where there area on the left of the curve equals a certain area e.g let's say we want to know what value of x corresponds to 90th percentile:

  qnorm(0.9, mean, sd)
  #Poisson distribution, (target, lambda)
  1-ppois(27, 20)
  
  ppois(2, 1)
  
  #Questions_Solutions_191016
  # (8) ????~????????????(???? = 0.5). What is the density function for ???? = 2, i.e. evaluate 
  #f (2) (pdf - density f-n)
  dexp(2, 0.5)
  #или F(u=2) = ????*e^(-????x)=
  # = 0.5 * e ^ (-2/2)
  
  # (9) F(2) cumulutive distribution u=2 (cdf, cumulative d-n)
  cumsum(pexp(2, 0.5))

  
  #Ch.3.7-3.7
  #Задача №103(а-с)
  
  # тарелка 5х10, bivariate normal distribution
  # means: 5 and 10, variances 0.01 and 0.04, corr = 0.8
  # периметр C, площадь A
  install.packages('MASS')
  library(MASS)
  xy <- mvrnorm(10000, c(5, 10), matrix(c(0.01, 0.016, 0.016, 0.04), nrow = 2, byrow =F))
  x <- xy[, 1]
  y <- xy[, 2]
  c <- 2* x + 2 * y
  # (a) E[C]
  mean(c)
  # (b) E(A)
  a <- x + y
  mean(a)
  # (c) p(!29<C<31)
  1- sum((c>29)*(c<31))/10000

  
  #Chapter 3.6
  #43. Let X and Y be independent and unif[0,1].
  #Find the cdf and pdf or the random variables
  # (a) |X-Y|  (b) X/(X+Y)
  library("ggplot2")
  
  n <- 10000
  #(a)
  X <- runif(n, 0, 1)
  Y <- runif(n, 0, 1)
  Z <- abs(X-Y)
  qplot(Z, geom="histogram", main ="pdf",col=I("red")) #pdf
  plot(ecdf(Z)) #cdf
  
  i = seq(0, 1, 0.01)
  plot(i, (1-(1-i)**2)) #cdf Kenneth (нихуя не понял)
  #(b)
  Zb <- X/(X+Y)
  qplot(Zb, geom="histogram", main ="pdf",col=I("red")) #pdf
  plot(ecdf(Zb)) #cdf
  #ещё одно оухительное решение (нихуя не понял)
  i= seq(0,1,0.01)
  j= seq(0,0.5,0.01)
  k= seq(0.51,1,0.01)
  F_j <- j/(2*(1-j))
  F_k <- 1-((1-k)/(2*k))
  A <- c(F_j, F_k)
  plot(i, A)
  
  ##2. expected time that Billy must wait for Adam (ref.problem 34)
  
  n <- 10000
  A <- runif(n, 0, 30)
  B <- runif(n, 0, 45)
  
  B_waiting_time <- (A-B)
  exp_time_mean  <- mean(B_waiting_time[B_waiting_time>0])
  exp_time_mean
  
  #решение Kenneth, с учётом ожидания =0 (ср.меньше)
  sum(A[A>B]-B[A>B])/n #нихуя не понял!!
  #48. X and Y are indep. and unif[0,1]
  ##Find (a) E[XY]  (c) E[log(XY)]
  n <- 10000
  X <- runif(n, 0, 1)
  Y <- runif(n, 0, 1)
  E_XY <- mean(X)*mean(Y) #or mean(X*Y)
  E_log_XY <- mean(log(X))+mean(log(Y))
  mean(log(X*Y))
  #решение через интеграл =-2
  
  
  #1 140911
  rbinom(6, 3, 0.5)
  
  u <- runif(10)
  x <- 0*(u<0.125)+1*((u>0.125)*(u<0.5))+2*((u>0.5)*...)
  
  #Prob Ch3_34
  
  #Adam and Billy Bob have agreed to meet at 12:30. Assume that their arrival times
  #are independent random variables, AdamвЂ™s uniformly distributed between 12:30 and
  #1:00 and Billy BobвЂ™s uniformly distributed between 12:30 and 1:15.
  #(a) Compute the probability that Billy Bob arrives first.
  
  n=100000
  A=runif(n, 0, 30)
  B=runif(n, 0, 45)
  mean(B<A)
  
  #(b) Compute the probability that the one who arrives first must wait more than 10 minutes.
  
  mean((abs(A-B)>10))
  
  
  # задача 13 экзамена. 3 кубика, Р(х=6)
  
n=100000
A=runif(n, 1, 6)
B=runif(n, 1, 6)
C=runif(n, 1, 6)

SUMMA <- 
mean(B<A)



# задача 15
# P(T<20 U  T >30)
q1 <- rbinom(15, 1, 0.5)
q2 <- rbinom(5, 1, 0.6)
q3 <- rbinom(22, 1, 0.4)
q4 <- rbinom(14, 1, 0.3)

X <- q1+q2+q3+q4

p <- 23.5/56 # 56 человек, из них ожидается 23.5
# compute P(30 <= k <= 20) using 'pbinom()'
answer<-pbinom(size = 56, prob = p, q = 30) - pbinom(size = 56, prob = p, q = 19)
answer1<-1-answer
