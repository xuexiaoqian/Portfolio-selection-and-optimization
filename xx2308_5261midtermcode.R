#####################################
## S5261: Midterm Project Code
## Author: Xiaoqian Xue (xx2308)
#####################################

setwd("~/Desktop/full_history")
data <- list.files(path = "~/Desktop/full_history")

#Select sub dataset with more than 2520 rows
n1 <- length(data)
subset0 <- c()
for (i in 3:n1){
  d <- read.csv(data[i])
  if (nrow(d) > 2520) {
    subset0 <- append(subset0, data[i])
  }
}

n2 <- length(subset0)
subset <- c(subset0[1])
for (i in 2:n2){
  d0 <- read.csv(subset0[i-1])
  d1 <- read.csv(subset0[i])
  if (d0$date[1] == d1$date[1] && d0$date[2520] == d1$date[2520]){
    subset <- append(subset, subset0[i])
  }
}

n3 <- length(subset)
subset1 <- c()
for (i in 1:n3){
  d2 <- read.csv(subset[i])
  zero <- apply(d2,2, function(x){ length(which(x==0))/length(x)})
  if (sum(zero) < 0.05){
    subset1 <- append(subset1, subset[i])
  }
}

#select only the top 2521 rows and flip the data
n4 <- length(subset1)
for (i in 1:n4){
  d <- read.csv(subset1[i])
  d <- d[1:2521, ]
  d1 <- d[seq(dim(d)[1],1),]
  dir.create("cleandata", showWarnings = FALSE)
  write.csv(d1, file.path("cleandata", subset1[i]), row.names = FALSE)
}

setwd("~/Desktop/full_history/cleandata")
##Eliminate assets with more than 5% of volume below 1000
n <- length(subset1)
subset2 <- c()
for (i in 1:n){
  d <- read.csv(subset1[i])
  less1000 <- sum(d$volume < 1000)
  u <- nrow(d) * 0.05
  if (less1000 <= u){
    subset2 <- append(subset2, subset1[i])
  }else{
    file.remove(subset1[i])
  }
}
subset2 <- list.files(path = "~/Desktop/full_history/cleandata")
length(subset2)


#Calculate log-returns
n <- length(subset2)
for (i in 1:n){
  d <- read.csv(subset2[i])
  d <- d[1:2521,]
  d$log_returns[1] <- NA
  for (j in 1:2520){
    d$log_returns[j+1] <- log(d$adjclose[j+1]/d$adjclose[j])
  }
  d1 <- d[2:2521, ]
  write.csv(d1, subset2[i], row.names = FALSE)
}

name.stocks <- c()
for (i in 1:n){
  d <- subset2[i]
  nd <- nchar(d)
  name.stocks[i] <- substr(d, 1, nd-4)
}
logret <- data.frame(matrix(0, ncol = n, nrow = 2520))
colnames(logret) <- c(name.stocks)
for (i in 1:n){
  d5 <- read.csv(subset2[i])
  logret[,i] <- d5$log_returns
}
write.csv(logret, "logreturn.csv",row.names = FALSE)

#Obtain the trading volume from all assets (3084 total)
n <- length(subset2)
volume <- data.frame(matrix(0, ncol = n, nrow = 2520))
colnames(volume) <- c(name.stocks)
for (i in 1:n){
  d <- read.csv(subset2[i])
  volume[,i] <- d$volume
}
write.csv(volume, "tradingvolume.csv",row.names = FALSE)


#####################################
## Rolling window
#####################################
### (4)Rolling window : m = 252; h = 20; subsamples N = 2520-252+20 = 2288
logret <- read.csv("~/desktop/full_history/cleandata/logreturn.csv")
RW1 <- logret[1:252,] #Select the first 252 days
RW2 <- logret[253:504,]
RW3 <- logret[505:756,]
RW4 <- logret[757:1008,]
RW5 <- logret[1009:1260,]
RW6 <- logret[1261:1512,]
RW7 <- logret[1513:1764,]
RW8 <- logret[1765:2016,]
RW9 <- logret[2017:2268,]
RW10 <- logret[2269:2520,]

#(4.4.1) Compute mean returns for each rolling window
meanret <- function(r){
  mu <- apply(r, 2, mean)
  n <- ncol(r)
  meanret <- matrix(c(mu), nrow = n, ncol = 1)
  name.stocks <- colnames(r)
  rownames(meanret) <- c(name.stocks)
  colnames(meanret) <- c('Mean Returns')
  return(meanret)
}

rfr <- 0
exc_logret1 <- apply(RW1, 2, function(x){x - rfr})
RW1.mean <- meanret(exc_logret1)
exc_logret2 <- apply(RW2, 2, function(x){x - rfr})
RW2.mean <- meanret(exc_logret2)
exc_logret3 <- apply(RW3, 2, function(x){x - rfr})
RW3.mean <- meanret(exc_logret3)
exc_logret4 <- apply(RW4, 2, function(x){x - rfr})
RW4.mean <- meanret(exc_logret4)
exc_logret5 <- apply(RW5, 2, function(x){x - rfr})
RW5.mean <- meanret(exc_logret5)
exc_logret6 <- apply(RW6, 2, function(x){x - rfr})
RW6.mean <- meanret(exc_logret6)
exc_logret7 <- apply(RW7, 2, function(x){x - rfr})
RW7.mean <- meanret(exc_logret7)
exc_logret8 <- apply(RW8, 2, function(x){x - rfr})
RW8.mean <- meanret(exc_logret8)
exc_logret9 <- apply(RW9, 2, function(x){x - rfr})
RW9.mean <- meanret(exc_logret9)
exc_logret10 <- apply(RW10, 2, function(x){x - rfr})
RW10.mean <- meanret(exc_logret10)

#Compute volatility for each rolling window
volatility <- function(r){
  sig <- apply(r,2,sd)
  n <- ncol(r)
  volret <- matrix(c(sig), nrow = n, ncol = 1)
  name.stocks <- colnames(r)
  rownames(volret) <- c(name.stocks)
  colnames(volret) <- c('Variance of Returns')
  return(volret)
}
RW1.vol <- volatility(exc_logret1)
RW2.vol <- volatility(exc_logret2)
RW3.vol <- volatility(exc_logret3)
RW4.vol <- volatility(exc_logret4)
RW5.vol <- volatility(exc_logret5)
RW6.vol <- volatility(exc_logret6)
RW7.vol <- volatility(exc_logret7)
RW8.vol <- volatility(exc_logret8)
RW9.vol <- volatility(exc_logret9)
RW10.vol <- volatility(exc_logret10)

#Compute sharpe ratio for each rolling window
sharper <- function(r){
  mu <- apply(r, 2, mean)
  sig <- apply(r,2,sd)
  sharpe <- mu/sig
  n <- ncol(r)
  sharperatio <- matrix(c(sharpe), nrow = n, ncol = 1)
  name.stocks <- colnames(r)
  rownames(sharperatio) <- c(name.stocks)
  colnames(sharperatio) <- c('Sharpe ratio')
  return(sharperatio)
}

RW1.sharp <- sharper(exc_logret1)
RW2.sharp <- sharper(exc_logret2)
RW3.sharp <- sharper(exc_logret3)
RW4.sharp <- sharper(exc_logret4)
RW5.sharp <- sharper(exc_logret5)
RW6.sharp <- sharper(exc_logret6)
RW7.sharp <- sharper(exc_logret7)
RW8.sharp <- sharper(exc_logret8)
RW9.sharp <- sharper(exc_logret9)
RW10.sharp <- sharper(exc_logret10)

#Compute beta for each rollimg window
#Optain market return (rm) from SP500
df <- read.csv("~/desktop/SP500new.csv", header = TRUE)
#Select the dates and calculates the log-returns
n1 <- df[df$Date == "2009-04-15",] #35400
n2 <- df[df$Date == "2019-04-18",] #37920
df <- df[35400:37920, ]
df$log_returns[1] <- NA
for (i in 2:2521){
  df$log_returns[i] <- log(df$Close[i]/df$Close[i-1])
}
df <- df[2:2521,] #eliminate the first row
#Select the dates and create subsets that consistent with each rolling window
df1 <- df[1:252,]
df2 <- df[253:504,]
df3 <- df[505:756,]
df4 <- df[757:1008,]
df5 <- df[1009:1260,]
df6 <- df[1261:1512,]
df7 <- df[1513:1764,]
df8 <- df[1765:2016,]
df9 <- df[2017:2268,]
df10 <- df[2269:2520,]

exc_sp1 <- df1$log_returns
exc_sp2 <- df2$log_returns
exc_sp3 <- df3$log_returns
exc_sp4 <- df4$log_returns
exc_sp5 <- df5$log_returns
exc_sp6 <- df6$log_returns
exc_sp7 <- df7$log_returns
exc_sp8 <- df8$log_returns
exc_sp9 <- df9$log_returns
exc_sp10 <- df10$log_returns
#Define the beta function to calculate beta:
beta <- function(r, exc_sp){
  model <- lm(r ~ exc_sp)
  beta <- coef(model)[2,]
  beta <- data.frame(as.numeric(beta))
  name.stocks <- colnames(r)
  rownames(beta) <- c(name.stocks)
  colnames(beta) <- c('beta')
  return(beta)
}

alphabeta <- function(r, exc_sp){
  model <- lm(r ~ exc_sp)
  alpha <- coef(model)[1,]
  beta <- coef(model)[2,]
  alphabeta <- data.frame(as.numeric(alpha), as.numeric(beta))
  rownames(alphabeta) <- c(name.stocks)
  colnames(alphabeta) <- c('alpha', 'beta')
  return(alphabeta)
}

RW1.beta <- beta(exc_logret1, exc_sp1)
RW2.beta <- beta(exc_logret2, exc_sp2)
RW3.beta <- beta(exc_logret3, exc_sp3)
RW4.beta <- beta(exc_logret4, exc_sp4)
RW5.beta <- beta(exc_logret5, exc_sp5)
RW6.beta <- beta(exc_logret6, exc_sp6)
RW7.beta <- beta(exc_logret7, exc_sp7)
RW8.beta <- beta(exc_logret8, exc_sp8)
RW9.beta <- beta(exc_logret9, exc_sp9)
RW10.beta <- beta(exc_logret10, exc_sp10)

#Function to calculated expected returns 
expret <- function(logret, sharpe){
  rfr <- 0
  exc_logret <- apply(logret, 2, function(x){x-rfr})
  sig <- apply(exc_logret ,2,sd)
  var <- sig^2
  volret <- matrix(c(sig), nrow = 30, ncol = 1)
  exp.ret<- sharpe*volret
  colnames(exp.ret) <- c('Expected Returns')
  return(exp.ret)
}

#Function for portfolio optimization (cited from Casey Tirshfield 'Mean-variance Portfolio Optimization')
optimization <- function(logret, exp_ret){# we compute the standardized log returns
  logret <- as.matrix(logret)
  n <- dim(logret)[1]
  p <- dim(logret)[2]
  stand_logret <- apply(logret, 2, scale)
  # we perform PCA
  logret_pca <- prcomp(stand_logret)
  # the principal components are
  logret_pc <- stand_logret %*% logret_pca$rotation
  #the variance of the principal components are
  logret_varpc <- logret_pca$sdev ^ 2
  # we take the first two principal components as factors
  f1 <- logret_pc[,1]
  f2 <- logret_pc[,2]
  model <- lm(logret ~ f1 + f2)
  beta1 <- coef(model)[2,]
  beta2 <- coef(model)[3,]
  # we estimate F
  F2 <- logret_varpc[1] * (beta1 %*% t(beta1)) + logret_varpc[2] * (beta2 %*% t(beta2)) + diag(diag(cov(resid(model))))
  S <- t(logret - mean(logret , 2)) %*% (logret - mean(logret, 2)) / n
  # gamma
  gamma <- sum(sum((F2 - S) ^ 2))
  # sig_overline
  sig_overline <- 0
  for (i in 1:(p - 1)){
    for (j in (i + 1):p){
      sig_overline <- sig_overline + S[i,j] / sqrt(S[i,i] * S[j,j])
    }
  }
  sig_overline <- sig_overline * 2 / (p * (p - 1))
  # pi_ij
  pi_ij <- function(i,j){
    sum(((logret[,i] - mean(logret[,i])) * (logret[,j] - mean(logret[,j])) - S[i,j]) ^ 2) / n
  }
  # pi
  pi <- 0
  for (i in 1:p){
    for (j in 1:p){
      pi <- pi + pi_ij(i,j)
    }
  }
  # theta
  theta <- function(k,i,j){
    sum(((logret[,k] - mean(logret[,k])) ^ 2 - S[k,k]) * ((logret[,i] - mean(logret[,i])) * (logret[,j] - mean(logret[,j])) - S[i,j])) / n
  }
  # rho
  rho <- 0
  for (i in 1:p){
    rho <- rho + pi_ij(i,i)
    for (j in 1:p){
      if (j!=i){
        rho <- rho + sig_overline * (sqrt(S[j,j] / S[i,i]) * theta(i,i,j) + sqrt(S[i,i] / S[j,j]) * theta(j,i,j)) / 2
      }
    }
  }
  # kappa
  kappa <- (pi - rho) / gamma
  # delta
  delta <- min(1, max(kappa / n, 0))
  # estimated efficient frontier
  # this code was inspired by the following matlab code: https://web.stanford.edu/~xing/statfinbook/_BookFun/ex3.2.4_plot_6assets_effifrontier.m
  eef <- function(mean, Cov, mu_star){
    inverseCov <- solve(Cov)
    ind <- rep(1, length(mean))
    A <- as.numeric(mean %*% inverseCov %*% ind)
    B <- as.numeric(mean %*% inverseCov %*% mean)
    C <- as.numeric(ind %*% inverseCov %*% ind)
    D <- B * C - A ^ 2
    return((B * inverseCov %*% ind - A * inverseCov %*% mean + mu_star * (C * inverseCov %*% mean - A * inverseCov %*% ind)) / D)
  }
  mu <- apply(logret, 2, mean)
  # Sigma
  Sigma2 <- delta * F2 + (1 - delta) * S
  w <- sapply(exp_ret, function(x){eef(mean=mu, Cov=Sigma2, x)})
  weights <- apply(w, 1, mean)
  return(weights)
}


#(4.4.2) Asset Selection Method (Using mean sharpe ratio as the criterion for asset selection)
#####################################
## Rolling window 1
#####################################
n <- 3084
sort.sharpe1 <- RW1.sharp[order(RW1.sharp[,1]),]
RW1.topsharpe <- as.data.frame(tail(sort.sharpe1, 30))
# The top 30 assets
RW1.names <- rownames(RW1.topsharpe)
RW1.select <- as.data.frame(matrix(0,  ncol = 30, nrow = 252))
colnames(RW1.select) <- c(RW1.names)
for (i in 1:30){
  for (j in 1:3084){
    if (colnames(RW1)[j] == RW1.names[i]){
      RW1.select[,i] <- RW1[,j]
    }
  }
}
#Predict h = 20 steps from RW1: 
library(fpp2)
RW1.fc <- apply(RW1.select,2,holt, 20)
#Construct Portfolio for forecast
RW1.expect <- expret(RW1.select, RW1.topsharpe)
RW1.expect <- RW1.expect$`Expected Returns`
RW1.weights <- optimization(RW1.select, RW1.expect)
RW1.fc <- as.data.frame(RW1.fc)
RW1.wrfc <- data.frame(matrix(0,ncol=30, nrow=20))
colnames(RW1.wrfc) <- RW1.names
for (i in 1:30){
  n <- 1 + (i-1)*5
  a <- RW1.fc[n]
  aweights <- RW1.weights[i]
  RW1.wrfc[,i] <- aweights*a
}
sum(meanret(RW1.wrfc)) #Total mean returns of the weighted prediction: 0.002709289
sum(volatility(RW1.wrfc)) #Total volatility of the weighted prediction:  0.0002452429 #volatility increases a lot
mean(sharper(RW1.wrfc)) #Mean sharpe ratio of the weighted prediction: 23.86187 (very good sharpe ratio)
#Sortino Ratio
RW1.rd <- RW1.wrfc[RW1.wrfc < 0] #downside returns
sd(RW1.rd) #standard deviation of the downside
RW1.sortino <- meanret(RW1.wrfc)/sd(RW1.rd) #risk free return = 0
colnames(RW1.sortino) <- c("Sortino Ratio")
mean(RW1.sortino) #mean so0rtino ratio of RW1 = 1.556431

#####################################
## Rolling window 2
#####################################
sort.sharpe2 <- RW2.sharp[order(RW2.sharp[,1]),]
RW2.topsharpe <- as.data.frame(tail(sort.sharpe2, 30))
# The top 30 assets
RW2.names <- rownames(RW2.topsharpe)
RW2.select <- as.data.frame(matrix(0,  ncol = 30, nrow = 252))
colnames(RW2.select) <- c(RW2.names)
for (i in 1:30){
  for (j in 1:3084){
    if (colnames(RW2)[j] == RW2.names[i]){
      RW2.select[,i] <- RW2[,j]
    }
  }
}
#Predict h = 20 steps from RW2: 
RW2.fc <- apply(RW2.select,2,holt, 20)
#Construct Portfolio for forecast
RW2.expect <- expret(RW2.select, RW2.topsharpe)
RW2.expect <- RW2.expect$`Expected Returns`
RW2.weights <- optimization(RW2.select, RW2.expect)
RW2.fc <- as.data.frame(RW2.fc)
RW2.wrfc <- data.frame(matrix(0,ncol=30, nrow=20))
colnames(RW2.wrfc) <- RW2.names
for (i in 1:30){
  n <- 1 + (i-1)*5
  a <- RW2.fc[n]
  aweights <- RW2.weights[i]
  RW2.wrfc[,i] <- aweights*a
}
sum(meanret(RW2.wrfc)) #Total mean returns of the weighted prediction: 0.006055987
sum(volatility(RW2.wrfc)) #Total volatility of the weighted prediction: 0.0009049389
mean(sharper(RW2.wrfc)) #Mean sharpe ratio of the weighted prediction:  12.53561 (very good)
#The turnover rates:
RW2.names[RW2.names == RW1.names] #0 asset from rolling window 2 consistent with rolling window 1
#The turnover rates = 0
N  <- 0
name <- c()
for (i in 1:30){
  for (j in 1:30){
    if (RW2.names[i] == RW1.names[j]){
      N <- N+1
      name <- append(name, RW1.names[j])
    }
  }
}
N #0 asset from rolling window 2 consistent with rolling window 1
#Sortino Ratio
RW2.rd <- RW2.wrfc[RW2.wrfc < 0] #downside returns
sd(RW2.rd) #standard deviation of the downside
RW2.sortino <- meanret(RW2.wrfc)/sd(RW2.rd) #risk free return = 0
colnames(RW2.sortino) <- c("Sortino Ratio")
mean(RW2.sortino) #mean sortino ratio of RW2 = 1.125356

#####################################
## Rolling window 3
#####################################
sort.sharpe3 <- RW3.sharp[order(RW3.sharp[,1]),]
RW3.topsharpe <- as.data.frame(tail(sort.sharpe3, 30))
# The top 30 assets
RW3.names <- rownames(RW3.topsharpe)
RW3.select <- as.data.frame(matrix(0,  ncol = 30, nrow = 252))
colnames(RW3.select) <- c(RW3.names)
for (i in 1:30){
  for (j in 1:3084){
    if (colnames(RW3)[j] == RW3.names[i]){
      RW3.select[,i] <- RW3[,j]
    }
  }
}
#Predict h = 20 steps from RW3: 
RW3.fc <- apply(RW3.select,2,holt, 20)
#Construct Portfolio for forecast
RW3.expect <- expret(RW3.select, RW3.topsharpe)
RW3.weights <- optimization(RW3.select, RW3.expect$`Expected Returns`)
RW3.fc <- as.data.frame(RW3.fc)
RW3.wrfc <- data.frame(matrix(0,ncol=30, nrow=20))
colnames(RW3.wrfc) <- RW3.names
for (i in 1:30){
  n <- 1 + (i-1)*5
  a <- RW3.fc[n]
  aweights <- RW3.weights[i]
  RW3.wrfc[,i] <- aweights*a
}
sum(meanret(RW3.wrfc)) #Total mean returns of the weighted prediction:  0.001454948
sum(volatility(RW3.wrfc)) #Total volatility of the weighted prediction: 0.0004421253
mean(sharper(RW3.wrfc)) #Mean sharpe ratio of the weighted prediction: -8.112547 (the sharpe ratio low, try to rebalance)
sharper(RW3.wrfc) #"MYF" and "NAN" and "MUI" asset had pretty large negative sharpe ratio
tail(sort.sharpe3, 33) #find the 31st and 32nd largest sharpe ratio "MYN","IEI","MHN" 
RW3.names[RW3.names == "MYY"] <- "MYN"
RW3.names[RW3.names == "NAN"] <- "IEI"
RW3.names[RW3.names == "MUI"] <- "MHN"
RW3.select <- as.data.frame(matrix(0,  ncol = 30, nrow = 252)) #Iterate previous steps
colnames(RW3.select) <- c(RW3.names)
for (i in 1:30){
  for (j in 1:3084){
    if (colnames(RW3)[j] == RW3.names[i]){
      RW3.select[,i] <- RW3[,j]
    }
  }
}
#Predict h = 20 steps from RW3: 
RW3.fc <- apply(RW3.select,2,holt, 20)
#Construct Portfolio for forecast
RW3.expect <- expret(RW3.select, RW3.topsharpe)
RW3.weights <- optimization(RW3.select, RW3.expect$`Expected Returns`)
RW3.fc <- as.data.frame(RW3.fc)
RW3.wrfc <- data.frame(matrix(0,ncol=30, nrow=20))
colnames(RW3.wrfc) <- RW3.names
for (i in 1:30){
  n <- 1 + (i-1)*5
  a <- RW3.fc[n]
  aweights <- RW3.weights[i]
  RW3.wrfc[,i] <- aweights*a
}
sum(meanret(RW3.wrfc)) #Total mean returns of the weighted prediction:   0.00149954 (slightly larger than before)
sum(volatility(RW3.wrfc)) #Total volatility of the weighted prediction: 0.0004099797(lower than before)
mean(sharper(RW3.wrfc)) #Mean sharpe ratio of the weighted prediction: -12.64658 (larger than before)
#However since both mean return and volatility has been improved, decide to use this approach
#The turnover rates = 0
N  <- 0
name <- c()
for (i in 1:30){
  for (j in 1:30){
    if (RW3.names[i] == RW2.names[j]){
      N <- N+1
      name <- append(name, RW2.names[j])
    }
  }
}
N #0 asset from rolling window 3 consistent with rolling window 2
#Sortino Ratio
RW3.rd <- RW3.wrfc[RW3.wrfc < 0] #downside returns
sd(RW3.rd) #standard deviation of the downside
RW3.sortino <- meanret(RW3.wrfc)/sd(RW3.rd) #risk free return = 0
colnames(RW3.sortino) <- c("Sortino Ratio")
mean(RW3.sortino) #mean sortino ratio of RW3 =  2.510409 #sortino ratio ok. selection ok!

#####################################
## Rolling window 4
#####################################
sort.sharpe4 <- RW4.sharp[order(RW4.sharp[,1]),]
RW4.topsharpe <- as.data.frame(tail(sort.sharpe4, 30))
# The top 30 assets
RW4.names <- rownames(RW4.topsharpe)
RW4.select <- as.data.frame(matrix(0,  ncol = 30, nrow = 252))
colnames(RW4.select) <- c(RW4.names)
for (i in 1:30){
  for (j in 1:3084){
    if (colnames(RW4)[j] == RW4.names[i]){
      RW4.select[,i] <- RW4[,j]
    }
  }
}

#Predict h = 20 steps from RW4: 
RW4.fc <- apply(RW4.select,2,holt, 20)
#Construct Portfolio for forecast
RW4.expect <- expret(RW4.select, RW4.topsharpe)
RW4.weights <- optimization(RW4.select, RW4.expect$`Expected Returns`)
RW4.fc <- as.data.frame(RW4.fc)
RW4.wrfc <- data.frame(matrix(0,ncol=30, nrow=20))
colnames(RW4.wrfc) <- RW4.names
for (i in 1:30){
  n <- 1 + (i-1)*5
  a <- RW4.fc[n]
  aweights <- RW4.weights[i]
  RW4.wrfc[,i] <- aweights*a
}
sum(meanret(RW4.wrfc)) #Total mean returns of the weighted prediction:  0.00110432
sum(volatility(RW4.wrfc)) #Total volatility of the weighted prediction:  0.0001987451
mean(sharper(RW4.wrfc)) #Mean sharpe ratio of the weighted prediction: 163.8253 (pretty large sharpe ratio)
#The turnover rates = 0
N  <- 0
name <- c()
for (i in 1:30){
  for (j in 1:30){
    if (RW4.names[i] == RW3.names[j]){
      N <- N+1
      name <- append(name, RW3.names[j])
    }
  }
}
N #0 asset from rolling window 4 consistent with rolling window 3

#Sortino Ratio
RW4.rd <- RW4.wrfc[RW4.wrfc < 0] #downside returns
sd(RW4.rd) #standard deviation of the downside
RW4.sortino <- meanret(RW4.wrfc)/sd(RW4.rd) #risk free return = 0
colnames(RW4.sortino) <- c("Sortino Ratio")
mean(RW4.sortino) #mean sortino ratio of RW4 = 0.4053427

#####################################
## Rolling window 5
#####################################
sort.sharpe5 <- RW5.sharp[order(RW5.sharp[,1]),]
RW5.topsharpe <- as.data.frame(tail(sort.sharpe5, 31))
# The top 30 assets
RW5.names <- rownames(RW5.topsharpe)[1:30]
RW5.select <- as.data.frame(matrix(0,  ncol = 30, nrow = 252))
colnames(RW5.select) <- c(RW5.names)
for (i in 1:30){
  for (j in 1:3084){
    if (colnames(RW5)[j] == RW5.names[i]){
      RW5.select[,i] <- RW5[,j]
    }
  }
}
#Predict h = 20 steps from RW5: 
RW5.fc <- apply(RW5.select, 2, holt, 20)
#Construct Portfolio for forecast
RW5.expect <- expret(RW5.select[1:30], RW5.topsharpe)
RW5.weights <- optimization(RW5.select[1:30], RW5.expect$`Expected Returns`[1:30])
RW5.fc <- as.data.frame(RW5.fc)
RW5.wrfc <- data.frame(matrix(0,ncol=30, nrow=20))
colnames(RW5.wrfc) <- RW5.names
for (i in 1:30){
  n <- 1 + (i-1)*5
  a <- RW5.fc[n]
  aweights <- RW5.weights[i]
  RW5.wrfc[,i] <- aweights*a
}
sum(meanret(RW5.wrfc)) #Total mean returns of the weighted prediction: 0.004212682
sum(volatility(RW5.wrfc)) #Total volatility of the weighted prediction:0.0002705865
mean(sharper(RW5.wrfc)) #Mean sharpe ratio of the weighted prediction: 821.0408

#The turnover rates = 0.1363655
N  <- 0
name <- c()
for (i in 1:30){
  for (j in 1:30){
    if (RW5.names[i] == RW4.names[j]){
      N <- N+1
      name <- append(name, RW4.names[j])
    }
  }
}
N #1 asset from rolling window 5 consistent with rolling window 4
r4 <- as.data.frame(RW4.weights)
rownames(r4) <- RW4.names
r5 <- as.data.frame(RW5.weights[1:30])
rownames(r5) <- RW5.names
w4 <- RW4.weights[14]
w5 <- RW5.weights[24]
diff <- abs(w5-w4)
sum(diff)/20 #trading dates = 20; turnover rates =  0.001557922

#Sortino Ratio
RW5.rd <- RW5.wrfc[RW5.wrfc < 0] #downside returns
sd(RW5.rd) #standard deviation of the downside
RW5.sortino <- meanret(RW5.wrfc)/sd(RW5.rd) #risk free return = 0
colnames(RW5.sortino) <- c("Sortino Ratio")
mean(RW5.sortino) #mean sortino ratio of RW5 = 2.618381

#####################################
## Rolling window 6
#####################################
sort.sharpe6 <- RW6.sharp[order(RW6.sharp[,1]),]
RW6.topsharpe <- as.data.frame(tail(sort.sharpe6, 30))
# The top 30 assets
RW6.names <- rownames(RW6.topsharpe)[1:30]
RW6.select <- as.data.frame(matrix(0,  ncol = 30, nrow = 252))
colnames(RW6.select) <- c(RW6.names)
for (i in 1:30){
  for (j in 1:3084){
    if (colnames(RW6)[j] == RW6.names[i]){
      RW6.select[,i] <- RW6[,j]
    }
  }
}

#Predict h = 20 steps from RW6: 
RW6.fc <- apply(RW6.select, 2, holt, 20)
#Construct Portfolio for forecast
RW6.expect <- expret(RW6.select, RW6.topsharpe)
RW6.weights <- optimization(RW6.select, RW6.expect$`Expected Returns`)
RW6.fc <- as.data.frame(RW6.fc)
RW6.wrfc <- data.frame(matrix(0,ncol=30, nrow=20))
colnames(RW6.wrfc) <- RW6.names
for (i in 1:30){
  n <- 1 + (i-1)*5
  a <- RW6.fc[n]
  aweights <- RW6.weights[i]
  RW6.wrfc[,i] <- aweights*a
}
sum(meanret(RW6.wrfc)) #Total mean returns of the weighted prediction:  -0.0006735862 (need to rebalance)
sum(volatility(RW6.wrfc)) #Total volatility of the weighted prediction: 0.0003622141
mean(sharper(RW6.wrfc)) #Mean sharpe ratio of the weighted prediction: -97.62595 (sharpe ratio pretty high)
sharper(RW6.wrfc)  #"JBSS", "CI", "XPH" assets had a slighly larger negative sharpe ratio
tail(sort.sharpe6, 33) #find the 31st, 32nd, 33rd largest sharpe ratio "ENSG","DLTR", "FXH"
RW6.names[RW6.names == "JBSS"] <- "ENSG"
RW6.names[RW6.names == "XPH"] <- "DLTR"
RW6.names[RW6.names == "CI"] <- "FXH"
RW6.select <- as.data.frame(matrix(0,  ncol = 30, nrow = 252)) #Iterate previous steps
colnames(RW6.select) <- c(RW6.names)
for (i in 1:30){
  for (j in 1:3084){
    if (colnames(RW6)[j] == RW6.names[i]){
      RW6.select[,i] <- RW6[,j]
    }
  }
}
#Predict h = 20 steps from new selected RW6 
RW6.fc <- apply(RW6.select, 2, holt, 20)
#Construct Portfolio for forecast
RW6.expect <- expret(RW6.select, RW6.topsharpe)
RW6.weights <- optimization(RW6.select, RW6.expect$`Expected Returns`)
RW6.fc <- as.data.frame(RW6.fc)
RW6.wrfc <- data.frame(matrix(0,ncol=30, nrow=20))
colnames(RW6.wrfc) <- RW6.names
for (i in 1:30){
  n <- 1 + (i-1)*5
  a <- RW6.fc[n]
  aweights <- RW6.weights[i]
  RW6.wrfc[,i] <- aweights*a
}
sum(meanret(RW6.wrfc)) #Total mean returns of the weighted prediction: -0.001086017(better, but still try to rebalance)
sum(volatility(RW6.wrfc)) #Total volatility of the weighted prediction:0.0004103654
mean(sharper(RW6.wrfc)) #Mean sharpe ratio of the weighted prediction: -104.2685
#(sharpe ratio pretty low,need to rebalance)
#Decide to select the top mean returns in the asset to balance the low mean returns:
sort.mean6 <- RW6.mean[order(RW6.mean[,1]),]
tail(sort.mean6,4)
#Replace the 4 lower sharpe ratio with 4 top mean returns assets
sort(sharper(RW6.wrfc),decreasing = TRUE)
# Replace "RYH","FOLD","NSP","SWKS" with "CVTI", "ZIOP", "ACHN", "FOLD"
RW6.names[RW6.names == "RYH"] <- "CVTI"
RW6.names[RW6.names == "FOLD"] <- "ZIOP"
RW6.names[RW6.names == "NSP"] <- "ACHN"
RW6.names[RW6.names == "SWKS"] <- "FOLD"
RW6.select <- as.data.frame(matrix(0,  ncol = 30, nrow = 252)) #Iterate previous steps
colnames(RW6.select) <- c(RW6.names)
for (i in 1:30){
  for (j in 1:3084){
    if (colnames(RW6)[j] == RW6.names[i]){
      RW6.select[,i] <- RW6[,j]
    }
  }
}
#Predict h = 20 steps from new selected RW6 
RW6.fc <- apply(RW6.select, 2, holt, 20)
#Construct Portfolio for forecast
RW6.expect <- expret(RW6.select, RW6.topsharpe)
RW6.weights <- optimization(RW6.select, RW6.expect$`Expected Returns`)
RW6.fc <- as.data.frame(RW6.fc)
RW6.wrfc <- data.frame(matrix(0,ncol=30, nrow=20))
colnames(RW6.wrfc) <- RW6.names
for (i in 1:30){
  n <- 1 + (i-1)*5
  a <- RW6.fc[n]
  aweights <- RW6.weights[i]
  RW6.wrfc[,i] <- aweights*a
}
sum(meanret(RW6.wrfc)) #Total mean returns of the weighted prediction: -0.001051365(better than before)
sum(volatility(RW6.wrfc)) #Total volatility of the weighted prediction: 0.0005005771(slightly more volatile)
mean(sharper(RW6.wrfc)) #Mean sharpe ratio of the weighted prediction: -59.66212(better than before)

#The turnover rates = 100%
N  <- 0
for (i in 1:30){
  for (j in 1:30){
    if (RW6.names[i] == RW5.names[j]){
      N <- N+1
    }
  }
}
N #0 asset from rolling window 7 consistent with rolling window 6

#Sortino Ratio
RW6.rd <- RW6.wrfc[RW6.wrfc < 0] #downside returns
sd(RW6.rd) #standard deviation of the downside
RW6.sortino <- meanret(RW6.wrfc)/sd(RW6.rd) #risk free return = 0
colnames(RW6.sortino) <- c("Sortino Ratio")
mean(RW6.sortino) #mean sortino ratio of RW6 = -0.1827989

#####################################
## Rolling window 7
#####################################
sort.sharpe7 <- RW7.sharp[order(RW7.sharp[,1]),]
RW7.topsharpe <- as.data.frame(tail(sort.sharpe7, 30))
# The top 30 assets
RW7.names <- rownames(RW7.topsharpe)[1:30]
RW7.select <- as.data.frame(matrix(0,  ncol = 30, nrow = 252))
colnames(RW7.select) <- c(RW7.names)
for (i in 1:30){
  for (j in 1:3084){
    if (colnames(RW7)[j] == RW7.names[i]){
      RW7.select[,i] <- RW7[,j]
    }
  }
}

#Predict h = 20 steps from RW5: 
RW7.fc <- apply(RW7.select, 2, holt, 20)
#Construct Portfolio for forecast
RW7.expect <- expret(RW7.select, RW7.topsharpe)
RW7.weights <- optimization(RW7.select, RW7.expect$`Expected Returns`)
RW7.fc <- as.data.frame(RW7.fc)
RW7.wrfc <- data.frame(matrix(0,ncol=30, nrow=20))
colnames(RW7.wrfc) <- RW7.names
for (i in 1:30){
  n <- 1 + (i-1)*5
  a <- RW7.fc[n]
  aweights <- RW7.weights[i]
  RW7.wrfc[,i] <- aweights*a
}
sum(meanret(RW7.wrfc)) #Total mean returns of the weighted prediction: 0.00235538 (feasible)
sum(volatility(RW7.wrfc)) #Total volatility of the weighted prediction: 0.0002323673
mean(sharper(RW7.wrfc)) #Mean sharpe ratio of the weighted prediction:  4.272914(feasible)
#The turnover rates = 0
N  <- 0
for (i in 1:30){
  for (j in 1:30){
    if (RW7.names[i] == RW6.names[j]){
      N <- N+1
    }
  }
}
N #0 asset from rolling window 7 consistent with rolling window 6
#Sortino Ratio
RW7.rd <- RW7.wrfc[RW7.wrfc < 0] #downside returns
sd(RW7.rd) #standard deviation of the downside
RW7.sortino <- meanret(RW7.wrfc)/sd(RW7.rd) #risk free return = 0
colnames(RW7.sortino) <- c("Sortino Ratio")
mean(RW7.sortino) #mean sortino ratio of RW7 =1.535287


#####################################
## Rolling window 8
#####################################
sort.sharpe8 <- RW8.sharp[order(RW8.sharp[,1]),]
RW8.topsharpe <- as.data.frame(tail(sort.sharpe8, 30))
# The top 30 assets
RW8.names <- rownames(RW8.topsharpe)[1:30]
RW8.select <- as.data.frame(matrix(0,ncol = 30, nrow = 252))
colnames(RW8.select) <- c(RW8.names)
for (i in 1:30){
  for (j in 1:3084){
    if (colnames(RW8)[j] == RW8.names[i]){
      RW8.select[,i] <- RW8[,j]
    }
  }
}

#Predict h = 20 steps from RW5: 
RW8.fc <- apply(RW8.select, 2, holt, 20)
#Construct Portfolio for forecast
RW8.expect <- expret(RW8.select, RW8.topsharpe)
RW8.weights <- optimization(RW8.select, RW8.expect$`Expected Returns`)
RW8.fc <- as.data.frame(RW8.fc)
RW8.wrfc <- data.frame(matrix(0,ncol=30, nrow=20))
colnames(RW8.wrfc) <- RW8.names
for (i in 1:30){
  n <- 1 + (i-1)*5
  a <- RW8.fc[n]
  aweights <- RW8.weights[i]
  RW8.wrfc[,i] <- aweights*a
}
sum(meanret(RW8.wrfc)) #Total mean returns of the weighted prediction:  0.0002735704(feasible)
sum(volatility(RW8.wrfc)) #Total volatility of the weighted prediction:  0.003938246
mean(sharper(RW8.wrfc)) #Mean sharpe ratio of the weighted prediction: 30.74635
#The turnover rates = 0.2119427
N  <- 0
name <- c()
for (i in 1:30){
  for (j in 1:30){
    if (RW8.names[i] == RW7.names[j]){
      N <- N+1
      name <- append(name, RW7.names[j])
    }
  }
}
N #1 asset from rolling window 8 consistent with rolling window 7
name
r7 <- as.data.frame(RW7.weights)
r8 <- as.data.frame(RW8.weights)
rownames(r7) <- RW7.names
rownames(r8) <- RW8.names
rownames(r7) == name #25
rownames(r8) == name
w7 <- r7[25,]
w8 <- r8[22,]
diff <- abs(w7-w8)
sum(diff)/20 #trading dates = 20; turnover rates = 0.006306668

#Sortino Ratio
RW8.rd <- RW8.wrfc[RW8.wrfc < 0] #downside returns
sd(RW8.rd) #standard deviation of the downside
RW8.sortino <- meanret(RW8.wrfc)/sd(RW8.rd) #risk free return = 0
colnames(RW8.sortino) <- c("Sortino Ratio")
mean(RW8.sortino) #mean sortino ratio of RW8 = 1.711


#####################################
## Rolling window 9
#####################################
sort.sharpe9 <- RW9.sharp[order(RW9.sharp[,1]),]
RW9.topsharpe <- as.data.frame(tail(sort.sharpe9, 30))
# The top 30 assets
RW9.names <- rownames(RW9.topsharpe)[1:30]
RW9.select <- as.data.frame(matrix(0,ncol = 30, nrow = 252))
colnames(RW9.select) <- c(RW9.names)
for (i in 1:30){
  for (j in 1:3084){
    if (colnames(RW9)[j] == RW9.names[i]){
      RW9.select[,i] <- RW9[,j]
    }
  }
}

#Predict h = 20 steps from RW5: 
RW9.fc <- apply(RW9.select, 2, holt, 20)
#Construct Portfolio for forecast
RW9.expect <- expret(RW9.select, RW9.topsharpe)
RW9.weights <- optimization(RW9.select, RW9.expect$`Expected Returns`)
RW9.fc <- as.data.frame(RW9.fc)
RW9.wrfc <- data.frame(matrix(0,ncol=30, nrow=20))
colnames(RW9.wrfc) <- RW9.names
for (i in 1:30){
  n <- 1 + (i-1)*5
  a <- RW9.fc[n]
  aweights <- RW9.weights[i]
  RW9.wrfc[,i] <- aweights*a
}
sum(meanret(RW9.wrfc)) #Total mean returns of the weighted prediction:  0.00412462(feasible)
sum(volatility(RW9.wrfc)) #Total volatility of the weighted prediction:  0.003185267
mean(sharper(RW9.wrfc)) #Mean sharpe ratio of the weighted prediction: 47.06231
#The turnover rates = 0
N  <- 0
name <- c()
for (i in 1:30){
  for (j in 1:30){
    if (RW9.names[i] == RW8.names[j]){
      N <- N+1
      name <- append(name, RW8.names[j])
    }
  }
}
N #0 asset from rolling window 9 consistent with rolling window 8

#Sortino Ratio
RW9.rd <- RW9.wrfc[RW9.wrfc < 0] #downside returns
sd(RW9.rd) #standard deviation of the downside
RW9.sortino <- meanret(RW9.wrfc)/sd(RW9.rd) #risk free return = 0
colnames(RW9.sortino) <- c("Sortino Ratio")
mean(RW9.sortino) #mean sortino ratio of RW9 = 0.8836287



#####################################
## Rolling window 10
#####################################
sort.sharpe10 <- RW10.sharp[order(RW10.sharp[,1]),]
RW10.topsharpe <- as.data.frame(tail(sort.sharpe10, 30))
# The top 30 assets
RW10.names <- rownames(RW10.topsharpe)[1:30]
RW10.select <- as.data.frame(matrix(0,ncol = 30, nrow = 252))
colnames(RW10.select) <- c(RW10.names)
for (i in 1:30){
  for (j in 1:3084){
    if (colnames(RW10)[j] == RW10.names[i]){
      RW10.select[,i] <- RW10[,j]
    }
  }
}

#Predict h = 20 steps from RW10: 
RW10.fc <- apply(RW10.select, 2, holt, 20)
#Construct Portfolio for forecast
RW10.expect <- expret(RW10.select, RW10.topsharpe)
RW10.weights <- optimization(RW10.select, RW10.expect$`Expected Returns`)
RW10.fc <- as.data.frame(RW10.fc)
RW10.wrfc <- data.frame(matrix(0,ncol=30, nrow=20))
colnames(RW10.wrfc) <- RW10.names
for (i in 1:30){
  n <- 1 + (i-1)*5
  a <- RW10.fc[n]
  aweights <- RW10.weights[i]
  RW10.wrfc[,i] <- aweights*a
}
sum(meanret(RW10.wrfc)) #Total mean returns of the weighted prediction: 0.0001618733 (feasible)
sum(volatility(RW10.wrfc)) #Total volatility of the weighted prediction:  2.554445e-05
mean(sharper(RW10.wrfc)) #Mean sharpe ratio of the weighted prediction: 18.92652

#The turnover rates = 0.09392416
N  <- 0
name <- c()
for (i in 1:30){
  for (j in 1:30){
    if (RW10.names[i] == RW9.names[j]){
      N <- N+1
      name <- append(name, RW9.names[j])
    }
  }
}
N #2 asset from rolling window 10 consistent with rolling window 9 "SHV", "BIL"
r10 <- as.data.frame(RW10.weights)
r9 <- as.data.frame(RW9.weights)
rownames(r10) <- RW10.names
rownames(r9) <- RW9.names
w91 <- RW9.weights[27]
w92 <- RW9.weights[30]
w101 <- RW10.weights[30]
w102 <- RW10.weights[29]
diff1 <- abs(w91-w101)
diff2 <- abs(w92-w102)
sum(diff1,diff2)/20 #trading dates = 20; turnover rates = 0.2686636

#Sortino Ratio
RW10.rd <- RW10.wrfc[RW10.wrfc < 0] #downside returns
sd(RW10.rd) #standard deviation of the downside
RW10.sortino <- meanret(RW10.wrfc)/sd(RW10.rd) #risk free return = 0
colnames(RW10.sortino) <- c("Sortino Ratio")
mean(RW10.sortino) #mean sortino ratio of RW10 = 0.139486



#(5) Comparisons to two benchmarks
### Cumulative returns from each rolling window
cumulativer <- function(logret){
  n <- length(logret)
  cr <- c()
  cr[1] <- 1
  for (i in 2:n){
    cr[i] <- cr[i-1]*(1+logret[i])
  }
  return(cr)
}

w.portfolio <- function(w, r){
  n <- nrow(r)
  wr <- data.frame(matrix(0,ncol=30, nrow=252))
  t <- c()
  for (i in 1:n){
    for (j in 1:30){
      wr[i,j] <- w[j]*r[i,j]
    }
    t[i] <- sum(wr[i,])
  }
  return(t)
}
R1.portfolio <- w.portfolio(w = RW1.weights, r = RW1.select)
R2.portfolio <- w.portfolio(w = RW2.weights, r = RW2.select)
R3.portfolio <- w.portfolio(w = RW3.weights, r = RW3.select)
R4.portfolio <- w.portfolio(w = RW4.weights, r = RW4.select)
R5.portfolio <- w.portfolio(w = RW5.weights, r = RW5.select)
R6.portfolio <- w.portfolio(w = RW6.weights, r = RW6.select)
R7.portfolio <- w.portfolio(w = RW7.weights, r = RW7.select)
R8.portfolio <- w.portfolio(w = RW8.weights, r = RW8.select)
R9.portfolio <- w.portfolio(w = RW9.weights, r = RW9.select)
R10.portfolio <- w.portfolio(w = RW10.weights, r = RW10.select)
RW1.cumopt <- cumulativer(R1.portfolio)
RW2.cumopt <- cumulativer(R2.portfolio)
RW3.cumopt <- cumulativer(R3.portfolio)
RW4.cumopt <- cumulativer(R4.portfolio)
RW5.cumopt <- cumulativer(R5.portfolio)
RW6.cumopt <- cumulativer(R6.portfolio)
RW7.cumopt <- cumulativer(R7.portfolio)
RW8.cumopt <- cumulativer(R8.portfolio)
RW9.cumopt <- cumulativer(R9.portfolio)
RW10.cumopt <- cumulativer(R10.portfolio)
cumopt <- c(RW1.cumopt,RW2.cumopt,RW3.cumopt,RW4.cumopt,RW5.cumopt, 
           RW6.cumopt,RW7.cumopt,RW8.cumopt,RW9.cumopt,RW10.cumopt)

equallyw.portfolio <- function(r){
  n <- nrow(r)
  t <- c()
  for (i in 1:n){
    t[i] <- sum(r[i,]/30)
  }
  return(t)
}
R1.portequal <- equallyw.portfolio(RW1.select) 
R2.portequal <- equallyw.portfolio(RW2.select) 
R3.portequal <- equallyw.portfolio(RW3.select) 
R4.portequal <- equallyw.portfolio(RW4.select) 
R5.portequal <- equallyw.portfolio(RW5.select) 
R6.portequal <- equallyw.portfolio(RW6.select) 
R7.portequal <- equallyw.portfolio(RW7.select) 
R8.portequal <- equallyw.portfolio(RW8.select) 
R9.portequal <- equallyw.portfolio(RW9.select) 
R10.portequal <- equallyw.portfolio(RW10.select) 
RW1.cumeq <- cumulativer(R1.portequal) 
RW2.cumeq <- cumulativer(R2.portequal) 
RW3.cumeq <- cumulativer(R3.portequal) 
RW4.cumeq <- cumulativer(R4.portequal) 
RW5.cumeq <- cumulativer(R5.portequal) 
RW6.cumeq <- cumulativer(R6.portequal) 
RW7.cumeq <- cumulativer(R7.portequal) 
RW8.cumeq <- cumulativer(R8.portequal) 
RW9.cumeq <- cumulativer(R9.portequal) 
RW10.cumeq <- cumulativer(R10.portequal) 
cumequal <- c(RW1.cumeq,RW2.cumeq,RW3.cumeq,RW4.cumeq,RW5.cumeq, 
              RW6.cumeq,RW7.cumeq,RW8.cumeq,RW9.cumeq,RW10.cumeq)

#s&p500
RW1.cumsp <- cumulativer(exc_sp1) 
RW2.cumsp <- cumulativer(exc_sp2)
RW3.cumsp <- cumulativer(exc_sp3)
RW4.cumsp <- cumulativer(exc_sp4)
RW5.cumsp <- cumulativer(exc_sp5)
RW6.cumsp <- cumulativer(exc_sp6)
RW7.cumsp <- cumulativer(exc_sp7)
RW8.cumsp <- cumulativer(exc_sp8)
RW9.cumsp <- cumulativer(exc_sp9)
RW10.cumsp <- cumulativer(exc_sp10)
cumsp <- c(RW1.cumsp,RW2.cumsp,RW3.cumsp,RW4.cumsp,RW5.cumsp, 
           RW6.cumsp,RW7.cumsp,RW8.cumsp,RW9.cumsp,RW10.cumsp)

library(ggplot2)
AAL <- read.csv("~/Desktop/full_history/cleandata/AAL.csv")
dates <- c(rep(AAL$date,3))
cumr <- c(cumopt, cumequal, cumsp)
type <- c(rep("optimal",2520),rep("equal",2520),rep("s&p500",2520))
plot.data <- data.frame(years, cumr, type)

ggplot(data = plot.data, aes(x = dates, y=cumr))+
  geom_line(aes(color=type))+
  geom_point(aes(color=type)) +
  theme(axis.text.x = element_text(angle=45,hjust=1))+
  labs(title="Cumulative Returns")+
  xlab("Years")+
  ylab("Cumulative Returns")


#Mean return
m1 <- mean(R1.portfolio)
m2 <- mean(R2.portfolio)
m3 <- mean(R3.portfolio)
m4 <- mean(R4.portfolio)
m5 <- mean(R5.portfolio)
m6 <- mean(R6.portfolio)
m7 <- mean(R7.portfolio)
m8 <- mean(R8.portfolio)
m9 <- mean(R9.portfolio)
m10 <- mean(R10.portfolio)
m.total <- mean(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)
m1.e <- mean(R1.portequal)
m2.e <- mean(R2.portequal)
m3.e <- mean(R3.portequal)
m4.e <- mean(R4.portequal)
m5.e <- mean(R5.portequal)
m6.e <- mean(R6.portequal)
m7.e <- mean(R7.portequal)
m8.e <- mean(R8.portequal)
m9.e <- mean(R9.portequal)
m10.e <- mean(R10.portequal)
m.total.e <- mean(m1.e,m2.e,m3.e,m4.e,m5.e,m6.e,m7.e,m8.e,m9.e,m10.e)
m1.sp <- mean(exc_sp1)
m2.sp <- mean(exc_sp2)
m3.sp <- mean(exc_sp3)
m4.sp <- mean(exc_sp4)
m5.sp <- mean(exc_sp5)
m6.sp <- mean(exc_sp6)
m7.sp <- mean(exc_sp7)
m8.sp <- mean(exc_sp8)
m9.sp <- mean(exc_sp9)
m10.sp <- mean(exc_sp10)
m.total.sp <- mean(m1.sp,m2.sp,m3.sp,m4.sp,m5.sp,m6.sp,m7.sp,m8.sp,m9.sp,m10.sp)

#Volatility
v1 <- sd(R1.portfolio)
v2 <- sd(R2.portfolio)
v3 <- sd(R3.portfolio)
v4 <- sd(R4.portfolio)
v5 <- sd(R5.portfolio)
v6 <- sd(R6.portfolio)
v7 <- sd(R7.portfolio)
v8 <- sd(R8.portfolio)
v9 <- sd(R9.portfolio)
v10 <- sd(R10.portfolio)
v.total <- mean(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10)
v1.e <- sd(R1.portequal)
v2.e <- sd(R2.portequal)
v3.e <- sd(R3.portequal)
v4.e <- sd(R4.portequal)
v5.e <- sd(R5.portequal)
v6.e <- sd(R6.portequal)
v7.e <- sd(R7.portequal)
v8.e <- sd(R8.portequal)
v9.e <- sd(R9.portequal)
v10.e <- sd(R10.portequal)
v.total.e <- mean(v1.e,v2.e,v3.e,v4.e,v5.e,v6.e,v7.e,v8.e,v9.e,v10.e)
v1.sp <- sd(exc_sp1)
v2.sp <- sd(exc_sp2)
v3.sp <- sd(exc_sp3)
v4.sp <- sd(exc_sp4)
v5.sp <- sd(exc_sp5)
v6.sp <- sd(exc_sp6)
v7.sp <- sd(exc_sp7)
v8.sp <- sd(exc_sp8)
v9.sp <- sd(exc_sp9)
v10.sp <- sd(exc_sp10)
v.total.sp <- mean(v1.sp,v2.sp,v3.sp,v4.sp,v5.sp,v6.sp,v7.sp,v8.sp,v9.sp,v10.sp)

#Sharpe Ratio (assume risk free rate = 0)
s1 <- m1/v1
s2 <- m2/v2
s3 <- m3/v3
s4 <- m4/v4
s5 <- m5/v5
s6 <- m6/v6
s7 <- m7/v7
s8 <- m8/v8
s9 <- m9/v9
s10 <- m10/v10
spr.total <- mean(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10)
s1.e <- m1.e/v1.e
s2.e <- m2.e/v2
s3.e <- m3.e/v3.e
s4.e <- m4.e/v4.e
s5.e <- m5.e/v5.e
s6.e <- m6.e/v6.e
s7.e <- m7.e/v7.e
s8.e <- m8.e/v8.e
s9.e <- m9.e/v9.e
s10.e <- m10.e/v10.e
spr.total.e <- mean(s1.e,s2.e,s3.e,s4.e,s5.e,s6.e,s7.e,s8.e,s9.e,s10.e)
s1.sp <- m1.sp/v1.sp
s2.sp <- m2.sp/v2.sp
s3.sp <- m3.sp/v3.sp
s4.sp <- m4.sp/v4.sp
s5.sp <- m5.sp/v5.sp
s6.sp <- m6.sp/v6.sp
s7.sp <- m7.sp/v7.sp
s8.sp <- m8.sp/v8.sp
s9.sp <- m9.sp/v9.sp
s10.sp <- m10.sp/v10.sp
spr.total.sp <- mean(s1.sp,s2.sp,s3.sp,s4.sp,s5.sp,s6.sp,s7.sp,s8.sp,s9.sp,s10.sp)

#Sortino Ratio (assume risk free rate = 0)
st1 <- m1/sd(R1.portfolio[R1.portfolio < 0])
st2 <- m2/sd(R2.portfolio[R2.portfolio < 0])
st3 <- m3/sd(R3.portfolio[R3.portfolio < 0])
st4 <- m4/sd(R4.portfolio[R4.portfolio < 0])
st5 <- m5/sd(R5.portfolio[R5.portfolio < 0])
st6 <- m6/sd(R6.portfolio[R6.portfolio < 0])
st7 <- m7/sd(R7.portfolio[R7.portfolio < 0])
st8 <- m8/sd(R8.portfolio[R8.portfolio < 0])
st9 <- m9/sd(R9.portfolio[R9.portfolio < 0])
st10 <- m10/sd(R10.portfolio[R10.portfolio < 0])
str.total <- mean(st1,st2,st3,st4,st5,st6,st7,st8,st9,st10)
st1.e <- m1.e/sd(R1.portequal[R1.portequal < 0])
st2.e <- m2.e/sd(R2.portequal[R2.portequal < 0])
st3.e <- m3.e/sd(R3.portequal[R3.portequal < 0])
st4.e <- m4.e/sd(R4.portequal[R4.portequal < 0])
st5.e <- m5.e/sd(R5.portequal[R5.portequal < 0])
st6.e <- m6.e/sd(R6.portequal[R6.portequal < 0])
st7.e <- m7.e/sd(R7.portequal[R7.portequal < 0])
st8.e <- m8.e/sd(R8.portequal[R8.portequal < 0])
st9.e <- m9.e/sd(R9.portequal[R9.portequal < 0])
st10.e <- m10.e/sd(R10.portequal[R10.portequal< 0])
str.total.e <- mean(st1.e,st2.e,st3.e,st4.e,st5.e,st6.e,st7.e,st8.e,st9.e,st10.e)
st1.sp <- m1.sp/sd(exc_sp1[exc_sp1 < 0])
st2.sp <- m2.sp/sd(exc_sp2[exc_sp2 < 0])
st3.sp <- m3.sp/sd(exc_sp3[exc_sp3 < 0])
st4.sp <- m4.sp/sd(exc_sp4[exc_sp4 < 0])
st5.sp <- m5.sp/sd(exc_sp5[exc_sp5 < 0])
st6.sp <- m6.sp/sd(exc_sp6[exc_sp6 < 0])
st7.sp <- m7.sp/sd(exc_sp7[exc_sp7 < 0])
st8.sp <- m8.sp/sd(exc_sp8[exc_sp8 < 0])
st9.sp <- m9.sp/sd(exc_sp9[exc_sp9 < 0])
st10.sp <- m10.sp/sd(exc_sp10[exc_sp10 < 0])
str.total.sp <- mean(st1.sp,st2.sp,st3.sp,st4.sp,st5.sp,st6.sp,st7.sp,st8.sp,st9.sp,st10.sp)

#Maximum drawdown
library(PerformanceAnalytics)
mdd1 <- maxDrawdown(R1.portfolio)
mdd2 <- maxDrawdown(R2.portfolio)
mdd3 <- maxDrawdown(R3.portfolio)
mdd4 <- maxDrawdown(R4.portfolio)
mdd5 <- maxDrawdown(R5.portfolio)
mdd6 <- maxDrawdown(R6.portfolio)
mdd7 <- maxDrawdown(R7.portfolio)
mdd8 <- maxDrawdown(R8.portfolio)
mdd9 <- maxDrawdown(R9.portfolio)
mdd10 <- maxDrawdown(R10.portfolio)
mdd.total <- mean(mdd1,mdd2,mdd3,mdd4,mdd5,mdd6,mdd7,mdd8,mdd9,mdd10)

mdd1.e <- maxDrawdown(R1.portequal)
mdd2.e <- maxDrawdown(R2.portequal)
mdd3.e <- maxDrawdown(R3.portequal)
mdd4.e <- maxDrawdown(R4.portequal)
mdd5.e <- maxDrawdown(R5.portequal)
mdd6.e <- maxDrawdown(R6.portequal)
mdd7.e <- maxDrawdown(R7.portequal)
mdd8.e <- maxDrawdown(R8.portequal)
mdd9.e <- maxDrawdown(R9.portequal)
mdd10.e <- maxDrawdown(R10.portequal)
mdd.total.e <- mean(mdd1.e,mdd2.e,mdd3.e,mdd4.e,mdd5.e,mdd6.e,mdd7.e,mdd8.e,mdd9.e,mdd10.e)

mdd1.sp <- maxDrawdown(exc_sp1)
mdd2.sp <- maxDrawdown(exc_sp2)
mdd3.sp <- maxDrawdown(exc_sp3)
mdd4.sp <- maxDrawdown(exc_sp4)
mdd5.sp <- maxDrawdown(exc_sp5)
mdd6.sp <- maxDrawdown(exc_sp6)
mdd7.sp <- maxDrawdown(exc_sp7)
mdd8.sp <- maxDrawdown(exc_sp8)
mdd9.sp <- maxDrawdown(exc_sp9)
mdd10.sp <- maxDrawdown(exc_sp10)
mdd.total.sp  <- mean(mdd1.sp ,mdd2.sp ,mdd3.sp ,mdd4.sp ,mdd5.sp ,
                      mdd6.sp ,mdd7.sp ,mdd8.sp ,mdd9.sp ,mdd10.sp)

#Turnover: incorporated in the rolling window procedre above
#Rolling window statistics and graph
a1 <- sum(meanret(RW1.wrfc)) #Total mean returns of the weighted prediction: 0.002709289
b1 <- sum(volatility(RW1.wrfc)) #Total volatility of the weighted prediction:  0.0002452429 #volatility increases a lot
c1 <- mean(sharper(RW1.wrfc)) #Mean sharpe ratio of the weighted prediction: 23.86187 (very good sharpe ratio)
d1 <- mean(RW1.sortino) #mean so0rtino ratio of RW1 = 1.556431
#No rebalanced
a2 <- sum(meanret(RW2.wrfc)) #Total mean returns of the weighted prediction: 0.006055987
b2 <- sum(volatility(RW2.wrfc)) #Total volatility of the weighted prediction: 0.0009049389
c2 <- mean(sharper(RW2.wrfc)) #Mean sharpe ratio of the weighted prediction:  12.53561 (very good)
d2 <- mean(RW2.sortino) #mean sortino ratio of RW2 = 1.125356
#no rebalanced

a3 <- sum(meanret(RW3.wrfc)) #Total mean returns of the weighted prediction:   0.00149954 (slightly larger than before)
b3 <- sum(volatility(RW3.wrfc)) #Total volatility of the weighted prediction: 0.0004099797(lower than before)
c3 <- mean(sharper(RW3.wrfc)) #Mean sharpe ratio of the weighted prediction: -12.64658 (larger than before)
d3 <- mean(RW3.sortino) #mean sortino ratio of RW3 =  2.510409 #sortino ratio ok. selection ok!
#Rebalanced once

a4 <- sum(meanret(RW4.wrfc)) #Total mean returns of the weighted prediction:  0.00110432
b4 <- sum(volatility(RW4.wrfc)) #Total volatility of the weighted prediction:  0.0001987451
c4 <- mean(sharper(RW4.wrfc)) #Mean sharpe ratio of the weighted prediction: 163.8253 (pretty large sharpe ratio)
#No Rebalanced
d4 <- mean(RW4.sortino) #mean sortino ratio of RW4 = 0.4053427

a5 <- sum(meanret(RW5.wrfc)) #Total mean returns of the weighted prediction: 0.002934648
b5 <-sum(volatility(RW5.wrfc)) #Total volatility of the weighted prediction: 0.0002190711
c5 <-mean(sharper(RW5.wrfc)) #Mean sharpe ratio of the weighted prediction: 814.5448 (sharpe ratio pretty good, no need to rebalance)
#No rebalanced
d5 <-mean(RW5.sortino) #mean sortino ratio of RW5 = 1.833961

a6 <- sum(meanret(RW6.wrfc)) #Total mean returns of the weighted prediction: 0.0002759177(better than before)
b6 <- sum(volatility(RW6.wrfc)) #Total volatility of the weighted prediction: 6.609793e-05(slightly more volatile)
c6 <- mean(sharper(RW6.wrfc)) #Mean sharpe ratio of the weighted prediction: 52.85502(better than before)
#Rebalanced twice
d6 <- mean(RW6.sortino) #mean sortino ratio of RW6 = 1.14776

a7 <- sum(meanret(RW7.wrfc)) #Total mean returns of the weighted prediction:  0.00299723 (feasible)
b7 <- sum(volatility(RW7.wrfc)) #Total volatility of the weighted prediction: 0.0004100227
c7 <- mean(sharper(RW7.wrfc)) #Mean sharpe ratio of the weighted prediction: 21.01576(feasible)
#No rebalanced
d7 <- mean(RW7.sortino) #mean sortino ratio of RW7 = 0.8066066

a8 <- sum(meanret(RW8.wrfc)) #Total mean returns of the weighted prediction:  0.006293473 (feasible)
b8 <- sum(volatility(RW8.wrfc)) #Total volatility of the weighted prediction:  0.0004647535
c8 <- mean(sharper(RW8.wrfc)) #Mean sharpe ratio of the weighted prediction: 31.60787
#No rebalanced
d8 <- mean(RW8.sortino) #mean sortino ratio of RW8 = 0.9494773

a9 <- sum(meanret(RW9.wrfc)) #Total mean returns of the weighted prediction:  0.00412462(feasible)
b9 <- sum(volatility(RW9.wrfc)) #Total volatility of the weighted prediction:  0.003185267
c9 <- mean(sharper(RW9.wrfc)) #Mean sharpe ratio of the weighted prediction: 1.248712 (good)
#No Rebalanced
d9 <- mean(RW9.sortino) #mean sortino ratio of RW9 = 0.8836287

a10 <- sum(meanret(RW10.wrfc)) #Total mean returns of the weighted prediction: 0.0001618733 (feasible)
b10 <- sum(volatility(RW10.wrfc)) #Total volatility of the weighted prediction:  2.554445e-05
c10 <- mean(sharper(RW10.wrfc)) #Mean sharpe ratio of the weighted prediction: 18.92652
#No rebalanced
d10 <- mean(RW10.sortino) #mean sortino ratio of RW10 = 0.2385857

fc.mean <- c(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
mean.true <- c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)
fc.vola <- c(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10)
vola.true <- c(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10)
fc.sharp <- c(c1, c2, c3, c4, c5, c6, c7, c8, c8, c10)
sharp.true <- c(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10)
fc.sort <- c(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10)
sort.true <- c(st1, st2, st3, st4, st5, st6, st7, st8, st9, st10)

mean <- c(fc.mean, mean.true)
vola <- c(fc.vola, vola.true)
sharp <- c(fc.sharp,sharp.true)
sort <- c(fc.sort,sort.true)
#type <- c("fc1","fc2","fc3","fc4","fc5","fc6","fc7","fc8", "fc9","fc10" ,"tr1", "tr2", "tr3","tr4","tr5", "tr6", "tr7", "tr8", "tr9", "tr10")
type <- c(rep("forecast",10),rep("true", 10))
rw <- rep(c("RW1","RW2","RW3","RW4","RW5","RW6","RW7","RW8","RW9","RW10"),2)
rw.plot <- data.frame(rw, type, mean, vola, sharp, sort)
par(mfrow = c(2,2))
ggplot(data = rw.plot, aes(x=rw,y=mean, color = type,group=type))+
  geom_line(se=FALSE)+
  geom_point() +
  theme(axis.text.x = element_text(angle=45,hjust=1))+
  labs(title="Mean Returns")+
  xlab("Rollwing Window")+
  ylab("Mean Returns")

ggplot(data = rw.plot, aes(x=rw,y=vola, color = type,group=type))+
  geom_line(se=FALSE)+
  geom_point() +
  theme(axis.text.x = element_text(angle=45,hjust=1))+
  labs(title="Volatility")+
  xlab("Rollwing Window")+
  ylab("Volatility")

ggplot(data = rw.plot, aes(x=rw,y=sharp,color = type,group=type))+
  geom_line(se=FALSE)+
  geom_point() +
  theme(axis.text.x = element_text(angle=45,hjust=1))+
  labs(title="Sharpe Ratio")+
  xlab("Rollwing Window")+
  ylab("Sharpe Ratio")

ggplot(data = rw.plot, aes(x=rw,y=sort,color = type,group=type))+
  geom_line(se=FALSE)+
  geom_point() +
  theme(axis.text.x = element_text(angle=45,hjust=1))+
  labs(title="Sortino Ratio")+
  xlab("Rollwing Window")+
  ylab("Sortino Ratio")

turnover
