# Final Project STA640
# simulation replication for STAR
library(haven)
library(tidyverse)

# load data
star <- read.table("~/Desktop/SPRING2022/STA640/final project/replication files/star/star.raw", quote="\"", comment.char="")

# function: mregbygroup.m


# function: yhatf.m


# Simulation Star
N <- nrow(star)
N0 <- nrow(star[star[,2] == 0,])
N1 <- N-N0

Y0 <- star[star[,2] == 0, 1]
k = ncol(star[star[,2] == 0,])
lm1 <- lm(V1 ~.-V2, data = star %>% filter(V2 ==0))
beta <- lm1$coefficients
beta[64] <- 0
Uhat <- lm1$residuals
sigma <- sqrt((t(Uhat)%*%Uhat)/(N0-k))

X <- cbind(rep(1, N), star[, 3:83])
w <- c(rep(0,N0), rep(1,N1))

ps <- 1/2

R = 10000
SSR = 100
est = matrix(0,nrow = R, ncol = 6)
estl = matrix(0,nrow = R, ncol = 6)
ests = matrix(0,nrow = R, ncol = 6)
estss = matrix(0,nrow = R, ncol = 6)
estu = matrix(0,nrow = R, ncol = 6)

# loop
for(r in 1:R){
  print(r)
  I <- ceiling(N * c(runif(N)))
  x = X[I,]
  u = as.vector(sigma)*(c(rnorm(N)))
  y = as.matrix(x) %*% as.numeric(beta) + u
}


