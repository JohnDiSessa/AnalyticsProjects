library(readxl)

options(scipen = 100)
# Data Asset Values #
Markowitz <- read.csv("ALY6050_MOD6Project_DiSessaJ.csv", 
                      header = TRUE, sep = ",")
Markowitz
Markowitz <- as.matrix(Markowitz[,2:21])
dim(Markowitz)
Markowitz

# Calculate Returns #
R <- matrix(nrow = dim(Markowitz)[1]-1,ncol = 20)
dim(R)

for (j in 1:20) {
  for (i in 1:251) {
    R[i,j] <- (Markowitz[i+1,j] - Markowitz[i,j]) / Markowitz[i,j]
  }
}

MU <- vector(length=20)
for (i in 1:20) {
  MU[i] <- mean(R[,i])
}

# Matrix of Quadratic Programming #
Q <- 2*cov(R)
typeof(Q)
class(Q)

C <- rep(0,20)

A <- matrix(nrow = 22,ncol = 20)
A[1,] <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
A[2,] <- MU
A[3,] <- c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
A[4,] <- c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
A[5,] <- c(0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
A[6,] <- c(0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
A[7,] <- c(0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
A[8,] <- c(0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
A[9,] <- c(0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0)
A[10,] <- c(0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0)
A[11,] <- c(0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0)
A[12,] <- c(0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0)
A[13,] <- c(0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0)
A[14,] <- c(0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0)
A[15,] <- c(0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0)
A[16,] <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0)
A[17,] <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0)
A[18,] <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0)
A[19,] <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0)
A[20,] <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0)
A[21,] <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0)
A[22,] <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)

dim(A)

# Baseline Return #

muB <- 1/251

# vector b of constraint right hand sides #

b <- c(1,muB,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
length(b)

library(quadprog)
QP <- solve.QP(Q,-C,t(A),b,meq=1)
QP$solution
QP$value


Names <- c("A","AAPL","AMC","BABA","C","D","DIS","DOCU","F","FIZZ","HCYAX",
           "HON","IBM","K","KO","LABD","LABU","LCID","NVDA","PYPL")
DF1 <- data.frame("Asset"=Names,
                  "Optimal Allocations"=paste(100*round(QP$solution,4),"%"))
DF1

# Optimal Return from above solution #
paste(100*round(251*t(MU)%*%QP$solution,6),"%")
