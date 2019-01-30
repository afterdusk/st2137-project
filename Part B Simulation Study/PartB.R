# Functions for the 5 estimators
T1 <- function(x) {
  return(sd(x))
}

T2 <- function(x) {
  return(mad(x))
}


T3 <- function(x) {
  return(IQR(x) / 1.34898)
}

T4 <- function(x) {
  n <- length(x)
  sum <- 0
  for (i in 1:(n-1)){
    for (j in (i+1):n) {
      sum <- sum + abs(x[i]-x[j])
    }
  }
  g <- sum/choose(n,2)
  return(sqrt(pi)/2 * g)
}

T5 <- function(x) {
  n <- length(x)
  mi <- numeric(n)
  mj <- numeric(n)
  for (i in 1:n) {
    for (j in 1:n) {
     mj[j] <- abs(x[i]-x[j])
    }
    mi[i] <- median(mj)
  }
  return(1.1926*median(mi))
}

# Distributions
#   create distributions matrix, which stores our generate functions
#   and "true" statistics
distributions <- data.frame(matrix(ncol=4,nrow=0),
                            stringsAsFactors = FALSE)
colnames(distributions) <- c("Distribution Name","Function Name", "True Mean", "True Standard Deviation")
#   normal(0,1)
simnorm <- function(n) {
  x <- rnorm(n,0,1) # mean=0,sd=1
  return(x)
}
distributions[nrow(distributions)+1,] <- list("normal(0,1)","simnorm",0,1)
#   t(3)
simt <- function(n) {
  x <- rt(n,3) # df=3
  return(x)
}
distributions[nrow(distributions)+1,] <- list("t(3)","simt",0,sqrt(3/(3-2)))
#   chisquare(3)
simchisq <- function(n) {
  x <- rchisq(n,3) # df=3
  return(x)
}
distributions[nrow(distributions)+1,] <- list("chisquare(3)","simchisq",3,sqrt(2 * 3))
#   exponential(1)
simexp <- function(n) {
  x <- rexp(n, 1) # rate=1
  return(x)
}
distributions[nrow(distributions)+1,] <- list("exponential(1)","simexp",1/1,sqrt(1/(1^2)))
#   beta(0.5, 0.5)
simbeta <- function(n) {
  x <- rbeta(n,0.5,0.5) # shape1=0.5,shape2=0.5
}
distributions[nrow(distributions)+1,] <- list("beta(0.5, 0.5)", 
                                              "simbeta",
                                              0.5/(0.5+0.5),
                                              sqrt(0.5*0.5/(((0.5+0.5)^2)*(0.5+0.5+1))))
#   format and print distributions
distributions["True Mean"] <- round(distributions["True Mean"], 4)
distributions["True Standard Deviation"] <- round(distributions["True Standard Deviation"],4)
cat("Distributions Tested Against\n")
print(distributions)

ns <- 1000 # total number of sims
# Samples (selected from 5, 10, 20, 30, 50, 100, 200)
samples <- c(5, 30, 200)

for (n in samples) {
  # Simulations
  cat(rep("=", 40), "\n")
  cat("Sample Size: ", n, "\n")
  for (i in 1:nrow(distributions)) { # for all distributions
    dist <- distributions[i,"Function Name"] # fn to generate current distribution
    t1 <- numeric(ns)
    t2 <- numeric(ns)
    t3 <- numeric(ns)
    t4 <- numeric(ns)
    t5 <- numeric(ns)
    for (j in 1:ns) { # for each simulation
      x <- get(dist)(n) # generate distribution samples with function
      t1[j] <- T1(x) # get estimates
      t2[j] <- T2(x)
      t3[j] <- T3(x)
      t4[j] <- T4(x)
      t5[j] <- T5(x)
    }
    # Get stats of sims
    #   mean
    simumean <- apply(cbind(t1, t2, t3, t4, t5),2,mean)
    #   bias
    simubias <- simumean - distributions[i,'True Standard Deviation']
    #   standard deviation
    simusd <- apply(cbind(t1, t2, t3, t4, t5),2,sd)
    #   mean squared error (not yet done)
    simumse <- simubias^2 + simusd^2
    
    # Print stats
    ests <- c("T1", "T2", "T3", "T4", "T5")
    stats <- c("True Value","MC Mean", "MC Bias", "MC Std Deviation", "MC MSE")
    data <- rbind(distributions[i,"True Standard Deviation"], simumean, simubias, simusd, simumse)
    dimnames(data) <- list(stats,ests)
    cat(rep("-", 40), "\n")
    cat(distributions[i,"Distribution Name"],"\n")
    print(round(data, 4))
  }
  cat(rep("=", 40), "\n", "\n")
}