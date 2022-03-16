source("R2BootTest.R")

library(readxl)
Quality <- read_excel("Quality.xls")


# ASL estimation from observed data

X <- Quality$X

Ym <- Quality$Ym
gYl <- log(Quality$Yl)
hYr <- log(Quality$Yr)

quality.res <- R2BootTest(X = X, Ym = Ym, gYl = gYl, hYr = hYr, m = 10000, R = 1000, type = "real") #Tn = 60.43426
quality.res


# Simulation experiment

n <- 200

X <- rnorm(n, 0, 1)

Ym <- rnorm(n, 0, 1)
gYl <- rnorm(n, 0, 1)
hYr <- rnorm(n, 0, 1)

sim.res <- R2BootTest(X = X, Ym = Ym, gYl = gYl, hYr = hYr, m = 10000, R = 1000, alpha = 0.05, type = "simul")
sim.res


# Test's power estimation

n <- 100

nr.hn <- 10 # number of Pitman's hypotesis
delta <- seq(from = 0, to = 2, length.out = nr.hn)
an <- (n)^0.5 * delta # Pitman's hypotesis generation

X <- rnorm(n, 0, 1)

for (i in 1:nr.hn) {
  
  Ym <- X*an[i] + rnorm(n, 0, 1)
  gYl <- X*an[i] + rnorm(n, 0, 1)
  hYr <- X*an[i] + rnorm(n, 0, 1)
  
  pow[i] <- R2BootTest(X = X, Ym = Ym, gYl = gYl, hYr = hYr, m = 10000, R = 1000, alpha = 0.05, type = "power")
}

plot(x = an, y = pow)

