columnmean <- function(y) {
  nc <- ncol(y)
  means <- numeric(nc)
  for(i in 1:nc) {
    means[i] <- mean(y[, i])
  }
  means
}

columnmean <- function(y, removeNA = TRUE) {
  nc <- ncol(y)
  means <- numeric(nc)
  for(i in 1:nc) {
    means[i] <- mean(y[, i], na.rm = removeNA)
  }
  means
}

make.power <- function(n){
  pow <- function(x){
    x^n
  }
  pow
}

ls(enviroment(cube))


y <- 10
f <- function(x) {
  y <-2
  y^2 + g(x)
}

g <- function(x) {
  x^y
}
f(3)
} 

g <- function(x){
  a <-3
  x+a+y
}

set.seed(1); normals <- rnorm(100, 1, 2)
nLL <- make.NegLogLik(normals)
nLL
function(p){
  params[fixed] <- p
  mu <- params[1]
  sigma <- params[2]
  a <- -0.5*length(data)*log(2*pi*sigma^2)
  b <- -0.5*sum(data-mu)^2 / (sigma^2)
  -(a + b)
  
}


make.NegLogLik <- function(data, fixed = c(FALSE, FALSE)){
  params <- fixed
  functions(p) {
    params[!fixed] <- p
    mu <- params[1]
    sigma <- params[2]
    
    a <- -0.5*length(data)*log(2*pi*sigma^2)
    b <- -0.5*sum((data-mu)^2) / (sigma^2)
    -(a + b)
    
  }
}
}


x <- as.Date("1970-01-01")

x <- Sys.time()
