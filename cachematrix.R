## makeCacheMatrix generates a Matrix
## cacheSolve inverts a Matrix and stores it in the cache 
## and gets it from there if the same data are called again

## mmatrix <- matrix(rnorm(size^2), nrow=size, ncol=size)
## smatrix   <- makeCacheMatrix(mmatrix)

## create the invert matrix

## invmatrix <- cacheSolve(smatrix)

## the matrix is now stored, if you call the same function with the same value again,
## it won't get calculated again, it comes directly and fast from the cache

## invmatrix2 <- cacheSolve(smatrix)


## Setting the Matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Get an inverse Matrix of X
## Store it in Cache and load it from there, if X hasn't changed

cacheSolve <- function(x, ...) {
  
  ##installing MASS-package and load it if it is not
  ##http://stackoverflow.com/questions/9341635/how-can-i-check-for-installed-r-packages-before-running-install-packages
  pkgTest <- function(x)
  {
    if (!require(x,character.only = TRUE))
    {
      install.packages(x,dep=TRUE)
      if(!require(x,character.only = TRUE)) stop("Package not found")
    }
  }
  
  pkgTest("MASS")
  
  ##mpinv <- function(A, eps = 1e-13) {
  ####mpinv for non square matrix found over at
  ####https://stat.ethz.ch/pipermail/r-help/2001-October/015927.html
  ##  s <- svd(A)
  ##  e <- s$d
  ##  e[e > eps] <- 1/e[e > eps]
  ##  return(s$v %*% diag(e) %*% t(s$u))
  ##}
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  ##m <- mpinv(data, ...)
  m <- ginv(data, ...)
  x$setinv(m)
  m
}