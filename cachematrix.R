## First function is a cache matrix with four functions which the second functions
## uses to find the inverse of the cached matrix.

## This first function contains the four functions - set, get, setinv and getinv. These
## functions will be defined and used later in cacheSolve. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <- y
    inv <- NULL
  }
  get <- function()x
  setinv <- function(inverse)inv <- inverse
  getinv <- function()inv
  list(set = set, get = get,
    setinv = setinv,
    getinv = getinv)
}


## cacheSolve, when used with makeCacheMatrix, uses the defined functions in makeCacheMatrix, with
## the solve function, to return the inverse of matrix X.

cacheSolve <- function(x, ...) {
    inv <-x$getinv()
    if(!is.null(inv)){
        message("getting chached data")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix)
    x$setinv(inv)
    inv
}

##Trial
x = matrix(c(1,3,9,5,3,2,7,3,4), nrow = 3, ncol = 3)
cacheSolve(makeCacheMatrix(x))
## Result:
##             [,1]        [,2]        [,3]
## [1,] -0.09090909  0.09090909  0.09090909
## [2,] -0.22727273  0.89393939 -0.27272727
## [3,]  0.31818182 -0.65151515  0.18181818