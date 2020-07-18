## Assignament 2
## The objective that this functions is create a matrix, save it in the caché and
## then calculate his inverse without use a lot of resources

## Set matrix and clear his inverse

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  setmatrix<-function(y){
    x<<-y
    i<<-NULL
  }
  getmatrix <- function() x
  setinv <- function(inv) i<<- inv
  getinv <- function() i
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinv = setinv,
       getinv = getinv)
}


## Verify the inverse previous and if this exist getting it of cache, else then 
## calculate his inverse and set it in the cache

cacheSolve <- function(x, ...) {
  i<-x$getinv()
  if(!is.null(i)){
    message("Obtained Matrix from cache")
    return(i)
  }
  i<-x$getmatrix()
  inv<-solve(i)
  x$setinv(inv)
  inv
}

#For example if you want you can delete the # and test with this randomly matrix 3x3

#matriz<-makeCacheMatrix(matrix(rnorm(9),3,3))
#cacheSolve(matriz)
