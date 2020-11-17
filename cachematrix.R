##The function store the outcome of inverse matrix, when it is
##being used. It creates a shortcut to the previous calculated
##matrices

## Makecachematrix create a cache matrix to keep inverse.

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set <- function(y){
    x<<-y
    m<<-NULL
  }
  get <- function () x
  setinverse <- function(solve) m<<-inverse
  getinverse <- function()m
  list(set=set, get=get,
       setinverse=setinverse, getinverse=getinverse)
  
}

## This function calculated the inverse matrix if it has not been 
## calculated

cacheSolve <- function(x, ...) {
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached matrix")
    return(m)
    data<-x$get()
    m<-solve(data,..)
    x$setinverse(m)
    m
  }
}