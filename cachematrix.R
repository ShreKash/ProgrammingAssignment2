##R programming , week 3 , Project Assignment


## This function creates a special "matrix" object that can cache its inverse



makeCacheMatrix <- function(x = matrix()) {
    inver<-NULL 
    set<- function(y){
      x<<-y
      inver<<-NULL
    }
    get<-Function()x
    setinverse<-function(inverse) inver<<-inverse
    getinverse<-function()inver
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inver)
  inv
       
}
