
##This Function contains list of four function.
## 1st Function sets the value of matrix.
## 2nd Function gets the value of the matrix.
## Third Function calculates the inverse of the matrix.
## Fourth Function gets the calculated inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL 
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  get<-function()x
  
  setinverse<-function(inv){
    inverse<<-inv
  }
  getinverse<-function()inverse
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}
#This matrix takes that matrix and calculates its inverse and cache it.
# If a inverse is already present in the cache it returns the inverse from the cache. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting inverse from cache")
    return(inv)
  }
  data<-x$get()
  inverse<-solve(data)
  x$setinverse(inverse)
  inverse
}