## Caching the Inverse of a Matrix
## 

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    get<-function() x
    setinverse<-function(solve) inv<<- solve
    getinverse<-function() inv
    list(set=set, get=get,
         setinverse=setinverse,
         getinverse=getinverse)
    inv<-NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    get<-function() x
    setinverse<-function(solve) inv<<- solve
    getinverse<-function() inv
    list(set=set, get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


## Computes the inverse of the special matrix returned by the makeCacheMatrix function. 
## If the inverse has already been caculated (and the matrix has not changes), 
## then the cacheSolve should retrieve the ivnerse from the cache.

cacheSolve <- function(x=matrix(), ...) {

    inv<-x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    matrix<-x$get()
    inv<-solve(matrix, ...)
    x$setinverse(inv)
    inv
    inv<-x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    matrix<-x$get()
    inv<-solve(matrix, ...)
    x$setinverse(inv)
    inv
}
