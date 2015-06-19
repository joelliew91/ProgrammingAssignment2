## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatirx requires an input matrix to initialise
## getInv/setInv gets/sets the inverse of the current matrix
## get/set gets or sets the current matrix

makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    
    setInv<-function(solved) i<<-solved
    
    getInv<-function() i
    
    set<-function(y){
        x<<-y
        i<<-NULL
    }
    
    get<-function() x
    
    list(set=set,get=get,getInv=getInv,setInv=setInv)

}


## Write a short comment describing this function
## cacheSolve computes the inverse of the matrix, if the 
## inverse of the object has yet to be computed

cacheSolve <- function(x, ...) {
    inv<-x$getInv()

    if(is.null(inv)){
          temp<-x$get()
          inv<-solve(temp)
          x$setInv(inv)
          x$getInv()
    }
    else
          x$getInv()

        ## Return a matrix that is the inverse of 'x'
}
