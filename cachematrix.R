## A pair of functions that cache the inverse of a matrix.

## This function creates a "special" matrix object that 
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        in<-NULL
        set<- function(y){
                x<<-y
                in<<-NULL
        }     ##sets the value of the matrix
        get<- function() x ##gets the value of the matrix
        setinverse<- function(solve) in<<-solve ##sets the value
                                                ##of the inverse
        getinverse<-function() in ##gets the inverse
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
        ##result
}


## This function computes the inverse of the 
## aforementioned matrix. if it has already been 
## calculated, this function will retireve the inverse 
## from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        in<-x$getinverse()
        if (!is.null(in)) {
                message("getting cahced data")
                return(in)
        }
        data<- x$get()
        in<- solve(data,...)
        x$setinverse(in)
        in
}
