## cachematrix.R
## ProgrammingAssignment2
## https://class.coursera.org/rprog-014/
## S.Terry
## 5/15/2015
## Revisions:
#  5/15/2015 - initial

# Function: makeCacheMatrix
# Description:
#   This function creates a special "matrix" object that can cache its inverse.
# Parameters:
#   x, an invertable matrix
# Returns:
#   A special "matrix" that can be used by cacheSolve
#
makeCacheMatrix <- function(x = matrix()) {
    # initialize
    xT <- NULL

    # define function 'set'
    # set: create or change the matrix
    set <- function(y) {
	x  <<- y
	xT <<- NULL
    }
    
    # define function 'get'
    # get: retrieve the original matrix 
    get <- function() x

    # define function 'setinverse'
    # setinverse: cache the inverse matrix 
    setinverse <- function(xT_) xT <<- xT_

    # define function 'getinverse'
    # getinverse: retrieve the cached, inverse matrix 
    getinverse <- function() xT

    # store our access functions in a list
    list(set = set, 
         get = get,
	 setinverse = setinverse,
	 getinverse = getinverse)
    # returns a list of access functions	 
}


# Function: cacheSolve
# Description:
#   This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# Parameters:
#   x, a matrix that was created with 'makeCacheMatrix'
# Returns:
#   A matrix that is the inverse of 'x'
#
cacheSolve <- function(x, ...) {
    # Try getting the cached inverse
    xT<- x$getinverse()

    # IF there is something in the cached THEN return the cached inverse
    if(!is.null(xT)) {
	message("getting cached data")
        return(xT) # return the inverse of the matrix in x
    }
    # ELSE... there is nothing in the cached...We'll have to do the work.

    # 1) Get the original matrix
    x1  <- x$get()

    # 2) Find the inverse
    x1T <- solve(x1)
    
    # 3) Store it in the cache
    x$setinverse(x1T)
    # return the inverse of the matrix in x
}

# unit test
test_cachesolve <- function(){
    z1<-c(1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1)
    dim(z1)<-c(4,4)
    z2<-rep(1,8*8)
    dim(z2)<-c(8,8)
    
    z1C<-makeCacheMatrix(z1);
    #z2C<-makeCacheMatrix(z2);
    z1T<-cacheSolve(z1C)
    #z2T<-cacheSolve(z2C)
    cat (c("1a) ",z1T,"\n"))
    z1T<-cacheSolve(z1C)
    cat (c("1b) ",z1T,"\n"))
    #cat (c("2a) ",z2T,"\n"))
    #z2T<-cacheSolve(z2C)
    #cat (c("2b) ",z2T,"\n"))
}

