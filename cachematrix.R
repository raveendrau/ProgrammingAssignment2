## MakeCacheMatrix function store the input matrix and inverse matrix
## cache."cacheSolve" function computes the inverse matrix and returnsinverse 

## The first function, makeMatrix creates a special "Matrix", 
## which is really a list containing a function to cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
    im<-NULL
    set<-function(y){
        x<<-y
        im<<-NULL
    }
    get<-function() x
    setinverse<-function(solve) im<<-solve
    getinverse<-function() im
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

##The following function calculates the mean of the special "matrix"
##created with the above function.However, it first checks to see if 
##the inverse has already been calculated. If so, it gets the inverse
##from the cache and skips the computation.Otherwise, it calculates 
##the inverse of the matrix and sets the values of the inverse in the 
##cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached matrix")
        return(m)
    }
    matrix<- x$get()
    m <- solve(matrix)
    x$setinverse(m)
    m
}
