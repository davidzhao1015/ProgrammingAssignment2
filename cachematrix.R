## Put comments here that give an overall description of what your
## functions do

## Make a vector of the type list containing a matrix and functions 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL 
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse  
        getinv <- function()inv
        
        list(set = set, get = get, 
             setinv = setinv, getinv = getinv)
}


## Make a caching function that creates and stores a new inverse of the matrix while retrives 
# the cached inverse of the matrix above 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get() 
        inv <- solve(data) 
        x$setinv(inv)
        
        print(inv)
        
}



