##These two functions are used in conjunction to save resources during potentially
##time consuming computations. makeCacheMatrix creates a special matrix object, 
##and cacheSolve calculates the inverse of the matrix, or instead will return
##the cached data if the computation has already been carried out.

##makeCacheMatrix creates a list containing a function to set the value of the matrix;
##get the value of the matrix; set the value of the inverse; get the value of the inverse. 
##the matrix inversion is store via the variable m, which is used in cacheSolve to check
##to see if cached data is available or needs to be computed. 


makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                
                setmatrix <- function(inverse) m <<- inverse
                
                getmatrix <- function() m 
                list(set = set, get = get, 
                     setmatrix = setmatrix,
                     getmatrix = getmatrix)
}


##cacheSolve will compute the value of the matrix. However, if the value has already
##been computed, then the function will return the phrase "getting cached data". The matrix 
##conversion is computed via the solve command. If the matrix conversion has not yet been completed
##then cacheSolve will compute it, and cache the value for future reference via set matrix. 

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
        
}
