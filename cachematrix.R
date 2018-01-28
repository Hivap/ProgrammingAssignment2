## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
###This function have 2 objects (matrix) and  4 functions in it 
###the 2 function get and getinverse  read the matrix and the inverse of it repectivly
### the 2 set functions set assign the matrix and the inverse of it repectivly
makeCacheMatrix <- function(x = matrix()) {
        invrs <- NULL
        set <- function(y) {
                x <<- y
                invrs <<- NULL
        }
        get <- function() x                                ## readthe matrix x
        setinverse <- function(inverse) invrs <<- inverse  ## get the inverse of the matrix and assign it to invrs
        getinverse <- function() invrs                     ## reads the value of invrs
        list(set = set, get = get,
             setmean = setinverse,
             getmean = getinverse)
     
}


## Write a short comment describing this function
### This function checks if the inverse of given matrix has been assigned(invrs)
###if not it calculates it and assigned the value to varible invrs
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'?
        invrs <- x$getinverse()
        if(!is.null(invrs)) {
                message("getting cached data")
                return(invrs)
        }
        data <- x$get()
        invrs <- solve(data, ...)
        x$setmean(invrs)
        invrs
        
}
