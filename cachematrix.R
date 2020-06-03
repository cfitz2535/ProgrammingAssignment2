## These functions take an inputted matrix and calculates the inverse of that
## matrix. To avoid repeated calculations of a previously calcualted inverse,
## the first function saves the inverse calculation, while the second function
## checks to see if the inverse has already been calculated. If so, the cached
## calculation is returned. If not, a the inverse of the new matrix object is 
## returned.

## This function get's the inverse of the matrix and prepares the results to be
## searched and retrieved by the second function.

makeCacheMatrix <- function(x = matrix()) {
        # Initialization of the objects x and m in the func environment
        m <- NULL
        # Assigning values y and NULL in the parent environment x and m, respectively 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # retrieve x from parent environment
        get <- function() x
        # assign calculated inv of m to parent environment
        setinv <- function(solve) m <<- solve
        # assign the retreival of m from parent environment
        getinv <- function () m
        # assigns each function as an element of a list in parent environment
        list (set = set, get = get, setinv = setinv, getinv = getinv)

}
 


## This function checks to see if the inv has been previously calculated and if
## so returns that product, or initiates calculation of the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        # logic argument to check to see if inverse has already been stored in cache
        if(!is.null(m)) {
        message("getting cached data")        
        return(m)        
        }
        # if m is NULL, initiate the calculation of the inverse and return inv matrix
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
         
