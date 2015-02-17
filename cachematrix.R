## Matrix inversion is usually a costly computation. The
## following functions, makeCacheMatrix and cacheSolve, enable
## you to create a special "matrix" object and cache its inverse
## for repeated use. For any invertible matrix A:
## X <- makeCacheMatrix(A)  # creates a special "matrix" object
## cacheSolve(X)            # returns its inverse only computing
##                          # it the first time round

## makeCacheMatrix creates a special "matrix" object that can
## cache its inverse. It can be used for storing invertible
## matrices whose inverse is repeatedly needed.

makeCacheMatrix <- function(x = matrix()) {
    ## 'x' is a square matrix. It is assumed to be invertible.
    
    ## Return a list of functions (get, set, getinv and setinv)
    ## that can be used to directly access and manipulate
    ## matrix 'x' and its (possibly) cached inverse
    
    # the inverse of 'x' ('xinv') is only calculated if needed
    # and therefore set to NULL initially
    xinv <- NULL
    
    # a function to set a new value for the matrix 'x':
    set <- function(new_x) {
        ## 'new_x' is another invertible matrix that replaces 'x'
        
        # the <<- operator sets 'x' and 'xinv' in the parent
        # environment (makeCacheMatrix)
        x <<- new_x
        # remove the (possibly) cached inverse
        xinv <<- NULL
    }
    
    # a function to access the current matrix 'x':
    get <- function() x
    
    # a function to set the inverse of 'x':
    setinv <- function(inv) {
        ## 'inv' is the inverse matrix of 'x'
        
        # the <<- operator sets 'xinv' in the parent
        # environment (makeCacheMatrix)
        xinv <<- inv
    } 
    
    # a function to access the inverse of 'x':
    getinv <- function() xinv
    
    # we return the setters and getters to enable later access
    # and manipulation of the "matrix" object
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}


## cacheSolve computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been computed for the current matrix, then it retrieves
## the inverse from the cache. This is informed to the caller.

cacheSolve <- function(x, ...) {
    ## 'x' is a cached matrix created by makeCacheMatrix()
    
    ## Return a matrix that is the inverse of 'x'
    
    # first we check the current stored inverse
    xinv <- x$getinv()
    
    # if we already have cached the inverse, we'll use it and
    # notify the caller
    if(!is.null(xinv)) {
        message("getting cached inverse")
        return(xinv)
    }
    
    # if we hadn't yet cached the inverse, we'll compute it
    # by solving the equation X %*% Xinv = Identity,
    # cache the newly calculated inverse and return it
    matrix <- x$get()
    xinv <- solve(matrix, ...)
    x$setinv(xinv)
    xinv
}
