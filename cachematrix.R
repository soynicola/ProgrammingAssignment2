## Overall the function is to get inverse Matrix from cache if it's been stored before;
## if it's a new matrix, the function will get the inverse matrix.
## Mainly it's to save computing time if it's already calucalted before.
## Example- x is a matrix
## a <- makeCacheMatrix(x)
## b <- cacheSolve(a)
## b will be the inverse matrix of x
## when inputing cacheSolve(a) again, b will be retrieved from stored matrix.

## makeCacheMatrix contains a list of fucntion that stores x's inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(inversematrix) m <<-inversematrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

## cacheSolve is to get inverse matrix from cache if matrix x is calculated and stored before.
## If not, cacheSolve will do the calcuation and get x's inverse matrix.

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
                ## Return a matrix that is the inverse of 'x'
}
