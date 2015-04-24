## This basically calculates the inversion of a matrix. 
## If I were to create a xyz <- matrix (6:10, 2, 2) 
## I would later cacheSolve(xyz), the result would be an inverse of 
## the matrix defined

#makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

#cacheSolve
cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
