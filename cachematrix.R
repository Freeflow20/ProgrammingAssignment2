## These are the 2 functions for the Assignment2 at R Programming Coursera course
## I didn't spend much time on them: just modified the previously given example functions
## (makeVector and cachemean). Firstly, I edited the variable names to be more meaningful.
## Afterwards, I changed the mean calculating part to the "solve" function.
## Then I tested the output with some code snippets from the
## course forum. That was all.

## This comment was copy-pasted from the Assignment2 description
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix(numeric()))
{
        m <- NULL
        set <- function(y)
        {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This comment was copy-pasted from the Assignment2 description
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...)
{
        m <- x$getInverse()
        if(!is.null(m))
        {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
