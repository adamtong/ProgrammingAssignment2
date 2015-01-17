# -----------------------------------------------------------------------------
# R Programming
# Programming Assignment 2: Lexical Scoping
#
# The pair of functions in this file take advantage of the scoping rules of 
# the R language to cache the inverse of a matrix.
# -----------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
    # Creates a special "matrix" object that can cache its inverse.
    #
    # Args:
    #   x: matrix whose inverse is to be computed
    #
    # Returns:
    #   A list with the following methods:
    #     set(), get(), setInverse(), getInverse()
    #
    
    # The cached inverse of matrix x
    xi <- NULL

    list(
        set = function(y) {
            x <<- y
            xi <<- NULL
        },
        get = function() { x },
        setInverse = function(inv) { xi <<- inv },
        getInverse = function() { xi } )
}

cacheSolve <- function(x, ...) {
    #
    # Computes the inverse of the special "matrix" returned by makeCacheMatrix.
    # If the inverse has already been calculated, then cacheSolve will retrieve 
    # the inverse from the cache.
    #
    # Args:
    #   x: special matrix created by makeCacheMatrix.
    #
    # Returns:
    #   The inverse of the matrix x
    #

    # Look for the inverse of 'x' in the cache
    xi <- x$getInverse()
    if (!is.null(xi)) {
        message("returning cached inverse")
        return(xi)
    }

    # Solve for the inverse
    data <- x$get()
    xi <- solve(data, ...)

    # Store inverse in cache and return result
    x$setInverse(xi)
    xi
}

# -----------------------------------------------------------------------------
# ChangeLog
# 2015-01-17  Adam Tong  <adam.tong@gmail.com>
#   * Created based on example in assignment
# -----------------------------------------------------------------------------
