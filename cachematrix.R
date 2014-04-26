## The goal of this program is to compute the inverse of a given matrix
## *after* accessing its cache, i.e. if the solution ahs already been computed
## it will be accessed from the cache. Otherwise a solution is computed and 
## stored into the cache for future use

## To achieve this goal write two functions are written, which in turn 
## contain additional functions
## These two 'wrapper' functions are called 'makeCacheMatrix'
## and 'cacheSolve'. 

## The first 'wrapper' function "makeCacheMatrix" takes an argument 'X' which
## must be a matrix and returns a list of 4 items, each of which are 
## functions in their own right, namely set, get, setInverse and getInverse
## Function 'set' is used to set the original matrix 'X' which is inversed
## Function 'get' is used to access/display the matrix 'X' prior to inversion
## Function 'setInverse' is used to set the inverse of the matrix 'X'
## Function 'getInverse' is used to access/display the inverse of the matrix 'X'

makeCacheMatrix <- function(X = matrix()) {
        ## inv is initially 'NULL' before the inverse of 'X' is obtained by solving
        inv <-NULL
        set <- function(Y){
                X <<- Y
                inv <<- NULL
        }
        get <- function() X
        setInverse <- function(Inverse) inv <<- Inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## 'cacheSolve' takes 'makeCacheMatrix' as its argument
## Return a matrix that is the inverse of 'X'
## If cache exists, prints out 'Getting Cached Data' and directly
## spits out cache. Otherwise, calculates inverse and stores in cache

cacheSolve <- function(X, ...) {
        inv <- X$getInverse()     # Check the matrix 'X''s cache
        if(!is.null(inv)) {       # If cache exists,
                message("Getting Cached Data")
                return(inv)       # Return cache, and solve(X) not needed
        }
        data <- X$get()           # If no cache exists 
        inv <- solve(data, ...)   # Compute the inverse
        X$setInverse(inv)         # Store the result in X's cache
        inv                       # Return result
}
