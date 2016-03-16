## This pair of functions allow cached invertion of the matrix. 

## Wraps the given matrix into an entity with 4 methods, which
## provides get/set access to the matrix and its invert value.
## Setting the matrix will reset the cached invert value

makeCacheMatrix <- function(value = matrix()) 
{
        inverse <- NULL
        set <- function(new) 
        {
                value <<- new
                inverse <<- NULL
        }
        get <- function() value
        setInverse <- function(newInverse) inverse <<- newInverse
        getInverse <- function() inverse
        list(
                set = set, 
                get = get,
                setInverse = setInverse,
                getInverse = getInverse
                )
}


## Computes the invert value for a wrapped matrix object
## If the value was computed earlier, returns the cached value.

cacheSolve <- function(x) 
{
        inverse <- x$getInverse()
        if(!is.null(inverse)) 
        {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setInverse(inverse)
        inverse
}







## How would I solve this problem without the given pattern
## Here we create an object of matrix, that can inverse itself,
## So we don't need 2 functions.
## The matrix inside this object cannot be changed, hence the data 
## always consistant (in original solution, data consistancy can be
## violated, if $setInverse is called with a wrong inverse matrix from
## some other function)

makeCacheMatrix1 <- function(value)
{
        inverse <<- NULL
        get <- function() value
        invert <- function()
        {
                if (!is.null(inverse)) 
                {
                        message("getting cached data")
                        return(inverse)
                }
                inverse<<- solve(value)
                inverse
        }
        list(
                get=get,
                invert=invert
                )
}

## test to automatize the correctness check

makeTest <- function()
{
        x<-makeCacheMatrix(matrix(rnorm(16),ncol=4))
        print("No cache message expected")
        y<-cacheSolve(x)
        print("Cache message expected")
        y<-cacheSolve(x)
        x$set(matrix(rnorm(9),ncol=3))
        print("No cache message expected")
        cacheSolve(x)
        print("Cache message expected")
        cacheSolve(x)
        x<-makeCacheMatrix1(matrix(rnorm(16),ncol=4))
        print("No cache message expected")
        y<-x$invert()
        print("Cache message expected")
        y<-x$invert()
}