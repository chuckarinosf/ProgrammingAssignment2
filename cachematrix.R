## The following program uses the example given as a template for developing the required submission
## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of the inverse of the matrix
##      4. get the value of the inverse of the matrix

## Requires input of a SQUARE matrix
##    eg: x<-matrix(1:4,2,2)
##                      [,1] [,2]
##              [1,]    1    3
##              [2,]    2    4

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        ##      1. set the value of the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ##      2. get the value of the matrix
        get <- function() x
        
        ##      3. set the value of the inverse of the matrix
        setinverse <- function(inverse) i <<- inverse
        
        ##      4. get the value of the inverse of the matrix
        getinverse <- function() i
        
        ##      5. Return the list
        list(
                set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## Write a short comment describing this function

## Returns the inverse of a matrix using an object that has been assigned the output 
## of makeCacheMatrix
##
## assign the output of MakeCacheMatrix to another object prior to solving
##   eg: mcm_out<-makeCacheMatrix(x)
##              cacheSolve(mcm_out)

cacheSolve<- function(x, ...) {
        inv <- x$getinverse()

        ## Condition if inverse is not null
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## Otherwise....
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        
        ## Return the results
        inv
}