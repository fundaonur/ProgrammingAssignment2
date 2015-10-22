## Put comments here that give an overall description of what your
## functions do


#FundaBernaOnur

## We create a special matrix here.
## By this function we set and get value of the matrix
## and we set and get inverse value of the matrix by solve()

makeCacheMatrix <- function(x = matrix()) 
{
    inverseMatrix <- NULL
    
    set <- function(y) 
    {
        x <<- y
        inverseMatrix <<- NULL
    }
    
    get <- function() x
    
    setsolve <- function(solve) inverseMatrix <<- solve ##mean yerine inverse
    getsolve <- function() inverseMatrix
    
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)

}


## we calculate the inverse of matrix by makeCacheMatrix function.
## cacheSolve function first controls if inverse of the matrix
## has been calculated or not. if it is calculated before we return
## the cached inverse matrix value. if not we calculate inverse of
## the matrix and set this value to cache by setSolve function.

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    
    
    inverseMatrix <- x$getsolve()
    
    if(!is.null(inverseMatrix)) 
    {
        message("getting cached matrix")
        return(inverseMatrix)
    }
    
    data <- x$get()
    
    inverseMatrix <- solve(data, ...)
    x$setsolve(inverseMatrix)
    inverseMatrix
    
}
