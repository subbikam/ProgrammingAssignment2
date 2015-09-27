## Caching the Inverse of a Matrix. Generate inverse of a matrix and store in cache. 
## If matrix hasnt changed then inverse is recalled from cache instead of 
## computing again. This saves memory

## Create a cacheable matrix

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        set <- function(y = matrix()){
                x <<- y
                inv <<- NULL
        }
        
        get <- function(){
                x
        }
        
        setinv <- function(i){
                inv <<- i
        }
        
        getinv <- function(){
                inv
        }
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        
}

## Computes the inverse of the cacheable matrix
## If the inverse has already been calculated and theres no change in matrix
## then the cacheSolve() returns the cached inverse

cacheSolve <- function(x, ...) {
        
        inv <- x$getinv()
        
        if(!is.null(inv)){
                message("***Getting Data from Cache***")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        
}