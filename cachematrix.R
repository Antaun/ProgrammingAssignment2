## Computing the Inverse of a Matrix and caching it for future use rather than 
## computing it repeatedly

## Creates a special object that caches the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
       i <- NULL
       
       set <- function(y){
               x <<- y
               i <<- NULL
       }
       
       get <- function() x
       
       setinverse <- function(inverse) i <<- inverse
       
       getinverse <- function() i
       
       list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Computes and returns the inverse of the matrix
## If the inverse has already been calculated, it retrieves the inverse from
## the cache

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        
        if(!is.null(i)){
                message("Getting Cached Data!")
                return(i)
        }
        
        matrix <- x$get()
        i <- solve(matrix,...)
        x$setinverse(i)
        i
}
