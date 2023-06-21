## The functions desgined to calculate the inverse of a given matrix and cache the results

## Follow the "makeVector" example, here we create a list containing function to set the value of the matrix, get the value of the matrix, set the value of the inverse and get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        inverse_of_matrix <- NULL
        set <- function(y){
                x <<- y
                inverse_of_matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverse_of_matrix <<- inverse
        getinverse <- function() inverse_of_matrix
        list(set=set, get= get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##Compute the inverse of the given matrx
##If the inverse has already been calculated and the matrix has not been changed, we just retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        inverse_of_matrix <- x$getinverse()
        if(!is.null(inverse_of_matrix)) {
                message("inversed matrix already calculated and the matrix has not been cahnged")
                return(inverse_of_matrix)
        }
        data <- x$get()
        inverse_of_matrix <- solve(data)
        x$setinverse(inverse_of_matrix)
        inverse_of_matrix
}
