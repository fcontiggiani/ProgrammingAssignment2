## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function:
## makeCacheMatrix is a function of a matrix x that stores the cached value.
makeCacheMatrix <- function(x = matrix()) {
######## Starts setting the value as NULL
		m <- NULL
######## Creates the matrix in the working environment
        set <- function(y) {
        		x <<- y
        		m <<- NULL
}
######## Retrieves the value of the matrix    
    get <- function() x
######## Calculates the inverse and stores it in cache
    setInvMatrix <- function(inverse) m <<- inverse
######## Get the inverted matrix
    getInvMatrix <- function() m
######## return the four functions to the working environment
    list(set = set, get = get,
        setInvMatrix = setInvMatrix,
        getInvMatrix = getInvMatrix)
}


## Write a short comment describing this function:
## cacheSolve calcluates the inverse of the matrix stored with makeCacheMatrix

cacheSolve <- function(x, ...) {
######## Get the inverse of the matrix stored in the cache	
        m <- x$getInvMatrix()
######## If the cache is empty, it creates the matrix
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
######## else it calculates the inverse matrix and return it as a result
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setInvMatrix(m)
        return (m)
}

### Example: type
### > a <- makeCacheMatrix()
### > a$set(matrix(5:8, 2, 2))
### > cacheSolve(a)
