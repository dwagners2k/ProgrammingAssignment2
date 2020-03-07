## Put comments here that give an overall description of what your functions do

## Write a short comment describing this function

## This function, makeCasheMatrix, creates a special "matrix" object that can cashe its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function()x
        setInverse <- function(inverse)m <<- inverse
        getInverse <- function(j)
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function

## This function computers the inverse of the special "matrix" returned by makeCasheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then
## casheSolve should retrieve the inverse from the cashe. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)){
                message("getting cashed data")
                return(m)
                mat <- x$get()
                m <- solve(mat, ...)
                x$setInverse(m)
                m
        }
}
