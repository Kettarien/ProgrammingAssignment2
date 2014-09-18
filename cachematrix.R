## Coursera - R Programming
## Programming Assignment 2

## Following functions provide calculating and fast retrieving from cache
## of a previously calculated value for matrix inverse

# This function is responsible for creating a special "matrix" object
# List is returned which consists of functions providing access to the cached 
# matrix and its computed inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # get() returns a matrix stored in the cache
    get <- function() x
    # setinverse() updates the cached inverse matrix
    setinverse <- function(inverse) inv <<- inverse
    # getinverse() returns the cached inverse matrix
    getinverse <- function() inv
    # list is returned which contains functions for commmunication with cache
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# This function calculates or retrieves from cache the value of given
# matrix inverse. Returned is the inverse matrix of the input matrixNew
cacheSolve <- function(x, matrixNew) {
    # Getting cached inverse matrix 
    inv <- x$getinverse()
    # Getting cached original matrix (for comparison with new input)
    matrixOld <- x$get()
    
    # Checking if inverse matrix was already computed
    if(!is.null(inv)) {
        # Checking if the input matrix has changed compared to the cache
        if (identical(matrixNew, matrixOld)) {        
            message("Matrix did not change; getting cached inverse")
            inv <- x$getinverse()
            return(inv)
        }
        message("Matrix changed; calculation will follow")
        # Inverse matrix calculated, matrix and its inverse updated in cache
        inv <- solve(matrixNew)    
        x$set(matrixNew)
        x$setinverse(inv)   
        message("Inverse calculated")
        return (inv)
    }
    message("Inverse was not calculated before; calculation will follow")
    # Inverse matrix calculated, matrix and its inverse updated in cache
    inv <- solve(matrixNew)    
    x$set(matrixNew)
    x$setinverse(inv)   
    message("Inverse calculated")
    return (inv)
}

## Following code provides usage examples and testing of the provided functions

# Example matrices
matrix1 <- matrix(c(1,3,5,2,3,2,8,7,1),3,3)
matrix2 <- matrix(c(3,3,5,2,5,2,1,3,1,3,4,2,3,5,4,1),4,4)

# Creating a caching "matrix" object
myCachedMatrix = makeCacheMatrix()

## Possible scenarious (incorrect input matrix is not relevant according to the 
## specifications in the assignment)

# 1. Inverse was never calculated; input is matrix1
inverse <- cacheSolve(myCachedMatrix, matrix1)
print (inverse)

# 2. Input matrix did not change; retrieving from cache
inverse <- cacheSolve(myCachedMatrix, matrix1)
print (inverse)

# 3. New input matrix; new inverse is computed
inverse <- cacheSolve(myCachedMatrix, matrix2)
print (inverse)

# 4. Repeat with the first input, inverse is calculated again
inverse <- cacheSolve(myCachedMatrix, matrix1)
print (inverse)

# To check if the return value is really the inverse matrix for the input,
# you can multiply them: inverse %*% matrix1
# Result should be an identity matrix
# Note that several elements may be not exactly 0, but very close (e.g. 1.5e-15)

