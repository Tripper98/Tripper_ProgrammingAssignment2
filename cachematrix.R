## Author : A.D Tripper [ Just a nickname] :)
## LinkedIn/instagram : adnane driouche

## The function, makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to : 
## 1.setMatrix the value of the matrix
## 2.getMatrix the value of the matrix
## 3.setInverse the value of the inverse
## 4.getInverse the value of the inverse

## In the 2nd Function "cacheSolve" : I if
## I have the Inverse of the Matrix of interest. 
## If these exist then I don't need calculate and I use
## the cache variable.

## 

makeCacheMatrix <- function(m = matrix()) {
        inv <- NULL
        setMatrix <- function(y) {
                m <<- y
                inv <<- NULL
        }
        getMatrix <- function() m
        setInverse <- function(invMatrix) inv <<- invMatrix
        getInverse <- function() inv
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}


## First, i assume that the matrix have an inverse by testing 
## if det(matrix) == 0, then i return a message that the matrix 
## doesn't have an inverse :)

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        mAux <- x$getMatrix()
        detMatrix <- det(mAux)
        
        if (detMatrix == 0) {
                message("This matrix doesn't have an inverse :(")
                return(mAux)
        }
        
        if(!is.null(inv)) {
                message("getting cached Inverse of Matrix")
                return(inv)
        }
        
        inv <- solve(mAux)
        x$setInverse(inv)
        inv
}
