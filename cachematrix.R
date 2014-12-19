## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix accepts a matrix as input, initializes the variable i, defines
## three functions and returns a list that contains those functions and their names. 
## I got rid of the set function (from makeVector) because it wasn't called anywhere.
## The function setinverse, when called, uses the <<- operator to assign the value of 
## the inverted matrix to an object that can be used in different environments.

makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
   get <- function() x
   setinverse <- function(solve) i <<- solve
   getinverse <- function() i
   list(get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve calls the functions defined in makeCacheMatrix by calling each
## element of the list created in makeCacheMatrix by name.
## This function checks whether the variable i is still NULL (meaning that
## there's no previous solution stored). If it's not NULL, it retrives its 
## value, i.e., the solution we've already found.
## If it's NULL, it retrives the original matrix, performs the solve function,
## then it calls the setinverse function (described above) and returns the
## result.

cacheSolve <- function(x, ...) {
   i <- x$getinverse()
   if(!is.null(i)) {
      message("getting cached data")
      return(i)
   }
   data <- x$get()
   i <- solve(data, ...)
   x$setinverse(i)
   i
        ## Return a matrix that is the inverse of 'x'
}

## Copy this to create square matrices: 
## mavi <- matrix(c(4,3,3,2), ncol=2, nrow=2)
## mavi <- matrix(c(1,0,1,2,4,0,3,5,6), ncol=3, nrow=3)
## I use this to easily seed an intial matrix for makeCacheMatrix.
## The inverses of these matrices are listed here:
## http://www.mathwords.com/i/inverse_of_a_matrix.htm