## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ## NULL is assigned to variable inv
  creatematrix <- function(nr, nc, content){ ## creates matrix with arguments for number of rows, number of colums and matrix values
    x <<- matrix(nrow = nr, ncol = nc, content) ## the matrix is stored in x
    inv <<- NULL ## NULL is assigned to variable inv in different environment
  }
  getmatrix <- function() x ## a function to get the matrix x
  setinverse <- function(inverse) inv <<- inverse ## a function which assigns the inverse matrix to the variable inv
  getinverse <- function() inv ## a function to get the inverse matrix
  list(create = creatematrix, ## lists the four functions
       getmatrix = getmatrix,
       createinverse = createinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) { ## a function to create the inverse of a matrix
  inv <- x$getinverse() ## assigns a cached inverse matrix or NULL to the variable inv by function getinverse()
  if(!is.null(inv)){ ## if an inverse matrix has already been cached it returns this matrix
    message("getting cached data")
    return(inv) ## returns inv
  }
  data <- x$getmatrix() ## assigns a special matrix to the variable data
  inv <- solve(data, ...) ## computes the inverse matrix of variable data
  x$setinverse(inv) ## calls the function setinverse
  inv ## prints out the value of inv
        ## Return a matrix that is the inverse of 'x'
}
