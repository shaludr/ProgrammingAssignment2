#makeCacheMatrix will create a matrix that will get the value of the matrix and set the value fo the matrix
#It will also get the inverse of the matrix and set the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(y) {
    x <<- y
    invMatrix<<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) 
    invMatrix <<- inverse
  getinverse <- function() invMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## CacheSolve will calculate the inverse of the matrix created with CacheSolveMatrix function


cacheSolve <- function(x) {
  invMatrix <- x$getinverse()
  #If the inverse is calculated then skip computation
  #Else, the function will calculate the inverse of matrix and set the value of the inverse in the cache (using setinverse)

  if(!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  }
  data <- x$get()
  invMatrix <- solve(data)
  x$setinverse(invMatrix)
  invMatrix
}



##sample code to show the inverse matrix works
## m <- makeCacheMatrix()
## y <- rbind(c(1,2),c(4,5))
## m$set(y)
# cacheSolve(m) 
# This will give the result:
#[,1]       [,2]
#[1,] -1.666667  0.6666667
#[2,]  1.333333 -0.3333333
# THis yields the same result and proves that we can get the inverse of a matrix
#m$getinverse()
#[,1]       [,2]
#[1,] -1.666667  0.6666667
#[2,]  1.333333 -0.3333333


