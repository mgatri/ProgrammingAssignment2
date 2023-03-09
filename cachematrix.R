
####################################################       
############    Casheing the inverse of the Matrix
####################################################
#   
#   This is a peer graded assignement, it is about 
#   cashing the inverse of a matrix. this program is composed of two functions
#   1. The first is the makeCasheMatrix function. It creates a special matrix
#   that can cashe its inverse.
#   2. The second is the casheSolve function. This function computes the inverse
#   of the special matrix returned by makeCasheMatrix.
#   If the inverse has already been calculated and the matrix has not changed, 
#   then the casheSolve should reteave the inverse from the cashe.

#############    1. The makeCasheMatrix function

makeCacheMatrix <- function(x = matrix()) {

    im <- NULL
    set <- function(y) {
      x <<- y
      im <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) im <<- inverse
    getinverse <- function() im
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


#############   2. The casheSolve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  im <- x$getinverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setinverse(im)
  im
}
