## These following functions will cache the inverse of a matrix


## Function that creates a list of 4 functions  
## to set and get both the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
          
          inv <- NULL
          set <- function(y) {
                    x <<- y
                    inv <<- NULL
          }
          get <- function() x
          setinv <- function(inverse) inv <<- inverse
          getinv <- function() inv
#Finally we return the list containing 4 functions
          list(set = set, get = get,
               setinv = setinv,
               getinv = getinv)
        }

##Function that gets the inverse of the matrix returned by the
##makeCacheMatrix function and caches it. If the inverse has
##already been calculated, it takes it from the getinv function
##contained in the above function.
cacheSolve <- function(x, ...) {

          inv <- x$getinv()
          if(!is.null(inv)) { 
#Checking wether the inverse has already been calculated or not
                    message("getting cached data")
                    return(inv)
          }
#If the inverse has not been calculated yet, the following lines
#calculate it
          data <- x$get()
          inv <- solve(data)
          x$setinv(inv)
## Return a matrix that is the inverse of 'x'
          inv
}
