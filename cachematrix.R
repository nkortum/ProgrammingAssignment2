## Put comments here that give an overall description of what your
## functions do

## Creates a special 'matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
	m <- x  ## CREATE m as the same matrix as X
	m <- NULL ## Set all values of m to NULL
	
	set <- function(y){
		x <<- y
		m <<- NULL
	}

	get <- function() x  ##geting the matrix

	setinverse <- function(cacheSolve) m ## Setting the inverse

	getinverse <- function() m  ##returning the inverse

	list( set = set, get=get, setinverse = setinverse, getinverse= getinverse)
}
 

	


}


## Computes the inversve of the special matrix returned by makeCacheMatrix.  If the inverse has already been calculated, then the cashe solve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Find out if an an inverse is already stored and if so return that instead
	inverse <- x$getinverse()
	if(!is.null(inverse)){
		message("getting cached data")
		return(inverse)
	}
	##If inverse isn't already stored, then we need to calculate it.	
	data<-x$get()
	inverse <- solve(data) %*% data  ## Solve calculates the inverse of the matrix.
	x$setinverse(inverse)  ##Caching inverse to memory
	inverse	##returning inverse
}




}
