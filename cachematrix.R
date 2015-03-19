## These two functiuon are used together for calculate the inverse of à matrix and 
## cache the result for better  performance
##
## Exemple of use :
## m<-makeCacheMatrix(x) ## creates the matrix object
## m$get() ## returns the value of the matrix
## cacheSolve(m) ## computes the inverse of the matrix, store the result in cache, then return it
## m$$getinverse() ## return the cached value of the inverse of the matrix


##  function that creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
		
		i <- NULL;
		
        set <- function(y) {
            x <<- y;
            i <<- NULL;
        }
		
		get <- function() x
		
		setinverse <- function(solve) i <<- solve;
		
		getinverse <- function() i;
		
		list(set = set, get = get,
			setinverse = setinverse,
            getinverse = getinverse);
}


## function that computes the inverse of the special "matrix" returned by "makeCacheMatrix"
## if the inverse of the matrix is already been calculated and the matrix has not changed, return the cached value of inverse.
cacheSolve <- function(x, ...) {
        		
		i <- x$getinverse();
		
		if(!is.null(i)) {
			message("getting cached data");
            return(i);
        }
		
		data <- x$get();
        i <- solve(data);
        x$setinverse(i);
    	return(i);
}
