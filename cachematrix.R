## create a special object that stores a matrix and caches its inverse

## creates a special matrix that is really a list containing a function to
## (1)set the value of a matrix, (2)get the value of the matrix, (3)set the 
## value of the inverse, (4)get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- null				## m is value of cached inverse
	set <- function(y){		## set sets the matrix value, x, and caches it
		x <<- y
		m <<- Null
	}
	get <- function()x	      ## gets value of matrix
	## do inverse calculation to set inverse
	setinverse <- function(solve)m <<- solve
	## get the cached inverse value
	getinverse <- function()m
	## make a list of sets and gets, it is what makes up the special "matrix"
	list(set = set, get = get, 
	setinverse = setinverse, getinverse = getinverse)
}


## calculates the inverse of the special "matrix" created in above function

cacheSolve <- function(x, ...) {
	## get the inverse(m) of the matrix (x)
	m <- x$getinverse()
	## if m is not null, the value is cached, and is returned
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	## store value (x) which is the matrix, in data, and solve the inverse
      ## then set the inverse value and return it
	data <- x$get()
 	m <- solve(data, ...)
	x$setinverse(m)
	m
}
        

