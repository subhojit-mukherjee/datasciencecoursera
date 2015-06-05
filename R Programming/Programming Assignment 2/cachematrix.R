## makeCacheMatrix Function takes a matrix type input and create an object
##with get,set,getInverse and setInver methods.

##Inverse of a matrix is stored in inverseMatrix variable


makeCacheMatrix <- function(x = matrix()) {
		inversematrix <- NULL
		set <- function(y){
				x <<- y
				inversematrix <<- NULL
		}
		get <- function() x
		setInverse <- function(inv) inversematrix <<- inv
		getInverse <- function() inversematrix
		list(set = set, get = get,
			setInverse = setInverse,
			getInverse = getInverse)
  
  
}


## This method takes a Matrix object created by makeCacheMatrix method and 
##gets its inverse. First time calling of it creates the inverse and stores it.
##From the next time it gets the cached Value
##See at the bottom for sample output

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
		invertedMatrix <- x$getInverse()
		if(!is.null(invertedMatrix)) {
				message("getting cached data")
				return(invertedMatrix)
		}
		data <- x$get()
		invertMatrix <- solve(data,)
		x$setInverse(invertMatrix)
		invertMatrix
}

##Sample Output
## m<-makeCacheMatrix(matrix(c(2,2,3,2),2,2))

## cacheSolve(m) ----First time call
##      [,1] [,2]
##[1,]   -1  1.5
##[2,]    1 -1.0

## cacheSolve(m) ----Second time call
##getting cached data
##      [,1] [,2]
##[1,]   -1  1.5
##[2,]    1 -1.0