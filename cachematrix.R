## I'd love to say something verbose and impressive about the purpose 
##of these functions. And I get the feeling I'm somewhat expected to.
##But I'm not going to. Read below. It tells you what they do.

## This function creates a special "matrix" object that can cache its inverse.
##for maximum efficacy, make sure you set this funtion as a veriable in R when
##it is run. This then allows cacheSolve to run that variable.

makeCacheMatrix <- function(x = matrix()) {
	a<-NULL
	get<-function()x
	setinverse<-function(solve)a<<-solve
	getinverse<-function()a
	list(get=get,
		setinverse=setinverse,
		getinverse=getinverse)
}


##This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then cacheSolve retrieves 
##the inverse from the cache.

cacheSolve <- function(x, ...) {
        a<-x$getinverse()
	if(!is.null(a)){
		message("Hold. Getting cached matrix.")
		return(a)
	}
	new<-x$get()
	a<-solve(new,...)
	x$setinverse(a)
	a
