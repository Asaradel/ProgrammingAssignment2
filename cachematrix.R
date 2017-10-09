## The setInverse property will hold the cached solved matrix inverse.  The getInverse propery will return a the saved matrix, or return NULL if there isn't one cached.
makeCacheMatrix <- function(x = matrix()) 
{
		m <- NULL
		set <- function(y) 
		{
        	x <<- y
            m <<- NULL
        }
        get <- function() x
	  setInverse <- function(inverse) m <<- inverse
	  getInverse <- function() m
	 
	  list(set = set,
	      get = get,
	      setInverse = setInverse,
	      getInverse = getInverse)
	      
}


## Inverts a matrix and stores it in the setInverse global variable
cacheSolve <- function(x, ...) 
{        
		m <- x$getInverse()
		
		if (!is.null(m)) 
		{
				 message("If you see this message, then the version of the matrix you are getting is a previously calculated, cached version.")

	          return(m)
	  }
	  
	  data <- x$get()
	  m <- solve(data, ...)
	  x$setInverse(m)
	  
	  m
  
  
}
