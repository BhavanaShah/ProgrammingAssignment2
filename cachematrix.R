# Assignment 2
## Function: creates a matrix which can cache its inverse
makeCacheMatrix <- function(x = matrix()) 
{ 
      inv_mat <- NULL
      
      #set the input matrix
      set <- function(y)
      {
              x <<- y 
              inv_mat <<- NULL
      }
      #return input matrix
      get <- function() x
      
      #set the inverse matrix, in parent env. using superassignment operator (<<-)
      setInverse <- function(solve) 
      {
              inv_mat <<- solve
      }
      #return inverse matrix
      getInverse <- function() inv_mat
      
      list(set = set, get = get, setInverse = setInverse, getInverse= getInverse)
} 

## Function :Computes, caches and then return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) 
{ 
    inv_mat <- x$getInverse()
    
    #check if it is already in cache
    if(!is.null(inv_mat))
    {
            #true, found in cache, just return the value
            message("Getting cached data...")
            return(inv_mat)
    }
    
    #Not found in cache, so calculate and then save
    
    data <- x$get()
    
    #Do inverse of matrix here using in-built 'solve' function
    inv_mat <- solve(data, ...)
    
    #cache the inv_mat 
    x$setInverse(inv_mat)
    
    #return the matrix(inverse)
    inv_mat
} 