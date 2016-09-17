## Rajeev Agrawal
## This function is gonna create a set of setter and getter
## Functions.
## This will save the inverse of a matrix in cache
makeCacheMatrix <- function(m = matrix(NA,2,2)){

# Assigned NA value to r, which is reverse of matrix
    r <<- matrix(NA,2,2)

# Set function will reset the value of m and r
    set <- function(y){
  m <<- y
  r <<- matrix(NA,2,2)
    }

# Get function will pull the value of m, which is input    
get <- function() m

# This function will push the reverse of m to r
setrev <- function(solv) r <<- solv

# This function will get the value of r
getrev <- function() r
list(set = set, get = get,
     setrev = setrev,
     getrev = getrev)
}

# This function calls
cacheSolve <- function(m,...){
# Assignin cached value of r
    r <- m$getrev()
  if(!all(is.na(r))) {
    message("getting cached data")
    return(r)
  }  
# Extracting value of input mtrix m
      data <- m$get()
# Calculating the reverse of m
        r <- solve(data, ...)
# Setting the reverse of m to r in makeCacheMatrix
          m$setrev(r)
  r
}
