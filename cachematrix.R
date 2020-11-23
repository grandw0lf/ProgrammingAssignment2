## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function -- This is a short function that takes a value and inverts the
## value and stores it in a variable and then returns the value in inverse.

makeCacheMatrix <- function(x = matrix()) {                                #establishes the function makeCacheMatrix
                    inv <- NULL                                            #creates a placeholder for the inverted value
                    set <- function(y){                                    #sets the value of the matrix
                            x <<- y 
                            inv <<- NULL
                    }
                    get <- function() {x}                                  #pulls the value of the function 
                    setInverse <- function(inverse) {inv <<- inverse}      #sets the value of the inverse
                    getInverse <- function() {inv}                         #pulls the value of the inverse
                    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function - This function takes makeCacheMatrix and uses it to invert the
## value of x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
              inv <- x$getInverse()                                        #This retrieves the function from makeCacheMatrix and stores the value.
              if(!is.null(inv)){                                           # If there is already a value in inv it will return this value, otherwise it will process
                    message("Retrieving Stored information: ")
                    return(inv)
              }
              mat <- x$get()
              inv <- solve(mat, ...)
              x$setinverse(inv)                                           #inverts the function from the previously defined function
              inv                                                         #returns the data
}
