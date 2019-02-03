## This function creates a "matrix", in reality it returns a list containing functions that allow
# access and modifications to the matrix. Once the object has been initialized the matrix will be cached
# until it is modified (this is done for optimization purposes)

makeCacheMatrix <- function(x = matrix()) {

    #Initialize the i variable which will be used to store the inverse once it has been computed
    i <- NULL
    #create the set function which will store the matrix data and clear the cached inverse
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    #create the get function which will return the stored matrix
    get <- function() x
    #The set inverse function accepts an argument that is the inverse of the stored matrix and stores it
    setInverse <- function(inverse) i <<- inverse
    #The get inverse function returns the stored inverse function
    getInverse <- function() i
    #Create the list to be returned out of this function which contains the set,get, setInverse and getInverse functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The cacheSolve function accepts a custom "matrix" argument as created with the makeCacheMatrix function and
## calculates the cache if not yet present, or returns the stored (Cached) inverse if it has already been stored
cacheSolve <- function(x, ...) {
    #retrieve the stored inverse from the custom matrix object
    i <- x$getInverse()
    #check if the stored inverse is not null and return the inverse matrix if present
    if(!is.null(i)) {
        message("getting cached data")
        #calling the return function here will end the exection of the function (I.e. the lines after the if will 
        #not run)
        return(i)
    }
    #Retrieve the actual matrix from the list
    data <- x$get()
    #Call the solve function on the retrieved matrix and store it in the i variable
    i <- solve(data, ...)
    #Store the inverse matrix so we can retrieve a cached version in future
    x$setInverse(i)
    #Return the inverse matrix
    i
}