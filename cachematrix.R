# MakeCacheMatrix is a parent function composed of four sub-functions.
# It creates an object composed of two parts: a matrix and its inverse, and it caches 
# the matrix and the inverse of the matrix.
# The user will set the matrix and the inverse of the matrix via the set and setinv sub-functions
# The syntax of the set and setinv functions (use of "<<-") makes the matrix (matx) and
# its inverse (inv) available to the other sub-functions within the same parent function 
# environment.

makeCacheMatrix <- function(matx = numeric()) {
    #Initializes the inverse of the matrix to null
    inv <- NULL
    
    #The user can save the matrix in the object using the set function
    set <- function(y) {
        matx <<- y
        inv <<- NULL  #The inverse is null until it is set by the user
    }
    
    #The user can retrieve the matrix using the get function
    get <- function() {
        matx
    }
    
    #The user can save the matrix inverse in the object using the set inverse function
    setinv <- function(inverse) {
        inv <<- inverse
    }
    
    #The user can retrieve the inverse of the matrix using the getinv function
    getinv <- function() {
        inv
    }
    
    #List of all of the object components
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv,
         getevn = getevn)
}


# The function cacheSolve allows the user to determine if the matrix inverse
# has already been calculated for a particular matrix in an object created using
# makeCacheMAtrix (see "matx" below).  If it has already been calculate it retrieves 
# the inverse; otherwise the matrix inverse is calculated.
cacheSolve <- function(matx, ...) {
    # Retrieving matrix inverse cached in the "matx" object
    inv <- matx$getinv()
    
    # If inverse exists, return the inverse
    if(!is.null(inv)) {
        message("getting cached matrix inverse")
        return(inv)
    }
    
    # If inverse does not exist use solve to calculate and store in "matx" object using
    # setinv function
    data <- matx$get()
    inv <- solve(data, ...)
    matx$setinv(inv)
    inv
}


