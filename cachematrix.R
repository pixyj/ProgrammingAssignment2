#Functions:

#makeCacheMatrix: Creates a cacheable matrix

#cacheSolve: Returns inverse of matrix

#closeEnough:  Checks if two matrices are "identical" enough. Used for testing
                #as product of matrix and its inverse doesn't always yield integer 1s and 0s

#testCacheSolve:  Runs test case for a random rnorm matrix for square n*n matrix



#Creates a special matrix that can cache its inverse
#Similar to the one in the assignment example

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }

    get <- function() {
        x
    }

    setInverse <- function(i) {
        inverse <<- i
    }

    getInverse <- function() {
        inverse
    }

    #Returns container list encapsulating all methods required 
    #to cache the inverse of the matrix
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


#Accepts a cached matrix created using makeCacheMatrix as input and returns inverse. 
#If value is cached, uses cached value instead of recomputing the inverse

cacheSolve <- function(x, ...) {

    #Check if inverse is already calculated
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("Getting cached Data")
        return(inverse)
    }
    
    #Inverse not calcuated, so calculate and cache it
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}

#Checks if two matrices are good enough to be considered equal
#Used for testing 

closeEnough <- function(a, b, maxDiff) {

    diff <- a - b
    vectorDiff <- as.vector(diff)

    for(value in vectorDiff) {
        if(abs(value) > maxDiff) {
            return(FALSE)
        }
    }

    TRUE

}

#Check if inverse works correctly for a random n * n square matrix
#And also tests if inverse is cached

testCacheSolve <- function(n) {
    
    #Create a random n * n square matrix and initialize the cache
    x <- matrix(rnorm(n * n), nrow=n, ncol=n)
    print(x)
    
    cachedMatrix <- makeCacheMatrix(x)

    inverse <- cacheSolve(cachedMatrix)
    product <- x %*% inverse
    
    testResult <- TRUE

    #Test if inverse works correctly
    print(product)
    if(!closeEnough(product, diag(n), 0.001)) {
        message("Error in inverse calculation")
        testResult <- FALSE
    }

    #Test if value is cached
    cachedInverse <- cachedMatrix$getInverse()

    if(!identical(inverse, cachedInverse)) {
        message("Error: Value not cached")
        testResult <- FALSE
    }

    testResult
}

