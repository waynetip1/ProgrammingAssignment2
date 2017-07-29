## makeCacheMAtrix creates a matrix object that caches its inverse

makeCacheMatrix <- function(x = numeric()) {
## initialize objects x and m
        m <- NULL
## Define behaviors 
## Set function:
        set <- function(y) {
                ## assigns input to object x
                x <<- y
                ##  assigns null to object m, clears m of of previous value
                m <<- NULL 
        }
## getter function, gets x from makeCacheMatrix
        get <- function() x 
## setter function: use solve function to find inverse 
        setinv <- function(solve) m <<- solve  
## getter function: get the the inverse value of object m
        getinv <- function() m 
## assigns the get, set, setinv, getinv into a list       
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}
## makeCacheMatrix creates a object of type 'makeCacheMatrix'

## cacheSolve computes the inverse of matrix object created by makeCacheMatrix
## cacheSolve retrives the inverse of an object of tyoe makeCacheMatrix
cacheSolve <- function(x, ...) {
## call getinv to get inverse of matrix
        m <- x$getinv()
## if returned value is not NULL, get cached data 
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
## if returned value is NULL, then get matrix value
        data <- x$get()
## calculate matrix inverse
        m <- solve(data, ...)
## setinv to the calculated matrix inverse
        x$setinv(m)
        m

        ## Return a matrix that is the inverse of 'x'
}
