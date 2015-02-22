#Programming assignment 2 - cachematrix

# 1. set the value of the matrix --> set()
# 2. get the value of the matrix --> get()
# 3. set the value of inverse of the matrix --> setInv()
# 4. get the value of inverse of the matrix --> getInv()

makeCacheMatrix <- function(mat = matrix()) {
    inv <- NULL
    set <- function(y) {
        mat <<- y
        inv <<- NULL
    }
    get <- function() return(mat)
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() return(inv)
    
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    ##if inverse matrix already in cache (inv != null) then return the iverted matrix
    if(!is.null(inv)) { 
        message("getting cached data.") #Evidence message :-)
        return(inv)
    }
    
    ## we declared before inv as NULL in makeCacheMatrix(), so this is the case for the first run
    data <- x$get() 
    inv <- solve(data) #inverse the matrix
    x$setInv(inv)      #save the inversed matrix into makeCacheMatrix(), so inv will for the next run not be NULL anymore!
    inv
}

##TEST
# x = matrix(-2:1,2)
# m = makeCacheMatrix(x)
# m$get()
#      [,1] [,2]
# [1,]   -2    0
# [2,]   -1    1

# cacheSolve(m)
#      [,1] [,2]
# [1,] -0.5    0
# [2,] -0.5    1

# cacheSolve(m)
# getting cached data.
#      [,1] [,2]
# [1,] -0.5    0
# [2,] -0.5    1
