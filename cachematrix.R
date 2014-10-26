##The function makeCacheMatrix can be considered as a "container" that returns 
##a list with 4 functions.
##Function makeCacheMatrix needs one parameter: matrix "X"

makeCacheMatrix <- function(X = matrix()) {
        
        ##Variable S is initially empty
        S <- NULL
        
        ##Function 1:function set
        ##creates matrix "X" and variable "S" remains empty
        set <- function(Y) {
                X <<- Y
                S <<- NULL
        }
        
        ##Function 2: function get
        ##returns matrix "X"
        get <- function() X
        
        ##Function 3: setsolve
        ##assingns a new value to "S". "S" is not empty anymore
        setsolve <- function(solve) S <<- solve
        
        ##Function 4: getsolve
        ##returns the new "S" value
        getsolve <- function() S
        
        ##Function makeCacheMatrix returns a list containing 4 functions: 
        ##set, get, setsolve, getsolve
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

##The "container" makeCacheMatrix has already been defined
##Function cacheSolve computes the inverse of matrix "X" returned by 
##makeCacheMatrix
##object<-makeCacheMatrix(X)
cacheSolve <- function(object, ...) {
        
        ##inv gets the value of "S". Initially inv is also empty
        inv <- object$getsolve()
        
        ## if the inverse of matrix "X" has already been caluclated, then
        ##"inv" is not empty and a message and the value of the inverse of 
        ##matrix "X" are shown.
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## if the inverse of matrix "X" has not yet been caluclated, then
        ##"inv" is empty  and it has to be calculated
        matrix <- object$get()
        inv <- solve(matrix, ...)
        object$setsolve(inv)
        inv
}
