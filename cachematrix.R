## Put comments here that give an overall description of what your
## functions do

## create special Matrix
## return a list contain functions: get, set, getMatrix, setMatrix

makeCacheMatrix <- function(x = matrix()){
        s <- NULL
        set <- function(y){
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setMatrix <- function(solve) s <<- solve
        getMatrix <- function() s
        list(set = set, get = get, 
             setMatrix = setMatrix,
             getMatrix = getMatrix
             )
}


## perform solve(matrix)

cacheSolve <- function(x, ...){
        ## Return a matrix that is the inverse of 'x'
        s <- x$getMatrix()
        if(!is.null(s)){
                message("getting cached matrix")
                return(s)
        }
        matrix <- x$get()
        s <- solve(matrix, ...)
        x$setMatrix(s)
        s
}
