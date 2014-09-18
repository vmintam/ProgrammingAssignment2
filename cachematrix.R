## Put comments here that give an overall description of what your
## functions do

## create special Matrix
## return a list contain functions: get, set, getMatrix, setMatrix

makeCacheMatrix <- function(x = matrix()){
        #initial s to NULL
        s <- NULL
        # set function assign x to new enviroment :y and S to NULL
        set <- function(y){
                x <<- y
                s <<- NULL
        }
        # get x - matrix
        get <- function() x
        # caculate invert matrix
        setMatrix <- function(solve) s <<- solve
        # get invert matrix
        getMatrix <- function() s
        #return list
        list(set = set, get = get, 
             setMatrix = setMatrix,
             getMatrix = getMatrix
             )
}


## perform solve(matrix)

cacheSolve <- function(x, ...){
        ## get matrix(x) inverted.
        s <- x$getMatrix()
        # if not null, s cached
        if(!is.null(s)){
                message("getting cached matrix")
                return(s)
        }
        # if not cached, caculate invert matrix
        matrix <- x$get()
        s <- solve(matrix, ...)
        x$setMatrix(s)
        # return invert matrix
        s
}
