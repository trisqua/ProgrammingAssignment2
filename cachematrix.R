## This R-script contains 2 functions that - together - can be used to compute and store inverses of matrices.

## The first function, makeCacheMatrix, takes the matrix you entered as input and stores it. It also predefines a set of 4 functions that are needed by/called by the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
              x <<- y
              s <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
        
}

## The makeCacheMatrix function has only one required argument: the matrix of interest, 'x'. 

## Here is some more information on the variables and functions:
## 's' will be the inverse of the matrix 'x' you entered
## The set function allows you to enter a new matrix for 'x' without having to call both functions again. If used, it resets 's' to NULL, so when cacheSolve is called next, the inverse of the new matrix will be computed (instead of returning the old one)
## The get function returns matrix 'x'.
## The setinverse function computes the inverse 's' of matrix 'x'
## The getinverse function returns the inverse 's'.



## The cacheSolve function takes the 'output' of the makeCacheMatrix function, checks whether its inverse is already cached and either returns the cached inverse (plus a message) or computes it and returns the newly computed inverse.
cacheSolve <- function(x, ...) {
        s <- x$getinverse()
        if(!is.null(s)){
                message("getting cached inverse")
                return(s)
        }
        data <- x$get()
        s <- solve(data,...)
        x$setinverse(s)
        s
        ## Return a matrix that is the inverse of 'x'
}

## The cacheSolve function also has only one required argument: the output of funcion makeCacheMatrix 'x'
## Here's a more detailed description of the function:
        ## 1. take output 'x' and get its input, store it in 's'.
        ## 2. check if output 's' is empty. If the inverse of 'x' was cached, 's' will be not empty. If so, the message "getting cached inverse" is printed and the cached inverse is returned (which is the end of the function) . If not, the function continues at the next step:
        ## 3. function 'get' is called; matrix x is stored in 'data'
        ## 4. the inverse of matrix 'data' is computed using 'solve()', its output is stored in 's'
        ## 5. the inverse of matrix 'data', 's', is stored by calling function 'setinverse' for matrix 'x'
        ## 6. the inverse of matrix 'x' is returned. 
