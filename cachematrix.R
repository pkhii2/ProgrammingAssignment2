## This file contains two functions, makeCacheMatrix and 
## cacheSolve.
## Line 11-16 : Description of makeCacheMatrix.
## Line 18-29 : Coding of makeCacheMatrix.
## Line 31-50 : Demystifying makeCacheMatrix.
## Line 54-60 : Description of cacheSolve.
## Line 62-72 : Coding of cacheSolve.
## Line 74-88 : Demystifying cacheSolve.
## Line 91-135: Detailed example

## makeCacheMatrix function creates a special 'vector',
## which is really a list containing a function to
## 1. Set the value of matrix
## 2. Get the value of matrix
## 3. Set the value of inverse matrix
## 4. Get the value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse.x <- NULL
        set <- function(y){
                  x <<- y
                  inverse.x <<- NULL
        }
        get <- function () x
        setinverse <- function (inverse) inverse.x <<- inverse
        getinverse <- function () inverse.x
        list (set=set,get=get,setinverse=setinverse,
              getinverse=getinverse)
}

## Demystifying makeCacheMatrix function.
## Line 11: A function named makeCacheMatrix is created,
##          with the initialization of two object, x and inverse.x.
##          x is initialized as a function argument.
## Line 12: The value of NULL is assigned to inverse.x.
## Line 13: A function called set is nested in makeCacheMatrix
##          function, with one argument, which is y in matrix 
##          form.
## Line 14: y is assigned to x, and stored in parent environment
##          of makeCacheMatrix function.
## Line 15: The value of NULL is assigned to inverse.x object
##          in the parent environment of makeCacheMatrix function.
## Line 17: A nested function called get, retrieves the x from the parent
##          environment of makeCacheMatrix function.
## Line 18: A nested function called setinverse, to set the inverse.x
##          in the parent environment of makeCacheMatrix function.
## Line 19: A nested function called getinverse, retrieves the inverse.x
##          from the parent environment of makeCacheMatrix function.
## Line 20: A list is created to name the nested functions above.
##          You may now use $ to access the nested functions above.



## cacheSolve function calculates the inverse of 'x'
## created with the makeCacheMatrix function. However,
## it first checks if the inverse of 'x' has already
## been calculated. If so, it gets the inverse of 'x'
## from the cache and skips the computation. Otherwise,
## it calculates the inverse of 'x' and set it in the
## cache via the set function.

cacheSolve <- function(x, ...) {
        inverse.x2 <-x$getinverse()
        if(!is.null(inverse.x2)){
                message("getting cache data")
                return (inverse.x2)
        }
        data <- x$get()
        inverse.x3<-solve(data,...)
        x$setinverse(inverse.x3)
        inverse.x3
}

## Line 62: A function named cacheSolve is created with a single
##          argument, x, and ellipsis that allows to pass any
##          additional arguments into the function.
## Line 63: Object x is passed in as the argument to call the getinverse
##          function, and the result is assigned to inverse.x2.
## Line 64: If the inverse.x2 is not NULL, then print the message
##          and return the inverse.x2. No calculation is needed.
## Line 68: If the inverse.x2 is NULL, then object x is passed in
##          as the argument to call the get function, and the result
##          is assigned to data variable.
## Line 69: solve function is used to get the inverse matrix of data
##          and the result is assigned to inverse.x3.
## Line 70: setinverse function is used to store the inverse.x3 to
##          the parent environment of makeCacheMatrix.
## Line 71: print the inverse matrix in the R console.


## Detailed example

## Source("cachematrix.R")
## The functions of this R script are loaded into R console.

## m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
## A 2x2 matrix, m1, is created.

## n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
## A 2x2 matrix, m1, is created. Noted that n1 is the inverse of m1.

## mymatrix <- makeCacheMatrix(m1)
## A variable is created to run makeCacheMatrix with m1 as the argument.

## mymatrix$get()
## m1 matrix is printed in R console.

## mymatrix$getinverse()
## NULL is printed in R console because the calculation is not done yet.

## cacheSolve(mymatrix)
## The inverse matrix of m1 is printed in R console. The matrix should be
## the same with n1 matrix.

## mymatrix$getinverse()
## The inverse matrix of m1 is printed in R console because it is cached
## in the parent environment of makeCacheMatrix.

## mymatrix$set(n1)
## n1 is set as argument to be executed in mymatrix function.

## mymatrix$get()
## n1 matrix is printed in R console.

## mymatrix$getinverse()
## NULL is printed in R console because new input clears the cache data
## stored at parent environment of makeCacheMatrix.

## cacheSolve(mymatrix)
## The inverse of n1 is printed in R Console. Noted that the result should
## be similar with m1 matrix.

## mymatrix$getinverse()
## The inverse of n1 is printed in R console because the result is stored
## at the parent environment of makeCacheMatrix.



