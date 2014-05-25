
## Matrix inversion is usually a costly computation and it is benefitial to caching the inverse of a matrix
## rather than compute it repeatedly.
## we write a pair of functions that cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list (set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        message("calculating new data")
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

## Testing these functions, courtesy of the helful folks at coursera discussion forms. 
## Both sucessful cases and some error cases are tested. Performance is also tested using bigger matrices.
## 0. Setup your matrix 
##    load package magic to generate invertable square matrix.
##    install.packages("magic")
##    library(magic)
##       Loading required package: abind
##    A<-magic(5) 
##    A
##TestCase1.  Check function makeCacheMatrix, 
##   check function call works without confirm names returned are the ones defined in the function
##   B<-makeCacheMatrix(A)
##   names(B)
##   "set"        "get"        "setinverse" "getinverse"
##   B$get()   returns original matrix
##   B$getinverse() returns NULL
##
##TestCase2. calculate inverse of matrix using cachesolve - first time.
##  You should see "calculating new data" printed
##  cacheSolve(B)
## 
##TestCase3. calculate inverse of matrix using cachesolve - second time.
##  You should see "getting cached data" printed
##  Try few more times to confirm that you are getting cached data.
##  cacheSolve(B)
##
##TestCase4: make sure it is cached properly in makeCacheMatrix()
##  call get and getinverse
##  B$getinverse() B$get()
##
##TestCase5: Reset the matrix and recalculate inverse
## B$set(A)
## B$getinverse() - this now returns null
## cacheSolve(B) -- "calculating new data" printed
## On subsequent calls, "getting cached data" is printed.
##
## TestCase6: reverse input and inverse matrix. Set inverse as original and then calculate inverse. 
## The original matrix should be returned as inverse.
##  C<-cacheSolve(B)
## B$set(C)
## cacheSolve(B)
##
## TestCase7: Error case, test with singular matrix
## a=1:5
## A=cbind(a,a,a,a,a)
## B<-makeCacheMatrix(A)
## returns error message. try solve(A) and comapre error messages, should be same.
##
## Testcase8: Performance test cases. Run with 100 by 100,  1000 by 1000 and 10000 by 10000 matrices.
## testData <- matrix(stats::rnorm(90000), nrow=300, ncol=300)
## test <- makeCacheMatrix(testData)
## system.time(t1 <- cacheSolve(test)) ## first call; w/o cache
## user  system elapsed 
## 0.027   0.001   0.027 
## system.time(t2 <- cacheSolve(test)) ## second call; with cache
## getting cached data
## user  system elapsed 
## 0.001   0.000   0.000 