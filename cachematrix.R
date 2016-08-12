> Our aim in this experiment is to write a pair of functions, namely, 
Error: unexpected symbol in "Our aim"
> ## "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix
> 
> ## makeCacheMatrix is a function which creates a special "matrix" object that can 
> ## cache its inverse for the input (which is an invertible square matrix)
> 
> makeCacheMatrix <- function(x = matrix()) {
+   inv <- NULL
+   set <- function(y) {
+     x <<- y
+     inv <<- NULL
+   }
+   get <- function() x
+   setinv <- function(inverse) inv <<- inverse
+   getinv <- function() inv
+   list(set = set, get = get, setinv = setinv, getinv = getinv)
+ }
> 
> 
> ## cacheSolve is a function which computes the inverse of the special "matrix" 
> ## returned by makeCacheMatrix above. If the inverse has already been calculated 
> ## (and the matrix has not changed), then the cachesolve should retrieve the 
> ## inverse from the cache
> 
> cacheSolve <- function(x, ...) {
+         ## Return a matrix that is the inverse of 'x'
+   inv <- x$getinv()
+   if(!is.null(inv)) {
+     message("getting cached result")
+     return(inv)
+   }
+   data <- x$get()
+   inv <- solve(data, ...)
+   x$setinv(inv)
+   inv
+ }
> 
> ## ---Running the Program--
>  m <- matrix(rnorm(16),4,4)
>  m1 <- makeCacheMatrix(m)
>  cacheSolve(m1)
          [,1]       [,2]       [,3]      [,4]
[1,]  3.497547  0.0621861  0.3956282 -1.420722
[2,] -2.114663 -0.4904507 -0.1593364 -1.070080
[3,]  6.763608  2.8035743  1.2513060  1.345672
[4,]  9.342607  3.0065794 -0.1609145  2.042687
> 

