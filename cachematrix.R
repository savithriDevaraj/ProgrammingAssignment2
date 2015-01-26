## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix() constructs the special matrix that can store the original and the reverse matrix
# "elc" stores the reversed matrix and "saved" stores the original matrix


makeCacheMatrix <- function(x = matrix()) {
elc <- NULL
saved <- NULL
set <- function(y) {
x <<- y
elc <<- NULL
}
get <- function() x
setreverse<- function(reverse) {
elc <<-reverse
}
getreverse <- function() elc
list(set = set, get = get,
setreverse = setreverse,
getreverse = getreverse)
}

## cacheSolve checks if reverse was calculated before and if the original matrix is unchanged. 
## returns cached value if both TRUE, 
## else calculates reverse matrix and stores the changed matrix and its reverse
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
elc <- x$getreverse()
if ((!is.null(elc)) 
&& (identical(saved,x$get())) ){
message("getting cached reverse matrix!!")
return(elc)
} else {
elc <- solve(x$get())
x$setreverse(elc)
saved <<- x$get()
return(elc)
}
}
