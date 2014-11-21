## The first function, `makeCacheMatrix` creates a special "matrix", which is
## really a list containing functions to set & get the value of the passed matrix and to 
## get and set the value of the inverse. It will cache the inverse value.

## The second function `cacheSolve` will check if the inverse of the passed matrix was computed
## previously and if so, will returns a cached copy of the inverse
##  else it computes the inverse of the passed matrix and caches the value. 

## The following function,creates a special "matrix", which is
## really a list containing a function to
## 
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the inverse of the matrix
## 4.  get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
   invmat <- NULL
   mat <<- x
   
   set <- function(x) {
     mat <<- x
     invmat <<- NULL
   }
   get <- function () mat
   setinvmat <- function(i) invmat <<- i
   getinvmat <- function () invmat
   
   list(set = set, get = get,
        setinvmat = setinvmat,
        getinvmat = getinvmat)
}

## The following function calculates the nverse of the special "matix"
## created with the above function. However, it first checks to see if the
## inverse matrix has already been calculated. If so, it `get`s the inverse matrix from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the `setinvmat`
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmat <- x$getinvmat()
  
  if (!is.null(invmat)  && identical(x$get(), mat)) 
    return (invmat)
  
  data <- x$get()
  invmat <- solve(data)
  x$setinvmat(invmat)
  invmat
  
}
