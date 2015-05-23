## There are following two functions in this program
## 1. "makeCacheMatrix" creates a special matrix to cache the inverse of the matrix 
## 2. "cacheSolve" computes the inverse of the matrix for the inverse.
## To test it, create the matrix using makeCacheMatrix say m. Then invoke cacheSolve(m).
## If you invoke cacheSolve(m) one more time, it will returned the already computed result.

## This function creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()){
  inv<- NULL
  
  #This is a function to set the new matrix y.
  set <- function(y){
         x<<- y
         #set the inverse of this new matrix to NULL.
         inv<<- NULL
  }
  
  #This is a function to get the matrix
  get<-function() x
  
  #This is function to set the inverse(cache it for future.)
  setinv<- function(inverse) inv<<-inverse
  
  #This is function to get the inverse value.
  getinv<- function() inv
  list(set=set, get=get, setinv=setinv,getinv=getinv)
  
}
## This function computes the inverse of the special matrix returned 
## by makeCacheMatrix above. We assume the matrix x is invertible.
cacheSolve <- function(x, ...){
  inv<- x$getinv()
  #if inverse is already computed then return it(cached value).
  if(!is.null(inv)){
           message("getting cached data!")
           return(inv)
  }
  #Otherwise, compute the inverse and cache it for future use. 
  data<-x$get()
  inv<-solve(data, ...)
  x$setinv(inv)
  inv
}
