## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This assigment is part of coursera's "Data Science: R Programming" module
## Week 3 assignment
## Description: This function creates a special "matrix" object that can cache its inverse

## Date of submission of assigment: 22-June-2017
## GitHub User Name: coolfox0407
## Email ID: coolfox.hari@gmail.com
## Name: Hariharan D

## Create a function and define the argument "x" with default format as "matrix"
makeCacheMatrix <- function(x = matrix()) {
  
  ## Defines "MatInv" as NULL, to hold the value of matrix inverse 
    MatInv <- NULL 
      
    ## Function to set the matrix
      set <- function(y) {
          x <<- y
          MatInv <<- NULL
      }
    
    ## Function to get the matrix  
      get <- function() {
          x   ## Returns the matrix
      }
      
    ## Function to set the inverse of the matrix
      setInverse <- function(inverse) {
          MatInv <<- inverse
      }

    ## Function to get the inverse of the matrix      
      getInverse <- function() {
          MatInv  ## Returns the matrix inverse
      }
 
  ## Returns the list of the functions     
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

## Computation for inverse of the special matrix returned by "makeCacheMatrix"
## Returns inverse of the matrix from the cache, if its already been computed
cacheSolve <- function(x, ...) {
  
  ## Returns a matrix that is the inverse of 'x'
      MatInv <- x$getInverse()
  
  ## Returns the inverse if its already been computed  
      if(!is.null(MatInv)) {
          message("Cached Data")
          return(MatInv)
      }
    
    ## Gets the matrix
    data <- x$get()
    
    ## Computes inverse of the matrix using matrix multiplication
    MatInv <- solve(data) %*% data
    
    ## Sets the inverse
    x$setInverse(MatInv)
    
    ## Returns the matrix
    MatInv
}
