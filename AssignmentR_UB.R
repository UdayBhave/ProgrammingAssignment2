## cachematrix.R        by Uday Bhave  
## Put comments here that give an overall description of what your
## functions do

## These functions are written to make use of already calculated values (cached values)
## to use, so that once calculated, no need to re-calculate for the subsequent calls of the function
## thus increasing the performance.

makeCacheMatrix <- function(x = matrix()) {

## Write a short comment describing this function

## This function makeCacheMatrix gets a matrix as an input, set the value of the matrix,
## get the value of the matrix, set the inverse Matrix and get the inverse Matrix. The matrix object
## can cache its own object. 

 
  invMatrix <- NULL			# take the matrix as an input
  setMatrix <- function(y) { 		# set the value of the Matrix
    x <<- y
    invMatrix <<- NULL      		## <<- operator is used to assign a value to 
					#  an object in an environment that is different 
					#  from the current environment 
  }
  
  getMatrix <- function() x                              #get the value of the Matrix
  setInverse <- function(inverse) invMatrix <<- inverse  #set the value of the invertible matrix
  getInverse <- function() invMatrix                     #get the value of the invertible matrix
  list(setMatrix = setMatrix, getMatrix = getMatrix,		 
       setInverse = setInverse, getInverse = getInverse)
 
 }
 
 
## Write a short comment describing this function
cacheSolve <- function(x, ...) {


## The function cacheSolve takes the output of the previous matrix makeCacheMatrix(matrix) as an 
# input and checks inverse matrix from makeCacheMatrix(matrix) has any value in it or not.
# In case inverse matrix from makeCacheMatrix((matrix) is empty, it gets the original matrix data from 
# and set the invertible  matrix by using the solve function.
# In case inverse matrix from makeCacheMatrix((matrix) has some value in it (always works
# after running the code 1st time), it returns a message  "Getting Cached Invertible Matrix" 
# and the cached object
# get the value of the invertible matrix from the makeCacheMatrix function

        invMatrix <- x$getInverse()
        if(!is.null(invMatrix)) {                       # if inverse matrix is exist
          message("Getting Cached Invertible Matrix")   # display message: Getting Cached Invertible Matrix 
          return(invMatrix)                             # return the invertible matrix
        }
          
							#if value of the invertible matrix is Not exist (NULL) then  
        MatrixData <- x$getMatrix()                     #get the original Matrix Data 
        invMatrix <- solve(MatrixData, ...)             #use solve function to inverse the matrix
        x$setInverse(invMatrix)                         #set the invertible matrix 
        return(invMatrix)                               #Return a matrix that is the inverse of 'x'
        						
}


#########Testing##########

####Test 1 [2*2 Matrix] #####
TestMatrix <- matrix(1:4,2,2)
TestMatrix

CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()

cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)


####Test 2 [2*2 Matrix] #####
TestMatrix <- matrix(c(1,5,8,2),2,2)
TestMatrix

CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()

cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)
