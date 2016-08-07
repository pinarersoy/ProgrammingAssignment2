
## R Programming Course - Programming Assignment 2 
## pinarersoy/ProgrammingAssignment2 forked from rdpeng/ProgrammingAssignment2

## There are two main funcitons in this file: makeCacheMatrix, cacheSolve
## Also there is a test function: test 
## Some testing parameters are added at the end of file to be complied in console

makeCacheMatrix <- function(x = matrix()) {
  
  ## @x is a square invertible matrix
  ## returns a list containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  ##         this list is used as the input to cacheSolve()
  
  inv = NULL
  set = function(y) {
    
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
  
  ## @x is output of makeCacheMatrix()
  ## returns inverse of the original matrix input to makeCacheMatrix()
  
  inv = x$getinv()
  
  # if the inverse has already been calculated
  
  if (!is.null(inv)){
    
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
}

test = function(mat){
  ## @mat: an invertible matrix
  
  temp = makeCacheMatrix(mat)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
}

## In order to test the functions the below code can be compiled in the console

## set.seed(1110201)
## r = rnorm(1000000)
## mat1 = matrix(r, nrow=1000, ncol=1000)
## test(mat1)

## The result will be 0 or a very small number that is close to 0
## This will show us the time difference is highly reduced
 