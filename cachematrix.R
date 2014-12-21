## Function makeCacheMatrix receives a matrix and returns a list containing
## functions that can be used to retrieve the matrix , reset the matrix,
## get the matrix's inverse and set the matrix's inverse

## Funcion cacheSolve receives a list argument with functions as 
## constructed by makeCacheMatrix. List function getinverse is used to 
## get the cached inverse value of the matrix , if it exists, 
## otherwise it will retreive the matrix via the list function getmatr
## and call solve() to get the inverse and store this value via setinverse
## for subsequent calls for this matrix. 

                             
# makeCacheMatrix receives a matrix argument. This 
# function then defines a further 4 functions:
# 1) resetmatrix , which basically sets the original matrix to that received 
# by this function and clears the inverse value i.e the result of any previous
# call to solve to invert the matrix
# 2) getmatr , simply returns the matrix
# 3) setinverse, retains the value of the call to solve that inverts the matrix
# 4) getinverse, returns the inverseval i.e inverted matrix, 
# which could be the cached previously set value, or null 
# These functions are returned by the function within a list, which 
# effectively serves as an interface to the submitted matrix

makeCacheMatrix <- function(x = matrix())
{ 
    inverseval <- NULL                 # clear the inverse matrix val
    resetmatrix <- function(matrixarg) # resets the original matrix to  
    {                                  # matrixarg and clears the inverted value
        x <<- matrixarg                # the scope <<- operator enables the 
        inverseval <<- NULL            # matrix and it's inverted form to be 
    }                                  # accessible in a parent session scope 
                                       # i.e x is accessible from this function
                                       # when it is called from cacheSolve.
    getmatr <- function() x            
    setinverse <- function(inverse) inverseval <<- inverse  # sets the inverted
                                                 # matrix using the scope
                                                 # operator as described 
                                                 # above, this effectively
                                                # caches the inverted matrix
                                              # so the process need not be 
                                              # repeated in this session

    getinverse <- function() inverseval       # returns the cached value

    list( resetmatrix = resetmatrix,       # return list of functions  
          getmatr = getmatr,               # effectively the interface to the 
          setinverse = setinverse,         # matrix and its inverted form
          getinverse = getinverse)
}


# cacheSolve takes the populated list argument set up by a prior 
# call to makeCacheMatrix. A call to the list's function getinverse()
# is made to retrieve the inverted matrix. if the retrieved value is not null 
# then it has been cached and therefore the cached values can be returned
# and the function completes. Otherwise ,using the list's functions, 
# get the matrix, make a call to solve with this matrix to 
# get the inverted version and then call the list function setinverse() 
# to store the value for any subsequent calls for this object 

cacheSolve <- function(x, ...) 
{
    inverseval <- x$getinverse()    # get the inverted matrix value
    if(!is.null(inverseval)) 
    {
        return(inverseval)          # if it's not null then use this
                                    # cached value for the inverted matrix
    }
    data <- x$getmatr()             # matrix has not been inverted yet
    inverseval <- solve(data,...)   # therefore get the matrix and call solve()
    x$setinverse(inverseval)        # retain the inverted matrix for future
                                    # calls 
    inverseval                      # last statement returns the inverted
                                    # matrix
}
