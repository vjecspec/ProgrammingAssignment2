## This functions calculate and cache the inverse of a matrix
## There are two functions. makeCacheMatrix  creates a 
## special "matrix" object that can cache its inverse, while 
## cacheSolve computes the inverse of the special "matrix"

##----------------------------------------------------------

## makeCacheMatrix is a function that takes a matrix as an
## input variable. The outpot variable is the special "matrix"
## object, which is effectively a list containing the following:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
## It is very important to note that makeCacheMatrix is a function
## that stores a list of functions. Some party of a functions are
## available only through subsetting the main function.

makeCacheMatrix <- function(x = matrix()) {
    inv  <- NULL   #sets the value of the inv as NULL for future
    set <- function (y) {
      x <<- y
      inv <<- NULL
    }                      #the set is subfunction accesible only through
                           #subsetting. It serves to change the values in 
                           #whole function environment (the <<- sign ensures
                           #that the x value is changed through the whole main
                           # function, not just the set function)
    get <- function () x   # this part gets the value of the matrix x.
                           # If we have changed the value of x, we have to
                           # get it and store it somewhere.
    setinv <- function (solve) inv<<-solve  #this part of the code assings the value 
                                            #to the variable inv, and then gets it. As
    getinv <- function () inv               #far as i understood, this doesn't acctualy
                                            #calculate the inverse, but just stores the i
                                            #input to the makeCacheMatrix$setinv and then gets it
    list (set =set, get = get, setinv=setinv, getinv=getinv)    #finally, this forms a list which is 
                                                                #our special 'matrix'
}

#------------------------------------------------------------------------------------------------

## Function cacheSolve calculates the inverse of the special 'matrix'
## created with the above function. It first checkes to see if the inverse
## already exists (i would say whether it is calculated, but as i have
## written, the inverse is not neccesarily calculated, but only given as 
## an input). If the inverse exists, the function doesn't neccesarily calculate it.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()       #gets the inverse value from the special 'matrix'
        if(!is.null(inv)){
          message("getting the inverse from cache")
          return (inv)        #if the value of inv existed in cache, return it
        }
        data <- x$get()       #this line gets the matrix value from the get variable
        inv <- solve(data,...)#this line calculates the inverse of the matrix
        x$setinv(inv)         #this line inputs the calculated value (from now on we have 
                              #the correct inv value in the cache for the given matrix)
        inv
}
