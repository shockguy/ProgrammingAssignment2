## The functions creates a special envrioment for the matrix and its inverse,
## given that inverse has been calculated.  That envrionment can be accesed 
## with the list of function returned from makeCacheMatrix.
## This allows the inverse of a matrix to be calculate by cacheSolve, 
## and assigned inside that unique enviroment, making it unnecesary for it to be recalulated 
## in order to be called and used.
## I think a useful aspect of this is that a many unique envs can be created for
## many matrix and inverse pairs.  Just grab one when needed.

## I interleaved comments throught the code, which I realize is not very readable.  
## But I do think it clearly explains what is going on in a clear,
## step-by-step fashion.

## The code seems to work as intended.
## Below the functions is an example input and output.
## Try it if you like, I promise it will not turn your BIOS into a whisp of smoke.

## This function takes a matrix, and returns a list of functions for accessing a 
## unique environment containing that matrix and the inverse of that matrix,
## given the inverse has been calculated

makeCacheMatrix <- function(x = matrix()) {
        ## b is the solved matrix.  It will always be the inverses because of the way solve,below, is called.
        ## Here is it set to NULL in the local (makeCacheMatrix) space
        b <- NULL
        ## defining the function that sets the matrix in the case that we want to set it directly.  
        set <- function(y) {
                # Assigns the input for set,y, to x in the enclosing environment 
                x <<- y
                ##Make sure the inverse is reset in the enclosing environment
                b <<- NULL
        }
        ##Simply return/display the matrix
        get <- function() x
        ##Funtion that actual creates the inverse, b.
        setinverse <- function(inverse) b <<- inverse
        ##Simply return/display the inverse
        getinverse <- function() b
        ##Create/return the list, with proper names, that contains all the functions made up
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calculates the inverse of the matix from the list
## created by makeCacheMatrix.  In the case that the inverse
## has already be calculated it returns the inverse from 
## cache, and lets the user know that is where it is coming from.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ##Set the local b to the current inverse of x from the envrioment above.  
        ##It should be either a NULL, or the already solve and cached inverse of x
        b <- x$getinverse()
        ##Check to see if b has already be calculated for x.  
        ##If it is cached already, i.e. not NULL, let the user know and return.
        if(!is.null(b)) {
                message("getting cached inverse")
                #Return pops b up to output, ending cacheSolve, 
                #and preventing a recalculation of the inverse of x
                return(b)
        }
        ##Assign the matrix x to a variable that will be processed by solve to get the inverse
        mtx <- x$get()
        ##Solve for the inverse
        b <- solve(mtx)
        ##Assign the inverse b to the list created in makeCacheMatrix
        x$setinverse(b)
        #Return/display the inverse for the users edification
        b
}

## Here is example input and output for the code above
# > a<-makeCacheMatrix(matrix(as.vector(c(.5,2.0,6.0,5.4,2.3,7.9,10,11,8.4)),nrow=3))
# > a$get()
# [,1] [,2] [,3]
# [1,]  0.5  5.4 10.0
# [2,]  2.0  2.3 11.0
# [3,]  6.0  7.9  8.4
# > a$getinverse()
# NULL
# > cacheSolve(a)
# [,1]       [,2]        [,3]
# [1,] -0.268291715  0.1335504  0.14450752
# [2,]  0.195323355 -0.2215253  0.05756481
# [3,]  0.007939974  0.1129461 -0.03831037
# > cacheSolve(a)
# getting cached inverse
# [,1]       [,2]        [,3]
# [1,] -0.268291715  0.1335504  0.14450752
# [2,]  0.195323355 -0.2215253  0.05756481
# [3,]  0.007939974  0.1129461 -0.03831037
> 