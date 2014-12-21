###############################################################
#Functions used to cache inverse of a matrix                  #
#                                                             #
# makeCacheMatrix function:                                   #
# - Create a matrix object                                    #
# - The maxtrix object can set itself                         #
# - The maxtrix object can get itself                         #
# - The maxtrix object can set the inverse matrix of it       #
# - The maxtrix object can get the inverse maxtrixof it       #
#                                                             #
# cacheSolve funtion:                                         #
# - Try to get the inverse of the matrix first. If it is      #
#   already calculted, use it without re-calculating          #
# - If the inverse of matrix is not calculated, call the      #
#   solve function to compute it and return the result        #
###############################################################


makeCacheMatrix <- function(x = numeric()) {
        ###############################################################
        #initial inverse of matrix 
        ###############################################################
        i <- NULL
        
        ###############################################################        
        #set the input matrix, and initial the inverse                #
        ###############################################################
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ###############################################################
        #get the input matrix                                         #
        ###############################################################
        get <- function() x                                           #
                                                                      #
        ###############################################################
        #set the the inverse of matrix, will be re-used in cacheSolve #
        #function                                                     #
        ###############################################################
        setsolve <- function(solve) i <<- solve                       #
                                                                      #
        ###############################################################
        #get the inverse of matrix. Before the first time calling the #
        #setsolve function, the inverse of matrix will be null        #
        ###############################################################
        getsolve <- function() i
        
        ###############################################################
        #return the definition of the makeCacheMatrix function        #
        ###############################################################
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
        ###############################################################
        #try to get the inverse of matrix first                       #
        ###############################################################
        i <- x$getsolve()
        
        ###############################################################
        #if already existing, return the cached inverse matrix        #
        ###############################################################
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        ###############################################################
        #if not existing, call sovle function got get inverse matrix  #
        #call setsolve function of the original matrix to cache the   #
        #the inverse of it, return the inverse maxtrix                #
        ###############################################################
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i
}