##These functions are meant to create an object that can be used to store a 
##matrix and cache it's inverse.

#MakeCacheMatrix() creates and stores(cache) a matrix
makeCacheMatrix <- function(x = matrix()) {
        #Initializing a local empty inverse matrix of x(local placeholder)
        invMat <- NULL        
        #Set() creates and store a matrix. The <<- assignment
        #means that the matrix and the inverse of it can be accessed 
        #outside the function i.e. nolonger just local variables
        set <- function(matrix){
                x <<- matrix
                invMat <<- NULL
        } 
        #Get() retrieves the stored matrix
        get <- function(){
                x
        }
        #setInverse() stores the inverse of the matrix
        setInverse <- function(inverse){
                invMat <<- inverse
        }
        #Get() retrieves a stored inverse of the matrix
        getInverse <- function(){
                invMat
        }
        #The output of makeCacheMatrix(), is a list of the fuctions
        #that would create and store a matrix as well as cache it's inverse
        list(set = set, get = get, 
             setInverse = setInverse,
             getInverse = getInverse)
}
#Note, to create a matrix, it would be advisable to create a variable first by
#making a call to the main function(makeCacheMatrix) and then using the variable
#as function name for the set() i.e. x <- makeCacheMatrix(); x$set(matrix)


# With the matrix stored in the earlier function, this function will retrieve  
# the cache or generate and cache it if it is not available.

cacheSolve <- function(x, ...) {
        # But first we check to see if there is a stored inverse of the matrix
        invMat <- x$getInverse()
        #If there is a cached inverse, it is retrieved and the function ends
        if(!is.null(invMat)){
                message("Getting cached inverse matrix...")
                return(invMat)
        }
        #However, if there is no cached inverse, then it is generated
        matrix <- x$get() #This gets the stored matrix
        invMat <- solve(matrix) #This generates the inverse
        x$setInverse(invMat) #The inverse is Cached
        invMat #The inverse matrix is returned
}