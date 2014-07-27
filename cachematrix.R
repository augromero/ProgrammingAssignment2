## I get the inverse matrix from the cache if the x Matrix is equal to 
## the cache's input matrix, otherwise I calculate the inverse matrix.

## I Verify the existence of the cacheMatrix.
## I compare the cacheMatrix with the x matrix using the matEqual function.
## The matEqual function is built globally on the cacheSolve function.
## I return the cacheInverse matrix or an empty matrix if it's different.

makeCacheMatrix <- function(x = matrix()) {
        
        #Verifies the existance of the cacheMatrix (this is useful for the first time)
        if (!exists("cacheMatrix")){
                cacheMatrix <<- matrix()
                cacheInverse <<-matrix()
        }
        #Verifies if the matrices are equal, if they're not, set an empty inverse matrix
        # and stores the new input matrix in the cache.
        if(!matequal(x, cacheMatrix)){
                cacheMatrix <<- x
                cacheInverse <<-matrix()
        }
        #Returns the cacheInverse matrix or empty matrix
        cacheInverse
}


## Solve the inverse matrix or returns the cacheInverse 
 
cacheSolve <- function(x, ...) {
        #Checks whether the matrices are equal
        matequal <<- function(x, y){
                is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && !is.na(all(x == y)) && all(x == y)
        }
        
        matInverse <- makeCacheMatrix(x)
        
        # if the input matrix and the cacheMatrix are different, calculates the inverse matrix 
        if (!matequal(matInverse, cacheInverse)){
                cacheInverse <<- solve(x)
                matInverse <- cacheInverse
        }
        else{
                #retrive the cache inverse matrix
                print("Retrieving cache inverse matrix")
                matInverse <- cacheInverse
        }
        matInverse
}
