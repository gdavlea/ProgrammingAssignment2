## Functions to return the inverse of a matrix. Will cache the
## result, and use cached version on subsequent calls if it 
## exists and matrix is unchanged.  Otherwise will calculate again.


makeCacheMatrix <- function(x = matrix()) {
        Invrs<-NULL
        set<-function(y){
                x<<-y
                Invrs<<-NULL
        }
        get<-function() x
        setNeo<-function(doInvert) Invrs<<- doInvert
        getNeo<-function() Invrs
        list(set=set, get=get,
             setNeo=setNeo,
             getNeo=getNeo)
}

cacheSolve <- function(x=matrix(), ...) {
        Invrs<-x$getNeo()
        if(!is.null(Invrs)){
                message("Getting cached data.")
                return(Invrs)
        }
        matrix<-x$get()
        Invrs<-solve(matrix, ...)
        x$setNeo(Invrs)
        Invrs
}
## I kept creating matrices that couldn't be inverted, and I 
## didn't trust my 2x2 results. Using random numbers for the
## matrix seemed to eliminate the errors. So start with inputting 
## the size of your matrix. This example will create an 8x8. Change 
## '8' to whatever you want, though you might get max.print errors 
## if you go too high:
## > matrix_size <- c(8)
## Then create a random dataset:
## > matrix_data <- runif(n=matrix_size^2,min=1,max=10)
## Define "a" and create the actual matrix:
## > a <- makeCacheMatrix()
## > a$set(matrix(matrix_data,matrix_size,matrix_size))
## And finally invert it, using the cached version if it exists and
## matrix is unchanged, otherwise calculating:
## > cacheSolve(a)