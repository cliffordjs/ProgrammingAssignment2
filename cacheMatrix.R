## The first function, makeCacheMatrix includes code to set a null matrix first
## which then goes on to populate 

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL # assigns a null value to object m (locally)
        set<-function(y){ ## the secondary function, set
                x<<-y ## assigns x (in the global environment) to y
                m<<-NULL ## assigns a null value to object m (globally)
        }
        get<-function() x ## sets the object get to the function
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get, # lists out the arguments
             setmatrix=setmatrix, ## assigns the setmatrix function
             getmatrix=getmatrix) ## assigns the getmatrix function
}

## the secondary function, cacheSolve, will take the above created matrix and apply the solve function
## or the transpose of that matrix

cacheSolve <- function(x=matrix(), ...) {
        m<-x$getmatrix() # loading data
        if(!is.null(m)){ # just to tell you that it foudn the data
                message("getting cached data")
                return(m)
        }
        dat<-x$get() # assigns matrix to the dat object
        m<-solve(dat, ...)  # gets the transpose of matrix dat and assigns new object m
        x$setmatrix(m) # sets the m to 
        m
}
