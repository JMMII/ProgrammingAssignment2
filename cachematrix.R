#Taking the inverse of a matrix can be time consuming. The following two 
#functions work to reduce computation time by checking whether the inverse of a matrix
#has already been computed and cached. If so, cachSolve() provides the matrix inverse as output
#without recalculating it.

#Steps to use these functions:
#Step 1: execute makeCacheMatrix() using an invertible matrix as input and store in fnList. Running this first for each new matrix initializes the inverse as NULL 
#Step 2: execute cacheSolve(fnList). This first run sees the inverse as NULL, so it calculates the inverse.
#Step 3: execute cacheSolve(fnlist) a second time. With this second run, cachSolve() sees the inverse is not NULL, so pulls it directly, rather than recomputing it.
#Step 4: if you want to try it again on a new matrix, return to step 1 using the new matrix.

#The first function, makeCacheMatrix(), initializes the inverse as NULL, caches this and caches the input matrix

makeCacheMatrix <- function(mat = matrix()) {     
        
        inv <- NULL       #initiates inv and sets inv to NULL as a default
        
        setMatrix <- function(y) {    #creates subfunction, setMatrix(), with argument y. This will be used later to compare the current matrix with an incoming matrix
                mat <<- y         #caches y into mat, which is the current matrix
                inv <<- NULL      #caches NULL into inv. When you use setMatrix,it willreinitialize inv.
        }                       
        
        getMatrix <- function() {       #creates function to return the inputted matrix from makeCacheMatrix()
                mat
        }
        
        setInverse <- function(inverse) { #creates function to take a matrix inverse and cache it into inv
                inv <<- inverse
        }
        
        getInverse <- function() {      #creates function to output inv (the inverse of mat) which has the inverse stored in it
                inv
        }
        
        list(setMatrix = setMatrix,    #outputs the functions just created into a list
             getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}

#The following function, cacheSolve(), solves the inverse of a matrix, but
#only recomputing the inverse of mat if the matrix mat is different from the input matrix

cacheSolve <- function(fList, ...) { 
        inv <- fList$getInverse()     #if inverse of mat has already been calculated, this stores it into inv. Otherwise, inv gets NULL
        
        if(!is.null(inv)) {       #if inv is not NULL, then inv has an inverse in it that we need to check
                message("getting cached inverse")       #message explaining cached inverse being pulled   
                return(inv)       #returns the cached inverse
        }
                y <- fList$getMatrix()      #run getMatrix to get value of input matrix
                fList$setMatrix(y)          #run setMatrix on the input matrix to cache it                
                inv <- solve(y, ...)         #compute the value of the inverse of the input matrix and store in inv
                fList$setInverse(inv)       #run the setInverse function on inv to cache the inverse just computed
                return(inv)               #return matrix that is inverse of mat
}