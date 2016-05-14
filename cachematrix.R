## This function caching the inverse function
## There are very costly computational processes like the inverse of a matrix
## For this reason this function was created

## This function was created to caching its inverse

makeCacheMatrix <- function(x = matrix()){
        inversa = NULL
        grupo = function(y){
                x <<- y
                inversa <<- NULL
        }
        obtener <- function() x
        grupo_inver <- function(inverso_1) inversa <<- inverso_1
        obtener_inver <- function() inversa
        list(inversa=inversa, grupo=grupo, grupo_inver=grupo_inver
             , obtener_inver=obtener_inver, obtener=obtener)
}


## This function return the inverse matrix
## My function was made in spanish. Grupo = set, obtenet = get
cacheSolve <- function(x, ...){
        inversa = x$obtener_inver()
        if(!is.null(inversa)){
               return(inversa)
                
        }
        matr = x$obtener()
        inversa = solve(matr, ...)
        x$grupo_inver(inversa)
        inversa
}
