## First function creates a list representing a matrix. Each element of the list
## just points to the functions within the makeCacheMatrix function. This
## calling those functions like an attributes of a list...

## Second function simply checks if the inverse of a matrix is already cached in
## memory and if so, just simply returns the value. Otherwise the inverse of a
## matrix is computed and the value is stored inside an initial list-matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set_matrix <- function(y) {
        if (!identical(y, x)) {     # only continues if the new set matrix differs from the previous one
            x <<- y
            inv <<- NULL      # ensures that whenever the new matrix is set, the previous cached inverse
                              # will be deleted
        }
    }
    
    get_matrix <- function() x
    set_inv <- function(inverse) inv <<- inverse
    get_inv <- function() inv
    
    list(set_matrix = set_matrix,
         get_matrix = get_matrix,
         set_inv = set_inv,
         get_inv = get_inv)

}


## Checks if the x$get_inv returns NULL or not. If not, then it simply returns
## the already stored value, otherwise computes the inverse, stores it inside a
## matix-list and finally returns the value

cacheSolve <- function(x, ...) {
    i <- x$get_inv()
    if (!is.null(i)) {
        print('Cache value found and returned')
        return(i)
    }
    
    mat <- x$get_matrix()
    i <- solve(mat)
    x$set_inv(i)
    i
    
}
