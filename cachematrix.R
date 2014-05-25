## Take a matrix, store it and create an inverted matrix.  Cache the inverted
## matrix such that it isn't recalculated unless the original matrix
## has changed


# Returns a list that acts as the container for the matrix and cached solved
# matrix
makeCacheMatrix <- function(x=matrix()) {
  
  cached_solved_matrix <- NULL 
  original_matrix <- matrix 
  
  ## To me it makes the most sense to create and cache the solved matrix here.
  ## When set, if the given matrix isn't equal to the existing matrix
  ## or the cached solved matrix doesn't exist - create it and cache it.
  set_matrix <- function(matrix) {
  
    # sanity check
    matrix = as.matrix(matrix)
    
    if (!identical(matrix,original_matrix) || is.null(cached_solved_matrix)) {
      set_solved_matrix(cacheSolve(matrix))
      original_matrix <<- matrix  
    }
    else {
      original_matrix <<- matrix
      solved_matrix <<- NULL
    }
  }
  
  get_matrix <- function() {
    original_matrix
  }
  
  set_solved_matrix <- function(matrix) {
    cached_solved_matrix <<- matrix
  }
  
  get_solved_matrix <- function() {
    cached_solved_matrix
  }

  # Gets everything set up
  set_matrix(x) 
  
  ## Return the container - left out set_cached_matrix as it should be
  ## a private method
  list(set_matrix=set_matrix, get_matrix=get_matrix,
       get_cache=get_solved_matrix)
}


## Take a matrix and return its inverse
cacheSolve <- function(x, ...) {
  # print just for testing to insure it's not recalulating when I don't expect
  print("Calculating Inverse")
  solve(x, ...)
}


##  Test the code
test <- function() {
  test_matrix <- matrix(1:4,2,2)
  print(test_matrix)
  print("Calling makeCacheMatrix - Should see 'Calculating Inverse' next and not again")
  test_cache_matrix = makeCacheMatrix(test_matrix)
  print("Original matrix is: ")
  print(test_cache_matrix$get_matrix())
  print("Inverted matrix is: ")
  print(test_cache_matrix$get_cache())
  
  print("And let's set a new matrix, should see 'Calculating Inverse' again")
  test_matrix_2 = matrix(8:11,2,2)
  test_cache_matrix$set_matrix(test_matrix_2)
  print("And the new matrix: ")
  print(test_cache_matrix$get_matrix())
  print("And the new inverted matrix: ")
  print(test_cache_matrix$get_cache())
  print("Reassigning the same matrix, should NOT see 'Calculating Inverse")
  test_cache_matrix$set_matrix(test_matrix_2)
  print("and verify the inverted is the same: ")
  print(test_cache_matrix$get_cache())
}
