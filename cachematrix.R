## Week #3 programming quiz on Coursera R lang course
## Two functions illustrating lexical scoping:
##  makeCacheMatrix function builds closure with two variables and getter/setter methods
##  cacheSolve function uses pseudo-object returned by makeCacheMatrix and either retuns or calculates second member variable
## AK 06.2018


## this function constructs pseudo-object capable of caching matrix and its inverted value
makeCacheMatrix <- function(ma = matrix()) {
  # init cached value of inverted matrix
  ma_inverted <- NULL
  
  #member function: set new matrix value
  set <- function(new_ma) {
    # matrix comparison adapted from https://stat.ethz.ch/pipermail/r-help/2012-June/315408.html
    if (!(is.matrix(ma) &&
          is.matrix(new_ma) &&
          dim(ma) == dim(new_ma) && all(ma == new_ma))) {
      #if new matrix differs from the old one - reset cache
      ma_inverted <<- NULL
    }
    ma <<- new_ma
  }
  
  #member function: get existing matrix value
  get <- function() {
    return(ma)
  }
  
  #member function: set new inverted value of the matrix
  set_ma_inverted <-
    function(new_ma_inverted) {
      ma_inverted <<- new_ma_inverted
    }
  
  #member function: get inverted value of the matrix
  get_ma_inverted <- function() {
    return(ma_inverted)
  }
  
  #construct pseudo-object and return as a list
  ret <-
    list(
      set = set,
      get = get,
      set_ma_inverted = set_ma_inverted,
      get_ma_inverted = get_ma_inverted
    )
  
  return(ret)
  
}


## Return a matrix that is the inverse of the matrix_cache$get()
cacheSolve <- function(matrix_cache, ..., silent = FALSE) {
  mai <- matrix_cache$get_ma_inverted()
  if (is.null(mai)) {
    #cache miss
    if (silent == FALSE) message("calculating...")
    ma <- matrix_cache$get()
    mai <- solve(ma, ...)
    matrix_cache$set_ma_inverted(mai)
    if (silent == FALSE) message("...done")
  }
  else {
    if (silent == FALSE) message("cache hit!")
  }
  return(mai)
  
}


## TEST above functions
TEST <- function(n = 10) {

   ## TEST makeCacheMatrix
  set.seed(179)
  ma <- matrix(runif(n * n, min = 1, max = 99), ncol = n)
  mac <- makeCacheMatrix(ma)
  set.seed(2018)
  ma2 <- matrix(runif(n * n, min = 1, max = 99), ncol = n)
  message("testing makeCacheMatrix() -- passed")
  
  
  ## TEST SET() AND GET()
  if(all.equal(ma, mac$get()))  { message("testing get() -- passed") 
  } else { message("testing get() -- failed") }
  
  mac$set(ma2)
  
  if(all.equal(ma2, mac$get())) { message("testing set()- passed") 
  } else { message("testing set() -- failed") }
  
  
  ## TEST set_ma_inverted and get_ma_inverted
  if (is.null(mac$get_ma_inverted())) {
    message("testing get_ma_inverted()==NULL -- passed")
  } else { message("testing get_ma_inverted()==NULL -- failed") }
  
  with(mac, set_ma_inverted(solve(get())))
  if (all.equal(solve(ma2), mac$get_ma_inverted())) { message("testing set_ma_inverted() -- passed")
  } else { message("testing set_ma_inverted() -- failed") }
  
  
  ## TEST cache reset
  mac$set(ma)
  if (is.null(mac$get_ma_inverted())) { message("testing cache reset -- passed")
  } else { message("testing cache reset -- failed") }
  
  
  ## TEST cacheSolve()
  mai <- cacheSolve(mac)
  if (all.equal(ma %*% mai, diag(n))) { message("testing cacheSolve() -- passed")
  } else { message("testing cacheSolve() -- failed") }
  
  
  ## benchmark cache
  if(FALSE == ("microbenchmark" %in% rownames(installed.packages()))) {
    message("microbenchmark package is not installed - skipping further tests")
    return()
  }
  
  require("microbenchmark")
  mac$set(ma2)
  mbm = microbenchmark(
    base = solve(ma2),
    cacheSolve = cacheSolve(mac, silent = TRUE),
    times=1e2
  )
  message("benchmarking solve() vs cacheSolve() :")
  print(mbm)
}
  