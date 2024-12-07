makeCacheVector <- function(x = numeric()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setMean <- function(mean) cache <<- mean
  getMean <- function() cache
  list(set = set, get = get, setMean = setMean, getMean = getMean)
}

cacheMean <- function(x, ...) {
  m <- x$getMean()
  if (!is.null(m)) {
    message("Obtendo a média do cache")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setMean(m)
  m
}

vec <- makeCacheVector(c(1, 2, 3, 4, 5))
cacheMean(vec)
cacheMean(vec)
vec$set(c(10, 20, 30, 40, 50))
cacheMean(vec)
