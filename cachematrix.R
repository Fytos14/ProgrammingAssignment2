
makeCacheMatrix<-function(x=matrix()){
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  } ##setting the different environment for x to be calculated and stored
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,setmatrix=setmatrix,getmatrix=getmatrix) ##lists alla commands in function
}

##function to used cached stored information to return the answer
cacheSolve <- function(x, ...) {
  inv<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    inv
  } ##control that this is not NA therefore it will bring back a number
  matrix<-x$get()
  inv<-solve(matrix, ...) ##calculates the inverse
  x$setmatrix(inv)
  inv ##returns the inverse thus the answer
}
