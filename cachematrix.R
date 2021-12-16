## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x,D) {
  ##B=inverse of matrix
  B<-NULL
  ## 
  intro<-function() A {
    x<<-A
    B<<-NULL
  }
  grab<-function() x
  ## calcinv= calculates the inverse of matrix
  calcinv<-function() B<<-solve(x)
  ## showinv= inverse of matrix
  showinv<-function B
  
 list(intro=intro,grab=grab,calcinv=calcinv,showinv=showinv)
 
}
####################################################
cacheSolve <- function(x,...){
    B<-x$showinv()
  if(!is.null(B)){
      return(B)
    }
    X <- x$intro()
    inv <- solve(X, ...)
    x$calcinv(B)
    B
    
}
