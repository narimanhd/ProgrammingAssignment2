## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCachMatrix calculates the inverse of matrix and store the matrix and inverse in a list
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
## cacheSolve search whether the inverse matrix has already been calculated and the extract 
## the inverse matrix but if it was not already calculated it calculates the inverse matrix and add it to the list
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
##
##I passed the matrix "z1<-matrix(seq(1,20),2,2) to makeCashMatrix and calculated the inverse matrix "matrix(c(-2,1,1.5,-.5),2,2)" and then stored to the list
##then I passed z1 to the cacheSolve and it extracted the result from the list and did not add data to the list
## after I 
