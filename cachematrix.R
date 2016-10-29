## Put comments here that give an overall description of what your
## functions do

## the makeCacheMatrix function defines a makeCacheMatrix()
## environment under the global enrionment and defines four
## functions that can be accessed by subsequent command.
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL #initalize storage matrix
  
  #define set() function
  set<-function(y){
    x<<-y  #assign the input y to x object in the parent environment
    m<<-NULL #assign the value of NULL to m object in the paraent environment
  }
  get<-function() x #retrieve matrix x from parent envrionment
  setmatrix<-function(solve) m<<- solve  #setter for the inverse
  getmatrix<-function() m #getter for the inverse
  
  #Assign each of the previous four functions as an element with a list()
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## cacheSolve () function will retrieve input matrix under makeCacheMatrix()
## environment and returns the inverse for that input matrix
## 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getmatrix() #access the getmatrix() function in MakeCacheMatrix
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  datos<-x$get() #use get() function in MakeCasheMatrix to retrieve input matrix
  m<-solve(datos, ...) #solve for the inverse of the input matrix
  x$setmatrix(m) #get the inverse matrix
  m #return the inverse matrix
}

#make up a matrix and try if the code works well
cacheSolve(makeCacheMatrix(x=rbind(c(1, -1/4), c(-1/4, 1)) ))
#the following is the inverse for the input matrix. Yes,the functions work well.
 #      [,1]      [,2]
#[1,] 1.0666667 0.2666667
#[2,] 0.2666667 1.0666667
