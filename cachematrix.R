#Function 1= makeCacheMatrix

#IN SUMMARY;makeCacheMatrix
#creates a special "vector" that
#sets the value of the vector
#gets the value of the vector
#sets the value of the mean
#get the value of the mean

#Function 2 = cacheSolve

#IN SUMMARY;cacheSolve
#Calculates the mean of the vector above HOWEVER it first
#(a)checks to see if the mean has already been calculated & fetches
#it from the cache. 
#(b)If not, it calculates the mean & sets the value of the mean in the cache. 


#Function 1: makeCache Matrix will create a special "matrix" object that can cache the inverse of the matrix
#that I will have created

makeCacheMatrix<-function(c=matrix()){
  inv<-NULL
  set<-function(d){
    c<<-d
    inv<<-NULL
  }
  get<-function()c
  
  setinverse<-function(inverse)inv<<-inverse
  getinverse<-function()inv
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}
makeCacheMatrix(matrix(1:4, ncol = 2))
#The function above is supposed to set and get the values of both the matrix and the inverse of the matrix


##Function 2;cacheSolve is supposed to calculate the inverse of the matrix created using the first function

cacheSolve<-function(c,...){
  #  so, object c is meant to be this matrix object
  inv<-c$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
    #the above code will return the inverse of the function from the cache
  }
  data<-c$get()#it will get the original matrix data
  inv<-solve(data,...)#the solve function will inverse the matrix
  c$setinverse(inv)
  inv
}

cacheMatrix <- makeCacheMatrix(matrix(1:4, ncol = 2))
cacheSolve(cacheMatrix)
#The code above will calculate the inverse of the matrix incase none is present within the cache
