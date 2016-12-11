f <- function(j,k,r){
  j^(k-r)
}

emf <- function(j,r){
  exp(j-r)
}

genmatrix <- function(s,r){
  m <- matrix(0,s,s)
  for(i in c(1:s)){
    for(j in c(0:s-1)){
      m[i,j+1]=f(i,j,r) }
  }
  m
}

genemmatrix <- function(s,r){
  m<- matrix(0,s,1)
  for(i in c(1:s)){
    m[i,1]=emf(i,r)
  }
  m
} 

gengammatrix <- function(s,r){
  m<-matrix(0,s,1)
  for(i in c(1:s)){
    m[i,1]=1/gamma(i-r+1)
  }
  m
}

s=9
m<-genmatrix(s,0)
em<-genemmatrix(s,0)
gm<-gengammatrix(s,0)
gmish<-solve(m)%*%em
#em=m(r)*[fi(0-1/2)`fi(1-1/2)`....`fi(n-1/2)`.....]
#fi is 1/gamma ish #m(r)^(-1)*em=?=[1/gamma(0-1/2+1)`1/gamma(1-1/2+2)`....]
 
#mv=c(2:10)
#for(i in c(2:10)){
#  mv[i]=solve(genmatrix(i,0))*genemmatrix(i,0)
#}
