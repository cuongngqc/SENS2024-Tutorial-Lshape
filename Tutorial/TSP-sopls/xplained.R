xplained <- function(object){
  
  Xcat <- scale(do.call(cbind, object$data$X), scale=FALSE)
  
  P <- crossprod(object$decomp$T, Xcat)
  
  ssxx <- 100 * apply(P^2,1,sum)/sum(Xcat^2)
  
  xve <- ssxx
  
  for(i in 1:length(xve)){
    
    xve[i] <- sum(ssxx[multiblock:::pathComp(object$decomp$compList[i,], object$decomp$compList)$hits])
    
  }
  
  xve
  
}