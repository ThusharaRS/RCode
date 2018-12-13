#'trignometry
#'@param radians
#'@return value
#'@export





trigo <- function(x,MY_WL1){
 
 
MY_WL<-unlist(MY_WL1)
WL <- matrix(c(MY_WL),ncol = 2, byrow = TRUE)
 print(WL[1])
return(sin(x) + cos(x))


}
