#' @title Format data to a PCA transformation
#' @param data the data frame to transform.
#' @param pca object of class \code{\link{prcomp}}.
#' @param id the index of the id columns of \code{data}.
#' @return A data frame representing the dataset \code{data} transformed according to \code{pca}.
#' @export
format_pca.to.data=function(pca.data,pca){  
  data.num=pca.data%*%t(pca$rotation)
  if(!is.logical(pca$scale)) data.num=data.num*rep(pca$scale,each=nrow(data.num))
  if(!is.logical(pca$center)) data.num=data.num+rep(pca$center,each=nrow(data.num))
  data.num
}

# format_data.to.pca behaves exactly as predict.prcomp
# format_data.to.pca=function(data,pca,id=1){
#   data.num=data[setdiff(seq(data),id)]
#   if(!is.logical(pca$center)) data.num=data.num-rep(pca$center,each=nrow(data.num))
#   if(!is.logical(pca$scale)) data.num=data.num/rep(pca$scale,each=nrow(data.num))
#   cbind(data[id],as.data.frame(as.matrix(data.num)%*%(pca$rotation)))
# }