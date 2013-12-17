#' @title Scatter plot using ggplot2
#' @rdname ggscatter.xy
#' @param x the x-coordinates of points in the plot
#' @param y the y-coordinates of points in the plot
#' @param color the color of points in the plot (\code{color} should be the same length as \code{x}, or of length 1).
#' @param shape the shape of points in the plot (\code{color} should be the same length as \code{x}, or of length 1).
#' @param size the size of points in the plot (\code{color} should be the same length as \code{x}, or of length 1).
#' @param obsnames the names of each point. Providing \code{obsnames} can be useful when modifying the plot at a later
#' stage.
#' @param ... additional arguments for \code{\link{geom_point}}.
#' @return A ggplot2 object.
#' @export
#' @examples
#' \donttest{
#' x=mtcars$wt
#' y=mtcars$disp
#' color=as.factor(mtcars$cyl)
#' ggscatter.xy(x,y,color)
#' M=cbind(x,y)
#' ggscatter(M,color="red",shape=as.factor(mtcars$am))
#' }
ggscatter.xy <- function(x,y,color,shape,size,alpha,fill,obsnames,...) {
  if(missing(obsnames)) obsnames=seq(length(x))
  data <- data.frame(obsnames=obsnames,x=x,y=y)
  
  aes.args=list()
  args=list()
  if(!missing(color)){
    if(length(color)==1){args=c(args,list(color=color))}
    else{data=cbind(data,color=color)
         aes.args=c(aes.args,list(color="color"))}
  }
  
  if(!missing(shape)){
    if(length(shape)==1){args=c(args,list(shape=shape))}
    else{data=cbind(data,shape=shape)
         aes.args=c(aes.args,list(shape="shape"))}
  }
  
  if(!missing(size)){
    if(length(size)==1){args=c(args,list(size=size))}
    else{data=cbind(data,size=size)
         aes.args=c(aes.args,list(size="size"))}
  }
  
  if(!missing(alpha)){
    if(length(alpha)==1){args=c(args,list(alpha=alpha))}
    else{data=cbind(data,alpha=alpha)
         aes.args=c(aes.args,list(alpha="alpha"))}
  }
  
  if(!missing(fill)){
    if(length(fill)==1){args=c(args,list(fill=fill))}
    else{data=cbind(data,fill=fill)
         aes.args=c(aes.args,list(fill="fill"))}
  }
  
  mapping=NULL
  if(length(aes.args)!=0)mapping=do.call(aes_string,aes.args)
  geom_point.args=c(list(mapping=mapping),args,list(...))
  ggplot(data, aes(x=x, y=y))+do.call(geom_point,geom_point.args)
}

#' @rdname ggscatter.xy
#' @export
ggscatter <- function(data,color,shape,size,obsnames,...){
  ggscatter.xy(x=data[,1],y=data[,2],color=color,shape=shape,size=size,obsnames,...)
}