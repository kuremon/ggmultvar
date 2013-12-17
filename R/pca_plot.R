#' @title 2D Plot for PCA result using ggplot2
#' @rdname ggpca.biplot
#' @param data PCA results coordinates.
#' @param rotation PCA rotation matrix
#' @param pca.fit object of class \code{\link{prcomp}}.
#' @param color vector representing the color variable of the observations on which the pca was run.
#' @param shape equivalent of \code{color} for shape values.
#' @param size equivalent of \code{color} for size values.
#' @param obsnames names of the observations.
#' @param arrows.to.keep Specify \code{arrow.to.keep} when only a subset of the initial variables should appear as arrows.
#' @param x1 name or index of the PCA component to plot on the x-axis
#' @param x2 name or index of the PCA component to plot on the y-axis
#' @return A ggplot2 object.
#' @seealso \code{\link{ggpca.check}}
#' @export
ggpca.biplot <- function(data,rotation,color,shape,size,obsnames,arrows.to.keep,x1=1,x2=2) {
  if(missing(obsnames)) obsnames=row.names(data)
  x=data[,x1]
  y=data[,x2]
  plot <- ggscatter.xy(x=x,y=y,color=color,shape=shape,size=size,obsnames=obsnames)
    
  plot <- plot + geom_hline(aes(0), size=.1,color="darkgrey") + geom_vline(aes(0), size=.1,color="darkgrey")
  datapc <- data.frame(rotation)
  mult <- min(
    (max(y) - min(y)/(max(datapc[,x2])-min(datapc[,x2]))),
    (max(x) - min(x)/(max(datapc[,x1])-min(datapc[,x1])))
  )
  datapc$v1= .7 * mult *datapc[[x1]]
  datapc$v2= .7 * mult *datapc[[x2]]
  datapc$varnames=rownames(rotation)
  if(!missing(arrows.to.keep)) datapc=datapc[arrows.to.keep,]
  plot <- plot + coord_equal() + geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), size = 3, vjust=1, color="black")
  plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2),alpha=0.75, color="black",arrow=arrow(length=unit(0.2,"cm")))
}

#' @rdname ggpca.biplot
#' @export
#' @examples
#' \donttest{
#' pca.mtcars=prcomp(mtcars,center=TRUE,scale=TRUE)
#' ggpca.biplot.prcomp(pca.mtcars,as.factor(mtcars$cyl),size=4)
#' ggpca.biplot.prcomp(pca.mtcars,as.factor(mtcars$cyl),size=4,arrows.to.keep=c("gear","disp","mpg"))
#' }
ggpca.biplot.prcomp <- function(pca.fit,color,shape,size,obsnames,arrows.to.keep,x1="PC1",x2="PC2") {
  plot=ggpca.biplot(pca.fit$x,pca.fit$rotation,color,shape,size,obsnames,arrows.to.keep,x1,x2)
  if((x1=="PC1")&&(x2=="PC2")) plot=plot+labs(x="First component",y="Second component")
  plot
}

#' @title Check variance PCA plot (look for specific name for this function)
#' @param pca.fit object of class \code{prcomp}.
#' @return A ggplot2 object.
#' @seealso \code{\link{ggpca.biplot}}
#' @export
ggpca.check.prcomp=function(pca.fit){
  variance=pca.fit$sdev^2;
  df=data.frame(dimension=as.factor(seq(length(pca.fit$center))),y=cumsum(variance)/sum(variance));
  g.check=ggplot(data=df,aes(x=dimension,y=y))+geom_bar(stat="identity")+
    ylab("% explained of the variance");
  return(g.check);
}