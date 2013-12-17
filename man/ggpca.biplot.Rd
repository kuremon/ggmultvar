\name{ggpca.biplot}
\alias{ggpca.biplot}
\alias{ggpca.biplot.prcomp}
\title{2D Plot for PCA result using ggplot2}
\usage{
  ggpca.biplot(data, rotation, color, shape, size,
    obsnames, arrows.to.keep, x1 = 1, x2 = 2)

  ggpca.biplot.prcomp(pca.fit, color, shape, size,
    obsnames, arrows.to.keep, x1 = "PC1", x2 = "PC2")
}
\arguments{
  \item{data}{PCA results coordinates.}

  \item{rotation}{PCA rotation matrix}

  \item{pca.fit}{object of class \code{\link{prcomp}}.}

  \item{color}{vector representing the color variable of
  the observations on which the pca was run.}

  \item{shape}{equivalent of \code{color} for shape
  values.}

  \item{size}{equivalent of \code{color} for size values.}

  \item{obsnames}{names of the observations.}

  \item{arrows.to.keep}{Specify \code{arrow.to.keep} when
  only a subset of the initial variables should appear as
  arrows.}

  \item{x1}{name or index of the PCA component to plot on
  the x-axis}

  \item{x2}{name or index of the PCA component to plot on
  the y-axis}
}
\value{
  A ggplot2 object.
}
\description{
  2D Plot for PCA result using ggplot2
}
\examples{
\donttest{
pca.mtcars=prcomp(mtcars,center=TRUE,scale=TRUE)
ggpca.biplot.prcomp(pca.mtcars,as.factor(mtcars$cyl),size=4)
ggpca.biplot.prcomp(pca.mtcars,as.factor(mtcars$cyl),size=4,arrows.to.keep=c("gear","disp","mpg"))
}
}
\seealso{
  \code{\link{ggpca.check}}
}

