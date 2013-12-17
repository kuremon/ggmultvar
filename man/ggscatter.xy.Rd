\name{ggscatter.xy}
\alias{ggscatter}
\alias{ggscatter.xy}
\title{Scatter plot using ggplot2}
\usage{
  ggscatter.xy(x, y, color, shape, size, alpha, fill,
    obsnames, ...)

  ggscatter(data, color, shape, size, obsnames, ...)
}
\arguments{
  \item{x}{the x-coordinates of points in the plot}

  \item{y}{the y-coordinates of points in the plot}

  \item{color}{the color of points in the plot
  (\code{color} should be the same length as \code{x}, or
  of length 1).}

  \item{shape}{the shape of points in the plot
  (\code{color} should be the same length as \code{x}, or
  of length 1).}

  \item{size}{the size of points in the plot (\code{color}
  should be the same length as \code{x}, or of length 1).}

  \item{obsnames}{the names of each point. Providing
  \code{obsnames} can be useful when modifying the plot at
  a later stage.}

  \item{...}{additional arguments for
  \code{\link{geom_point}}.}
}
\value{
  A ggplot2 object.
}
\description{
  Scatter plot using ggplot2
}
\examples{
\donttest{
x=mtcars$wt
y=mtcars$disp
color=as.factor(mtcars$cyl)
ggscatter.xy(x,y,color)
M=cbind(x,y)
ggscatter(M,color="red",shape=as.factor(mtcars$am))
}
}

