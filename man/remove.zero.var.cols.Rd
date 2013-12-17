\name{remove.zero.var.cols}
\alias{remove.zero.var.cols}
\title{Remove 0 variance columns in a data frame}
\usage{
  remove.zero.var.cols(data, num.vars = seq(data))
}
\arguments{
  \item{data}{a data frame.}

  \item{num.vars}{Index (or names) of the numeric variables
  of \code{data}. By default all variables are considered.}
}
\value{
  The updated data frame.
}
\description{
  This function detects and remove columns with 0 variance.
  This function is mainly used before multivariate analyis
  like PCA to ensure there is variability in all
  parameters.
}

