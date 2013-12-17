\name{remove.na.rows}
\alias{remove.na.rows}
\title{Remove rows with NA}
\usage{
  remove.na.rows(data, num.vars = seq(data))
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
  This function detects and remove rows where there is at
  least one occurence of NA. This function is mainly used
  before multivariate analyis like PCA to ensure the data
  are NA-free. In addition a visualisation of the NA
  precense is given in order to check for the reliability
  of the rows removal.
}

