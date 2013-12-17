\name{format_pca.to.data}
\alias{format_pca.to.data}
\title{Format data to a PCA transformation}
\usage{
  format_pca.to.data(pca.data, pca)
}
\arguments{
  \item{data}{the data frame to transform.}

  \item{pca}{object of class \code{\link{prcomp}}.}

  \item{id}{the index of the id columns of \code{data}.}
}
\value{
  A data frame representing the dataset \code{data}
  transformed according to \code{pca}.
}
\description{
  Format data to a PCA transformation
}

