\name{remove.identical.rows}
\alias{remove.identical.rows}
\title{Remove duplicated rows}
\usage{
  remove.identical.rows(data, id = NULL)
}
\arguments{
  \item{data}{a data frame.}

  \item{id}{Index (or names) of the id variables of
  \code{data}. By default all variables are considered.}
}
\value{
  The updated data frame.
}
\description{
  This function removes any duplicated rows.
}

