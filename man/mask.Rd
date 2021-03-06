% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write.R
\name{mask}
\alias{mask}
\alias{mask.default}
\alias{mask.mlth.data.frame}
\title{Specify a Matrix Mask}
\usage{
mask(x, ...)

\method{mask}{default}(x, rows = numeric(0), cols = numeric(0),
  singleCells = NULL)

\method{mask}{mlth.data.frame}(x, rows = numeric(0), cols = numeric(0),
  singleCells = NULL, header = FALSE, rn = FALSE)
}
\arguments{
\item{x}{\code{matrix} or \code{data.frame} or \code{mlth.data.frame}.}

\item{rows, cols}{rows and columns to be selected.}

\item{singleCells}{the list of single cells (or cell sets) to be selected.
Each element of \code{singleCells} must be a two-element vector specifying
rows and columns of the cells.}

\item{header}{a logical value indicating if the header rows should be appended to the mask.}

\item{rn}{a logical value indicating if the row names should be appended to the mask.}
}
\description{
Specify a mask based on the matrix or matrix-like object.
}
\details{
\code{rows}, \code{cols} and \code{singleCells} must take into account if header and row names are included.
E.g., \code{cols=2} indicates the second column of the matrix when \code{rn=FALSE} and the first column when \code{rn=TRUE}.
}
\examples{
L<-mlth(X=c('A','B','C'),
		Y=list(N=1:3,M=4:6))
mask(L)
mask(L,h=TRUE)
mask(L,c=2,rows=3)
mask(L,c=2,rows=3,h=TRUE)

}

