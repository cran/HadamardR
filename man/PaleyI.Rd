% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/paleyI.R
\name{PaleyI}
\alias{PaleyI}
\title{PaleyI}
\usage{
PaleyI(n)
}
\arguments{
\item{n}{integer (order of the matrix)}
}
\value{
hadamard matrix of n
}
\description{
This function performs constructing the Hadamard matrix by Paley method.
}
\details{
let q = n-1 ,  and q = 3 (mod 4), q is the prime number, then obtained the Hadamard
matrix of order q+1.if input satisfies these condition it retuns Hadamard Matrix; otherwise
returns NULL.
}
\examples{
PaleyI(8)
#' PaleyI(8)
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
#[1,]    1    1    1    1    1    1    1    1
#[2,]   -1    1   -1   -1    1   -1    1    1
#[3,]   -1    1    1   -1   -1    1   -1    1
#[4,]   -1    1    1    1   -1   -1    1   -1
#[5,]   -1   -1    1    1    1   -1   -1    1
#[6,]   -1    1   -1    1    1    1   -1   -1
#[7,]   -1   -1    1   -1    1    1    1   -1
#[8,]   -1   -1   -1    1   -1    1    1    1
PaleyI(16)
#NULL
}
\references{
Paley, R.E.A.C. (1933). On Orthogonal matrices. J. Combin. Theory, A 57(1), 86-108.
}
