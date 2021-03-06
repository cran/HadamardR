% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PaleyII.R
\name{PaleyII}
\alias{PaleyII}
\title{PaleyII}
\usage{
PaleyII(n)
}
\arguments{
\item{n}{integer(order of the matrix)}
}
\value{
Hadamard matrix of order n
}
\description{
This function create the Hadamard matrix by Paley method 2
}
\details{
q=n/2-1, If there is an Hadamard matrix of order h>1, and q = 1 (mod 4) is a prime number,
then there exists an Hadamard matrix of order nh.
}
\examples{
PaleyII(12)
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
# [1,]    1    1    1    1    1    1    1   -1   -1    -1    -1    -1
# [2,]    1    1    1   -1   -1    1   -1    1   -1     1     1    -1
# [3,]    1    1    1    1   -1   -1   -1   -1    1    -1     1     1
# [4,]    1   -1    1    1    1   -1   -1    1   -1     1    -1     1
# [5,]    1   -1   -1    1    1    1   -1    1    1    -1     1    -1
# [6,]    1    1   -1   -1    1    1   -1   -1    1     1    -1     1
# [7,]    1   -1   -1   -1   -1   -1   -1   -1   -1    -1    -1    -1
# [8,]   -1    1   -1    1    1   -1   -1   -1   -1     1     1    -1
# [9,]   -1   -1    1   -1    1    1   -1   -1   -1    -1     1     1
#[10,]   -1    1   -1    1   -1    1   -1    1   -1    -1    -1     1
#[11,]   -1    1    1   -1    1   -1   -1    1    1    -1    -1    -1
#[12,]   -1   -1    1    1   -1    1   -1   -1    1     1    -1    -1
PaleyII(8)
#NULL
}
\references{
Paley, R.E.A.C. (1933). On Orthogonal matrices. J. Combin. Theory, A 57(1), 86-108.
}
