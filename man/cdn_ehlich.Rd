% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cdn_ehlich.R
\name{cdn_ehlich}
\alias{cdn_ehlich}
\title{cdn_ehlich}
\usage{
cdn_ehlich(order)
}
\arguments{
\item{order}{integer}
}
\value{
4 or NULL
}
\description{
Checks Hadamard Matrix can be constructed using Ehlich's method.
}
\details{
Ehlich (1965)'s construction method requires order of the Hadamard
matrix must be a of the form (n-1)^2. Conditions are (i)Order=(n-1)^2;
(ii) n is a prime or prime power and 3(mod 4). (iii) (n-2) must be a
prime or prime power. In case, if all the three conditions are satisfied,
function will return 4 or NULL.
}
\examples{
cdn_ehlich(36)
#Condition 1:(n-1)^2 = 36 = 6^2
#Condition 2: n=7 (prime)and n=3(mod 4)
#Condition 3: n-2=5 (prime)
#Return
#4
cdn_ehlich(64)
#Condition 1:(n-1)^2 = 64 = 8^2
#Condition 2: n=9 (prime power) but n=1(mod 4).
#Condition 2 fails
#Return
#NULL
}
\references{
Ehlich, H. (1965). Neue Hadamard-matrizen. Arch. Math., 16, 34-36.
}
\seealso{
\code{\link{had_ehlich}} for Ehlich's construction method.
}
