% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GFCheck.R
\name{GFCheck}
\alias{GFCheck}
\title{GFCheck}
\usage{
GFCheck(GFElem, r, cardin, add)
}
\arguments{
\item{GFElem}{integer array}

\item{r}{integer}

\item{cardin}{integer}

\item{add}{integer array}
}
\value{
i integer
The position of the element checked in GFElem
}
\description{
This is an internal function to return the position of argument add in elements of GF(cardin)
}
\details{
This function is not exported. Used for checking the result of addition or multiplication of GFElements.
}
