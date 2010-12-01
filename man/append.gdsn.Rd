\name{append.gdsn}
\alias{append.gdsn}
\title{Append New Data to Data Field}
\description{
    Appends new data to the data field for a GDS node of hierarchical
  structures, in a CoreArray Generic Data Structures (GDS) file.
}

\usage{
  append.gdsn(node, val, check = TRUE)
}
\arguments{
  \item{node}{An object of class "gdsn", the node of hierarchical structures.}
  \item{val}{Vector of data data appended.}
  \item{check}{Whether a warning is given, when appended data can not match
    the capability of data field.}
}
\details{
    \code{storage.mode(val)} should be "integer", "double", "character", or
  "logical".
}
\value{
  None.
}

\references{}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{ \code{\link[gdsfmt]{read.gdsn}}, \code{\link[gdsfmt]{write.gdsn}},
  \code{\link[gdsfmt]{add.gdsn}} }

\examples{
library(gdsfmt)

# cteate the GDS file "test.gds"
(f <- createfn.gds("test.gds"))

# commom types
n <- add.gdsn(f, "int", val=matrix(as.integer(1:10000), nrow=100, ncol=100),
  compress="ZIP")
# no warning
append.gdsn(n, -1:-100)
f
# a warning
append.gdsn(n, -1:-50)
f

closefn.gds(f)
}

\keyword{utilities}
