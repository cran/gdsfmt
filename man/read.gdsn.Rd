\name{read.gdsn}
\alias{read.gdsn}
\title{Read data field of a GDS node}
\description{
    Reads data from a GDS node of hierarchical structures, in a CoreArray
  Generic Data Structures (GDS) file.
}

\usage{
  read.gdsn(node, start, count)
}
\arguments{
  \item{node}{An object of class "gdsn", the node of hierarchical structures.}
  \item{start}{A vector of integers, starting from 1 for each dimension.}
  \item{count}{A vector of integers, the length of each dimnension. As a
    special case, the value "-1" indicates that all entries along that
    dimension should be written.}
}
\details{
    \code{start}, \code{count}: The values in data are taken to be those
  in the array with the leftmost subscript moving fastest.
}
\value{
  Returns an array, list, or data.frame.
}

\references{}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{ \code{\link[gdsfmt]{append.gdsn}}, \code{\link[gdsfmt]{write.gdsn}},
  \code{\link[gdsfmt]{add.gdsn}} }

\examples{
library(gdsfmt)

# cteate the GDS file "test.gds"
(f <- createfn.gds("test.gds"))
L <- -2500:2499

add.gdsn(f, "vector", val=1:128)
add.gdsn(f, "list", val=list(X=1:10, Y=seq(1, 10, 0.25)))
add.gdsn(f, "data.frame", val=data.frame(X=1:19, Y=seq(1, 10, 0.5)))

f

read.gdsn(index.gdsn(f, "vector"))
read.gdsn(index.gdsn(f, "list"))
read.gdsn(index.gdsn(f, "data.frame"))

closefn.gds(f)
}

\keyword{utilities}
