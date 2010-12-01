\name{put.attr.gdsn}
\alias{put.attr.gdsn}
\title{Put an attribute into a GDS file}
\description{
  Puts an attribute in a CoreArray Generic Data Structures (GDS) file.
}

\usage{
  put.attr.gdsn(node, name, val = NULL)
}
\arguments{
  \item{node}{An object of class "gdsn", the node of hierarchical structures.}
  \item{name}{Name of an attribute.}
  \item{val}{Value of an attribute.}
}

\references{}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{ \code{\link[gdsfmt]{get.attr.gdsn}},
  \code{\link[gdsfmt]{delete.attr.gdsn}} }

\examples{
library(gdsfmt)

# cteate the GDS file "test.gds"
(f <- createfn.gds("test.gds"))

node <- add.gdsn(f, "int", val=as.integer(1:10000))
put.attr.gdsn(node, "misval", 10000)

f
get.attr.gdsn(node)

closefn.gds(f)
}

\keyword{utilities}
