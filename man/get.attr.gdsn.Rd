\name{get.attr.gdsn}
\alias{get.attr.gdsn}
\title{Get attributes}
\description{
  Gets attributes from a CoreArray GDS node.
}

\usage{
  get.attr.gdsn(node)
}
\arguments{
  \item{node}{An object of class "gdsn", the node of hierarchical structures.}
}
\value{
  Returns a list of attributes.
}

\references{}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{ \code{\link[gdsfmt]{put.attr.gdsn}},
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
