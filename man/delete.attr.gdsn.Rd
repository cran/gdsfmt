\name{delete.attr.gdsn}
\alias{delete.attr.gdsn}
\title{Delete an attribute}
\description{
  Deletes an attribute from a CoreArray GDS node.
}

\usage{
  delete.attr.gdsn(node, name)
}
\arguments{
  \item{node}{An object of class "gdsn", the node of hierarchical structures.}
  \item{name}{Name of an attribute.}
}
\details{
  If there is not an attribute with \code{name}, a warning will be given.
}

\references{\url{http://sourceforge.net/projects/corearray/}}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{ \code{\link[gdsfmt]{put.attr.gdsn}},
  \code{\link[gdsfmt]{get.attr.gdsn}} }

\keyword{utilities}
