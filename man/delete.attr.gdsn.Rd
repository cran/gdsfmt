\name{delete.attr.gdsn}
\alias{delete.attr.gdsn}
\title{Delete an attribute}
\description{
	Removes an attribute of a GDS node.
}

\usage{
delete.attr.gdsn(node, name)
}
\arguments{
	\item{node}{an object of class \code{\link[gdsfmt]{gdsn.class}}, a GDS node}
	\item{name}{the name of an attribute}
}
\details{
	If there is not an attribute with \code{name}, a warning will be given.
}

\references{\url{http://sourceforge.net/projects/corearray/}}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{
	\code{\link[gdsfmt]{put.attr.gdsn}}, \code{\link[gdsfmt]{get.attr.gdsn}}
}

\keyword{GDS}
\keyword{utilities}
