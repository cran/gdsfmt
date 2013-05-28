\name{get.attr.gdsn}
\alias{get.attr.gdsn}
\title{Get attributes}
\description{
	Gets the attributes of a CoreArray GDS node.
}

\usage{
get.attr.gdsn(node)
}
\arguments{
	\item{node}{an object of class \code{\link[gdsfmt]{gdsn.class}}, a GDS node}
}
\value{
	A list of attributes.
}

\references{\url{http://sourceforge.net/projects/corearray/}}
\author{Xiuwen Zheng}
\seealso{
	\code{\link[gdsfmt]{put.attr.gdsn}}, \code{\link[gdsfmt]{delete.attr.gdsn}}
}

\examples{
# cteate the GDS file "test.gds"
(f <- createfn.gds("test.gds"))

node <- add.gdsn(f, "int", val=as.integer(1:10000))
put.attr.gdsn(node, "missing.value", 10000)

f
get.attr.gdsn(node)

closefn.gds(f)
}

\keyword{GDS}
\keyword{utilities}
