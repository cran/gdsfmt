\name{put.attr.gdsn}
\alias{put.attr.gdsn}
\title{Add an attribute into a GDS file}
\description{
	Adds an attribute to a GDS node, in a CoreArray Genomic Data Structure (GDS) file.
}

\usage{
put.attr.gdsn(node, name, val = NULL)
}
\arguments{
	\item{node}{an object of class \code{\link[gdsfmt]{gdsn.class}}, a GDS node}
	\item{name}{the name of an attribute}
	\item{val}{the value of an attribute}
}

\references{\url{http://sourceforge.net/projects/corearray/}}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{
	\code{\link[gdsfmt]{get.attr.gdsn}}, \code{\link[gdsfmt]{delete.attr.gdsn}}
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
