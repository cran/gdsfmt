\name{name.gdsn}
\alias{name.gdsn}
\title{Return the variable name of a node}
\description{
	Gets the variable name of a node, in a CoreArray Genomic Data Structure (GDS) file.
}

\usage{
name.gdsn(node, fullname = FALSE)
}
\arguments{
	\item{node}{an object of class \code{\link[gdsfmt]{gdsn.class}}, a GDS node}
	\item{fullname}{if FALSE, return the node name (by default), otherwise the full name}
}
\value{
	A vector of charactors.
}

\references{\url{http://sourceforge.net/projects/corearray/}}
\author{Xiuwen Zheng}
\seealso{
	\code{\link[gdsfmt]{cnt.gdsn}}, \code{\link[gdsfmt]{objdesp.gdsn}},
	\code{\link[gdsfmt]{ls.gdsn}}, \code{\link[gdsfmt]{rename.gdsn}}
}

\examples{
# cteate the GDS file "test.gds"
f <- createfn.gds("test.gds")

# add a list to "test.gds"
node <- add.gdsn(f, name="list", val=list(x=c(1,2), y=c("T", "B", "C"), z=TRUE))

name.gdsn(node)
# "list"

name.gdsn(node, fullname=TRUE)
# "test/list"

closefn.gds(f)
}

\keyword{GDS}
\keyword{utilities}
