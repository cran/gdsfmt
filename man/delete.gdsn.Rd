\name{delete.gdsn}
\alias{delete.gdsn}
\title{Delete a GDS node}
\description{
	To delete a specified node, in a CoreArray Genomic Data Structure (GDS) file.
}

\usage{
delete.gdsn(node)
}
\arguments{
	\item{node}{an object of class \code{\link[gdsfmt]{gdsn}}, the node of hierarchical structure.}
}

\references{\url{http://sourceforge.net/projects/corearray/}}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{\code{\link[gdsfmt]{cnt.gdsn}}, \code{\link[gdsfmt]{objdesp.gdsn}},
	\code{\link[gdsfmt]{ls.gdsn}}, \code{\link[gdsfmt]{index.gdsn}},
	\code{\link[gdsfmt]{name.gdsn}}, \code{\link[gdsfmt]{add.gdsn}}}

\examples{
library(gdsfmt)

# cteate the GDS file "test.gds"
f <- createfn.gds(fn="test.gds")

# add a list to "test.gds"
node <- add.gdsn(f, name="list", val=list(x=c(1,2), y=c("T", "B", "C"), z=TRUE))
f

# delete "node"
delete.gdsn(node)

closefn.gds(f)
}

\keyword{utilities}
