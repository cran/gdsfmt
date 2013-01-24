\name{index.gdsn}
\alias{index.gdsn}
\title{Return the specified node}
\description{
	Returns a specified node, in a CoreArray Genomic Data Structure (GDS) file.
}

\usage{
index.gdsn(node, index, silent=FALSE)
}
\arguments{
	\item{node}{an object of class \code{\link[gdsfmt]{gdsn.class}} or \code{\link[gdsfmt]{gds.class}}:
		"gdsn" -- the node of hierarchical structure;
		"gdsclass" -- the root of hieracrchical structure}
	\item{index}{a vector of integers or characters, specifying the path}
	\item{silent}{if TRUE, return NULL if the specified path does not exist}
}
\details{
	If \code{index} is a vector of integers, e.g., \code{c(1, 2)}, the result is
the second child node of the first child of \code{node}. If \code{index} is a vector of
characters, e.g., \code{c("list", "x")}, the result is the child node with name "x" of
the "list" child node.
}
\value{
	An object of class \code{\link[gdsfmt]{gdsn.class}} for the specified node.
}

\references{\url{http://sourceforge.net/projects/corearray/}}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{
	\code{\link[gdsfmt]{cnt.gdsn}}, \code{\link[gdsfmt]{objdesp.gdsn}},
	\code{\link[gdsfmt]{ls.gdsn}}, \code{\link[gdsfmt]{name.gdsn}},
	\code{\link[gdsfmt]{delete.gdsn}}, \code{\link[gdsfmt]{add.gdsn}}
}

\examples{
# cteate the GDS file "test.gds"
f <- createfn.gds(fn="test.gds")

# add a list to "test.gds"
node <- add.gdsn(f, name="list", val=list(x=c(1,2), y=c("T", "B", "C"), z=TRUE))
f

index.gdsn(f, c("list", "x"))
index.gdsn(f, c(1, 1))

index.gdsn(f, c("list", "z"))

closefn.gds(f)
}

\keyword{GDS}
\keyword{utilities}
