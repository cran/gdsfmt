\name{readex.gdsn}
\alias{readex.gdsn}
\title{Read data field of a GDS node with a selection}
\description{
	Gets data from a GDS node with subset selection, in a CoreArray Genomic
Data Structure (GDS) file.
}

\usage{
readex.gdsn(node, sel=NULL)
}
\arguments{
	\item{node}{an object of class \code{\link[gdsfmt]{gdsn}}, the GDS node}
	\item{sel}{a list of m logical vectors, where m is the number of dimensions of \code{node}
		and each logical vector should have the same size of dimension in \code{node}}
}
\value{
	Return an array.
}

\references{\url{http://sourceforge.net/projects/corearray/}}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{
	\code{\link[gdsfmt]{read.gdsn}}, \code{\link[gdsfmt]{append.gdsn}},
	\code{\link[gdsfmt]{write.gdsn}}, \code{\link[gdsfmt]{add.gdsn}}
}

\examples{
# cteate the GDS file "test.gds"
(f <- createfn.gds("test.gds"))

add.gdsn(f, "vector", val=1:128)
add.gdsn(f, "matrix", val=matrix(as.character(1:(10*6)), nrow=10))
f

# read vector
readex.gdsn(index.gdsn(f, "vector"), sel=rep(c(TRUE, FALSE), 64))

# read matrix
readex.gdsn(index.gdsn(f, "matrix"))
readex.gdsn(index.gdsn(f, "matrix"),
	sel=list(d1=rep(c(TRUE, FALSE), 5), d2=rep(c(TRUE, FALSE), 3)))

closefn.gds(f)
}

\keyword{utilities}
