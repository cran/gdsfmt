\name{setdim.gdsn}
\alias{setdim.gdsn}
\title{Set the dimension of data field}
\description{
	Assigns a new dimension to the data field of a GDS node, in a CoreArray
Genomic Data Structure (GDS) file.
}

\usage{
setdim.gdsn(node, valdim)
}
\arguments{
	\item{node}{an object of class \code{\link[gdsfmt]{gdsn.class}}, a GDS node}
	\item{valdim}{the new dimension for the array to be created, which is a
		vector of length one or more giving the maximal indices in each dimension}
}
\value{
	Returns \code{node}.
}

\references{\url{http://sourceforge.net/projects/corearray/}}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{
	\code{\link[gdsfmt]{read.gdsn}}, \code{\link[gdsfmt]{write.gdsn}},
	\code{\link[gdsfmt]{append.gdsn}}, \code{\link[gdsfmt]{add.gdsn}}
}

\examples{
# cteate the GDS file "test.gds"
(f <- createfn.gds("test.gds"))
n <- add.gdsn(f, "int", val=as.integer(1:100))
f
setdim.gdsn(n, c(120, 4))
f
closefn.gds(f)
}

\keyword{GDS}
\keyword{utilities}
