\name{write.gdsn}
\alias{write.gdsn}
\title{Write data to a GDS node}
\description{
	To write data to a GDS node of hierarchical structure, in a CoreArray Genomic Data
Structure (GDS) file.
}

\usage{
write.gdsn(node, val, start, count)
}
\arguments{
	\item{node}{an object of class \code{\link[gdsfmt]{gdsn}}, the node of hierarchical structure.}
	\item{val}{the data to be written.}
	\item{start}{a vector of integers, starting from 1 for each dimension.}
	\item{count}{a vector of integers, the length of each dimnension.}
}
\details{
	\code{start}, \code{count}: The values in data are taken to be those in the array with
the leftmost subscript moving fastest.

	\code{start} and \code{count} should both exist or be missing. If \code{start} and
\code{count} are both missing, the dimensions and values of \code{val} will be assigned to
the data field.
}
\value{
	None.
}

\references{\url{http://sourceforge.net/projects/corearray/}}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{ \code{\link[gdsfmt]{append.gdsn}}, \code{\link[gdsfmt]{read.gdsn}},
  \code{\link[gdsfmt]{add.gdsn}} }

\examples{
library(gdsfmt)

(f <- createfn.gds("test.gds"))
L <- -2500:2499

# commom types
add.gdsn(f, "int.matrix", val=matrix(L, nrow=100, ncol=50),
  valdim=c(110, 60))
write.gdsn(index.gdsn(f, "int.matrix"), val=rep(0, 100), start=c(20, 20),
  count=c(10, 10))
f
closefn.gds(f)
}

\keyword{utilities}
