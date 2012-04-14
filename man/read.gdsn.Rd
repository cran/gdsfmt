\name{read.gdsn}
\alias{read.gdsn}
\title{Read data field of a GDS node}
\description{
	Gets data from a GDS node, in a CoreArray Genomic Data Structure (GDS) file.
}

\usage{
read.gdsn(node, start, count)
}
\arguments{
	\item{node}{an object of class \code{\link[gdsfmt]{gdsn}}, a GDS node}
	\item{start}{a vector of integers, starting from 1 for each dimension component}
	\item{count}{a vector of integers, the length of each dimnension. As a
		special case, the value "-1" indicates that all entries along that
		dimension should be written}
}
\details{
    \code{start}, \code{count}: the values in data are taken to be those in the array
with the leftmost subscript moving fastest.
}
\value{
	Returns an array, list, or data.frame.
}

\references{\url{http://sourceforge.net/projects/corearray/}}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{
	\code{\link[gdsfmt]{readex.gdsn}}, \code{\link[gdsfmt]{append.gdsn}},
	\code{\link[gdsfmt]{write.gdsn}}, \code{\link[gdsfmt]{add.gdsn}}
}

\examples{
# cteate the GDS file "test.gds"
(f <- createfn.gds("test.gds"))

add.gdsn(f, "vector", val=1:128)
add.gdsn(f, "list", val=list(X=1:10, Y=seq(1, 10, 0.25)))
add.gdsn(f, "data.frame", val=data.frame(X=1:19, Y=seq(1, 10, 0.5)))

f

read.gdsn(index.gdsn(f, "vector"))
read.gdsn(index.gdsn(f, "list"))
read.gdsn(index.gdsn(f, "data.frame"))

closefn.gds(f)
}

\keyword{utilities}
