\name{objdesp.gdsn}
\alias{objdesp.gdsn}
\title{Return node description}
\description{
	To get the descritpion of a specified node, in a CoreArray Genomic Data Structure (GDS) file.
}

\usage{
objdesp.gdsn(node)
}
\arguments{
	\item{node}{an object of class \code{\link[gdsfmt]{gdsn}}, the node of hierarchical structure.}
}
\value{
	Returns a list:
	\item{desp}{the description of data field.}
	\item{name}{the variable name of the specified node.}
	\item{svtype}{the internal type of the specified node: 1 -- Int8, 2 -- UInt8,
		3 -- Int16, 4 -- UInt16, 5 -- Int32, 6 -- UInt32, 7 -- Int64, 8 -- UInt64,
		9 -- Int128, 10 -- UInt128, 11 -- Float32, 12 -- Float64, 13 -- Long Float,
		14 -- UTF8 String, 15 -- UTF16 String, 16 -- UTF32 String.}
	\item{dim}{the dimension of data field.}
	\item{compress}{the compression method: "", "ZIP", etc.}
	\item{cpratio}{the compression ratio, NaN for no compression.}
}

\references{\url{http://sourceforge.net/projects/corearray/}}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{ \code{\link[gdsfmt]{cnt.gdsn}}, \code{\link[gdsfmt]{name.gdsn}},
	\code{\link[gdsfmt]{ls.gdsn}}, \code{\link[gdsfmt]{index.gdsn}},
	\code{\link[gdsfmt]{delete.gdsn}}, \code{\link[gdsfmt]{add.gdsn}} }

\examples{
library(gdsfmt)

# cteate the GDS file "test.gds"
f <- createfn.gds("test.gds")

# add a vector to "test.gds"
node <- add.gdsn(f, name="vector", val=1:10000, compress="ZIP")

objdesp.gdsn(node)

closefn.gds(f)
}

\keyword{utilities}
