\name{readmode.gdsn}
\alias{readmode.gdsn}
\title{Return to read mode in the compression mode}
\description{
	Return to read mode for a GDS node, in a CoreArray Genomic Data Structure (GDS) file.
}

\usage{
readmode.gdsn(node)
}
\arguments{
	\item{node}{an object of class \code{\link[gdsfmt]{gdsn}}, the node of hierarchical structure.}
}
\details{
	After the compressed data field is created, it is in writing mode. Users can add new data
to the compressed data field, but can not read data from the data field. Users need to call
\code{readmode.gdsn} for reading.

	Once return to read mode, users can not add more data to the data field. If users would like
to append more data or modify the data field, please call \code{\link[gdsfmt]{compression.gdsn}
(node, compress="")} to decompress data first.
}
\value{
	Return \code{node}.
}

\references{\url{http://sourceforge.net/projects/corearray/}}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{ \code{\link[gdsfmt]{compression.gdsn}}, \code{\link[gdsfmt]{add.gdsn}} }

\examples{
library(gdsfmt)

# cteate the GDS file "test.gds"
(f <- createfn.gds("test.gds"))

# commom types
n <- add.gdsn(f, "int", val=as.integer(1:10000), compress="ZIP")

# you can not read "int", because "int" is in writing mode.
# read.gdsn(n)

readmode.gdsn(n)

# now you can read "int"
read.gdsn(n)

closefn.gds(f)
}

\keyword{utilities}
