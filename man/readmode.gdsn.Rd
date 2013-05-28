\name{readmode.gdsn}
\alias{readmode.gdsn}
\title{Get into read mode in the compression mode}
\description{
	Gets into the read mode for a GDS node, in a CoreArray Genomic Data Structure (GDS) file.
}

\usage{
readmode.gdsn(node)
}
\arguments{
	\item{node}{an object of class \code{\link[gdsfmt]{gdsn.class}}, a GDS node}
}
\details{
	After the compressed data field is created, it is in writing mode. Users can add new data
to the compressed data field, but can not read data from the data field. Users need to call
\code{readmode.gdsn} for reading.

	Once get into the read mode, users can not add more data to the data field. If users would
like to append more data or modify the data field, please call \code{\link[gdsfmt]{compression.gdsn}
(node, compress="")} to decompress data first.
}
\value{
	Return \code{node}.
}

\references{\url{http://sourceforge.net/projects/corearray/}}
\author{Xiuwen Zheng}
\seealso{ \code{\link[gdsfmt]{compression.gdsn}}, \code{\link[gdsfmt]{add.gdsn}} }

\examples{
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

\keyword{GDS}
\keyword{utilities}
