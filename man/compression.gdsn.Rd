\name{compression.gdsn}
\alias{compression.gdsn}
\title{Modify compression mode}
\description{
	Modifies the data compression mode of data field, in a CoreArray Genomic Data
Structure (GDS) file.
}

\usage{
compression.gdsn(node, compress = c("", "ZIP", "ZIP.fast", "ZIP.default", "ZIP.max"))
}
\arguments{
	\item{node}{an object of class \code{\link[gdsfmt]{gdsn}}, a GDS node}
	\item{compress}{specify the compression method}
}
\details{
	Z compression algorithm can be used to deflate the data stored in the GDS file.
"ZIP" option is equivalent to "ZIP.default". "ZIP.fast", "ZIP.default" and "ZIP.max"
correspond to different compression levels.
}
\value{
	Return \code{node}.
}

\references{ \url{http://sourceforge.net/projects/corearray/}, \url{http://zlib.net/} }
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{
	\code{\link[gdsfmt]{readmode.gdsn}}, \code{\link[gdsfmt]{add.gdsn}}
}

\keyword{utilities}
