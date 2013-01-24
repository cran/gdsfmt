\name{gds.class}
\alias{gds.class}
\title{the class of GDS file}
\description{
	The class of a GDS file, and its instance is returned from \link[gdsfmt]{openfn.gds} or
\link[gdsfmt]{createfn.gds}.
}
\value{
	There are three components:
	\item{filename}{the file name to be created.}
	\item{id}{ID of GDS file, internal use.}
	\item{root}{an object of class \code{\link[gdsfmt]{gdsn.class}}, the root of hierachical structure.}
	\item{readonly}{whether it is read-only or not}
}

\references{\url{http://sourceforge.net/projects/corearray/}}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{
	\code{\link[gdsfmt]{createfn.gds}}, \code{\link[gdsfmt]{openfn.gds}},
	\code{\link[gdsfmt]{closefn.gds}}
}

\keyword{GDS}
\keyword{utilities}
