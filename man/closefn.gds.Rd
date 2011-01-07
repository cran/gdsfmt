\name{closefn.gds}
\alias{closefn.gds}
\title{Close a GDS File}
\description{
  Close an open CoreArray Generic Data Structures (GDS) file.
}

\usage{
	closefn.gds(gds)
}
\arguments{
	\item{gds}{An object of class "gdsclass", returned by \code{createfn.gds}
		or \code{openfn.gds}}
}
\details{
    For better performance, Data in a GDS file are usually cached in memory.
 Keep in mind that the new file may not actually be written to disk,
 until \code{\link[gdsfmt]{closefn.gds}} or \code{\link[gdsfmt]{sync.gds}} is
 called. Anyway, when R shuts down, all GDS files created or opened would
 be automatically closed.
}
\value{
  None.
}

\references{\url{http://sourceforge.net/projects/corearray/}}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{\code{\link[gdsfmt]{createfn.gds}}, \code{\link[gdsfmt]{openfn.gds}}}

\keyword{utilities}
