\name{createfn.gds}
\alias{createfn.gds}
\title{Create a GDS file}
\description{
	Creates a new CoreArray Genomic Data Structure (GDS) file.
}

\usage{
createfn.gds(fn)
}
\arguments{
	\item{fn}{the file name of a new GDS file to be created}
}
\details{
	Keep in mind that the new file may not actually be written to disk until
\code{\link[gdsfmt]{closefn.gds}} or \code{\link[gdsfmt]{sync.gds}} is called.
}
\value{
	Return an object of class \code{\link[gdsfmt]{gdsclass}}:
	\item{filename}{the file name to be created.}
	\item{id}{ID of GDS file, internal use.}
	\item{root}{an object of class \code{\link[gdsfmt]{gdsn}}, the root of hierachical structure.}
	\item{readonly}{whether it is read-only or not}
}

\references{\url{http://sourceforge.net/projects/corearray/}}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{
	\code{\link[gdsfmt]{openfn.gds}}, \code{\link[gdsfmt]{closefn.gds}}
}

\examples{
# cteate the GDS file "test.gds"
f <- createfn.gds(fn="test.gds")

# add a list to "test.gds"
node <- add.gdsn(f, val=list(x=c(1,2), y=c("T", "B", "C"), z=TRUE))

closefn.gds(f)
}

\keyword{utilities}
