\name{openfn.gds}
\alias{openfn.gds}
\title{Open a GDS File}
\description{
	Opens an existing CoreArray Generic Data Structures (GDS) file for
reading or writing.
}

\usage{
	openfn.gds(fn, readonly = TRUE)
}
\arguments{
	\item{fn}{The file name of the GDS file to be opened.}
	\item{readonly}{If TRUE (default), the file is opened read-only. Otherwise,
		writing to the file is allowed.}
}
\details{
	This routine opens an existing GDS file for reading (or, if readonly=FALSE,
for writing). To create a new GDS file, use createfn.gds() instead.

	If the file is opened read-only, all data structures in the file are not
allowed to change, including hierachical structure, node names, data field,
etc.
}
\value{
	Return an object of class "gdsclass".
	\item{filename}{Name of the GDS file to be opened.}
	\item{id}{ID of GDS file, internal use.}
	\item{root}{An object of class "gdsn", the root of hierachical structures.}
}

\references{}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{\code{\link[gdsfmt]{createfn.gds}}, \code{\link[gdsfmt]{closefn.gds}}}
\examples{
library(gdsfmt)

# cteate the GDS file "test.gds"
f <- createfn.gds("test.gds")
# add a list to "test.gds"
node <- add.gdsn(f, name="list", val=list(x=c(1,2), y=c("T", "B", "C"), z=TRUE))
# close
closefn.gds(f)

# open
f <- openfn.gds("test.gds")
# read
(node <- index.gdsn(f, "list"))
read.gdsn(node)
# close
closefn.gds(f)
}

\keyword{utilities}
