\name{openfn.gds}
\alias{openfn.gds}
\title{Open a GDS file}
\description{
	Opens an existing file of CoreArray Genomic Data Structure (GDS) for reading or writing.
}

\usage{
openfn.gds(fn, readonly = TRUE)
}
\arguments{
	\item{fn}{the file name of a GDS file to be opened}
	\item{readonly}{if TRUE, the file is opened read-only; otherwise, writing to
		the file is allowed.}
}
\details{
	This routine opens an existing GDS file for reading (or, if readonly=FALSE,
for writing). To create a new GDS file, use \code{\link[gdsfmt]{createfn.gds}} instead.

	If the file is opened read-only, all data in the file are not allowed to change,
including hierachical structure, variable names, data field, etc.
}
\value{
	Return an object of class \code{\link[gdsfmt]{gdsclass}}.
	\item{filename}{the file name}
	\item{id}{ID of GDS file, internal use}
	\item{root}{an object of class \code{\link[gdsfmt]{gdsn}}, the root of hierachical structure}
	\item{readonly}{whether it is read-only or not}
}

\references{\url{http://sourceforge.net/projects/corearray/}}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{
	\code{\link[gdsfmt]{createfn.gds}}, \code{\link[gdsfmt]{closefn.gds}}
}

\examples{
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
