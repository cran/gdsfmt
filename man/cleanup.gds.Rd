\name{cleanup.gds}
\alias{cleanup.gds}
\title{Clean up fragments}
\description{
	Cleans up the fragments of a CoreArray Genomic Data Structure (GDS) file.
}

\usage{
cleanup.gds(fn, deep=FALSE, verbose=TRUE)
}
\arguments{
	\item{fn}{the file name of a GDS file to be opened}
	\item{deep}{if TRUE, performs a deep-construction strategy;
		otherwise, performs fast construction}
	\item{verbose}{if TRUE, show information}
}
\value{
	None.
}

\references{\url{http://sourceforge.net/projects/corearray/}}
\author{Xiuwen Zheng}
\seealso{
	\code{\link[gdsfmt]{openfn.gds}}, \code{\link[gdsfmt]{createfn.gds}},
	\code{\link[gdsfmt]{closefn.gds}}
}

\examples{
# cteate the GDS file "test.gds"
(f <- createfn.gds("test.gds"))

# commom types
add.gdsn(f, "int", val=as.integer(1:10000))
L <- -2500:2499
add.gdsn(f, "int.matrix", val=matrix(L, nrow=100, ncol=50))

# save a .RData object
obj <- list(X=1:10, Y=seq(1, 10, 0.1))
save(obj, file="tmp.RData")
addfile.gdsn(f, "tmp.RData", filename="tmp.RData")

f

closefn.gds(f)

# clean up fragments
cleanup.gds("test.gds")

# cteate the GDS file "test.gds"
(f <- openfn.gds("test.gds"))
closefn.gds(f)
}

\keyword{GDS}
\keyword{utilities}
