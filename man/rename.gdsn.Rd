\name{rename.gdsn}
\alias{rename.gdsn}
\title{Rename a GDS node}
\description{
	Renames a GDS node, in a CoreArray Genomic Data Structure (GDS) file.
}

\usage{
rename.gdsn(node, newname)
}
\arguments{
	\item{node}{an object of class \code{\link[gdsfmt]{gdsn.class}}, a GDS node}
	\item{newname}{the new name of a specified node}
}
\details{
	CoreArray hierarchical structure does not allow duplicate names in the same path.
}
\value{
	Returns \code{node}.
}

\references{\url{http://sourceforge.net/projects/corearray/}}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{
	\code{\link[gdsfmt]{cnt.gdsn}}, \code{\link[gdsfmt]{objdesp.gdsn}},
	\code{\link[gdsfmt]{ls.gdsn}}, \code{\link[gdsfmt]{index.gdsn}}
}

\examples{
# cteate the GDS file "test.gds"
f <- createfn.gds("test.gds")
n <- add.gdsn(f, "old.name", val=1:10)
f

rename.gdsn(n, "new.name")
f

closefn.gds(f)
}

\keyword{GDS}
\keyword{utilities}
