\name{ls.gdsn}
\alias{ls.gdsn}
\title{Return the names of child nodes}
\description{
	Gets a list of names for its child nodes, in a CoreArray Genomic Data
Structure (GDS) file.
}

\usage{
ls.gdsn(node)
}
\arguments{
	\item{node}{an object of class \code{\link[gdsfmt]{gdsn}}, a GDS node}
}
\value{
	A vector of characters.
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

# add a list to "test.gds"
node <- add.gdsn(f, name="list", val=list(x=c(1,2), y=c("T", "B", "C"), z=TRUE))

ls.gdsn(node)

closefn.gds(f)
}

\keyword{utilities}
