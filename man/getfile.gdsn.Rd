\name{getfile.gdsn}
\alias{getfile.gdsn}
\title{Output a file from a stream container}
\description{
	To get a file from a stream container, in a CoreArray Genomic Data Structure (GDS) file.
}

\usage{
getfile.gdsn(node, out.filename)
}
\arguments{
	\item{node}{an object of class \code{\link[gdsfmt]{gdsn}} or \code{\link[gdsfmt]{gdsclass}}:
		"gdsn" -- the node of hierarchical structure;
		"gdsclass" -- the root of hieracrchical structure.}
	\item{out.filename}{the file name of output stream.}
}

\value{
	None.
}

\references{\url{http://sourceforge.net/projects/corearray/}}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{ \code{\link[gdsfmt]{cnt.gdsn}}, \code{\link[gdsfmt]{objdesp.gdsn}},
	\code{\link[gdsfmt]{ls.gdsn}}, \code{\link[gdsfmt]{index.gdsn}},
	\code{\link[gdsfmt]{name.gdsn}}, \code{\link[gdsfmt]{add.gdsn}},
	\code{\link[gdsfmt]{addfile.gdsn}} }

\examples{
# save a .RData object
obj <- list(X=1:10, Y=seq(1, 10, 0.1))
save(obj, file="tmp.RData")

# cteate a GDS file "test.gds"
f <- createfn.gds("test.gds")

add.gdsn(f, "double", val=seq(1, 1000, 0.4))
addfile.gdsn(f, "tmp.RData", "tmp.RData")

f

getfile.gdsn(index.gdsn(f, "tmp.RData"), "tmp1.RData")
(obj <- get(load("tmp1.RData")))

# open the GDS file
closefn.gds(f)
}

\keyword{utilities}
