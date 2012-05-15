\name{applylt.gdsn}
\alias{applylt.gdsn}
\title{Apply functions over matrix margins}
\description{
	Returns a vector or list of values obtained by applying a function to margins of a list
of matrices, in a CoreArray Genomic Data Structure (GDS) file.
}

\usage{
applylt.gdsn(nodes, margin=2, row.sel=NULL, col.sel=NULL, buf.size=1024,
	simplify=TRUE, FUN, ...)
}
\arguments{
	\item{nodes}{a list of \code{\link[gdsfmt]{gdsn}} objects, GDS nodes}
	\item{margin}{an integer giving the subscripts which the function will be applied
		over. E.g., for a matrix 1 indicates rows, 2 indicates columns}
	\item{row.sel}{a logical vector giving which rows will be used, or NULL for all rows}
	\item{col.sel}{a logical vector giving which columns will be used, or NULL for all columns}
	\item{buf.size}{the size of blocking buffer, the algorithm is optimized by blocking
		the computations to exploit the high-speed memory instead of disk. See details}
	\item{simplify}{if TRUE, the result will be simplified to a vector, matrix or higher
		dimensional array if possible}
	\item{FUN}{the function to be applied}
	\item{...}{optional arguments to \code{FUN}}
}
\details{
	The algorithm of applying is optimized by blocking the computations to exploit the
high-speed memory instead of disk. It loads \code{buf.size} of columns or rows each time,
and then call \code{FUN}. In general, it is better to choose a large value of \code{buf.size},
however, the value of \code{buf.size} should not make the internal buffer exceed the main
memory.
}
\value{
	A vector or list of values.
}

\references{\url{http://sourceforge.net/projects/corearray/}}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{
	\code{\link[gdsfmt]{read.gdsn}}, \code{\link[gdsfmt]{readex.gdsn}},
	\code{\link[gdsfmt]{append.gdsn}}, \code{\link[gdsfmt]{write.gdsn}},
	\code{\link[gdsfmt]{add.gdsn}}
}

\examples{
# cteate the GDS file "test.gds"
(f <- createfn.gds("test.gds"))

X <- matrix(1:50, nrow=10)
Y <- matrix((1:50)/100, nrow=10)
Z1 <- paste("Z", 1:10, sep="")
Z2 <- paste("Z", 1:5, sep="")

node.X <- add.gdsn(f, "X", X)
node.Y <- add.gdsn(f, "Y", Y)
node.Z1 <- add.gdsn(f, "Z1", Z1)
node.Z2 <- add.gdsn(f, "Z2", Z2)

v <- applylt.gdsn(list(X=node.X, Y=node.Y, Z=node.Z1), 1,
	FUN=function(x) print(x))

v <- applylt.gdsn(list(X=node.X, Y=node.Y, Z=node.Z2), 2,
	FUN=function(x) print(x))

v <- applylt.gdsn(list(X=node.X, Y=node.Y, Z=node.Z1), 1,
	row.sel=rep(c(TRUE, FALSE), 5),
	col.sel=list(X=c(TRUE, FALSE, TRUE, FALSE, TRUE), Y=c(TRUE, TRUE, FALSE, FALSE, TRUE), Z=NULL),
	FUN=function(x) print(x))

v <- applylt.gdsn(list(X=node.X, Y=node.Y, Z=node.Z2), 2,
	row.sel=list(X=rep(c(TRUE, FALSE), 5), Y=rep(c(FALSE, TRUE), 5), Z=NULL),
	col.sel=c(TRUE, FALSE, FALSE, FALSE, TRUE),
	FUN=function(x) print(x))


closefn.gds(f)
}

\keyword{utilities}
