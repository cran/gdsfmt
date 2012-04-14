\name{apply.gdsn}
\alias{apply.gdsn}
\title{Apply functions over matrix margins}
\description{
	Returns a vector or list of values obtained by applying a function to margins of a matrix,
in a CoreArray Genomic Data Structure (GDS) file.
}

\usage{
apply.gdsn(node, margin=2, row.sel=NULL, col.sel=NULL, row.buf.size=128, FUN, ...)
}
\arguments{
	\item{node}{an object of class \code{\link[gdsfmt]{gdsn}}, a GDS node}
	\item{margin}{an integer giving the subscripts which the function will be applied
		over. E.g., for a matrix 1 indicates rows, 2 indicates columns}
	\item{row.sel}{a logical vector giving which rows will be used, or NULL for all rows}
	\item{col.sel}{a logical vector giving which columns will be used, or NULL for all columns}
	\item{row.buf.size}{if \code{margin = 1}, the algorithm is optimized by blocking
		the computations to exploit the high-speed memory instead of disk. See details}
	\item{FUN}{the function to be applied}
	\item{...}{optional arguments to \code{FUN}}
}
\details{
	\code{row.buf.size} is used only when \code{margin = 1}, the algorithm of applying
is optimized by blocking the computations to exploit the high-speed memory instead of disk.
It loads \code{row.buf.size} of rows each time, and then call \code{FUN}.
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

(n <- add.gdsn(f, "matrix", val=matrix(1:(10*6), nrow=10)))
read.gdsn(index.gdsn(f, "matrix"))

# Apply functions over rows of matrix
tmp <- apply.gdsn(n, margin=1, row.buf.size=2, FUN=function(x) print(x))
tmp <- apply.gdsn(n, margin=1,
	row.sel=rep(c(TRUE, FALSE), 5), col.sel=rep(c(TRUE, FALSE), 3),
	FUN=function(x) print(x))

# Apply functions over columns of matrix
tmp <- apply.gdsn(n, margin=2, FUN=function(x) print(x))
tmp <- apply.gdsn(n, margin=2,
	row.sel=rep(c(TRUE, FALSE), 5), col.sel=rep(c(TRUE, FALSE), 3),
	FUN=function(x) print(x))


closefn.gds(f)
}

\keyword{utilities}
