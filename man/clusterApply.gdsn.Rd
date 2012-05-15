\name{clusterApply.gdsn}
\alias{clusterApply.gdsn}
\title{Apply functions over matrix margins via clusters}
\description{
	Returns a vector or list of values obtained by applying a function to margins of a matrix
via clusters, in a CoreArray Genomic Data Structure (GDS) file.
}

\usage{
clusterApply.gdsn(cl, gds.fn, node, margin=2, row.sel=NULL, col.sel=NULL,
	buf.size=1024, simplify=TRUE, FUN, ...)
}
\arguments{
	\item{cl}{a cluster object, created by this package or by the package
		\link[parallel]{parallel} or \href{http://CRAN.R-project.org/package=snow}{snow}.
		If NULL, use the registered default cluster.}
	\item{gds.fn}{the file name of GDS file}
	\item{node}{a vector of integers or characters, specifying the path}
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
	\code{\link[gdsfmt]{apply.gdsn}}, \code{\link[gdsfmt]{applylt.gdsn}},
	\code{\link[gdsfmt]{clusterApplylt.gdsn}}
}

\examples{
# cteate the GDS file "test.gds"
f <- createfn.gds("test.gds")

(n <- add.gdsn(f, "matrix", val=matrix(1:(10*6), nrow=10)))
read.gdsn(index.gdsn(f, "matrix"))

closefn.gds(f)


library(parallel)

# Use option cl.core to choose an appropriate cluster size.
cl <- makeCluster(getOption("cl.cores", 2))


# Apply functions over rows or columns of matrix

clusterApply.gdsn(cl, "test.gds", "matrix", margin=1, simplify=FALSE,
	FUN=function(x) x)

clusterApply.gdsn(cl, "test.gds", "matrix", margin=2, simplify=FALSE,
	FUN=function(x) x)

clusterApply.gdsn(cl, "test.gds", "matrix", margin=1, simplify=FALSE,
	row.sel=rep(c(TRUE, FALSE), 5), col.sel=rep(c(TRUE, FALSE), 3),
	FUN=function(x) x)

clusterApply.gdsn(cl, "test.gds", "matrix", margin=2, simplify=FALSE,
	row.sel=rep(c(TRUE, FALSE), 5), col.sel=rep(c(TRUE, FALSE), 3),
	FUN=function(x) x)
}

\keyword{utilities}
