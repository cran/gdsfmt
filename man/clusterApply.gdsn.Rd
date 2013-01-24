\name{clusterApply.gdsn}
\alias{clusterApply.gdsn}
\title{Apply functions over matrix margins in parallel}
\description{
	Returns a vector or list of values obtained by applying a function to margins of a
matrix in parallel, in a CoreArray Genomic Data Structure (GDS) file.
}

\usage{
clusterApply.gdsn(cl, gds.fn, node.names, margins, FUN, selection=NULL,
	as.is = c("list", "integer", "double", "character", "none"), ...)
}
\arguments{
	\item{cl}{a cluster object, created by this package or by the package
		\link[parallel]{parallel} or \href{http://CRAN.R-project.org/package=snow}{snow}}
	\item{gds.fn}{the file name of a GDS file}
	\item{node.names}{a character vector or a list of character vector indicating the
		GDS nodes}
	\item{margins}{an integer giving the subscripts which the function will be applied
		over. E.g., for a matrix 1 indicates rows, 2 indicates columns}
	\item{FUN}{the function to be applied}
	\item{selection}{a list or NULL; if a list, it is a list of logical vectors according
		to dimensions indicating selection; if NULL, uses all data}
	\item{as.is}{returned value: a list, an integer vector, etc}
	\item{...}{optional arguments to \code{FUN}}
}
\details{
	The algorithm of applying is optimized by blocking the computations to exploit the
high-speed memory instead of disk.
}
\value{
	A vector or list of values.
}

\references{\url{http://sourceforge.net/projects/corearray/}}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{
	\code{\link[gdsfmt]{apply.gdsn}}
}

\examples{
###########################################################
# prepare a GDS file

# cteate the GDS file "test1.gds"
f <- createfn.gds("test1.gds")

(n <- add.gdsn(f, "matrix", val=matrix(1:(10*6), nrow=10)))
read.gdsn(index.gdsn(f, "matrix"))

closefn.gds(f)


# cteate the GDS file "test2.gds"
(f <- createfn.gds("test2.gds"))

X <- matrix(1:50, nrow=10)
Y <- matrix((1:50)/100, nrow=10)
Z1 <- factor(c(rep(c("ABC", "DEF", "ETD"), 3), "TTT"))
Z2 <- c(TRUE, FALSE, TRUE, FALSE, TRUE)

node.X <- add.gdsn(f, "X", X)
node.Y <- add.gdsn(f, "Y", Y)
node.Z1 <- add.gdsn(f, "Z1", Z1)
node.Z2 <- add.gdsn(f, "Z2", Z2)
f

closefn.gds(f)



###########################################################
# apply in parallel

library(parallel)

# Use option cl.core to choose an appropriate cluster size.
cl <- makeCluster(getOption("cl.cores", 2))


# Apply functions over rows or columns of matrix

clusterApply.gdsn(cl, "test1.gds", "matrix", margin=1, FUN=function(x) x)

clusterApply.gdsn(cl, "test1.gds", "matrix", margin=2, FUN=function(x) x)

clusterApply.gdsn(cl, "test1.gds", "matrix", margin=1,
	selection = list(rep(c(TRUE, FALSE), 5), rep(c(TRUE, FALSE), 3)),
	FUN=function(x) x)

clusterApply.gdsn(cl, "test1.gds", "matrix", margin=2,
	selection = list(rep(c(TRUE, FALSE), 5), rep(c(TRUE, FALSE), 3)),
	FUN=function(x) x)



# Apply functions over rows or columns of matrix

clusterApply.gdsn(cl, "test2.gds", list(X="X", Y="Y", Z="Z1"), c(1, 1, 1),
	FUN=function(x) x)

clusterApply.gdsn(cl, "test2.gds", list(X="X", Y="Y", Z="Z2"), c(2, 2, 1),
	FUN=function(x) x)
}

\keyword{GDS}
\keyword{utilities}
