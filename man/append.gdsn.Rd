\name{append.gdsn}
\alias{append.gdsn}
\title{Append data to a specified variable}
\description{
	Appends data to the data field of a GDS node, in a CoreArray Genomic Data
Structure (GDS) file.
}

\usage{
append.gdsn(node, val, check = TRUE)
}
\arguments{
	\item{node}{an object of class \code{\link[gdsfmt]{gdsn.class}}, a GDS node}
	\item{val}{data to be appended}
	\item{check}{whether a warning is given, when appended data can not match
		the capability of data field}
}
\details{
	\code{storage.mode(val)} should be "integer", "double", "character", or "logical".
}
\value{
	None.
}

\references{\url{http://sourceforge.net/projects/corearray/}}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{
	\code{\link[gdsfmt]{read.gdsn}}, \code{\link[gdsfmt]{write.gdsn}},
	\code{\link[gdsfmt]{add.gdsn}}
}

\examples{
# cteate the GDS file "test.gds"
(f <- createfn.gds("test.gds"))

# commom types
n <- add.gdsn(f, "int", val=matrix(as.integer(1:10000), nrow=100, ncol=100),
  compress="ZIP")

# no warning, and add a new column
append.gdsn(n, -1:-100)
f

# a warning
append.gdsn(n, -1:-50)
f

# no warning here, and add a new column
append.gdsn(n, -51:-100)
f

# you should call "readmode.gdsn", before reading
readmode.gdsn(n)

# check the last column
read.gdsn(n, start=c(1, 102), count=c(-1, 1))

# close the gds file
closefn.gds(f)
}

\keyword{GDS}
\keyword{utilities}
