\name{readmode.gdsn}
\alias{readmode.gdsn}
\title{Return to read mode}
\description{
    Returns to read mode for a GDS node of hierarchical structures, in a
  CoreArray Generic Data Structures (GDS) file.
}

\usage{
  readmode.gdsn(node)
}
\arguments{
  \item{node}{An object of class "gdsn", the node of hierarchical structures.}
}
\details{
    After the compressed data field is created, it is in writing mode. Users
  can add new data to the compressed data field, but can not read data from the
  data field. Users need to call \code{readmode.gdsn} for reading.
}
\value{
  Returns \code{node}.
}

\references{\url{http://sourceforge.net/projects/corearray/}}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{\code{\link[gdsfmt]{compression.gdsn}}, \code{\link[gdsfmt]{add.gdsn}}}

\examples{
library(gdsfmt)

# cteate the GDS file "test.gds"
(f <- createfn.gds("test.gds"))

# commom types
n <- add.gdsn(f, "int", val=as.integer(1:10000), compress="ZIP")

# you can not read "int", because "int" is in writing mode.
# read.gdsn(n)

readmode.gdsn(n)

# now you can read "int"
read.gdsn(n)

closefn.gds(f)
}

\keyword{utilities}
