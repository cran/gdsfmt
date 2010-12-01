\name{setdim.gdsn}
\alias{setdim.gdsn}
\title{Setup Dimension of Data Field}
\description{
    Setups new dimension of the data field for a GDS node of hierarchical
  structures, in a CoreArray Generic Data Structures (GDS) file.
}

\usage{
  setdim.gdsn(node, valdim)
}
\arguments{
  \item{node}{An object of class "gdsn", the node of hierarchical structures.}
  \item{valdim}{The dim attribute for the array to be created, which is a
    vector of length one or more giving the maximal indices in each dimension.}
}
\value{
  Returns \code{node}.
}

\references{}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{\code{\link[gdsfmt]{read.gdsn}}, \code{\link[gdsfmt]{write.gdsn}},
  \code{\link[gdsfmt]{append.gdsn}}, \code{\link[gdsfmt]{add.gdsn}}}

\examples{
library(gdsfmt)

# cteate the GDS file "test.gds"
(f <- createfn.gds("test.gds"))
n <- add.gdsn(f, "int", val=as.integer(1:100))
f
setdim.gdsn(n, c(120, 4))
f
closefn.gds(f)
}

\keyword{utilities}
