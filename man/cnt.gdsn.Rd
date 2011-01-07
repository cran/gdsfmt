\name{cnt.gdsn}
\alias{cnt.gdsn}
\title{Return the number of child nodes}
\description{
    Returns the number of child nodes for the current node, in a CoreArray
  Generic Data Structures (GDS) file.
}

\usage{
  cnt.gdsn(node)
}
\arguments{
  \item{node}{An object of class "gdsn", the node of hierarchical structures.}
}
\value{
  The number of child nodes.
}

\references{\url{http://sourceforge.net/projects/corearray/}}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{\code{\link[gdsfmt]{objdesp.gdsn}}, \code{\link[gdsfmt]{ls.gdsn}},
  \code{\link[gdsfmt]{index.gdsn}}, \code{\link[gdsfmt]{delete.gdsn}},
  \code{\link[gdsfmt]{add.gdsn}}}

\examples{
library(gdsfmt)

# cteate the GDS file "test.gds"
f <- createfn.gds(fn="test.gds")

# add a list to "test.gds"
node <- add.gdsn(f, name="list", val=list(x=c(1,2), y=c("T", "B", "C"), z=TRUE))
cnt.gdsn(node)
# 3

closefn.gds(f)
}

\keyword{utilities}
