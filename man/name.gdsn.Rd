\name{name.gdsn}
\alias{name.gdsn}
\title{Return the name of a node}
\description{
    Returns the name of a node, in a CoreArray Generic Data Structures (GDS)
  file.
}

\usage{
  name.gdsn(node, fullname = FALSE)
}
\arguments{
  \item{node}{An object of class "gdsn", the node of hierarchical structures.}
  \item{fullname}{Returns the node name (by default), or the full name
    including the path.}
}
\value{
  Returns a charactor string.
}

\references{\url{http://sourceforge.net/projects/corearray/}}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{\code{\link[gdsfmt]{cnt.gdsn}}, \code{\link[gdsfmt]{objdesp.gdsn}},
  \code{\link[gdsfmt]{ls.gdsn}}, \code{\link[gdsfmt]{rename.gdsn}}}

\examples{
library(gdsfmt)

# cteate the GDS file "test.gds"
f <- createfn.gds("test.gds")

# add a list to "test.gds"
node <- add.gdsn(f, name="list", val=list(x=c(1,2), y=c("T", "B", "C"), z=TRUE))

name.gdsn(node)
# "list"

name.gdsn(node, fullname=TRUE)
# "test/list"

closefn.gds(f)
}

\keyword{utilities}
