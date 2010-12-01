\name{ls.gdsn}
\alias{ls.gdsn}
\title{Return the names of children nodes}
\description{
    Returns the names of children nodes, in a CoreArray Generic Data
  Structures (GDS) file.
}

\usage{
  ls.gdsn(node)
}
\arguments{
  \item{node}{An object of class "gdsn", the node of hierarchical structures.}
}
\value{
  A vector of characters.
}

\references{}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{\code{\link[gdsfmt]{cnt.gdsn}}, \code{\link[gdsfmt]{objdesp.gdsn}},
  \code{\link[gdsfmt]{ls.gdsn}}, \code{\link[gdsfmt]{index.gdsn}}}

\examples{
library(gdsfmt)

# cteate the GDS file "test.gds"
f <- createfn.gds("test.gds")

# add a list to "test.gds"
node <- add.gdsn(f, name="list", val=list(x=c(1,2), y=c("T", "B", "C"), z=TRUE))

ls.gdsn(node)

closefn.gds(f)
}

\keyword{utilities}
