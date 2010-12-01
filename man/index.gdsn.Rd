\name{index.gdsn}
\alias{index.gdsn}
\title{Return the specified node}
\description{
    Returns a node (an object of class "gdsn") specified by users, in a
  CoreArray Generic Data Structures (GDS) file.
}

\usage{
  index.gdsn(node, index)
}
\arguments{
  \item{node}{An object of class "gdsn" or "gdsclass": "gdsn" -- the node of
    hierarchical structures; "gdsclass" -- the root of hieracrchical
    structures.}
  \item{index}{A vector of integers or characters, specifying the node
    subscripts.}
}
\details{
    If \code{index} is a vector of integers, for example, c(1, 2), the result
  is the second child node of the first child of \code{node}. If \code{index}
  is a vector of characters, for example, c("list", "x"), the result is the
  child node with name "x" of the "list" child node of \code{node}.
  
    A warning will be given if the specified node is not available.
}
\value{
  An object of class "gdsn" for the specified node.
}

\references{}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{\code{\link[gdsfmt]{cnt.gdsn}}, \code{\link[gdsfmt]{objdesp.gdsn}},
  \code{\link[gdsfmt]{ls.gdsn}}, \code{\link[gdsfmt]{name.gdsn}},
  \code{\link[gdsfmt]{delete.gdsn}}, \code{\link[gdsfmt]{add.gdsn}}}

\examples{
library(gdsfmt)

# cteate the GDS file "test.gds"
f <- createfn.gds(fn="test.gds")

# add a list to "test.gds"
node <- add.gdsn(f, name="list", val=list(x=c(1,2), y=c("T", "B", "C"), z=TRUE))
f

index.gdsn(f, c("list", "x"))
index.gdsn(f, c(1, 1))

index.gdsn(f, c("list", "z"))

closefn.gds(f)
}

\keyword{utilities}
