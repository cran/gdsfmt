\name{rename.gdsn}
\alias{rename.gdsn}
\title{Rename a GDS node}
\description{
    Renames a GDS node of hierarchical structures, in a CoreArray Generic Data
  Structures (GDS) file.
}

\usage{
  rename.gdsn(node, newname)
}
\arguments{
  \item{node}{An object of class "gdsn", the node of hierarchical structures.}
  \item{newname}{A new name of the specified node.}
}
\details{
  CoreArray hierarchical structure allows duplicate names in the same level.
}
\value{
  Returns \code{node}.
}

\references{\url{http://sourceforge.net/projects/corearray/}}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{\code{\link[gdsfmt]{cnt.gdsn}}, \code{\link[gdsfmt]{objdesp.gdsn}},
  \code{\link[gdsfmt]{ls.gdsn}}, \code{\link[gdsfmt]{index.gdsn}}}

\examples{
library(gdsfmt)

# cteate the GDS file "test.gds"
f <- createfn.gds("test.gds")
n <- add.gdsn(f, "old.name", val=1:10)
f

rename.gdsn(n, "new.name")
f

closefn.gds(f)
}

\keyword{utilities}
