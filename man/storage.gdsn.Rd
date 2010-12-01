\name{storage.gdsn}
\alias{storage.gdsn}
\title{Storage Mode of Data Field}
\description{
  Changes storage mode of data field in a specified GDS node.
}

\usage{
  storage.gdsn(node, mode = c("InMemory", "InStream"))
}
\arguments{
  \item{node}{An object of class "gdsn", the node of hierarchical structures.}
  \item{mode}{"InMemory" -- load all data from disk to memory; "InStream" --
    keep data in disk.}
}
\details{
    The CoreArray GDS file has to be read-only, otherwise a warning is given.
  It is helpful to improve read speed for small or medium size of GDS files,
  using "InMemory" option. If data field exceeds the limit of memory, a warning
  will be given and no other action occurs.
}
\value{
  Returns \code{node}.
}

\references{}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{\code{\link[gdsfmt]{readmode.gdsn}}, \code{\link[gdsfmt]{objdesp.gdsn}}}
\examples{
library(gdsfmt)

# cteate the GDS file "test.gds"
f <- createfn.gds("test.gds")
# add a vector to "test.gds"
node <- add.gdsn(f, name="matrix", val=matrix(c(1,2), 2000, 2000))
# close
closefn.gds(f)

# open
f <- openfn.gds("test.gds")
# the GDS nod
node <- index.gdsn(f, "matrix")
# test "in stream"
storage.gdsn(node, "InStream")
proc.time()
v <- read.gdsn(node)
proc.time()
objdesp.gdsn(node)

# test "in memory"
storage.gdsn(node, "InMemory")
proc.time()
v <- read.gdsn(node)
proc.time()
objdesp.gdsn(node)

# close
closefn.gds(f)
}

\keyword{utilities}
