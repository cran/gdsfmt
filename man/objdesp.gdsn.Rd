\name{objdesp.gdsn}
\alias{objdesp.gdsn}
\title{Return Node Description}
\description{
    Returns the descritpion of the specified node, in a CoreArray Generic
  Data Structures (GDS) file.
}

\usage{
  objdesp.gdsn(node)
}
\arguments{
  \item{node}{An object of class "gdsn", the node of hierarchical structures.}
}
\value{
  Returns a list:
  \item{desp}{Description of data field.}
  \item{name}{Name of the specified node.}
  \item{svtype}{Internal Type of the specified node: 1 -- Int8, 2 -- UInt8,
    3 -- Int16, 4 -- UInt16, 5 -- Int32, 6 -- UInt32, 7 -- Int64, 8 -- UInt64,
    9 -- Int128, 10 -- UInt128, 11 -- Float32, 12 -- Float64, 13 -- Long Float,
	14 -- UTF8 String, 15 -- UTF16 String, 16 -- UTF32 String.}
  \item{dim}{Dimension of data field.}
  \item{compress}{Compression method: "", "ZIP", etc.}
  \item{cpratio}{Compression ratio, NaN for no compression.}
  \item{storage}{storage[1]: "InMemory" or "InStream", see
    \code{\link[gdsfmt]{storage.gdsn}}; storage[2]: description, can be a
    temporary file.}
}

\references{\url{http://sourceforge.net/projects/corearray/}}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{\code{\link[gdsfmt]{cnt.gdsn}}, \code{\link[gdsfmt]{name.gdsn}},
  \code{\link[gdsfmt]{ls.gdsn}}, \code{\link[gdsfmt]{index.gdsn}},
  \code{\link[gdsfmt]{delete.gdsn}}, \code{\link[gdsfmt]{add.gdsn}},
  \code{\link[gdsfmt]{storage.gdsn}}}

\examples{
library(gdsfmt)

# cteate the GDS file "test.gds"
f <- createfn.gds("test.gds")

# add a vector to "test.gds"
node <- add.gdsn(f, name="vector", val=1:10000, compress="ZIP")

objdesp.gdsn(node)

closefn.gds(f)
}

\keyword{utilities}
