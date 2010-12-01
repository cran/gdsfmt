\name{createfn.gds}
\alias{createfn.gds}
\title{Create a GDS File}
\description{
  Create a new CoreArray Generic Data Structures (GDS) file.
}

\usage{
  createfn.gds(fn)
}
\arguments{
  \item{fn}{Name of the GDS file to be created.}
}
\details{
 Keep in mind that the new file may not actually be written to disk until 
 \code{\link[gdsfmt]{closefn.gds}} is called.
}
\value{
  Return an object of class "gdsclass":
  \item{filename}{Name of the GDS file to be created.}
  \item{id}{ID of GDS file, internal use.}
  \item{root}{An object of class "gdsn", the root of hierachical structures.}
}

\references{}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{\code{\link[gdsfmt]{openfn.gds}}, \code{\link[gdsfmt]{closefn.gds}}}
\examples{
library(gdsfmt)

# cteate the GDS file "test.gds"
f <- createfn.gds(fn="test.gds")

# add a list to "test.gds"
node <- add.gdsn(f, val=list(x=c(1,2), y=c("T", "B", "C"), z=TRUE))

closefn.gds(f)
}

\keyword{utilities}
