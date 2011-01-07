\name{compression.gdsn}
\alias{compression.gdsn}
\title{Change Compression Mode}
\description{
  Changes the data compression mode of a CoreArray GDS node.
}

\usage{
  compression.gdsn(node, compress = c("", "ZIP", "ZIP.fast", "ZIP.default", "ZIP.max"))
}
\arguments{
  \item{node}{An object of class "gdsn", the node of hierarchical structures.}
  \item{compress}{Compression method.}
}
\details{
    For large dataset, please make sure you have enough disk space. Be careful,
  if something wrong happens, you may not recover the dataset.
}
\value{
  Returns \code{node}.
}

\references{\url{http://sourceforge.net/projects/corearray/}}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{\code{\link[gdsfmt]{readmode.gdsn}}, \code{\link[gdsfmt]{add.gdsn}}}

\keyword{utilities}
