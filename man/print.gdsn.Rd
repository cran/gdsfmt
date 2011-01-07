\name{print.gdsn}
\alias{print.gdsn}
\title{Print class "gdsn"}
\description{
    Print an object of class "gdsn", a node of hierarchical structures
  in a CoreArray Generic Data Structures (GDS) file.
}

\usage{
	\method{print}{gdsn}(x, expand = TRUE, ...)
}
\arguments{
	\item{x}{An object of class "gdsn", the node of hierarchical structures.}
	\item{expand}{Whether print the child nodes.}
	\item{...}{Arguments passed to or from other methods.}
}

\references{\url{http://sourceforge.net/projects/corearray/}}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{\code{\link[gdsfmt]{print.gdsclass}}}

\keyword{utilities}
