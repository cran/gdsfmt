\name{print.gdsn}
\alias{print.gdsn}
\title{Show the information of class "gdsn"}
\description{
	To display an object of class "gdsn", a node of hierarchical structure in a CoreArray
Genomic Data Structure (GDS) file.
}

\usage{
\method{print}{gdsn}(x, expand = TRUE, ...)
}
\arguments{
	\item{x}{an object of class \code{\link[gdsfmt]{gdsn}}, the node of hierarchical structures.}
	\item{expand}{whether print the child nodes.}
	\item{...}{the arguments passed to or from other methods.}
}

\references{\url{http://sourceforge.net/projects/corearray/}}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{\code{\link[gdsfmt]{print.gdsclass}}}

\keyword{utilities}
