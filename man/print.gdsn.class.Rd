\name{print.gdsn.class}
\alias{print.gdsn.class}
\title{Show the information of class "gdsn.class"}
\description{
	Displays an object of class "gdsn.class", a GDS node in a CoreArray Genomic Data
Structure (GDS) file.
}

\usage{
\method{print}{gdsn.class}(x, expand = TRUE, ...)
}
\arguments{
	\item{x}{an object of class \code{\link[gdsfmt]{gdsn.class}}, a GDS node}
	\item{expand}{whether enumerate all of child nodes}
	\item{...}{the arguments passed to or from other methods}
}

\references{\url{http://sourceforge.net/projects/corearray/}}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{\code{\link[gdsfmt]{print.gds.class}}}

\keyword{GDS}
\keyword{utilities}
