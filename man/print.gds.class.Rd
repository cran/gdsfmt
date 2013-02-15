\name{print.gds.class}
\alias{print.gds.class}
\title{Show the information of class "gds.class"}
\description{
	Displays an object of class "gds.class", in a CoreArray Genomic Data Structure (GDS) file.
}
\usage{
\method{print}{gds.class}(x, all=FALSE, ...)
}
\arguments{
	\item{x}{an object of class \code{\link[gdsfmt]{gds.class}}, a CoreArray GDS file}
	\item{all}{if FALSE, hide GDS nodes with an attribute "R.invisible"}
	\item{...}{the arguments passed to or from other methods}
}

\references{\url{http://sourceforge.net/projects/corearray/}}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{\code{\link[gdsfmt]{print.gdsn.class}}}

\keyword{GDS}
\keyword{utilities}
