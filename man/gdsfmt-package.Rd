\name{gdsfmt-package}
\alias{gdsfmt-package}
\alias{gdsfmt}
\docType{package}
\title{ CoreArray Generic Data Structures (GDS) R Interface }
\description{
	R interface of CoreArray GDS is based on the CoreArray project initiated and developed by Xiuwen Zheng from 2007.
}
\details{
\tabular{ll}{
Package: \tab gdsfmt\cr
Type: \tab Package\cr
Version: \tab 0.9.2\cr
Date: \tab 2011-03-29\cr
License: \tab LGPL version 3\cr
}
	R interface of CoreArray GDS is based on the CoreArray project initiated and developed by Xiuwen Zheng from 2007.
	CoreArray is to develop portable, scalable, bioinformatics data visualization and storage technologies.
}
\references{\url{http://sourceforge.net/projects/corearray}}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\examples{                     

library(gdsfmt)

# cteate the GDS file "test.gds"
(f <- createfn.gds("test.gds"))
L <- -2500:2499

# commom types
add.gdsn(f, "int", val=as.integer(1:10000), compress="ZIP", closezip=TRUE)
add.gdsn(f, "int.matrix", val=matrix(L, nrow=100, ncol=50), valdim=c(110, 60))
add.gdsn(f, "double", val=seq(1, 1000, 0.4))
add.gdsn(f, "logical", val=rep(c(TRUE, FALSE, NA), 50))
add.gdsn(f, "factor", val=as.factor(c(letters, NA, "AA", "CC")))
add.gdsn(f, "NA", val=rep(NA, 10))
add.gdsn(f, "NaN", val=c(rep(NaN, 20), 1:20))
add.gdsn(f, "bit2-matrix", val=matrix(L[1:5000], nrow=50, ncol=100), storage="bit2")
# list and data.frame
add.gdsn(f, "list", val=list(X=1:10, Y=seq(1, 10, 0.25)))
add.gdsn(f, "data.frame", val=data.frame(X=1:19, Y=seq(1, 10, 0.5)))

f

read.gdsn(index.gdsn(f, "list"))
read.gdsn(index.gdsn(f, "data.frame"))

closefn.gds(f)
}

\keyword{utilities}
\keyword{IO}
\keyword{interface}
\keyword{file}
\keyword{database}
