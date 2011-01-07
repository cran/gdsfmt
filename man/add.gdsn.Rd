\name{add.gdsn}
\alias{add.gdsn}
\title{Add a GDS node}
\description{
	Adds a GDS node of hierarchical structures, in a CoreArray Generic Data
Structures (GDS) file.
}

\usage{
	add.gdsn(node, name, val = NULL, storage = storage.mode(val),
valdim = NULL, compress = c("", "ZIP", "ZIP.fast", "ZIP.default", "ZIP.max"),
closezip = FALSE)
}

\arguments{
	\item{node}{An object of class "gdsn" or "gdsclass": "gdsn" -- the node of
		hierarchical structures; "gdsclass" -- the root of hieracrchical
		structures.}
	\item{name}{Name of the added node.}
	\item{val}{R values: vector, list, data.frame}
	\item{storage}{Specifies Data Type: integer (
		signed integer:
		"int8", "int16", "int24", "int32", "int64",
		"sbit2", "sbit3", "sbit4", "sbit5", "sbit6", "sbit7", "sbit8",
		"sbit9", "sbit10", "sbit11", "sbit12", "sbit13", "sbit14", "sbit15", "sbit16",
		"sbit17", "sbit18", "sbit19", "sbit20", "sbit21", "sbit22", "sbit23", "sbit24",
		"sbit17", "sbit18", "sbit19", "sbit20", "sbit21", "sbit22", "sbit23", "sbit24",
		"sbit25", "sbit26", "sbit27", "sbit28", "sbit29", "sbit30", "sbit31", "sbit32" ;
		unsigned integer:
		"uint8", "uint16", "uint24", "uint32", "uint64",
		"bit1", "bit2", "bit3", "bit4", "bit5", "bit6", "bit7", "bit8",
		"bit9", "bit10", "bit11", "bit12", "bit13", "bit14", "bit15", "bit16",
		"bit17", "bit18", "bit19", "bit20", "bit21", "bit22", "bit23", "bit24",
		"bit17", "bit18", "bit19", "bit20", "bit21", "bit22", "bit23", "bit24",
		"bit25", "bit26", "bit27", "bit28", "bit29", "bit30", "bit31", "bit32" );
		float ("float32", "float64"), string ("string", "wstring", "dwstring").
		Or "integer" (="int32"), "double" (="float64"), "character" (="string"),
		"logical", "list", "factor", "folder".}
	\item{valdim}{The dim attribute for the array to be created, which is a
		vector of length one or more giving the maximal indices in each dimension.}
	\item{compress}{The compression method can be "", "ZIP", "ZIP.fast", "ZIP.default", or "ZIP.max".}
	\item{closezip}{If allowed to compress data, return to read mode after compression.}
}

\details{
	\code{name}  If missing, ``Item n'' is assigned to \code{name}, where \code{n} is
		the number of child nodes + 1.

	\code{val}  If val is \code{list} or \code{data.frame}, the child node(s)
		will be added corresponding to objects in \code{list} or \code{data.frame}.

	\code{storage}  The default value is \code{storage.mode(val)}, "int" denotes
		signed integer, "uint" denotes unsigned integer, 8, 16, 24, 32 and 64 denote
		the number of bits. "bit1" to "bit32" denote the packed data types for 1
		to 32 bits which are packed on disk, and "sbit2" to "sbit32" denote the
		corresponding signed integers.
		"float32" denotes single-precision number, and "float64" denotes
		double-precision number. "string" represents strings of 8-bit characters,
		"wstring" represents strings of 16-bit characters following UTF16 industry
		standard, and "dwstring" represents strings of 32-bit characters following
		UTF32 industry standard. "folder" is to create a folder.

	\code{valdim}  The values in data are taken to be those in the array with the
		leftmost subscript moving fastest. Some of entries can be ZERO!

	\code{compress}  In the comprssion mode, you can not change the none-first
		dimension and read data field. Call \code{\link[gdsfmt]{readmode.gdsn}} to
		return read mode.

	\code{closezip}  Once return to read mode, users can not add data to the data
		field. CoreArray 1.1 will solve this problem.
}

\value{
	An object of class "gdsn" of the added node.
}

\references{\url{http://sourceforge.net/projects/corearray/}}
\author{Xiuwen Zheng \email{zhengx@u.washington.edu}}
\seealso{\code{\link[gdsfmt]{cnt.gdsn}}, \code{\link[gdsfmt]{objdesp.gdsn}},
	\code{\link[gdsfmt]{ls.gdsn}}, \code{\link[gdsfmt]{index.gdsn}},
	\code{\link[gdsfmt]{name.gdsn}}, \code{\link[gdsfmt]{add.gdsn}}}

\examples{
library(gdsfmt)

# cteate the GDS file "test.gds"
(f <- createfn.gds("test.gds"))
L <- -2500:2499

# commom types
add.gdsn(f, "int", val=as.integer(1:10000), compress="ZIP")
add.gdsn(f, "int.matrix", val=matrix(L, nrow=100, ncol=50), valdim=c(110, 60))
add.gdsn(f, "double", val=seq(1, 1000, 0.4))
add.gdsn(f, "logical", val=as.logical(c(rep(c(TRUE, FALSE, NA), 50))))
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
