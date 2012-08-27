#############################################################
#
# DESCRIPTION: test data conversion
#

library(RUnit)
library(gdsfmt)


# create a gds file, read and write data
gds_read_write <- function(class.name, data.kind, zip="")
{
	if (data.kind == 1)
		dta <- seq(-1000, 999)
	else
		dta <- seq(-499, 299)

	# create a new gds file
	gfile <- createfn.gds("tmp.gds")

	# add a 
	node <- add.gdsn(gfile, "data", val=dta, storage=class.name,
		compress=zip, closezip=TRUE)
	rv <- read.gdsn(node)
 
	# close the gds file
	closefn.gds(gfile)
	unlink("tmp.gds")

	rv
}



#############################################################
# list of data types
#

type.list <- c("int8", "int16", "int24", "int32", "int64",
	"sbit2", "sbit3", "sbit4", "sbit5", "sbit6", "sbit7", "sbit8", "sbit9", "sbit10",
	"sbit11", "sbit12", "sbit13", "sbit14", "sbit15", "sbit16", "sbit17", "sbit18",
	"sbit19", "sbit20", "sbit21", "sbit22", "sbit23", "sbit24", "sbit25", "sbit26",
	"sbit27", "sbit28", "sbit29", "sbit30", "sbit31", "sbit32",

	"uint8", "uint16", "uint24", "uint32", "uint64",
	"bit1", "bit2", "bit3", "bit4", "bit5", "bit6", "bit7", "bit8", "bit9", "bit10",
	"bit11", "bit12", "bit13", "bit14", "bit15", "bit16", "bit17", "bit18", "bit19",
	"bit20", "bit21", "bit22", "bit23", "bit24", "bit25", "bit26", "bit27", "bit28",
	"bit29", "bit30", "bit31", "bit32",

	"float32", "float64")

# gdsfmt path
gdsfmt.path <- system.file("unitTests", package="gdsfmt")


# create standard dataset
# dta <- list()
# for (n in type.list)
# {
#	# the first dataset
#	dta[[sprintf("valid1.%s", n)]] <- gds_read_write(n, 1)

#	# the second dataset
#	dta[[sprintf("valid2.%s", n)]] <- gds_read_write(n, 2)
# }
# save(dta, file="/Users/foxsts/Documents/Codes/Rpackages/gdsfmt/inst/unitTests/valid/standard.RData")




#############################################################
#
# test function
#

test.dataconvert <- function()
{
	valid.dta <- get(load(sprintf("%s/valid/standard.RData", gdsfmt.path)))

	for (n in type.list)
	{
		checkEquals(gds_read_write(n, 1), valid.dta[[sprintf("valid1.%s", n)]],
			sprintf("data conversion: %s", n))
		checkEquals(gds_read_write(n, 2), valid.dta[[sprintf("valid2.%s", n)]],
			sprintf("data conversion: %s", n))
	}
}


test.dataconvert.compress <- function()
{
	valid.dta <- get(load(sprintf("%s/valid/standard.RData", gdsfmt.path)))

	for (n in type.list)
	{
		checkEquals(gds_read_write(n, 1, "ZIP"), valid.dta[[sprintf("valid1.%s", n)]],
			sprintf("data conversion: %s", n))
		checkEquals(gds_read_write(n, 2, "ZIP"), valid.dta[[sprintf("valid2.%s", n)]],
			sprintf("data conversion: %s", n))
	}
}

