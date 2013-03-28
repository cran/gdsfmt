# ===========================================================
#     _/_/_/   _/_/_/  _/_/_/_/    _/_/_/_/  _/_/_/   _/_/_/
#      _/    _/       _/             _/    _/    _/   _/   _/
#     _/    _/       _/_/_/_/       _/    _/    _/   _/_/_/
#    _/    _/       _/             _/    _/    _/   _/
# _/_/_/   _/_/_/  _/_/_/_/_/     _/     _/_/_/   _/_/
# ===========================================================
#
# GDS.all.R: the R interface of CoreArray library
#
# Copyright (C) 2013	Xiuwen Zheng
#
# This file is part of CoreArray.
#
# CoreArray is free software: you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License Version 3 as
# published by the Free Software Foundation.
#
# CoreArray is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with CoreArray.
# If not, see <http://www.gnu.org/licenses/>.


##################################################################################
# File Operations
##################################################################################

#############################################################
# To create a new CoreArray Genomic Data Structure (GDS) file
#
createfn.gds <- function(fn)
{
	stopifnot(is.character(fn) & (length(fn)==1))

	r <- .C("gdsCreateGDS", filename=as.character(fn), id=integer(1),
		root=integer(2), err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")
	if (r$err != 0)
	{
		stop(lasterr.gds())
		return(invisible(NULL))
	} else {
		class(r$root) <- "gdsn.class"
		r$readonly <- FALSE
		r$filename <- normalizePath(r$filename)
		class(r) <- "gds.class"
		return(r)
	}
}


#############################################################
# To open an existing file
#
openfn.gds <- function(fn, readonly=TRUE)
{
	stopifnot(is.character(fn) & (length(fn)==1))
	stopifnot(is.logical(readonly) & (length(readonly)==1))

	r <- .C("gdsOpenGDS", filename=as.character(fn[1]), id=integer(1),
		root=integer(2), readonly=as.integer(readonly), err=integer(1),
		NAOK=TRUE, PACKAGE="gdsfmt")
	if (r$err != 0)
	{
		stop(lasterr.gds())
		return(invisible(NULL))
	} else {
		class(r$root) <- "gdsn.class"
		r$readonly <- readonly
		r$filename <- normalizePath(r$filename)
		class(r) <- "gds.class"
		return(r)
	}
}


#############################################################
# To close an open CoreArray Genomic Data Structure (GDS) file
#
closefn.gds <- function(gds)
{
	stopifnot(inherits(gds, "gds.class"))
	r <- .C("gdsCloseGDS", as.integer(gds$id), err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")
	if (r$err != 0)
		stop(lasterr.gds())
	return(invisible(NULL))
}


#############################################################
# To write the data cached in memory to disk
#
sync.gds <- function(gds)
{
	stopifnot(inherits(gds, "gds.class"))
	r <- .C("gdsSyncGDS", as.integer(gds$id), err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")
	if (r$err != 0)
		stop(lasterr.gds())
	return(invisible(NULL))
}


#############################################################
# To clean up fragments of a GDS file
#
cleanup.gds <- function(fn, deep=FALSE, verbose=TRUE)
{
	stopifnot(is.character(fn) & (length(fn)==1))
	stopifnot(is.logical(deep) & (length(deep)==1))

	r <- .C("gdsTidyUp", fn, deep, as.logical(verbose), err=integer(1),
		NAOK=TRUE, PACKAGE="gdsfmt")
	if (r$err != 0) stop(lasterr.gds())

	return(invisible(NULL))
}






##################################################################################
# File Structure Operations
##################################################################################

#############################################################
# To get the number of child nodes for a specified node
#
cnt.gdsn <- function(node)
{
	stopifnot(inherits(node, "gdsn.class"))
	r <- .C("gdsNodeChildCnt", as.integer(node), cnt=as.integer(1), NAOK=TRUE,
		PACKAGE="gdsfmt")
	if (r$cnt < 0)
		stop(lasterr.gds())
	return(r$cnt)
}


#############################################################
# To get the variable name of a node
#
name.gdsn <- function(node, fullname=FALSE)
{
	stopifnot(inherits(node, "gdsn.class"))
	r <- .C("gdsNodeName", as.integer(node), name=character(1),
		as.integer(fullname), err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")
	if (r$err != 0) {
		stop(lasterr.gds())
		return(invisible(NULL))
	} else { return(r$name) }
}


#############################################################
# To rename a GDS node
#
rename.gdsn <- function(node, newname)
{
	stopifnot(inherits(node, "gdsn.class"))
	r <- .C("gdsRenameNode", as.integer(node), as.character(newname),
		err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")
	if (r$err != 0)
		stop(lasterr.gds())
	return(node)
}


#############################################################
# To get a list of names for the child nodes
#
ls.gdsn <- function(node)
{
	if (inherits(node, "gds.class"))
		node <- node$root
	stopifnot(inherits(node, "gdsn.class"))

	cnt <- cnt.gdsn(node)
	r <- .C("gdsNodeEnumName", as.integer(node), names=character(cnt),
		err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")
	if (r$err != 0)
	{
		stop(lasterr.gds())
		return(invisible(NULL))
	} else
		return(r$names)
}


#############################################################
# To get a specified node
#
index.gdsn <- function(node, path=NULL, index=NULL, silent=FALSE)
{
	# check
	if (inherits(node, "gds.class"))
		node <- node$root
	stopifnot(inherits(node, "gdsn.class"))

	if (!is.null(index))
	{
		stopifnot(is.character(index) | is.numeric(index))

		if (is.character(index))
		{
			r <- .C("gdsNodeIndexEx", node=as.integer(node), as.character(index),
				as.integer(length(index)), err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")
		} else {
			r <- .C("gdsNodeIndex", node=as.integer(node), as.integer(index),
				as.integer(length(index)), err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")
		}
		if (r$err != 0)
		{
			if (!silent) stop(lasterr.gds())
			rv <- NULL
		} else {
			rv <- r$node
			class(rv) <- "gdsn.class"
		}
		return(rv)
	} else {
		stopifnot(is.character(path))
		if (length(path) > 1)
			stop("please use '/' as a separator.")
		index.gdsn(node, path=NULL, index=unlist(strsplit(path, "/")), silent=silent)
	}
}


#############################################################
# To get the descritpion of a specified node
#
objdesp.gdsn <- function(node)
{
	stopifnot(inherits(node, "gdsn.class"))
	cnt <- cnt.gdsn(node)
	r <- .C("gdsNodeObjDesp", as.integer(node), type=character(1), name=character(1),
		sv=integer(1), arr=logical(1), dimcnt=integer(1), dimeach=integer(1024),
		maxlen=integer(1), cp=character(1), cpratio=double(1), err=integer(1),
		NAOK=TRUE, PACKAGE="gdsfmt")
	if (r$err == 0)
	{
		if (r$dimcnt > 0)
			d <- rev(r$dimeach[1:r$dimcnt])
		else
			d <- NULL
		rv <- list(type = r$type, name = r$name, svtype = r$sv, is.array = r$arr,
			dim = d, compress = r$cp, cpratio = r$cpratio)
		if (r$maxlen >= 0)
			rv$maxlen <- r$maxlen
	} else {
		stop(lasterr.gds())
	}
	return(rv)
}


#############################################################
# To add a GDS node
#
add.gdsn <- function(node, name, val=NULL, storage=storage.mode(val), valdim=NULL,
	compress=c("", "ZIP", "ZIP.fast", "ZIP.default", "ZIP.max"),
	closezip=FALSE)
{
	if (inherits(node, "gds.class"))
		node <- node$root
	stopifnot(inherits(node, "gdsn.class"))

	if (missing(name))
		name <- paste("Item", cnt.gdsn(node)+1, sep="")

	# if val is factor-type
	if (is.factor(val))
	{
		val.levels <- levels(val)
		val <- as.integer(val)
		storage <- "factor"
	} else if (is.character(val))
	{
		val[is.na(val)] <- ""
	}

	if (storage == "") storage <- "NULL"

	if (is.null(valdim))
	{
		if (!(storage %in% c("NULL", "list", "folder")))
		{
			val <- as.array(val)
			valdim <- dim(val)
		} else
			valdim <- c()
	} else {
		if (storage == "NULL") storage <- "integer"

		if (!is.null(val))
		{
			rv <- add.gdsn(node, name, val=val, storage=storage,
				compress=compress, closezip=FALSE)
			r <- .C("gdsObjSetDim", as.integer(rv), length(valdim), rev(as.integer(valdim)),
				err = integer(1), NAOK = TRUE, PACKAGE = "gdsfmt")
			if (r$err != 0) stop(lasterr.gds())
			return(rv)
		}
	}

	if (is.character(val))
		MaxLen <- max(nchar(val))
	else
		MaxLen <- 1

	r <- .C("gdsAddNode", node=as.integer(node), as.character(name),
		as.character(tolower(storage)), as.character(compress[1]),
		length(valdim), as.integer(rev(valdim)), as.integer(MaxLen), is.null(val),
		err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")

	if (r$err == 0)
	{
		rv <- r$node; class(rv) <- "gdsn.class"
		if (!is.null(val))
		{
			if (!(storage %in% c("NULL", "list", "folder")))
			{
				append.gdsn(rv, val)
				if (compress[1] != "")
				{
					if (closezip) readmode.gdsn(rv)
				}
			}
		}

		if (storage == "list")
		{
			put.attr.gdsn(rv, "R.class", class(val))
			iNames <- names(val); iN <- 1
			for (v in val)
			{
				add.gdsn(rv, iNames[iN], v, compress=compress, closezip=closezip)
				iN <- iN + 1
			}
		} else if (storage == "logical")
		{
			put.attr.gdsn(rv, "R.logical")
		} else if (storage == "factor")
		{
			put.attr.gdsn(rv, "R.class", "factor")
			put.attr.gdsn(rv, "R.levels", val.levels)
		}

		return(rv)
	} else {
		stop(lasterr.gds())
		return()
	}
}


#############################################################
# To add a GDS node with a file
#
addfile.gdsn <- function(node, name, filename,
	compress=c("ZIP", "ZIP.fast", "ZIP.default", "ZIP.max", ""))
{
	if (inherits(node, "gds.class"))
		node <- node$root
	stopifnot(inherits(node, "gdsn.class"))
	stopifnot(is.character(filename) & (length(filename)==1))

	if (missing(name))
		name <- paste("Item", cnt.gdsn(node)+1, sep="")

	r <- .C("gdsAddFile", node=as.integer(node), as.character(name), as.character(filename[1]),
		as.character(compress[1]), err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")

	if (r$err != 0) stop(lasterr.gds())
	rv <- r$node; class(rv) <- "gdsn.class"
	return(rv)
}


#############################################################
# To delete a specified node
#
delete.gdsn <- function(node)
{
	stopifnot(inherits(node, "gdsn.class"))
	r <- .C("gdsDeleteNode", node=as.integer(node), err=integer(1),
		NAOK=TRUE, PACKAGE="gdsfmt")
	if (r$err != 0)
		stop(lasterr.gds())
	return(invisible(NULL))
}





##################################################################################
# Attribute
##################################################################################

#############################################################
# To add an attribute to a GDS node
#
put.attr.gdsn <- function(node, name, val=NULL)
{
	stopifnot(inherits(node, "gdsn.class"))
	name <- as.character(name)
	if (name != "")
	{
		storage <- switch( storage.mode(val),
			"NULL" = 0, "integer" = 1, "double" = 2, "character" = 3, "logical" = 4,
			stop("Unsupported type!") )
		if (is.null(val))
			val <- integer(0)
		else
			val <- as.vector(val)
		r <- .C("gdsPutAttr", as.integer(node), name, as.integer(storage),
			val, length(val), err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")
		if (r$err != 0)
			stop(lasterr.gds())
		return(invisible(NULL))
	} else
		stop("'attribute' needs a name!")
}


#############################################################
# To get the attributes of a CoreArray GDS node
#
get.attr.gdsn <- function(node)
{
	stopifnot(inherits(node, "gdsn.class"))
	r <- .C("gdsAttrCnt", as.integer(node), Cnt=integer(1), err=integer(1),
		NAOK=TRUE, PACKAGE="gdsfmt")
	if (r$err != 0) stop(lasterr.gds())

	if (r$Cnt > 0)
	{
		rv <- vector("list", r$Cnt)
		r1 <- .C("gdsAttrType", as.integer(node), rtype=integer(r$Cnt),
			rcnt=integer(r$Cnt), err=integer(1),
			NAOK=TRUE, PACKAGE="gdsfmt")
		if (r1$err != 0) stop(lasterr.gds())

		rn <- character(r$Cnt)
		for (i in 1:r$Cnt)
		{
			rt <- r1$rtype[i]
			L <- r1$rcnt[i]
			r2 <- .C( "gdsGetAttr", as.integer(node), as.integer(i), rt,
				data=switch(rt+1, integer(0), integer(L), double(L), character(L), logical(L)),
				name=character(1), err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")
			if (r2$err == 0)
			{
				if (rt == 0) r2$data <- NULL
				if (!is.null(r2$data)) rv[[i]] <- r2$data
				rn[i] <- r2$name
			} else
				stop(lasterr.gds())
		}
		names(rv) <- rn
	} else
		rv <- NULL
	return(rv)
}


#############################################################
# To remove an attribute from a CoreArray GDS node
#
delete.attr.gdsn <- function(node, name)
{
	stopifnot(inherits(node, "gdsn.class"))
	r <- .C("gdsDeleteAttr", as.integer(node), as.character(name),
		err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")
	if (r$err != 0)
		stop(lasterr.gds())
	return(invisible(NULL))
}





##################################################################################
# Data Operations
##################################################################################

#############################################################
# To modify the data compression mode of data field
#
compression.gdsn <- function(node,
	compress=c("", "ZIP", "ZIP.fast", "ZIP.default", "ZIP.max") )
{
	stopifnot(inherits(node, "gdsn.class"))
	r <- .C("gdsObjCompress", as.integer(node), as.character(compress[1]),
		err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")
	if (r$err != 0)
		stop(lasterr.gds())
	return(node)
}


#############################################################
# Get into read mode of compression
#
readmode.gdsn <- function(node)
{
	stopifnot(inherits(node, "gdsn.class"))
	r <- .C("gdsObjPackClose", as.integer(node), err=integer(1),
		NAOK=TRUE, PACKAGE="gdsfmt")
	if (r$err != 0)
		stop(lasterr.gds())
	return(node)
}


#############################################################
# To set the new dimension of the data field for a GDS node
#
setdim.gdsn <- function(node, valdim)
{
	stopifnot(inherits(node, "gdsn.class"))
	r <- .C("gdsObjSetDim", as.integer(node), length(valdim),
		rev(as.integer(valdim)), err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")
	if (r$err != 0)
		stop(lasterr.gds())
	return(node)
}


#############################################################
# Append data to a specified variable
#
append.gdsn <- function(node, val, check=TRUE)
{
	stopifnot(inherits(node, "gdsn.class"))
	if (is.character(val))
	{
		val[is.na(val)] <- ""
	}
	r <- switch( storage.mode(val),
		"integer" =
			.C( "gdsObjAppend", as.integer(node), as.integer(1), as.integer(val),
					length(val), CntWarn=integer(1), err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt"),
		"double" =
			.C( "gdsObjAppend", as.integer(node), as.integer(2), as.double(val),
					length(val), CntWarn=integer(1), err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt"),
		"numeric" =
			.C( "gdsObjAppend", as.integer(node), as.integer(2), as.double(val),
					length(val), CntWarn=integer(1), err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt"),
		"character" =
			.C( "gdsObjAppend", as.integer(node), as.integer(3), as.character(val),
					length(val), CntWarn=integer(1), err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt"),
		"logical" =
			.C( "gdsObjAppend", as.integer(node), as.integer(1), as.integer(val),
					length(val), CntWarn=integer(1), err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt"),
		stop("only support integer, double and character.") )
	if (r$err == 0)
	{
		if (r$CntWarn != 0)
			warning("No a complete sub-dataset.");
	} else {
		stop(lasterr.gds())
	}
	return(invisible(NULL))
}


#############################################################
# Read data field of a GDS node
#
read.gdsn <- function(node, start, count)
{
	stopifnot(inherits(node, "gdsn.class"))

	if (missing(start))
	{
		if (missing(count))
		{
			rvattr <- get.attr.gdsn(node)
			rvclass <- rvattr$R.class
			if (!is.null(rvclass))
			{
				if (rvclass %in% c("list", "data.frame"))
				{
					cnt <- cnt.gdsn(node)
					r <- vector("list", cnt)
					if (cnt > 0)
					{
						for (i in 1:cnt)
						{
							n <- index.gdsn(node, index=i)
							if (!is.null(d <- read.gdsn(n)))
								r[[i]] <- d
							names(r)[i] <- name.gdsn(n)
						}
					}
					if (rvclass == "data.frame")
						r <- as.data.frame(r, stringsAsFactors=FALSE)
					class(r) <- rvclass
					return(r)
				}
			} else {
				rvclass <- ""
			}

			r <- .C("gdsxObjDesp", as.integer(node), cnt=as.integer(-1),
				rstart=as.integer(rep(1, 256)), rcount=as.integer(rep(-1, 256)),
				total=integer(1), rtype=integer(1), err=integer(1),
				NAOK=TRUE, PACKAGE="gdsfmt")

			rfactor <- ("R.factor" %in% names(rvattr)) | (rvclass %in% "factor")

		} else {
			stop("start is missing!")
		}
	} else if (missing(count)) {
			stop("count is missing")
		} else {

			stopifnot(length(start)==length(count))

			rvattr <- get.attr.gdsn(node)
			rfactor <- ("R.factor" %in% names(rvattr))

			r <- .C("gdsxObjDesp", as.integer(node), cnt=as.integer(length(start)),
				rstart=rev(as.integer(start)), rcount=rev(as.integer(count)),
				total=integer(1), rtype=integer(1), err=integer(1),
				NAOK=TRUE, PACKAGE="gdsfmt")
		}

	if (r$err == 0)
	{
		r <- .C("gdsObjReadData", as.integer(node), cnt=as.integer(r$cnt),
			as.integer(r$rstart), count=as.integer(r$rcount), as.integer(r$rtype),
			data=switch(r$rtype+1, integer(0), integer(r$total), double(r$total),
				character(r$total), logical(r$total)),
			err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")

		if (rfactor)
		{
			if (is.character(r$data))
			{
				s <- r$data
				r$data <- factor(s)
				r$data[s==""] <- NA
			} else {
				attr(r$data, "levels") <- rvattr$R.levels
				attr(r$data, "class") <- "factor"
			}
		}

		if (r$err == 0)
		{
			if (r$cnt == 2)
			{
				if ((r$count[1] == 1) || (r$count[2] == 1))
					return(r$data)
				else
					return(array(r$data, dim=rev(r$count[1:r$cnt])))
			} else if (r$cnt <= 1)
				return(r$data)
			else
				return(array(r$data, dim=rev(r$count[1:r$cnt])))
		}
	}
	stop(lasterr.gds())
	return(invisible(NULL))
}


#############################################################
# Read data field of a GDS node
#
readex.gdsn <- function(node, sel=NULL)
{
	stopifnot(inherits(node, "gdsn.class"))
	stopifnot(is.null(sel) | is.logical(sel) | is.list(sel))

	if (!is.null(sel))
	{
		if (is.vector(sel) & !is.list(sel))
			sel <- list(d1=sel)
		DimSel <- sapply(sel, function(x) {
				if (!is.logical(x)) stop("The element of `sel' should be a logical variable.")
				length(x)
			})

		# check
		n <- objdesp.gdsn(node)
		if (length(n$dim) != length(DimSel))
			stop("The dimension of selection is not correct.")
		if (any(n$dim != DimSel))
			stop("The dimension of selection is not correct.")

		rvattr <- get.attr.gdsn(node)
		rfactor <- ("R.factor" %in% names(rvattr))
		DimCnt <- sapply(sel, function(x) sum(x, na.rm=TRUE))
		totalcnt <- prod(DimCnt)

		# check
		rd <- .C("gdsxObjType", as.integer(node), rtype=integer(1), err=integer(1),
			NAOK=TRUE, PACKAGE="gdsfmt")
		if (rd$err != 0) stop(lasterr.gds())

		# no selection
		if (totalcnt <= 0)
			return(switch(rd$rtype, integer(0), double(0), character(0), logical(0)))

		# reading
		r <- .C("gdsObjReadExData", as.integer(node),
			as.logical(unlist(sel)), as.integer(rd$rtype),
			data=switch(rd$rtype+1, integer(0), integer(totalcnt), double(totalcnt),
				character(totalcnt), logical(totalcnt)),
			err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")

		if (rfactor)
		{
			s <- r$data
			r$data <- factor(s)
			r$data[s==""] <- NA
		}

		if (r$err == 0)
		{
			if (length(DimCnt) <= 1)
				return(r$data)
			else
				return(array(r$data, dim=DimCnt))
		} else {
			stop(lasterr.gds())
		}
	} else {
		return(read.gdsn(node))
	}
}


#############################################################
# Apply functions over array margins of a GDS node
#
apply.gdsn <- function(node, margin, FUN, selection=NULL,
	as.is = c("list", "integer", "double", "character", "none"), ...)
{
	# check
	if (inherits(node, "gdsn.class"))
	{
		stopifnot(inherits(node, "gdsn.class"))
		stopifnot(is.numeric(margin) & (length(margin)==1))
		stopifnot(is.null(selection) | is.list(selection))

		node <- list(node)
		if (!is.null(selection))
			selection <- list(selection)
	} else {
		if (!is.list(node))
			stop("'node' should be 'gdsn.class' or a list of 'gdsn.class' objects.")
		for (i in 1:length(node))
		{
			if (!inherits(node[[i]], "gdsn.class"))
				stop(sprintf("node[[%d]] should be an object of 'gdsn' class.", i))
		}
	
		stopifnot(is.numeric(margin))
		stopifnot(length(margin) == length(node))
	
		stopifnot(is.null(selection) | is.list(selection))
		if (!is.null(selection))
			stopifnot(length(selection) == length(node))
	}

	as.is <- match.arg(as.is)
	FUN <- match.fun(FUN)

	ans <- .Call("gds_apply_call", node, as.integer(margin), FUN,
		selection, as.is, new.env())
	if (is.null(ans)) ans <- invisible()
	ans
}


#############################################################
# Apply functions over array margins of a list of GDS nodes in parallel
#
clusterApply.gdsn <- function(cl, gds.fn, node.name, margin, FUN, selection=NULL,
	as.is = c("list", "integer", "double", "character", "none"), ...)
{
	#########################################################
	# library
	#
	if (!require(parallel))
	{
		if (!require(snow))
			stop("the `parallel' or `snow' package should be installed.")
	}


	#########################################################
	# check
	#
	stopifnot(inherits(cl, "cluster"))
	stopifnot(is.character(gds.fn) & (length(gds.fn)==1))
	stopifnot(is.character(node.name))
	stopifnot(is.numeric(margin) & (length(margin)==length(node.name)))
	margin <- as.integer(margin)

	if (!is.null(selection))
	{
		if (!is.list(selection[[1]]))
			selection <- list(selection)
	}


	as.is <- match.arg(as.is)
	FUN <- match.fun(FUN)


	#########################################################
	# new selection
	#

	ifopen <- TRUE
	gfile <- openfn.gds(gds.fn)
	on.exit({ if (ifopen) closefn.gds(gfile) })

	nd_nodes <- vector("list", length(node.name))
	names(nd_nodes) <- names(node.name)
	for (i in 1:length(nd_nodes))
	{
		v <- index.gdsn(gfile, path=node.name[i], silent=TRUE)
		nd_nodes[[i]] <- v
		if (is.null(v))
		{
			stop(sprintf("There is no node \"%s\" in the specified gds file.",
				node.name[i]))
		}
	}

	new.selection <- .Call("gds_apply_create_selection", nd_nodes, margin, selection)

	# the count of elements
	MarginCount <- sum(new.selection[[1]][[ margin[1] ]], na.rm=TRUE)
	if (MarginCount <= 0)
		return(invisible())


	#########################################################
	# run
	#

	if (length(cl) > 1)
	{
		# close the GDS file
		ifopen <- FALSE
		closefn.gds(gfile)

		clseq <- splitIndices(MarginCount, length(cl))
		sel.list <- vector("list", length(cl))
		
		# for - loop: multi core
		for (i in 1:length(cl))
		{
			tmp <- new.selection
	
			# for - loop: multiple variables
			for (j in 1:length(tmp))
			{
				sel <- tmp[[j]]
				idx <- which(sel[[ margin[j] ]])
				flag <- rep(FALSE, length(sel[[ margin[j] ]]))
				flag[ idx[ clseq[[i]] ] ] <- TRUE
				sel[[ margin[j] ]] <- flag
				tmp[[j]] <- sel
			}
	
			sel.list[[i]] <- tmp
		}

		# enumerate
		ans <- clusterApply(cl, sel.list, fun =
				function(sel, gds.fn, node.name, margin, FUN, as.is, ...)
			{
				# load the package
				library(gdsfmt)

				# open the file
				gfile <- openfn.gds(gds.fn)
				on.exit({ closefn.gds(gfile) })

				nd_nodes <- vector("list", length(node.name))
				names(nd_nodes) <- names(node.name)
				for (i in 1:length(nd_nodes))
					nd_nodes[[i]] <- index.gdsn(gfile, path=node.name[i])

				# apply
				apply.gdsn(nd_nodes, margin, FUN, sel, as.is, ...)

			}, gds.fn=gds.fn, node.name=node.name, margin=margin,
				FUN=FUN, as.is=as.is, ...
		)

		if (as.is != "none")
		{
			ans <- unlist(ans, recursive=FALSE)
		}

		ans
	} else{
		apply.gdsn(nd_nodes, margin, FUN, selection, as.is, ...)
	}
}


#############################################################
# Write data to a GDS node
#
write.gdsn <- function(node, val, start, count)
{
	stopifnot(inherits(node, "gdsn.class"))
	stopifnot(!missing(val))

	if (missing(start))
	{
		if (missing(count))
		{
			if (is.character(val)) val[is.na(val)] <- ""

			rt <- switch( storage.mode(val),
				"integer" = 1, "double" = 2, "character" = 3, "logical" = 1,
				stop("only support integer, double and character.") )
			val <- as.array(val); dimval <- dim(val)
			r <- .C("gdsObjWriteAll", as.integer(node), length(dimval),
				rev(as.integer(dimval)), as.integer(rt),
				switch(rt, as.integer(val), as.double(val), as.character(val)),
				err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")
			if (r$err != 0)
				stop(lasterr.gds())
			return(invisible(NULL))

		} else {
			stop("start is missing!")
		}
	} else if (missing(count)) {
		stop("count is missing")
	} else {

		stopifnot(length(start)==length(count))
		r <- .C("gdsxObjDesp", as.integer(node), cnt=as.integer(length(start)),
			rstart=as.integer(rev(start)), rcount=as.integer(rev(count)),
			total=integer(1), rtype=integer(1), err=integer(1), NAOK=TRUE,
			PACKAGE="gdsfmt")

		if (r$err == 0)
		{
			if (r$total != length(val))
				stop(paste("the length of val ", length(val),
					" is not equal to count(", r$total, ").", sep=""))
			if (is.character(val)) val[is.na(val)] <- ""

			r <- .C("gdsObjWriteData", as.integer(node), as.integer(r$cnt),
				as.integer(r$rstart), as.integer(r$rcount), as.integer(r$rtype),
				switch(r$rtype, as.integer(val), as.double(val),
					as.character(val), as.integer(val)),
				err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")
			if (r$err != 0)
				stop(lasterr.gds())

		} else
			stop(lasterr.gds())
	}
	return(invisible(NULL))
}


#############################################################
# Assign a GDS variable from another variable
#
assign.gdsn <- function(dest.obj, src.obj, append)
{
	stopifnot(inherits(dest.obj, "gdsn.class"))
	stopifnot(inherits(src.obj, "gdsn.class"))
	stopifnot(is.logical(append) & (length(append)==1))

	# call C function
	.Call("gdsAssign", dest.obj, src.obj, append, PACKAGE="gdsfmt")

	invisible(NULL)
}


#############################################################
# To get a file from a stream container
#
getfile.gdsn <- function(node, out.filename)
{
	stopifnot(inherits(node, "gdsn.class"))
	stopifnot(is.character(out.filename))

	r <- .C("gdsGetFile", as.integer(node), out.filename, err=integer(1),
		NAOK=TRUE, PACKAGE="gdsfmt")
	if (r$err != 0) stop(lasterr.gds())

	return(invisible(NULL))
}





##################################################################################
# Error function
##################################################################################

#############################################################
# Return the last error
#
lasterr.gds <- function()
{
	r <- .C("gdsLastErrGDS", s=character(1), NAOK=TRUE, PACKAGE="gdsfmt")
	r$s
}





##################################################################################
# R Generic functions
##################################################################################

print.gds.class <- function(x, all=FALSE, ...)
{
	enum <- function(node, space, level)
	{
		at <- get.attr.gdsn(node)
		if (!all)
		{
			if ("R.invisible" %in% names(at))
				return(invisible(NULL))
		}
		n <- objdesp.gdsn(node)
		cnt <- cnt.gdsn(node)
		cat(space, "+ ", name.gdsn(node), "	[ ", n$type, sep="")

		# if logical, factor, list, or data.frame
		if ("R.logical" %in% names(at))
			cat(",logical")
		else if ("R.class" %in% names(at))
		{
			if (n$type != "")
				cat(",")
			if (!is.null(at$R.class))
				cat(paste(at$R.class, sep="", collapse=","))
		}
		cat(" "); cat(n$dim, sep="x")
		if (n$compress!="") cat("", n$compress)
		if (is.finite(n$cpratio))
			cat(sprintf("(%0.2f%%)", 100*n$cpratio))
		if (length(get.attr.gdsn(node)) > 0)
			cat(" ] *\n")
		else
			cat(" ]\n")

		r <- .C("gdsNodeEnumPtr", as.integer(node), id=integer(cnt*2),
			err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")
		if ((r$err == 0) & (cnt > 0))
		{
			for (i in 0:(cnt-1))
			{
				m <- r$id[(2*i+1):(2*i+2)]
				class(m) <- "gdsn.class"
				if (level==1)
					s <- paste("|--", space, sep="")
				else
					s <- paste("|  ", space, sep="")
				enum(m, s, level+1)
			}
		}
	}

	# check
	stopifnot(inherits(x, "gds.class"))
	stopifnot(is.logical(all) & length(all)==1)

	rv <- .C("gdsFileValid", x$id, valid=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")
	if (rv$valid == 0)
	{
		cat("The GDS file has been closed.\n")
	} else {
		cat("file name: ", x$filename, "\n\n", sep="");
		enum(x$root, "", 1)
	}
}

print.gdsn.class <- function(x, expand=TRUE, all=FALSE, ...)
{
	enum <- function(node, space, level, expand, fullname)
	{
		at <- get.attr.gdsn(node)
		if (!all)
		{
			if ("R.invisible" %in% names(at))
				return(invisible(NULL))
		}
		n <- objdesp.gdsn(node)
		cnt <- cnt.gdsn(node)
		cat(space, "+ ", name.gdsn(node, fullname), "	[ ", n$type, sep="")

		# if logical, factor, list, or data.frame
		if ("R.logical" %in% names(at))
			cat(",logical")
		else if ("R.class" %in% names(at))
		{
			if (n$type != "")
				cat(",")
			if (!is.null(at$R.class))
				cat(paste(at$R.class, sep="", collapse=","))
		}
		cat(" "); cat(n$dim, sep="x")
		if (n$compress != "") cat("", n$compress)
		if (is.finite(n$cpratio))
			cat(sprintf("(%0.2f%%)", 100*n$cpratio))
		if (length(get.attr.gdsn(node)) > 0)
			cat(" ] *\n")
		else
			cat(" ]\n")

		if (expand)
		{
			r <- .C("gdsNodeEnumPtr", as.integer(node), id=integer(cnt*2),
				err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")
			if ((r$err == 0) & (cnt > 0))
			{
				for (i in 0:(cnt-1))
				{
					m <- r$id[(2*i+1):(2*i+2)]
					class(m) <- "gdsn.class"
					if (level==1)
						s <- paste("|--", space, sep="")
					else
						s <- paste("|  ", space, sep="")
					enum(m, s, level+1, TRUE, FALSE)
				}
			}
		}
	}

	# check
	stopifnot(inherits(x, "gdsn.class"))
	stopifnot(is.logical(all) & length(all)==1)
	stopifnot(is.logical(expand) & length(expand)==1)

	rv <- .C("gdsNodeValid", as.integer(x), valid=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")
	if (rv$valid == 0)
	{
		cat("The GDS file has been closed.\n")
	} else {
		enum(x, "", 1, expand, TRUE)
	}
}





##################################################################################
# Unit testing
##################################################################################

#############################################################
# run all unit tests
#
gdsUnitTest <- function()
{
	# load R packages
	if (!require(RUnit))
		stop("Please install RUnit package!")

	# define a test suite
	myTestSuite <- defineTestSuite("gdsfmt examples",
		system.file("unitTests", package = "gdsfmt"))

	# run the test suite
	testResult <- runTestSuite(myTestSuite)

	# print detailed text protocol to standard out:
	printTextProtocol(testResult)

	# return
	invisible()
}
