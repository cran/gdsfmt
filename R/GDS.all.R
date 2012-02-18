
#############################################################
# File Operations
#############################################################

createfn.gds <- function(fn)
{
	r <- .C("gdsCreateGDS", filename=as.character(fn), id=integer(1),
		root=integer(2), err=integer(1), NAOK=TRUE,
		PACKAGE="gdsfmt")
	if (r$err != 0)
	{
		stop(lasterr.gds())
		return(invisible(NULL))
	} else {
		class(r$root) <- "gdsn"
		class(r) <- "gdsclass"
		return(r)
	}
}

openfn.gds <- function(fn, readonly=TRUE)
{
	r <- .C("gdsOpenGDS", filename=as.character(fn[1]), id=integer(1),
		root=integer(2), readonly=as.integer(readonly), err=integer(1),
		NAOK=TRUE, PACKAGE="gdsfmt")
	if (r$err != 0)
	{
		stop(lasterr.gds())
		return(invisible(NULL))
	} else {
		class(r$root) <- "gdsn"
		class(r) <- "gdsclass"
		return(r)
	}
}

closefn.gds <- function(gds)
{
	stopifnot(class(gds)=="gdsclass")
	r <- .C("gdsCloseGDS", as.integer(gds$id), err=integer(1), NAOK=TRUE,
		PACKAGE="gdsfmt")
	if (r$err != 0)
		stop(lasterr.gds())
	return(invisible(NULL))
}

sync.gds <- function(gds)
{
	stopifnot(class(gds)=="gdsclass")
	r <- .C("gdsSyncGDS", as.integer(gds$id), err=integer(1), NAOK=TRUE,
		PACKAGE="gdsfmt")
	if (r$err != 0)
		stop(lasterr.gds())
	return(invisible(NULL))
}



#############################################################
# File Structure Operations
#############################################################

cnt.gdsn <- function(node)
{
	stopifnot(class(node)=="gdsn")
	r <- .C("gdsNodeChildCnt", as.integer(node), cnt=as.integer(1),
		NAOK=TRUE, PACKAGE="gdsfmt")
	if (r$cnt < 0)
		stop(lasterr.gds())
	return(r$cnt)
}

name.gdsn <- function(node, fullname=FALSE)
{
	stopifnot(class(node)=="gdsn")
	r <- .C("gdsNodeName", as.integer(node), name=character(1),
		as.integer(fullname), err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")
	if (r$err != 0) {
		stop(lasterr.gds())
		return(invisible(NULL))
	} else { return(r$name) }
}

rename.gdsn <- function(node, newname)
{
	stopifnot(class(node)=="gdsn")
	r <- .C("gdsRenameNode", as.integer(node), as.character(newname),
		err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")
	if (r$err != 0)
		stop(lasterr.gds())
	return(node)
}

ls.gdsn <- function(node)
{
	if (class(node)=="gdsclass")
		node <- node$root
	stopifnot(class(node)=="gdsn")
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

index.gdsn <- function(node, index, silent=FALSE)
{
	if (class(node)=="gdsclass")
		node <- node$root
	stopifnot(class(node)=="gdsn")
	if (missing(index))
		return(node)
	cnt <- cnt.gdsn(node)
	if (is.character(index))
		r <- .C("gdsNodeIndexEx", node=as.integer(node), as.character(index),
			as.integer(length(index)), err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")
	else r <- .C("gdsNodeIndex", node=as.integer(node), as.integer(index),
		as.integer(length(index)), err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")
	if (r$err != 0)
	{
		if (!silent) stop(lasterr.gds())
		rv <- NULL
	} else {
		rv <- r$node
		class(rv) <- "gdsn"
	}
	return(rv)
}

objdesp.gdsn <- function(node)
{
	stopifnot(class(node)=="gdsn")
	cnt <- cnt.gdsn(node)
	r <- .C("gdsNodeObjDesp", as.integer(node), desp=character(1), name=character(1),
		sv=integer(1), dimcnt=integer(1), dimeach=integer(1024), cp=character(1),
		cpratio=double(1), err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")
	if (r$err != 0)
	{
		stop(lasterr.gds())
		return(invisible(NULL))
	} else {
		if (r$dimcnt > 0)
			d <- rev(r$dimeach[1:r$dimcnt])
		else
			d <- NULL
		return( list(desp=r$desp, name=r$name, svtype=r$sv, dim=d, compress=r$cp,
			cpratio=r$cpratio) )
	}
}

add.gdsn <- function(node, name, val=NULL, storage=storage.mode(val), valdim=NULL,
	compress=c("", "ZIP", "ZIP.fast", "ZIP.default", "ZIP.max"),
	closezip=FALSE)
{
	if (class(node)=="gdsclass")
		node <- node$root
	stopifnot(class(node)=="gdsn")

	if (missing(name))
		name <- paste("Item", cnt.gdsn(node)+1, sep="")

	# if val is factor
	if (is.factor(val))
	{
		tmp <- val
		val <- as.character(tmp)
		val[is.na(tmp)] <- ""; rm(tmp)
		storage <- "factor"
	} else if (is.character(val))
	{
		val[is.na(val)] <- ""
	}

	if (storage == "") storage <- "NULL"

	if (is.null(valdim))
	{
		if (!(storage %in% c("NULL", "list")))
		{
			val <- as.array(val)
			valdim <- dim(val)
		} else
			valdim <- c()
	} else {
		if (storage == "NULL") storage <- "integer"
	}

	if (is.character(val))
		MaxLen <- max(nchar(val))
	else
		MaxLen <- 1

	r <- .C("gdsAddNode", node=as.integer(node), as.character(name),
		as.character(storage), as.character(compress[1]),
		length(valdim), as.integer(rev(valdim)), as.integer(MaxLen), is.null(val),
		err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")

	if (r$err == 0)
	{
		rv <- r$node; class(rv) <- "gdsn"
		if (!is.null(val))
		{
			if (!(storage %in% c("NULL", "list")))
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
		} else if (storage == "logical") {
			put.attr.gdsn(rv, "R.logical")
		} else if (storage == "factor") {
			put.attr.gdsn(rv, "R.factor")
		}

		return(rv)
	} else {
		stop(lasterr.gds())
		return()
	}
}

addfile.gdsn <- function(node, name, filename,
	compress=c("ZIP", "ZIP.fast", "ZIP.default", "ZIP.max", ""))
{
	if (class(node)=="gdsclass")
		node <- node$root
	stopifnot(class(node)=="gdsn")
	stopifnot(is.character(filename))

	if (missing(name))
		name <- paste("Item", cnt.gdsn(node)+1, sep="")

	r <- .C("gdsAddFile", node=as.integer(node), as.character(name), as.character(filename[1]),
		as.character(compress[1]), err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")

	if (r$err != 0) stop(lasterr.gds())
	rv <- r$node; class(rv) <- "gdsn"
	return(rv)
}

delete.gdsn <- function(node)
{
	stopifnot(class(node)=="gdsn")
	r <- .C("gdsDeleteNode", node=as.integer(node), err=integer(1),
		NAOK=TRUE, PACKAGE="gdsfmt")
	if (r$err != 0)
		stop(lasterr.gds())
	return(invisible(NULL))
}



#############################################################
# Attribute
#############################################################

put.attr.gdsn <- function(node, name, val=NULL)
{
	stopifnot(class(node)=="gdsn")
	name <- as.character(name)
	if (name != "")
	{
		storage <- switch( storage.mode(val),
			"NULL" = 0, "integer" = 1, "double" = 2,
			"character" = 3, "logical" = 4,
			stop("Unsupported type!") )
		r <- .C("gdsPutAttr", as.integer(node), name, as.integer(storage),
			val, err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")
		if (r$err != 0)
			stop(lasterr.gds())
		return(invisible(NULL))
	} else
		stop("'attribute' needs a name!")
}

get.attr.gdsn <- function(node)
{
	stopifnot(class(node)=="gdsn")
	r <- .C("gdsAttrCnt", as.integer(node), Cnt=integer(1), NAOK=TRUE,
		PACKAGE="gdsfmt")
	if (r$Cnt > 0)
	{
		rv <- vector("list", r$Cnt)
		r1 <- .C("gdsAttrType", as.integer(node), rtype=integer(r$Cnt),
			NAOK=TRUE, PACKAGE="gdsfmt")
		rn <- character(r$Cnt)
		for (i in 1:r$Cnt)
		{
			rt <- r1$rtype[i]
			r2 <- .C( "gdsGetAttr", as.integer(node), as.integer(i), rt,
				data=switch(rt, integer(1), double(1), character(1), logical(1)),
				name=character(1), err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")
			if (r2$err == 0) {
				if (!is.null(r2$data))
					rv[[i]] <- r2$data
				rn[i] <- r2$name
			} else
				stop(lasterr.gds())
		}
		names(rv) <- rn
	} else
		rv <- NULL
	return(rv)
}

delete.attr.gdsn <- function(node, name)
{
	stopifnot(class(node)=="gdsn")
	r <- .C("gdsDeleteAttr", as.integer(node), as.character(name),
		err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")
	if (r$err != 0)
		stop(lasterr.gds())
	return(invisible(NULL))
}



#############################################################
# Data Operations
#############################################################

compression.gdsn <- function(node,
	compress=c("", "ZIP", "ZIP.fast", "ZIP.default", "ZIP.max") )
{
	stopifnot(class(node)=="gdsn")
	r <- .C("gdsObjCompress", as.integer(node), as.character(compress[1]),
		err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")
	if (r$err != 0)
		stop(lasterr.gds())
	return(node)
}

readmode.gdsn <- function(node)
{
	stopifnot(class(node)=="gdsn")
	r <- .C("gdsObjPackClose", as.integer(node), err=integer(1),
		NAOK=TRUE, PACKAGE="gdsfmt")
	if (r$err != 0)
		stop(lasterr.gds())
	return(node)
}

setdim.gdsn <- function(node, valdim)
{
	stopifnot(class(node)=="gdsn")
	r <- .C("gdsObjSetDim", as.integer(node), length(valdim),
		rev(as.integer(valdim)), err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")
	if (r$err != 0)
		stop(lasterr.gds())
	return(node)
}

append.gdsn <- function(node, val, check=TRUE)
{
	stopifnot(class(node)=="gdsn")
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
			warning("Not a complete sub-dataset.");
	} else {
		stop(lasterr.gds())
	}
	return(invisible(NULL))
}

read.gdsn <- function(node, start, count)
{
	stopifnot(class(node)=="gdsn")

	if (missing(start))
	{
		if (missing(count))
		{
			rvattr <- get.attr.gdsn(node)
			rvclass <- rvattr$R.class
			if (!is.null(rvclass))
			{
				cnt <- cnt.gdsn(node)
				r <- vector("list", cnt)
				if (cnt > 0)
				{
					for (i in 1:cnt)
					{
						n <- index.gdsn(node, i)
						if (!is.null(d <- read.gdsn(n)))
							r[[i]] <- d
						names(r)[i] <- name.gdsn(n)
					}
				}
				if (rvclass == "data.frame")
					r <- as.data.frame(r, stringsAsFactors=FALSE)
				if (!(rvclass %in% c("list", "data.frame")))
					class(r) <- rvclass
				return(r)
			}

			r <- .C("gdsxObjDesp", as.integer(node), cnt=as.integer(-1),
				rstart=as.integer(rep(1, 256)), rcount=as.integer(rep(-1, 256)),
				total=integer(1), rtype=integer(1), err=integer(1),
				NAOK=TRUE, PACKAGE="gdsfmt")

			rfactor <- ("R.factor" %in% names(rvattr))

		} else {
			stop("start is missing!")
		}
	} else if (missing(count)) {
			stop("count is missing")
		} else {

			stopifnot(length(start)==length(count))

			rfactor <- FALSE

			r <- .C("gdsxObjDesp", as.integer(node), cnt=as.integer(length(start)),
				rstart=rev(as.integer(start)), rcount=rev(as.integer(count)),
				total=integer(1), rtype=integer(1), err=integer(1),
				NAOK=TRUE, PACKAGE="gdsfmt")
		}

	if (r$err == 0)
	{
		r <- .C("gdsObjReadData", as.integer(node), cnt=as.integer(r$cnt),
			as.integer(r$rstart), count=as.integer(r$rcount), as.integer(r$rtype),
			data=switch(r$rtype, integer(r$total), double(r$total),
			character(r$total), logical(r$total)),
			err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")

		if (rfactor)
		{
			s <- r$data
			r$data <- factor(s)
			r$data[s==""] <- NA
		}

		if (r$err == 0)
		{
			if (r$cnt == 2)
			{
				if ((r$count[1] == 1) || (r$count[2] == 1))
					return(r$data)
				else return(array(r$data, dim=rev(r$count[1:r$cnt])))
			} else if (r$cnt <= 1)
				return(r$data)
			else
				return(array(r$data, dim=r$count[1:r$cnt]))
		}
	}
	stop(lasterr.gds())
	return(invisible(NULL))
}

write.gdsn <- function(node, val, start, count)
{
	stopifnot(class(node)=="gdsn")
	stopifnot(!missing(val))

	if (missing(start))
	{
		if (missing(count))
		{
			if (is.character(val))
				val[is.na(val)] <- ""

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
			if (is.character(val))
				val[is.na(val)] <- ""

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

getfile.gdsn <- function(node, out.filename)
{
	stopifnot(class(node)=="gdsn")
	stopifnot(is.character(out.filename))

	r <- .C("gdsGetFile", as.integer(node), out.filename, err=integer(1),
		NAOK=TRUE, PACKAGE="gdsfmt")
	if (r$err != 0) stop(lasterr.gds())

	return(invisible(NULL))
}





#############################################################
# Error function
#############################################################

lasterr.gds <- function()
{
	r <- .C("gdsLastErrGDS", s=character(1), NAOK=TRUE, PACKAGE="gdsfmt")
	r$s
}



#############################################################
# R Generic functions
#############################################################

print.gdsclass <- function(x, ...)
{
	enum <- function(node, space, level)
	{
		n <- objdesp.gdsn(node)
		cnt <- cnt.gdsn(node)
		cat(space, "+ ", name.gdsn(node), "	[ ", n$desp, " ", sep="")
		cat(n$dim, sep="x")
		if (n$compress!="") cat("", n$compress)
		if (is.finite(n$cpratio))
			cat(sprintf("(%0.2f%%)", 100*n$cpratio))
		if (length(get.attr.gdsn(node)) > 0)
			cat(" ] *\n")
		else cat(" ]\n")
		r <- .C("gdsNodeEnumPtr", as.integer(node), id=integer(cnt*2),
			err=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")
		if ((r$err == 0) & (cnt > 0))
		{
			for (i in 0:(cnt-1))
			{
				m <- r$id[(2*i+1):(2*i+2)]
				class(m) <- "gdsn"
				if (level==1)
					s <- paste("|--", space, sep="")
				else
					s <- paste("|  ", space, sep="")
				enum(m, s, level+1)
			}
		}
	}

	stopifnot(class(x)=="gdsclass")
	rv <- .C("gdsFileValid", x$id, valid=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")
	if (rv$valid == 0)
	{
		cat("The GDS file has been closed.\n")
	} else {
		cat("file name: ", x$filename, "\n\n", sep="");
		enum(x$root, "", 1)
	}
}

print.gdsn <- function(x, expand=TRUE, ...)
{
	enum <- function(node, space, level, expand, fullname)
	{
		n <- objdesp.gdsn(node)
		cnt <- cnt.gdsn(node)
		cat(space, "+ ", name.gdsn(node, fullname), "	[ ", n$desp, " ", sep="")
		cat(n$dim, sep="x")
		if (n$compress!="") cat("", n$compress)
		if (!is.nan(n$cpratio))
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
					class(m) <- "gdsn"
					if (level==1)
						s <- paste("|--", space, sep="")
					else s <- paste("|  ", space, sep="")
					enum(m, s, level+1, TRUE, FALSE)
				}
			}
		}
	}

	stopifnot(class(x)=="gdsn")
	rv <- .C("gdsNodeValid", as.integer(x), valid=integer(1), NAOK=TRUE, PACKAGE="gdsfmt")
	if (rv$valid == 0)
	{
		cat("The GDS file has been closed.\n")
	} else {
		enum(x, "", 1, expand, TRUE)
	}
}




#############################################################
# R library functions
#############################################################

.onAttach <- function(lib, pkg)
{
	library.dynam("gdsfmt", pkg, lib)
	TRUE
}
