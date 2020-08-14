#------------------------------------------------------------------------------
#'	Create Monitoring 1000 file header data
#'
#'	@param DATA_CREATOR name(s) of data creator(s).
#'	@param DATA_TITLE data title.
#'	@param SITE_NAME site name.
#'	@param PLOT_NAME plot name.
#'	@param PLOT_CODE plot code.
#'	@param PLOT_SIZE description of plot size.
#'	@param DATA_CREATED data creation date.
#'	@param PARAMETER_DEFINITIONS
#'		a \code{ParameterDefinitions} object having parameter descriptions.
#'
#'	@return Moni100Header object.
#'
#'	@export
#'	@importFrom stats na.omit reshape
#------------------------------------------------------------------------------
Moni1000Header <- function(
	DATA_CREATOR = NA, DATA_TITLE = NA, SITE_NAME = NA, PLOT_NAME = NA,
	PLOT_CODE = NA, PLOT_SIZE = NA, DATA_CREATED = format(Sys.Date(), "%Y%m%d"),
	PARAMETER_DEFINITIONS = ParameterDefinitions()
){
	x <- list(
		DATA_CREATED = DATA_CREATED, DATA_CREATOR = DATA_CREATOR,
		DATA_TITLE = DATA_TITLE, SITE_NAME = SITE_NAME, PLOT_NAME = PLOT_NAME,
		PLOT_CODE = PLOT_CODE, PLOT_SIZE = PLOT_SIZE,
		PARAMETER_DEFINITIONS = PARAMETER_DEFINITIONS
	)
	x <- na.omit(x)
	class(x) <- "Moni1000Header"
	return(x)
}


#------------------------------------------------------------------------------
#	Construct Moni1000 header data
#
#	@param x a Moni100Header object.
#
#	@return a matrix having Moni1000 file header.
#------------------------------------------------------------------------------
construct.moni1000.header <- function(x){
	n.param <- length(x$PARAMETER_DEFINITIONS)
	n.header <- length(x)
	header <- matrix("", ncol = 4, nrow = n.header + n.param)
	header[, 1] <- c(
		"#/log", "#/doc", rep("#", n.header + n.param - 3), "#/data"
	)
	header[1:length(x), 2] <- gsub("_", " ", names(x))
	for (i in 1:(length(x) - 1)) {
		header[i, 3] <- x[[i]]
	}
	for (i in 1:n.param) {
		header[i + n.header - 1, 3] <- names(x$PARAMETER_DEFINITIONS)[i]
		header[i + n.header - 1, 4] <- x$PARAMETER_DEFINITIONS[[i]]
	}
	return(header)
}


#------------------------------------------------------------------------------
#	Write Moni1000 file header.
#
#	@param path output file path.
#	@param header a \link{\code{Moni1000header}} object.
#	@param encoding file encoding.
#------------------------------------------------------------------------------
write.header <- function(path, header, encoding = "UTF-8") {
	data <- construct.moni1000.header(header)
	write.table(
		data, path, sep = ",", na = "", fileEncoding = encoding,
		row.names = FALSE, col.names = FALSE
	)
}
