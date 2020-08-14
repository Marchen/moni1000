#------------------------------------------------------------------------------
#	Column names of stme level  data.
#------------------------------------------------------------------------------
DEFAULT_STEM_COLUMNS <- c(
	"mesh_xcord", "mesh_ycord", "tag_no", "indv_no",
	"stem_xcord", "stem_ycord", "spc_japan"
)


#------------------------------------------------------------------------------
#	Column names of yearly  data.
#------------------------------------------------------------------------------
DEFAULT_CENSUS_COLUMNS <- c("tag_no", "gbh", "note", "s_date", "year", "dead")


#------------------------------------------------------------------------------
#	Create Moni1000 format census table
#
#	@param data a data.frame having census data.
#	@param column.map a ColumnMap object.
#	@param stem.columns
#		column names for stem-level information.
#		These columns are placed on the left side of the resultant table.
#	@param census.columns
#		column names for census-level information.
#		These columns are placed on the right side of the resultant table with
#		column names with census year.
#------------------------------------------------------------------------------
construct.moni1000.table <- function(
	data, column.map = ColumnMap(), stem.columns, census.columns
) {
	# Change column names.
	for (i in names(column.map)) {
		colnames(data)[colnames(data) == column.map[[i]]] <- i
	}
	stem  <- unique(data[stem.columns])
	census <- data[census.columns]
	census[["s_date"]] <- format(census[["s_date"]], "%Y%m%d")
	wide <- reshape(
		census, timevar = "year", idvar = "tag_no", direction = "wide",
		sep = ""
	)
	wide <- wide[sort(colnames(wide))]
	formatted <- merge(stem, wide)
	splitted <- split(formatted, 1:nrow(formatted))
	gbh <- do.call(rbind, lapply(splitted, format.gbh))
	formatted[grepl("gbh", colnames(formatted))] <- gbh
	return(formatted[!grepl("dead", colnames(formatted))])
}


#------------------------------------------------------------------------------
#	Create Moni1000 format gbh for each stem.
#
#	@param x
#		a data.frame having gbh and flag for dead stems.
#		The data.frame should have gbhYEAR and deadYEAR columns.
#------------------------------------------------------------------------------
format.gbh <- function(x) {
	gbh <- x[grepl("gbh", colnames(x))]
	dead <- x[grepl("dead", colnames(x))]
	result <- character(length(gbh))
	died <- measured <- FALSE
	for (i in seq_along(gbh)) {
		if (died) {
			result[i] <- "dd"
			next
		}
		if (!is.na(dead[, i])) {
			if (dead[,i]) {
				result[i] <- "d"
				died <- TRUE
				next
			}
		}
		if (is.na(gbh[, i])) {
			if (measured) {
				result[i] <- "nd"
			} else {
				result[i] <- "na"
			}
			next
		}
		result[i] <- gbh[, i]
		measured <- TRUE
	}
	return(result)
}


#------------------------------------------------------------------------------
#	Write census data block into Moni1000 file
#
#	@param data a data.frame having tree census data.
#	@param path path to write.
#	@param column.map a \code{\link{ColumnMap}} object.
#	@param encoding file encoding to write.
#	@param stem.columns
#		column names for stem-level information.
#		These columns are placed on the left side of the resultant table.
#	@param census.columns
#		column names for census-level information.
#		These columns are placed on the right side of the resultant table with
#		column names with census year.
#
#'	@importFrom utils write.table
#------------------------------------------------------------------------------
write.census.data <- function(
	data, path, column.map, encoding, stem.columns, census.columns
) {
	data.for.write <- construct.moni1000.table(
		data, column.map, stem.columns, census.columns
	)
	suppressWarnings(
		write.table(
			data.for.write, path, sep = ",", na = "na",
			fileEncoding = encoding, row.names = FALSE, append = TRUE
		)
	)
}


#------------------------------------------------------------------------------
#'	Write census data into Moni1000 format file.
#'
#'	@param data a data.frame having tree census data.
#'	@param path path to write.
#'	@param column.map a \code{\link{ColumnMap}} object.
#'	@param encoding file encoding to write.
#'	@param header a \code{\link{Moni1000Header}} object.
#'	@param stem.columns
#'		column names for stem-level information.
#'		These columns are placed on the left side of the resultant table.
#'	@param census.columns
#'		column names for census-level information.
#'		These columns are placed on the right side of the resultant table with
#'		column names with census year.
#'	@export
#------------------------------------------------------------------------------
write.moni1000.tree.census <- function(
	data, path, column.map, header, encoding = "UTF-8",
	stem.columns = DEFAULT_STEM_COLUMNS,
	census.columns = DEFAULT_CENSUS_COLUMNS
) {
	write.header(path, header, encoding)
	write.census.data(
		data, path, column.map, encoding, stem.columns, census.columns
	)
}
