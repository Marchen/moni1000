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
DEFAULT_CENSUS_COLUMNS <- c(
	"tag_no", "gbh", "note", "s_date", "year", "dead", "vine", "position_change"
)


#------------------------------------------------------------------------------
#	Create Moni1000 format census table
#
#	@param data a data.frame having census data.
#	@param column_map a \code{\link{ColumnMap}} object.
#	@param stem_columns
#		column names for stem-level information.
#		These columns are placed on the left side of the resultant table.
#	@param census_columns
#		column names for census-level information.
#		These columns are placed on the right side of the resultant table with
#		column names with census year.
#------------------------------------------------------------------------------
construct_moni1000_table <- function(
	data, column_map = ColumnMap(), stem_columns, census_columns
) {
	# Change column names.
	for (i in names(column_map)) {
		colnames(data)[colnames(data) == column_map[[i]]] <- i
	}
	stem  <- unique(data[stem_columns])
	census <- data[census_columns]
	census[["s_date"]] <- format(census[["s_date"]], "%Y%m%d")
	wide <- reshape(
		census, timevar = "year", idvar = "tag_no", direction = "wide", sep = ""
	)
	wide <- wide[sort(colnames(wide))]
	formatted <- merge(stem, wide)
	splitted <- split(formatted, 1:nrow(formatted))
	gbh <- do.call(rbind, lapply(splitted, format_gbh))
	formatted[grepl("gbh", colnames(formatted))] <- gbh
	result <- formatted[
		!grepl("dead|vine|position_change", colnames(formatted))
	]
	return(result)
}


#------------------------------------------------------------------------------
#	Create Moni1000 format gbh for each stem.
#
#	@param x
#		a data.frame having gbh and flag for dead stems.
#		The data.frame should have gbhYEAR and deadYEAR columns.
#------------------------------------------------------------------------------
format_gbh <- function(x) {
	gbh <- unlist(x[, grepl("gbh", colnames(x))])
	dead <- unlist(x[, grepl("dead", colnames(x))])
	vine <- unlist(x[, grepl("vine", colnames(x))])
	position_change <- unlist(x[, grepl("position_change", colnames(x))])
	gbh <- assign_dead_status(gbh, dead)
	gbh <- assign_vine_status(gbh, vine)
	gbh <- assign_position_change(gbh, position_change)
	return(gbh)
}


#------------------------------------------------------------------------------
#	Assign dead status for each GBH record.
#
#	@param gbh
#		a vector of GBH records.
#	@param dead
#		a logical vector of dead status.
#------------------------------------------------------------------------------
assign_dead_status <- function(gbh, dead) {
	result <- character(length(gbh))
	died <- measured <- FALSE

	for (i in seq_along(gbh)) {
		if (died) {
			result[i] <- "dd"
			next
		}
		if (!is.na(dead[i])) {
			if (dead[i]) {
				result[i] <- "d"
				died <- TRUE
				next
			}
		}
		if (is.na(gbh[i])) {
			if (measured) {
				result[i] <- "nd"
			} else {
				result[i] <- "na"
			}
			next
		}
		result[i] <- gbh[i]
		measured <- TRUE
	}
	return(result)
}


#------------------------------------------------------------------------------
#	Assign existence of vine on GBH data.
#
#	@param gbh
#		a vector of GBH records.
#	@param dead
#		a logical vector representing existence of vine(s).
#------------------------------------------------------------------------------
assign_vine_status <- function(gbh, vine) {
	result <- gbh
	had_vine_last_year <- FALSE
	for (i in seq_along(vine)) {
		# If the vine status is missing or the tree is dead, do nothing.
		if (is.na(vine[i]) | (gbh[i] %in% c("d", "dd"))) {
			next
		}
		# When a new vine appear, add "vi" to GBH.
		if (vine[i] & !had_vine_last_year) {
			result[i] <- paste0("vi", gbh[i])
			had_vine_last_year <- TRUE
			next
		}
		# When the vine disappear, add "vn" to GBH.
		if (!vine[i] & had_vine_last_year) {
			result[i] <- paste0("vn", gbh[i])
			had_vine_last_year <- FALSE
		}
	}
	return(result)
}


#------------------------------------------------------------------------------
#	Assign position change mark on GBH data.
#
#	@param gbh
#		a vector of GBH records.
#	@param position_change
#		a logical vector indicating measurement position change.
#------------------------------------------------------------------------------
assign_position_change <- function(gbh, position_change) {
	result <- character(length(gbh))
	for (i in seq_along(position_change)) {
		if (is.na(position_change[i])) {
			result[i] <- gbh[i]
		} else if (position_change[i]) {
			result[i] <- paste0("cd", gbh[i])
		} else {
			result[i] <- gbh[i]
		}
	}
	return(result)
}


#------------------------------------------------------------------------------
#	Write census data block into Moni1000 file
#
#	@param data a data.frame having tree census data.
#	@param path path to write.
#	@param column_map a \code{\link{ColumnMap}} object.
#	@param encoding file encoding to write.
#	@param stem_columns
#		column names for stem-level information.
#		These columns are placed on the left side of the resultant table.
#	@param census_columns
#		column names for census-level information.
#		These columns are placed on the right side of the resultant table with
#		column names with census year.
#
#'	@importFrom utils write.table
#------------------------------------------------------------------------------
write_census_data <- function(
	data, path, column_map, encoding, stem_columns, census_columns
) {
	data_for_write <- construct_moni1000_table(
		data, column_map, stem_columns, census_columns
	)
	suppressWarnings(
		write.table(
			data_for_write, path, sep = ",", na = "na",
			fileEncoding = encoding, row.names = FALSE, append = TRUE
		)
	)
}


#------------------------------------------------------------------------------
#'	Write census data into Moni1000 format file.
#'
#'	@param data a data.frame having tree census data.
#'	@param path path to write.
#'	@param column_map a \code{\link{ColumnMap}} object.
#'	@param encoding file encoding to write.
#'	@param header a \code{\link{Moni1000Header}} object.
#'	@param stem_columns
#'		column names for stem-level information.
#'		These columns are placed on the left side of the resultant table.
#'	@param census_columns
#'		column names for census-level information.
#'		These columns are placed on the right side of the resultant table with
#'		column names with census year.
#'	@export
#------------------------------------------------------------------------------
write_moni1000_tree_census <- function(
	data, path, column_map, header, encoding = "UTF-8",
	stem_columns = DEFAULT_STEM_COLUMNS,
	census_columns = DEFAULT_CENSUS_COLUMNS
) {
	write_header(path, header, encoding)
	write_census_data(
		data, path, column_map, encoding, stem_columns, census_columns
	)
}
