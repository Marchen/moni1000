#------------------------------------------------------------------------------
#'	Create column map
#'
#'	Create column name mapping between source data and Moni1000 format.
#'	Names of arguments represent Moni1000 data names and argument values
#'	represent column names of source data.
#'
#'	@param mesh_xcord x coordinate of grid mesh.
#'	@param mesh_ycord y coordinate of grid mesh.
#'	@param tag_no stem id, currently should be unique for all stems.
#'	@param indv_no individual id.
#'	@param stem_xcord x coordinate of stems.
#'	@param stem_ycord y coordinate of stems.
#'	@param spc_japan species name in Japanese.
#'	@param gbh girth at breast height.
#'	@param vine if TRUE, GBH measure includes vine(s).
#'	@param note remarks for each stem in each observation.
#'	@param s_date census date.
#'	@param dead logical indicating dead stems.
#'	@param year observation year.
#'
#'	@export
#------------------------------------------------------------------------------
ColumnMap <- function(
	mesh_xcord = "mesh_xcord", mesh_ycord = "mesh_ycord",
	tag_no = "tag_no", indv_no = "indv_no",
	stem_xcord = "stem_xcord", stem_ycord = "stem_ycord",
	spc_japan = "spc_japan", gbh = "gbh", vine = "vine", note = "note",
	s_date = "s_date", dead = "dead", position_change = "position_change",
	year = "year", ...
) {
	x <- list(
		mesh_xcord = mesh_xcord, mesh_ycord = mesh_ycord, tag_no = tag_no,
		indv_no = indv_no, stem_xcord = stem_xcord, stem_ycord = stem_ycord,
		spc_japan = spc_japan, gbh = gbh, vine = vine, note = note,
		s_date = s_date, dead = dead, position_change = position_change,
		year = year, ...
	)
	class(x) <- "ColumnMap"
	return(x)
}
