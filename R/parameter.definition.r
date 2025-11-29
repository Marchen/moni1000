#------------------------------------------------------------------------------
#'  Create data for PARAMETER DEFINITIONS field of Monitoring 1000 file header
#'
#'  @param ...
#'  parameter names and values. Parameter names represent column names in the
#'  Moni1000 file and parameter values represent description of the parameters.
#'
#'  @examples
#'  ParameterDefinitions(
#'      mesh_xcord = "A x-coordinate of each mesh grid",
#'      mesh_ycord = "A y-coordinate of each mesh grid",
#'      tag_no = "A unique identifier (ID) for each stem",
#'      indv_no = "A unique identifier for each individual",
#'      stem_xcord = "A x-coordinate of each stem (within the mesh grid)",
#'      stem_ycord = "A y-coordinate of each stem (within the mesh grid)",
#'      spc_japan = "A species name (in Japanese) of each stem",
#'      gbh = "A measuement of girth at breast height for each stem",
#'      note = "Remarks for the measurement",
#'      s_date = "A census date for each measurement"
#'  )
#'
#'  @export
#------------------------------------------------------------------------------
ParameterDefinitions <- function(...) {
    x <- list(...)
    class(x) <- "ParameterDefinitions"
    return(x)
}
