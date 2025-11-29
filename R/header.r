#------------------------------------------------------------------------------
#'  Create Monitoring 1000 file header data
#'
#'  @param DATA_CREATED data creation date.
#'  @param PARAMETER_DEFINITIONS
#'      a \code{ParameterDefinitions} object having parameter descriptions.
#'  @param ... other parameters placed on the header.
#'
#'  @return Moni100Header object.
#'
#'  @examples
#'  Moni1000Header(
#'      DATA_CREATOR = "John Doe", DATA_TITLE = "Important data",
#'      SITE_NAME = "Nenokuni", PLOT_NAME = "Yomotsu-Hirasaka",
#'      PLOT_CODE = "NK", PLOT_SIZE = "10ha",
#'      PARAMETER_DEFINITIONS = ParameterDefinitions()
#'  )
#'
#'  @export
#'  @importFrom stats na.omit reshape
#------------------------------------------------------------------------------
Moni1000Header <- function(
    DATA_CREATED = format(Sys.Date(), "%Y%m%d"),
    PARAMETER_DEFINITIONS = ParameterDefinitions(), ...
){
    x <- list(
        DATA_CREATED = DATA_CREATED, ...,
        PARAMETER_DEFINITIONS = PARAMETER_DEFINITIONS
    )
    class(x) <- "Moni1000Header"
    return(x)
}


#------------------------------------------------------------------------------
#   Construct Moni1000 header data
#
#   @param x a Moni100Header object.
#
#   @return a matrix having Moni1000 file header.
#------------------------------------------------------------------------------
construct_moni1000_header <- function(x){
    n_param <- length(x$PARAMETER_DEFINITIONS)
    n_header <- length(x)
    header <- matrix("", ncol = 4, nrow = n_header + n_param)
    header[, 1] <- c(
        "#/log", "#/doc", rep("#", n_header + n_param - 3), "#/data"
    )
    header[1:length(x), 2] <- gsub("_", " ", names(x))
    for (i in 1:(length(x) - 1)) {
        header[i, 3] <- x[[i]]
    }
    for (i in 1:n_param) {
        header[i + n_header - 1, 3] <- names(x$PARAMETER_DEFINITIONS)[i]
        header[i + n_header - 1, 4] <- x$PARAMETER_DEFINITIONS[[i]]
    }
    return(header)
}


#------------------------------------------------------------------------------
#   Write Moni1000 file header.
#
#   @param path output file path.
#   @param header a \link{\code{Moni1000header}} object.
#   @param encoding file encoding.
#------------------------------------------------------------------------------
write_header <- function(path, header, encoding = "UTF-8") {
    data <- construct_moni1000_header(header)
    write.table(
        data, path, sep = ",", na = "", fileEncoding = encoding,
        row.names = FALSE, col.names = FALSE
    )
}
