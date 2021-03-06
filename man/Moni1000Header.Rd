% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/header.r
\name{Moni1000Header}
\alias{Moni1000Header}
\title{Create Monitoring 1000 file header data}
\usage{
Moni1000Header(
  DATA_CREATED = format(Sys.Date(), "\%Y\%m\%d"),
  PARAMETER_DEFINITIONS = ParameterDefinitions(),
  ...
)
}
\arguments{
\item{DATA_CREATED}{data creation date.}

\item{PARAMETER_DEFINITIONS}{a \code{ParameterDefinitions} object having parameter descriptions.}

\item{...}{other parameters placed on the header.}
}
\value{
Moni100Header object.
}
\description{
Create Monitoring 1000 file header data
}
\examples{
Moni1000Header(
	DATA_CREATOR = "John Doe", DATA_TITLE = "Important data",
	SITE_NAME = "Nenokuni", PLOT_NAME = "Yomotsu-Hirasaka",
	PLOT_CODE = "NK", PLOT_SIZE = "10ha",
	PARAMETER_DEFINITIONS = ParameterDefinitions()
)

}
