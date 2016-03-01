#' city1990
#'
#' This dataset contains population and property valuation information for a hypothetical facility location problem (obtained from Daskin 1995).
#' Each row corresponds to a different city. The cities present in the dataset are the 50 most populous cities in the lower 48 states in addition to the state capitals.
#' The name, state, longitude and latitude of each city are the corresponding columns. The 'first.demand' column contains the 1990 population of each city. The 'second.demand' 
#' column contains the number of households in the city in 1990. The 'fixed.cost' is the 1990 median home values in the city. All data are obtained from the 1990 Population 
#' and Housing Census.
#'
#' The package vignette contains a worked example using this dataset. 
#' 
#' @references Daskin, M. S. (1995). Appendix G: Longitudes, latitudes, demands, and fixed costs for CITY1990.GRT: An 88-node problem defined on the Continental United States. In \emph{Network and Discrete Location} pp. 459-62, John Wiley & Sons Inc, New York. 
#' @docType data
#' @format \code{data.frame} object
#' @keywords datasets
#' @name city1990
#' @examples
#' # load data
#' data(city1990)
#' # plot cities
#' plot(latitude~longitude, data=city1990)
#' # open package vignette
#' dontrun{
#' vignette('rgurobi', package='rguorbi')
#' }
"city1990"


