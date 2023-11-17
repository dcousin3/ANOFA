#' Detergent data
#'
#' The data, taken from \insertCite{rs63;textual}{ANOFA}, is a dataset examining
#' the distribution of a large sample of customers, classified over
#' four factors:
#' `Softness of water used` (3 levels: soft, medium or hard),
#' `Expressed preference for brand M or X after blind test` (2 levels:
#' Brand M or Brand X), `Previously used brand M` (2 levels: yes
#' or no), and `Temperature of landry water` (2 levels: hot or
#' cold). It is therefore a 3 × 2 × 2 × 2 design with 24 cells.
#' 
#' @md
#'
#' @docType data
#'
#' @usage Detergent
#'
#' @format An object of class list.
#'
#' @keywords datasets
#'
#' @references 
#' \insertAllCited{}
#'
#' @source \doi{10.20982/tqmp.19.2.p173}
#'
#' @examples
#' 
#' # convert the data to a data.frame
#' dta <- data.frame(Detergent)
#' 
#' # run the anofa analysis
#' \dontrun{
#' w <- anofa( Freq ~  Temperature * M_User * Preference * Water_softness, dta)
#' 
#' # make a plot with all the factors
#' anofaPlot(w)
#' 
#' # ... or with just a few factors
#' anofaPlot(w, ~ Preference * M_User )
#' anofaPlot(w, ~ Temperature )
#' 
#' # extract simple effects
#' e <- emFrequencies(w, ~ M_User | Preference ) 
#' }
#' 
"Detergent"