######################################################################################
#'
#' @title Gillet1993
#'
#' @description The data, taken from \insertCite{g93;textual}{ANOFA}, is a dataset examining
#' the distribution of apple tree to produce new branches from grafts. The study has 
#' a sample of 713 trees subdivided into three factors:
#' `species` (2 levels: Jonagold or Cox);
#' `location` (3 levels: Order1, Order2, Order3);
#' is where the graft has been implanted (order 1 is right on the trunk);
#' and `florished` (2 levels: yes or no) indicates if the branch bear
#' flowers. It is therefore a 2 × 3 × 2 design with 12 cells.
#' 
#' @md
#'
#' @docType data
#'
#' @usage Gillet1993
#'
#' @format An object of class list.
#'
#' @keywords datasets
#'
#' @references 
#' \insertAllCited{}
#'
#' @examples
#' # The Gillet1993 presents data from appletrees having grafts.
#' Gillet1993
#' 
#' # run the base analysis
#' w <- anofa( Freq ~ species * location * florished, Gillet1993)
#'
#' # display a plot of the results
#' anofaPlot(w)
#' 
#' # show the anofa table where we see the 3-way interaction
#' summary(w)
#' 
#' # This returns the expected marginal frequencies analysis
#' e <- emFrequencies(w, Freq ~ species * location | florished )
#' summary(e)
#' 
#' # as seen, all the two-way interactions are significant. Decompose one more degree:
#' f <- emFrequencies(w, Freq ~ species | florished * location )
#' summary(f)
#' 
#' 
"Gillet1993"