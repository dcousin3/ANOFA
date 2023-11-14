######################################################################################
#' @title Generating random frequencies
#'
#' @md
#'
#' @description The function ``GRF()`` 
#'      generates random frequencies based on a design, i.e.,
#'      a list giving the factors and the categories with each factor.
#'      The data are given in the `compiled` format.
#'
#' @usage GRF( design, n, prob = NULL, f = "Freq" )
#'
#' @param design A list with the factors and the categories within each.
#' @param n      How many simulated participants are to be classified.
#' @param prob   (optional) the probability of falling in each cell of the design.
#' @param f      (optional) the column names that will contain the frequencies.
#'
#' @return a data frame containing frequencies per cells of the design.
#'
#' @details The name of the function `GRF()` is derived from `grd()`,
#' a general-purpose tool to generate random data \insertCite{ch19}{ANOFA} now bundled
#' in the `superb` package \insertCite{cgh21}{ANOFA}.
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' 
#' # The first example disperse 20 particants in one factor having
#' # two categories (low and high):
#' design <- list( A=c("low","high"))
#' GRF( design, 20 )
#' 
#' # This example has two factors, with factor A having levels a, b, c:
#' design <- list( A=letters[1:3], B = c("low","high"))
#' GRF( design, 40 )
#' 
#' # This last one has three factors, for a total of 3 x 2 x 2 = 12 cells
#' design <- list( A=letters[1:3], B = c("low","high"), C = c("cat","dog"))
#' GRF( design, 100 )
#' 
#' # To specify unequal probabilities, use
#' design <- list( A=letters[1:3], B = c("low","high"))
#' GRF( design, 100, c(.05, .05, .35, .35, .10, .10 ) )
#' 
#' # The name of the column containing the frequencies can be changes
#' GRF( design, 100, f="patate")
#'
######################################################################################
#' @importFrom stats rmultinom
#' @export GRF
#
######################################################################################


# generate compiled-format data from a list of the factors and their levels
GRF <- function( design, n, prob = NULL, f = "Freq" ) {
    levels <- sapply(design, length, simplify = TRUE)
    nlevels <- prod(levels)
    if (is.null(prob)) {
        prob <- rep(1/nlevels, nlevels) 
    } else { 
        if (length(prob) != nlevels) 
            stop("ANOFA::error(51): number of probabilities provided does not match the number of cells. Exiting...") 
        if (round(sum(prob),3) !=1.0)
            stop("ANOFA::error(52): The total of the probabilities does not equal 1. Exiting...")
    }
    res <- expand.grid(design)
    res[[f]] = rmultinom(1, n, prob)[,1]
    return(res)
}

