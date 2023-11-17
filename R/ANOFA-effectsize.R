######################################################################################
#' @title Computing effect size within the ANOFA.
#'
#' @md
#'
#' @description The function `anofaES()` compute effect size from observed frequencies
#'      according to the ANOFA framework. See \insertCite{lc23b;textual}{ANOFA} for more.
#'
#' @usage anofaES( props )
#'
#' @param props the expected proportions;
#'
#' @return The predicted effect size from a population with the given proportions.
#'
#' @details The effect size is given as an eta-square.
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' # if we assume the following proportions:
#' pred <- c(.35, .25, .25, .15)
#'
#' # then eta-square is given by 
#' anofaES( pred )
#' 
######################################################################################
#'
#' @export anofaES
#
######################################################################################

anofaES <- function( props ){

    ##############################################################################
    ## STEP 1: VALIDATION OF INPUT
    ##############################################################################
    # 1.1 proportions must totalize 1.00
    if (round(sum(props),4)!=1) {
        warning("ANOFA::error(41): The proportions do not totalize 1.00 (100%). Standardizing...")
        props <- props / sum(props)
    }

    ##############################################################################
    ## STEP 2: MAKES THE COMPUTATION
    ##############################################################################
    P <- length(props)
    f2 <- 2 * sum(props * log(P * props) )

    eta2 <- f2 / (1 + f2)
    eta2
}

