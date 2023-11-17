####################################################################################
#' @title Computing power within the ANOFA.
#'
#' @aliases anofaPower2N anofaN2Power
#'
#' @md
#'
#' @description The function `anofaN2Power()` performs an analysis of statistical power
#'      according to the `ANOFA` framework. See \insertCite{lc23b;textual}{ANOFA} for more.
#'      `anofaPower2N()` computes the sample size to reach a given power.      
#'
#' @usage anofaPower2N(power, P, f2, alpha)
#'
#' @usage anofaN2Power(N, P, f2, alpha)
#'
#' @param power target power to attain;
#'
#' @param N sample size;
#'
#' @param P number of groups;
#'
#' @param f2 effect size Cohen's $f^2$;
#'
#' @param alpha (default if omitted .05) the decision threshold.
#'
#'
#' @return a model fit to the given frequencies. The model must always be an omnibus model
#'         (for decomposition of the main model, follow the analysis with `emfrequencies()` or `contrasts()`)
#'
#'
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' # 1- The Landis et al. study had tremendous power with 533 participants in 15 cells:
#' # where 0.2671 is the observed effect size for the interaction.
#' anofaN2Power(533, 5*3, 0.2671)
#' # power is 100% because sample is large and effect size is as well.
#'
#' # Even with a quarter of the participants, power is overwhelming:
#' # because the effect size is quite large.
#' anofaN2Power(533/4, 5*3, 0.2671)
#' 
#' # 2- Power planning.
#' # Suppose we plan a four-classification design with expected frequencies of:
#' pred <- c(.35, .25, .25, .15)
#' # P is the number of classes (here 4)
#' P <- length(pred)
#' # We compute the predicted f2 as per Eq. 5
#' f2 <- 2 * sum(pred * log(P * pred) )
#' # the result, 0.0822, is a moderate effect size.
#' 
#' # Finally, aiming for a power of 80%, we run
#' anofaPower2N(0.80, P, f2)
#' # to find that a little more than 132 participants are enough.
#' 
####################################################################################
#'
#' @importFrom stats optimize
#' @export anofaN2Power
#' @export anofaPower2N
#' @importFrom utils capture.output
#
####################################################################################


# 1- Returns statistical power given N, P, f2 and alpha      
anofaN2Power <- function(N, P, f2, alpha = .05) {
    pFc <- qchisq( p = 1 - alpha, df = P-1 )
    1- pchisq(q = pFc,  df = P-1, ncp = N * f2)
}

# 2- Performs a search for N to reach a certain statistical power
library(stats) # for function optimize
anofaPower2N <- function(power, P, f2, alpha = .05) {
    # define the objective: minimize the lag between the desired power and
    # the power achieved by a certain N
    objective <- function(N, power, P, f2, alpha = .05) {
        (power - anofaN2Power(N, P, f2, alpha))^2 
    }
    # launch optimization search for the desired N between 10 and 1000
    optimize( objective, interval = c(10,1000), P = P, f2 = f2, power = power)$minimum
}


