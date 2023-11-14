##############################################################################
# DEFINITIONS of the three display methods: 
#   print: The usual, just show a list of three numbers, (lowerCI, dp, upperCI)
#   summarize (or summary): provides a human-readable output
#   explain : provides all sorts of information regarding the computations
##############################################################################
#
# As a reminder, the ANOFAobject has all these keys
#   type          : "ANOFAomnibus" or "ANOFAsimpleeffects"
#   formula       : the formula given to anofa
#   compiledData  : a data.frame with the compiled frequencies
#   freqColumn    : column in the compiled format; default "Frequency", 
#   factColumns   : c(fact mames)
#   nlevels       : number of levels for each factors,
#   clevels       : list of categories for each factor
#   marginals     : marginal frequencies
#   results       : the anofa table
#
##############################################################################
#'
#' @title explain 
#' 
#' @name explain
#'
#' @md
#'
#' @description
#' ``explain()`` provides a human-readable, exhaustive, description of
#' the results. It also provides references to the key results.
#' 
#' @usage explain(object, ...)
#' 
#' @param object      an object to explain
#' @param ...         ignored
#' 
#' @return a human-readable output with details of computations.
#' 
#' @export
explain <- function(object, ...) {  UseMethod("explain") }

#' @export 
explain.default <- function(object, ...) { print(object) } 


#' @title summarize 
#' 
#' @name summarize
#'
#' @md
#'
#' @description
#' ``summarize()`` provides a human-readable output of an ANOFAobject. it is 
#' synonym of ``summary()`` (but as actions are verbs, I used a verb).
#' 
#' @param object   an object to summarize
#' @param ...      ignored
#' 
#' @return a human-readable output as per articles.
#' 
#' @export 
summarize <- function(object, ...) {  UseMethod("summarize") }

#' @method summarize default 
#' @export 
summarize.default <- function(object, ...) { print(object$results, ...) }

#' @export
summary.ANOFAobject <- function(object, ...) {
    summarize(object, ...)
}

##############################################################################
##
##  Implementation of the three methods
##
##############################################################################

#' @method print ANOFAobject
#' @export 
print.ANOFAobject <- function(x, ...) {
    print("method print not yet done...")
    cat("ANOFA completed. My first advise is to use anofaPlot() now. \n")
    cat("Use summary() or summarize() to obtain the ANOFA table.\n")
    y <- unclass(x)
    class(y) <- "list"
    print(y, digits = 5)
    return(invisible(x))
}

#' @method print ANOFAtable
#' @export 
print.ANOFAtable <- function(x, ...) {
    r <- x
    class(r) <- "data.frame"
    print(round(r, getOption("ANOFA.digits")+2), 
            digits = getOption("ANOFA.digits"), scientific = FALSE
          )
}

#' @method summarize ANOFAobject 
#' @export 
summarize.ANOFAobject <-  function(object, ...) {
    if (length(object$result) == 1) 
        cat(object$result)
    else {
        u <- object$results
        class(u) <- c("ANOFAtable", class(u))
    }
    return(u)
}

#' @method explain ANOFAobject 
#' @export
explain.ANOFAobject <- function(object, ...) {
    print("method explain not yet done...")
    ## if one G is smaller than zero, refers to Feinberg, 1970b

    return(invisible(object))
}

