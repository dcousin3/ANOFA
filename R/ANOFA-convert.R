###################################################################################
#' @name conversion
#'
#' @title Converting between formats
#'
#' @aliases toWide toLong toCompiled toRaw toTabular
#'
#' @md
#'
#' @description The functions `toWide()`, `toLong()`, `toCompiled()` 
#'    `toRaw()` and `toTabular()`
#'      converts the data into various formats.
#' 
#' @usage toWide(w)
#' @usage toLong(w)
#' @usage toCompiled(w)
#' @usage toRaw(w)
#' @usage toTabular(w)
#' 
#' @param w An instance of an ANOFA object.
#'
#' @return a data frame in the requested format.
#'
#' @details The classification of a set of $n$ participants can be
#'   given using many formats. One basic format (called `wide` herein)
#'   has $n$ lines, one per participants, and category names assigned
#'   to each. 
#'   Another format (called `compiled` herein) is to have a list of all
#'   the categories and the number of participants falling in each 
#'   cells. This last format is typically much more compact (if there
#'   are 6 categories, the data are all contained in six lines). 
#'   However, we fail to see each individual contributing to the counts.
#'   See the vignette DataFormatsForFrequencies for more.
#'   A third possible format (called `raw` herein) put one column per
#'   category and 1 is the observation matches this category, 0 otherwise.
#'   This format results in $n$ lines, one participants, and as many 
#'   columns are there are categories.
#'   Lastly, a fourth format (called `long` herein) as, on a line, the
#'   factor name and the category assigned in that factor. If there are
#'   $f$ factors and $n$ participants, the data are in $f*n$ lines. 
#'
#' @details See the vignette DataFormatsForFrequencies for more.
#'
#' @examples
#' 
#' # The minimalExample contains $n$ of 20 participants categorized according
#' # to two factors $f = 2$, namely `Intensity` (three levels) 
#' # and Pitch (two levels) for 6 possible cells.
#' minimalExample
#'
#' # Lets incorporate the data in an anofa data structure
#' w <- anofa( Frequency ~ Intensity * Pitch, minimalExample )
#'
#' # The data presented using various formats looks like
#' toWide(w)
#' # ... has 20 lines ($n$) and 2 columns ($f$)
#'
#' toLong(w)
#' # ... has 40 lines ($n \times f$) and 3 columns (participant's `Id`, `Factor` name and `Level`)
#'
#' toRaw(w)
#' # ... has 20 lines ($n$) and 5 columns ($2+3$)
#'
#' toCompiled(w)
#' # ... has 6 lines ($2 \times 3$) and 3 columns ($f$ + 1)
#'
#' toTabular(w)
#' # ... has one table with $2 \times 3$ cells. If there had been
#' # more than two factors, the additional factor(s) would be on distinct layers.
#'
###################################################################################
#'
#' @export toWide
#' @export toLong
#' @export toRaw
#' @export toCompiled
#' @export toTabular
#' @importFrom utils tail
#
###################################################################################


toWide <- function( w = NULL ) {
    # is w of class ANOFA.object?
    if (!("ANOFAobject" %in% class(w) ))
        stop("ANOFA::error(101): argument is not an ANOFA generated object. Exiting...")

    return( ctow(w$compiledData, w$freqColumn) )
}

toLong <- function( w = NULL ) {
    # is w of class ANOFA.object?
    if (!("ANOFAobject" %in% class(w) ))
        stop("ANOFA::error(102): argument is not an ANOFA generated object. Exiting...")
    
    return( ctol(w$compiledData, w$freqColumn) )
}

toRaw <- function( w = NULL ) {
    # is w of class ANOFA.object?
    if (!("ANOFAobject" %in% class(w) ))
        stop("ANOFA::error(103): argument is not an ANOFA generated object. Exiting...")
    
    return( ctor(w$compiledData, w$freqColumn) )
}

toCompiled <- function( w = NULL ) {
    # is w of class ANOFA.object?
    if (!("ANOFAobject" %in% class(w) ))
        stop("ANOFA::error(104): argument is not an ANOFA generated object. Exiting...")
    
    return( w$compiledData ) # nothing to do, the data are internally stored in compiled form
}

toTabular <- function( w = NULL ) {
    # is w of class ANOFA.object?
    if (!("ANOFAobject" %in% class(w) ))
        stop("ANOFA::error(105): argument is not an ANOFA generated object. Exiting...")
    
    return( ctot(w$compiledData, w$freqColumn) )
}



###################################################################################
### CONVERSIONS from compiled to ...
###################################################################################

# compiled => wide  DONE
ctow <- function(x, f) {
    y <- x[,names(x)!=f, drop=FALSE]
    as.data.frame(lapply(y, rep, x[[f]]))
}


# compiled => raw  DONE
ctor <- function(x, f) {
    dummy.coding <- function(x){
        1 * sapply(unique(x), USE.NAMES = TRUE, FUN = function(v) {x == v}) 
    }
    res <- do.call("cbind", apply(ctow(x,f), 2, dummy.coding, simplify=FALSE))
    data.frame(res)
}


# compiled => long  DONE
ctol <- function(x, f) {
    #ANOFA.Id <<- 0 ## global variable
    assign('ANOFA.Id', 0, ANOFA.env)    ## global variable
    line.coding <-function(ln) { 
        assign('ANOFA.Id', ANOFA.env$ANOFA.Id + 1, ANOFA.env) 
        data.frame(
            Id = rep(ANOFA.env$ANOFA.Id, length(ln)),
            Factor = names(ln),
            Level = as.character(unlist(ln))
        )
    }
    do.call("rbind", apply(ctow(x, f), 1, line.coding ) )
}


# compiled => tabular  DONE
ctot <- function(x, f) {
    table(ctow(x, f))
}



###################################################################################
### CONVERSIONS from ... to compiled
###################################################################################
# These functions are hidden, but used in importing the data into anofa()
# tabular => compiled  DONE
ttoc <- function(x, f) {
    res <- expand.grid(dimnames(x))
    res[[f]] <- c(x)
    return(res)
}

# wide => compiled   DONE
wtoc <- function(x, f) {
    # https://stackoverflow.com/a/53775768/5181513
    res <- aggregate(f ~ ., transform(x, f = 1), FUN = sum)
    colnames(res)[which(names(res) == "f")] <- f
    res
}

# long => wide  DONE
ltow <- function(x, frm){
    if (!has.nested.terms(frm)) 
        stop("ANOFA:error: equation is not given with a nested term |. Exiting...")
    nestingvar <- sub.formulas(frm, "|")[[1]][[3]]
    nestedvar  <- sub.formulas(frm,"|")[[1]][[2]]
    
    if (length(all.vars(nestingvar))!=1)
        stop("ANOFA::error(105): the nesting term is composed of more than one variable. Exiting...")
    if (length(all.vars(nestedvar))!=1)
        stop("ANOFA::error(106): the nested factor is composed of more than one variable. Exiting...")
    ids <- unique(x[[nestingvar]])
    
    # for each participant
    res = data.frame()
    for (i in ids) {
        temp <- x[x[[nestingvar]] == i,]
        rownames(temp) <- temp[[nestedvar]]
        temp <- tail(t(temp),-2)
        rownames(temp) <- NULL
        res = rbind(res, data.frame(temp))
    }
    return(res)
}

# long => compiled  DONE
ltoc <- function(x, f, frm) {
    wtoc(ltow(x, frm), f)
}

# raw => wide   DONE
rtow <- function(x, frm, fact = NULL) {
    if (!has.cbind.terms(frm)) 
        stop("ANOFA:error(107): equation is not given with cbind. Exiting...")
    # extract sub.formulas for all terms
    res <- sub.formulas( frm, "cbind" )
    if (!(is.null(fact))) {
        if (length(fact)!= length(res)) 
            stop("ANOFA::error(108): The number of factors given does not correspond to the number of cbind in the formula. Exiting...")
    } else {
        fact <- LETTERS[1:length(res)]
    } 

    # get all.vars(%) for each
    cols <- lapply(res, function(x) all.vars(x))
    # set factor names if none provided
    # répéter pour tous avec le nom?
    df <- data.frame( Id = 1:dim(x)[1] )
    for (i in 1:length(fact)) {
        onesetofcols <- x[,cols[[i]]]
        df[[fact[i]]] <- apply(onesetofcols, 1, 
            function(x) colnames(onesetofcols)[x==1])
    }
    df$Id = NULL
    return(df)
}
    
# raw => compiled   DONE
rtoc <- function(x, f, frm, fact = NULL ) {
    wtoc(rtow(x, frm, fact), f)
}


