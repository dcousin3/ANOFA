context("ANOFA:: Testing anofaPlot function")
    # expect_output( str(res), "data.frame")
    # expect_equal( "ggplot" %in% class(plt), TRUE)
    # expect_message( p <- superbPlot(dta2a, etc ))
    # expect_error(), expect_warning(), expect_condition( , class = "")


test_that("TESTS of anofaPlot (1/2)", {

    set.seed(42)
    dta <- GRF( list(A=c("a1","a2"), B=c("b1","b2","b3"), C=c("c1","c2","c3","c4")), 1000 )  

    w <- anofa( Freq ~ A * B * C, dta)  ## interaction B:C
    plt <- anofaPlot(w)
    expect_equal( "ggplot" %in% class(plt), TRUE)

    # tests when the defaults are changed
    plt <- anofaPlot(w, factorOrder = c("C","B","A")) 
    expect_equal( "ggplot" %in% class(plt), TRUE)

    plt <- anofaPlot(w, errorbarParams = list( width =0.1, linewidth=2.0 )) 
    expect_equal( "ggplot" %in% class(plt), TRUE)

    plt <- anofaPlot(w, plotStyle = "bar") 
    expect_equal( "ggplot" %in% class(plt), TRUE)

    plt <- anofaPlot(w, plotStyle = "bar", barParams = list(width = .2)) 
    expect_equal( "ggplot" %in% class(plt), TRUE)

    plt <- anofaPlot(w, plotStyle = "bar", 
                        barParams = list(width = .2), 
                        errorbarParams = list(width =0.5, linewidth = 0.75) ) 
    expect_equal( "ggplot" %in% class(plt), TRUE)

})

test_that("TESTS of anofaPlot (2/2)", {

    set.seed(42)
    dta <- GRF( list(A=c("a1","a2"), B=c("b1","b2","b3"), C=c("c1","c2","c3","c4")), 1000 )  

    w <- anofa( Freq ~ A * B * C, dta)  ## interaction B:C
    expect_error( anofaPlot(w, B) )
    expect_error( anofaPlot(w, whatever ~ B) )

    plt <- anofaPlot(w, Freq ~ B)    
    expect_equal( "ggplot" %in% class(plt), TRUE)

    plt <- anofaPlot(w, Freq ~ A * B)    
    expect_equal( "ggplot" %in% class(plt), TRUE)

})

