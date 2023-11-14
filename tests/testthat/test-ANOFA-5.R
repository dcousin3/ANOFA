context("ANOFA:: Testing power functions function")
    # expect_output( str(res), "data.frame")
    # expect_equal( "ggplot" %in% class(plt), TRUE)
    # expect_message( p <- superbPlot(dta2a, etc ))
    # expect_error(), expect_warning(), expect_condition( , class = "")


test_that("TESTS of anofaN2Power and anofaPower2N (1/1)", {

    pw <- anofaN2Power(533, 5*3, 0.2671)
    expect_equal( pw > 0.99999, TRUE)

    pw <- anofaN2Power(533/4, 5*3, 0.2671)
    expect_equal( pw > 0.989, TRUE)

    pred <- c(.35, .25, .25, .15)
    P <- length(pred)
    f2 <- 2 * sum(pred * log(P * pred) )
    N <- anofaPower2N(0.80, P, f2)
    expect_equal( N, 132.5, tolerance = 0.01)
    
})

