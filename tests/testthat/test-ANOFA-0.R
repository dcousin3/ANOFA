context("ANOFA:: Testing logical functions (3 sections)")

test_that("Test is.formula and is.one.sided (1/3)", {
    formula0 <- toto     ~ .
    formula1 <- toto     ~ a * b
    formula2 <-          ~ a * b
    formula3 <- toto     ~ a | c
    formula4 <- toto     ~  a*b | c    
    formula5 <- toto     ~ (a*b | c) + (d*e|f)
    formula6 <- toto | a ~ b*c|d
    formula7 <- ~ cbind(a,b,c) + cbind(low,high) + cbind(chat,chien)

    expect_equal( is.formula( formula1 ), TRUE )
    expect_equal( is.formula( formula5 ), TRUE )
    expect_equal( is.formula( formula6 ), TRUE )

    expect_equal( is.one.sided( formula0 ), FALSE )
    expect_equal( is.one.sided( formula1 ), FALSE )
    expect_equal( is.one.sided( formula2 ), TRUE )
})

test_that("Test has.nested.terms and has.cinb.terms (2/3)", {
    formula0 <- toto     ~ .
    formula1 <- toto     ~ a * b
    formula2 <-          ~ a * b
    formula3 <- toto     ~ a | c
    formula4 <- toto     ~  a*b | c    
    formula5 <- toto     ~ (a*b | c) + (d*e|f)
    formula6 <- toto | a ~ b*c|d
    formula7 <- ~ cbind(a,b,c) + cbind(low,high) + cbind(chat,chien)

    expect_equal( has.nested.terms( formula0 ), FALSE )
    expect_equal( has.nested.terms( formula1 ), FALSE )
    expect_equal( has.nested.terms( formula2 ), FALSE )
    expect_equal( has.nested.terms( formula4 ), TRUE )
    expect_equal( has.nested.terms( formula5 ), TRUE )
    expect_equal( has.nested.terms( formula6 ), TRUE )

    expect_equal( has.cbind.terms( formula0 ), FALSE )
    expect_equal( has.cbind.terms( formula2 ), FALSE )
    expect_equal( has.cbind.terms( formula3 ), FALSE )
    expect_equal( has.cbind.terms( formula4 ), FALSE )
    expect_equal( has.cbind.terms( formula5 ), FALSE )
    expect_equal( has.cbind.terms( formula7 ), TRUE )
})

test_that("Test in.formula and sub.formulas (3/3)", {
    formula0 <- toto     ~ .
    formula1 <- toto     ~ a * b
    formula2 <-          ~ a * b
    formula3 <- toto     ~ a | c
    formula4 <- toto     ~  a*b | c    
    formula5 <- toto     ~ (a*b | c) + (d*e|f)
    formula6 <- toto | a ~ b*c|d
    formula7 <- ~ cbind(a,b,c) + cbind(low,high) + cbind(chat,chien)

    expect_equal( in.formula( formula0, "toto" ), TRUE ) 
    expect_equal( in.formula( formula1, "toto" ), TRUE ) 
    expect_equal( in.formula( formula2, "toto" ), FALSE ) 
    expect_equal( in.formula( formula0, "a"    ), FALSE ) 
    expect_equal( in.formula( formula2, "a"    ), TRUE ) 
    expect_equal( in.formula( formula5, "a"    ), TRUE ) 

    expect_equal( sub.formulas( formula4, "*" )[[1]] == "a * b", TRUE ) 
    res <- sub.formulas(formula6, "|")
    expect_equal( length(res), 2)
    expect_equal( res[[1]] == "toto | a", TRUE )
    expect_equal( res[[2]] == "b * c | d", TRUE )

})

