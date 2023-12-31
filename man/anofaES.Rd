% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ANOFA-effectsize.R
\name{anofaES}
\alias{anofaES}
\title{Computing effect size within the ANOFA.}
\usage{
anofaES( props )
}
\arguments{
\item{props}{the expected proportions;}
}
\value{
The predicted effect size from a population with the given proportions.
}
\description{
The function \code{anofaES()} compute effect size from observed frequencies
according to the ANOFA framework. See \insertCite{lc23b;textual}{ANOFA} for more.
}
\details{
The effect size is given as an eta-square.
}
\examples{
# if we assume the following proportions:
pred <- c(.35, .25, .25, .15)

# then eta-square is given by 
anofaES( pred )


}
\references{
\insertAllCited{}
}
