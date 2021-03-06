% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cpt_adl28.R
\name{cpt_adl28}
\alias{cpt_adl28}
\title{Compute ADL 28-point score}
\usage{
cpt_adl28(x, report=T)
}
\arguments{
\item{x}{A data.frame class object, with M3G0110 items.}

\item{report}{Generates a short report on computed ADL score}
}
\value{
A dataframe with added column adl_score
}
\description{
Scans MDS dataframe for 'std' named ADL items and computes
ADL score for each assessment.
}
\examples{
require(mdsR)
mds_dta <- mdsR::mds_cohort
cpt_adl28(mds_dta)

}
\references{
ADD LATER
}
