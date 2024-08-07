% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ex_meta.R
\docType{data}
\name{ex_meta}
\alias{ex_meta}
\title{Meta data of example sparse matrix}
\format{
A data frame containing 493 rows and 2 columns
\describe{
\item{cell_id_dna}{cell id}
\item{ptime}{pseudotime 1}
\item{ptime2}{pseudotime 2}
\item{pt_avg}{average pseudotime}}
}
\source{
Data frame containing cell annotations from \href{https://doi.org/10.1101/2022.07.13.499860}{(Kremer et al., 2022)}. Additionally, average values of the pseudotimes were calculated.
}
\usage{
data(ex_meta)
}
\description{
Data frame of meta data for example sparse matrix ex_spM, containing cell ids and pseudotimes.
}
\references{
Kremer, L. P., Cerrizuela, S., Shukairi, M. E. A., Ellinger, T., Straub, J., Dehler, S., Korkmaz, A., Weichenhan, D., Plass, C., Anders, S., & Martin-Villalba, A. (2022). \if{html}{\out{<br>}}
Single-cell triple-omics uncovers DNA methylation as key feature of stemness in the healthy and ischemic adult brain. \if{html}{\out{<br>}}
bioRxiv, 2022.2007.2013.499860 \url{https://doi.org/10.1101/2022.07.13.499860}.
}
\keyword{datasets}
