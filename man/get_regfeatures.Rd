% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_regfeatures.R
\name{get_regfeatures}
\alias{get_regfeatures}
\title{Get regulatory features of genomic region}
\usage{
get_regfeatures(gffpath, chr, startpos, endpos)
}
\arguments{
\item{gffpath}{Location of the gff file to be read. Can be a single string of the file path or the URL or can be a connection.}

\item{chr}{integer number of chromosome}

\item{startpos}{integer defining the start position of the analysed genomic region}

\item{endpos}{integer defining the end position of the analysed genomic region}
}
\value{
data frame ...
}
\description{
Get regulatory features of genomic region
}
\examples{
get_regfeatures("https://ftp.ensembl.org/pub/release-110/regulation/mus_musculus/mus_musculus.GRCm39.Regulatory_Build.regulatory_features.20221007.gff.gz", 8, 8628165, 8684055)
}
