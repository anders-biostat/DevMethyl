% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_cpgislands.R
\name{plot_cpgislands}
\alias{plot_cpgislands}
\title{Plot CpG islands}
\usage{
plot_cpgislands(species, genome, chr, startpos, endpos)
}
\arguments{
\item{species}{String of species name/alias.}

\item{genome}{String of the genome version used. Nomenclature from UCSC Genome Browser and Genome Reference Consortium are both acceptable inputs.}

\item{chr}{Integer number of chromosome.}

\item{startpos, endpos}{Integers defining the start and end position of the analysed genomic region.}
}
\value{
Segment plot indicating the position of CpG islands within the genomic region.
}
\description{
\code{plot_cpgislands} downloads and plots CpG islands of the chosen genomic region as a segment plot, visualizing the positions and lengths of the islands.
Information of the CpG islands are obtained from the UCSC Genome Browser data retrieval tool \url{https://genome.ucsc.edu/cgi-bin/hgTables}.
}
\examples{
plot_cpgislands("mouse", "mm10", 8, 8620000, 8680000)
}
