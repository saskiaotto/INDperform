% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clust_sc.R
\name{clust_sc}
\alias{clust_sc}
\title{Score-based cluster analysis}
\usage{
clust_sc(dist_mat, method_clust = "average", ...)
}
\arguments{
\item{dist_mat}{The distance matrix computed by the \code{\link{dist_sc}} function.}

\item{method_clust}{The agglomeration method to be used in the \code{hclust} function.
Default is "average", for alternatives see \code{\link[stats]{hclust}}.}

\item{...}{Further arguments to be passed to the method \code{hclust}.}
}
\value{
An object of class \code{hclust} is returned, which describes the tree produced by the
clustering process. See for more details \code{\link[stats]{hclust}}. Additionally,
the cophenetic correlation coefficient and the Gower distance are printed in the
console as guidance for selecting the best agglomeration method.
}
\description{
\code{clust_sc} computes a hierarchical cluster analysis for the identification
 of indicator redundancies.
}
\examples{
# Using the Baltic Sea demo data
scores_tbl <- scoring(trend_tbl = model_trend_ex,
  mod_tbl = all_results_ex, press_type = press_type_ex)
scores_mat <- summary_sc(scores_tbl)$scores_matrix
dist_matrix <- dist_sc(scores_mat, method_dist = "euclidean")
clust_analysis <- clust_sc(dist_matrix, method_clust = "complete")
}
\seealso{
\code{\link{hclust}}

Other score-based IND performance functions: 
\code{\link{dist_sc}()},
\code{\link{dist_sc_group}()},
\code{\link{expect_resp}()},
\code{\link{plot_clust_sc}()},
\code{\link{plot_spiechart}()},
\code{\link{scoring}()},
\code{\link{summary_sc}()}
}
\concept{score-based IND performance functions}
