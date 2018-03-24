#' Perform Classic Multi-dimensional scaling on a distance matrix, taking care of boilerplate code.
#'
#' Returns a list containing the dataframe, preformatted axis labels, and eigenvalue magnitudes.
#'
#' @param dist A distance matrix, either as a matrix or as a dist object
#' @param k The number of PCoA axes
#' @param metadata If not null, a dataframe (or tibble) describing additional sample metadata
#' @param by A vector informing `left_join` how to join (see the dplyr documentation)
#' @export
tidy_cmdscale = function(dist, k=9, metadata=NULL, by=NULL) {
    dist = as.dist(dist)
    mds = cmdscale(dist, k=9, eig=T)
    mds.df = as.data.frame(mds$points)
    variance.pct = mds$eig / sum(mds$eig) * 100
    colnames(mds.df) = paste0("PC", 1:ncol(mds.df))
    mds.df$sample = methods::slot(dist, "Labels")
    label = paste0("PCoA Axis ", 1:ncol(mds.df), " (", round(variance.pct, 0), "%)")
    if (!is.null(metadata)) {
        mds.df = dplyr::left_join(mds.df, metadata, by=by)
    }
    return(list(df=mds.df, variance.pct=variance.pct, axis.label=label))
}
