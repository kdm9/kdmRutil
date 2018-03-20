#' Convert a vector of file paths into sample names
#'
#' @param paths The file paths
#' @param exts The list of extensions to strip
#' @export
filepath2sample = function(paths, exts=c(".gz", ".fastq")) {
  paths = basename(paths)
  for (ext in exts) {
    ext=paste0(ext, '$') # build regex
    paths = sub(ext, "", paths)
  }
  return(paths)
}

#' Reads a labelled square matrix to a correctly named matrix object
#'
#' @param path Path to LSM file on disk
#' @param sep The column separation character
#' @export
read_lsm = function(path, sep='\t', exts.to.remove=c(".gz", ".fastq", ".ct", ".msh")) {
  df = read.table(path, header=T, sep=sep, row.names=1, comment.char="")
  rownames(df) = colnames(df) = filepath2sample(rownames(df), exts=exts.to.remove)
  return(as.matrix(df))
}
