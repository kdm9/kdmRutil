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
#' @param idfile Matrix labels are supplied in external file, rather than row/col headers
#' @export
read_lsm = function(path, sep='\t', exts.to.remove=c(".gz", ".fastq", ".ct", ".msh", ".bam"), idfile=NULL) {
    if (is.null(idfile)) {
        df = read.table(path, header=T, sep=sep, row.names=1, comment.char="")
    } else {
        df = read.table(path, header=F, sep=sep, row.names=NULL, comment.char="")
        ids = read.table(idfile, header=F, row.names=NULL, stringsAsFactors=F)[,1]
        rownames(df) = ids
    }
    rownames(df) = colnames(df) = filepath2sample(rownames(df), exts=exts.to.remove)
    return(as.matrix(df))
}
