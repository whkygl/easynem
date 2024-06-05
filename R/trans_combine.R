#' trans_formula
#' @description Sum meta by column.
#' @param data easynem type data.
#' @param col The name of the column to be summed.
#' @return An easynem object.
#' @export
trans_combine <- function(data, col){
  meta = data@meta
  if (!all(col %in% colnames(meta))) {
    stop("col must be present in the meta")
  }
  new_col_name <- paste(col, collapse = "_")
  meta[[new_col_name]] <- rowSums(meta[, col, drop = FALSE])
  data@meta = meta
  return(data)
}