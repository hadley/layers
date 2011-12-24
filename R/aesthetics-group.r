#' Ensure that the data frame contains a grouping variable.
#'
#' If the \code{group} variable is not present, then a new group
#' variable is generated from the interaction of all discrete (factor or
#' character) vectors excluding label.
#' 
#' @return a data.frame with group variable
add_group <- function(data) {
  if (empty(data)) return(data.frame())
  if (!is.null(data$group)) return(data)
  
  aes_vars <- intersect(names(data), setdiff(all_aesthetics, "label"))
  discrete_vals <- Filter(is.discrete, data[aes_vars])
  
  data$group <- id(discrete_vals)
  data
}
