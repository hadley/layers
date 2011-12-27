"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

resolution <- function(x, zero = TRUE) {
  x <- unique(as.numeric(x))
  if (length(x) == 1) return(1)

  if (zero) {
    x <- unique(c(0, x))
  }
  
  min(diff(sort(x)))
}

.pt <- 1 / 0.352777778


order_list <- function(list, by) {
  length <- vapply(list, NROW, integer(1))
  by_logical <- names(list) %in% by
  
  order_by <- do.call("order", list[by_logical & length != 1])
  list[length != 1] <- lapply(list[length != 1], "[", order_by)
  list
}

list_to_df <- function(list) {
  if (is.data.frame(list)) return(list)
  
  length <- vapply(list, NROW, integer(1))  
  n <- max(length)

  if (!all(length %in% c(1, n))) {
    stop("Recycling not supported", call. = FALSE)
  }
  
  list[length == 1] <- lapply(list[length == 1], rep.int, times = n)
  
  structure(list, 
    class = "data.frame",
    row.names = c(NA, -n))
}
