stat_transform <- function(stat, data, xrange, yrange) {
  UseMethod("stat_transform")
}

stat_from_call <- function(name, arguments = NULL) {
  if (is.null(arguments)) {
    parent <- sys.frame(-1)
    arguments <- as.list(parent)
  }
  
  structure(arguments, class = c(name, "stat"))
}

join_aesthetics <- function(stats, data) {
  unique <- ddply(data, "group", uniquecols)
  join(stats, unique, by = "group")
}

uniquecols <- function(df) {
  df <- df[1, sapply(df, function(x) length(unique(x)) == 1), drop=FALSE]
  rownames(df) <- 1
  df
}
