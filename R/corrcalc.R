#' correlation analysis
#'
#' this program takes input as data frame and performs correlation analysis on the data.
#'
#' @export
#' @import caret
#' @import corrplot
#' @import dplyr
#' @author Anushka Jain
#' @name correlation
#' @title correlation
#' @param df data frame
#' @return plots and data frames

corrcalc <- function(df) {
  as.factor(df$diagnosis)
  map_int(df, function(.x) sum(is.na(.x)))
  round(prop.table(table(df$diagnosis)), 2)
  df_corr <- cor(df %>% select(-id, -diagnosis, -X))
  corrplot(df_corr, order = "hclust", tl.cex = 1, addrect = 8)
}
