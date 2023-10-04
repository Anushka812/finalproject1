#' pc analysis
#'
#' this program takes input as data frame and performs principal component analysis on the data.
#'
#' @export
#' @import tidyverse
#' @import tibble
#' @author Anushka Jain
#' @name pca
#' @title pca
#' @param df data frame
#' @return plots and data frames

pcacalc <- function(df) {
  map_int(df, function(.x) sum(is.na(.x)))
  results <- prcomp(df %>% select(-id, -diagnosis, -X), scale = TRUE, center = TRUE)
  summary(results)
  pca_df <- as_tibble(results$x)
  ggplot(pca_df, aes(x = PC1, y = PC2, col = df$diagnosis)) + geom_point()
  autoplot(results, data = df,  colour = 'diagnosis',
           loadings = FALSE, loadings.label = TRUE, loadings.colour = "blue")
}
