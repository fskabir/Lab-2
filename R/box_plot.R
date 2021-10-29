#' Box Plot
#'
#' @param df a dataframe
#' @param column a string for a variable in the dataframe df
#'
#' @return A box plot of \code{column} versus DRG codes.
#' @export
#'
#' @import ggplot2
#'
#' @examples
#' DRG <- read_csv('DRG_data.csv')
#'
#' box_plot(DRG, 'Average Medicare Payments')
#'
box_plot <- function(df, column){
  df$`DRG Code` = substr(df$`DRG Definition`, 1, 3) # create column with only numerical code
  ggplot(df, aes(x=`DRG Code`, y = get(column))) +
    geom_boxplot() + guides(x = guide_axis(angle = 90)) +
    theme_bw() +
    labs(title = paste(column, "versus DRG Codes", sep = " "),
         y= paste(column)) + # customize title by inputted variable column
    theme(plot.title = element_text(hjust = .5)) # center title
}
