#' Summary Statistic
#'
#' @param df a dataframe
#' @param FUN a statistical function; i.e. mean, median, standard deviation
#'
#' @return
#' @export
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom magrittr %>%
#' @importFrom readr read_csv
#'
#' @examples
#' DRG <- read_csv('DRG_data.csv')
#'
#' summary_statistic(DRG, mean)
#'
summary_statistic <- function(df, FUN){
  df$`DRG Code` = substr(df$`DRG Definition`, 1, 3) # create column with only numerical code
  df %>%
    group_by(`DRG Code`) %>%
    summarise(statistic = FUN(`Average Medicare Payments`)) # FUN is the function inputted as argument
}


