library(plyr)

myfunction <- function(df, row, column) {
  new_df <- df[row,column]
  result_list <- list()
  for (i in names(new_df)){
    calc_value <- NULL
    if (class(new_df[[i]]) == 'numeric') {
      calc_value <- mean(new_df[[i]])
    } else if (class(new_df[[i]]) == 'logical' || class(new_df[[i]]) == 'character' || class(new_df[[i]]) == 'factor') {
      calc_value <- count(new_df[[i]], 1)
    } 
    calc_value
    
    result_list[[i]] <- calc_value
  }
  list(result_list, new_df)
}

#EXAMPLES
result <- myfunction(example_dataframe, c(1,2)) 

result <- myfunction(example_dataframe, example_dataframe$Sepal.Width > 3.5)

result <- myfunction(example_dataframe, column_selector=c(1,3,5))

