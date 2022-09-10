#' Split given dataframe in a given column by a range
#' of values defined by the user.
#' 
#' The minimum value in the range is included in the
#' subset, opposed to the maximum.
#'
#' @param df Dataframe to subset
#' @param column_name Name of the column in `df` containing the values to separate
#' @param initial_step Starting value used in data separation
#' @param step Incremental step used in separation
#'
#' @return List of filtered dataframes
#'
#' @examples
#' Given dataframe `df`:
#' column_1 | column_2
#'    10    |    1
#'    11    |    2
#'    12    |    3
#'    13    |    4
#'    14    |    5
#'    
#' subsets <- split_by_content(df = df,
#'                             column_name = "column_1",
#'                             initial_step = 10,
#'                             step = 2)
#'                             
#' `subsets` contains:
#' [1]: 
#'    column_1 | 10 11
#'    column_2 | 1 2
#' [2]: 
#'    column_1 | 12 13
#'    column_2 | 3 4
#' [3]: 
#'    column_1 | 14
#'    column_2 | 5
#'    
split_by_content <- function(df, column_name, initial_step, step) {
  # Arguments validation
  #stopifnot(is.matrix(df))
  
  # List of subsets to return
  subsets <- list()
  
  # Number of elements subdivided in new subsets
  subset_elements <- 0
  
  # Declare a dictionary containing the min/max range
  # of values used to subset the dataframe
  value_range <- list(min = initial_step,
                      max = initial_step + step)
  
  # Continue until all the elements in df are subdivided
  while (subset_elements < nrow(df)) {
    # Subset the dataframe based on the values of the current step
    # and the desired column
    subset <- df[df[column_name] >= value_range$min 
                 & df[column_name] < value_range$max, ]
    
    # Increment the number of elements in the new subsets
    subset_elements <- subset_elements + nrow(subset)
    
    # Increment the min/max value range
    value_range$min <- value_range$min + step
    value_range$max <- value_range$max + step
    
    # Append the new subset to the list
    # Use list(subset) to avoid destructuring of the dataframe
    # Eg.:
    # Without list: All the columns are added as single elements
    # With list: The entire dataframe is added as a single element
    subsets <- append(subsets, list(subset))
  }
  
  # Return a list of dataframes
  return(subsets)
}




