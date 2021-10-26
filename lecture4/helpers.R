read_and_clean <- function(f){
  ## Function for reading a (messy) .txt file and wrangling it into a clean
  ## data frame
  
  df <- read_csv(f, skip = 1) # Skip the first row
  df <- df[,-1] # Remove the first column
  col_names <- read_csv(f, n_max=0) %>% names() # Retrieve the column names as a separate csv file
  names(df) <- col_names
  return(df)
}

retrieve_data <- function(data_loc){
  ## Function for retrieving and combining the occupancy data files 
  ## found in `data_loc`
  
  # Get a list of all the files in data_loc
  files <- list.files(data_loc, full.names = TRUE)
  
  # Iterate through all files, applying the read_and_clean function
  list_df <- map(files, read_and_clean) 
  
  # Bind together the data frames
  df <- bind_rows(list_df)
  
  return(df)
}

downsample_by_occupancy <- function(df){
  ## Function for downsampling a data frame to even out class membership 
  ## (specifically by occupancy)
  
  # Determine the majority class
  class_counts <- count(df, Occupancy)
  majority_class <- class_counts$Occupancy[which.max(class_counts$n)]
  
  # Calculate difference in observations between majority/minority classes (how many rows to remove)
  n_to_downsample <- abs(diff(class_counts$n))
  
  # Add rownames
  df <- df %>% rownames_to_column()
  
  # Sample rows from majority class to remove
  to_remove <- df %>% 
    filter(Occupancy == majority_class) %>%
    sample_n(n_to_downsample)
  
  # Use anti_join to remove those rows
  downsampled_df <- df %>% anti_join(to_remove)
  
  # Remove rownames
  downsampled_df <- downsampled_df %>% select(-rowname)
  
  return(downsampled_df)
}
