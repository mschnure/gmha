convert_string =  function(input_string) {
    # Replace spaces with underscores and convert to lowercase
    result <- gsub(" ", "_", tolower(input_string))
    return(result)
}

convert_string_upper_periods =  function(input_string) {
  # Replace spaces with underscores and convert to lowercase
  result <- gsub(" ", ".", toupper(input_string))
  return(result)
}
