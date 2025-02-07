#one.df.t = one.df.t[,c("France","Thailand","Kenya")]


extract_suppressed_values <- function(column) {
    less_than_values <- grep("^<", column, value = TRUE)
    cleaned_values <- as.numeric(gsub("^<", "", less_than_values))
    c(0,sort(unique(cleaned_values)))
}

pick_new_values = function(values){
    medians <- (values[-length(values)] + values[-1]) / 2
    return(medians)
}


replace_with_conversion <- function(column, country_name,
                                    value.conversion) {
   # Get the specific country's conversion info
    country_conversion <- value.conversion[value.conversion$country == country_name, ]
    
    # Identify values starting with "<"
    is_less_than <- grepl("<", column)
    
    # Strip "<" to get the numeric part
    stripped_values <- gsub("<", "", column[is_less_than])
    
    # Replace based on conversion table
    replacement_values <- sapply(stripped_values, function(val) {
        # Match the stripped value with Old_Value in the conversion table
        matched_value <- country_conversion$new.values[country_conversion$suppressed.values == as.numeric(val)]
        if (length(matched_value) == 0) NA else matched_value
    })
    
    # Final column with replacements
    column[is_less_than] <- replacement_values
    
    # Convert entire column to numeric
    return(as.numeric(gsub(" ","",column)))
}

