library(dplyr)
library(readr)
library(tidyr)
library(purrr)


### TEMPORARY FOR TESTING
#cat_in = c('Economic', 'Negotiation')
#mech_in = list('Hand Management')
#pub_in = c('')


# calls data frame
# fills select na values
# creates list rom strings for mechanic, category, publisher
call_boardgame_data <- function(){
    
    # load data, this is relative to the root folder in the repository
    filename <- 'data/app_data/board_game.csv'
    boardgame_data <- read_csv(filename)

    # convert NA value to 'Unknown' for specific columns
    repl_val <- 'Unknown'
    # regex
    reg_use <- ','
    boardgame_data <- boardgame_data %>% 
        replace_na(list(category = repl_val, 
                        mechanic = repl_val,
                        publisher = repl_val)) %>%
        mutate(category = strsplit(category, reg_use),
               mechanic = strsplit(mechanic, reg_use),
               publisher = strsplit(publisher, reg_use)) %>%
        select(-X1)
    
    return(boardgame_data)
}

call_boardgame_filter <- function(data, cat, mech, pub, n = NULL){
    func_df_out <- data
    
    # check each category
    cat_check <- call_bool_series_and('category', cat, data)
    mech_check <- call_bool_series_and('mechanic', mech, data)
    publ_check <- call_bool_series_and('publisher', pub, data)
    
    # remove rows from df that aren't matched
    func_df_out <- func_df_out %>%
        mutate(cat_check, mech_check, publ_check) %>%
        filter(cat_check, mech_check, publ_check) %>%
        select(-cat_check, -mech_check, -publ_check)
    
    # if a number is specified, return the number of entires
    if (!is.null(n)){
        func_df_out <- func_df_out %>% 
            arrange(desc(average_rating)) %>%
            slice_head(n = n)
    }
    return(func_df_out)
}


call_boardgame_radio <- function(data, col_, list_) {
    func_df_out <- data
    func_df_out <- func_df_out[call_bool_series_or(col_, list_, data),]
    func_df_out <- form_group(col_, list_, func_df_out)    
    fucn_df_out <- filter(func_df_out, !is.na(.data[[col_]]))
    return(fucn_df_out)
}

form_group <- function(col_, list_, data) {
    func_df_out <- data
    output <- vector(mode = 'list', length = nrow(data))
    
    count <- 1
    for (i in data[[col_]]) {
        strings <- strsplit(i, ",")
        unlisted_strings <- unlist(strings)
        check <- (list_ %in% unlisted_strings)
        
        if (length(list_[check]) == 0) {
            list_result = NA
        }
        else if (length(list_) > 1 & all(check)) {
            list_result <- 'All Selected'
        }
        else {
            list_result <- list_[check]
        }
        
        output[count] <- list_result
        count <- count + 1
    }
    output <- unlist(output)
    
    func_df_out <- func_df_out %>% 
        mutate(group = output)
    
    return(func_df_out)
}


# helper function to check where user input matches
check_list <- function(col_data, list_) {
    map(col_data, ~(list_ %in% unlist(.x)))
}


# function which checks if one condition is met from requirements
call_bool_series_or <- function(col_, list_, data){
    
    # check if one of the values matches the user input list
    check <- check_list(data[[col_]], list_) %>%
        map(~any(.)) %>% unlist()
    
    return(check)
}


# function which checks if conditions are met for all requirements
call_bool_series_and <- function(col_, list_, data) {
    
    # check if all of the values contain the user input list
    check <- check_list(data[[col_]], list_) %>%
        map(~all(.)) %>% unlist()

    # if there are no TRUE values in the entire list, switch all values to TRUE
    if (sum(check) == 0) check = !check
    return(check)
}

# return top 5 values based on the average for a given column
call_boardgame_top <- function(col_, year_in, year_out, data) {
    func_df_out <- data %>%
        filter(year_published >= year_in, year_published <= year_out) %>%
        unnest(.data[[col_]]) %>% 
        # note, groupby did not like being passed curly curly
        group_by(.data[[col_]]) %>%
        summarize(average = mean(average_rating)) %>%
        arrange(desc(average)) %>%
        slice_head(n = 5)
    
    return(func_df_out)
}

# return unique values to populate dropdowns
subset_data <- function(data, col_='category') {
    func_df_out <- data %>% unnest(.data[[col_]])
    
    unique_out <- map(func_df_out[[col_]], ~(unlist(.x))) %>%
        unlist() %>% unique %>% na.omit()
    
    return(unique_out)
}


# used to remove columns prior to passing into plotting functions
remove_columns <- function(data) {
    reduced_data <- data %>% select(
        name,
        year_published,
        average_rating,
    )
    
    ### THIS MAY NEED TO BE FIXED ###
    if ('groups' %in% names(data)) {
        mutate(reduced_data, groups = data[[groups]])
    }
    
    return(reduced_data)
}
    
    
    
    



