library(dplyr)
library(readr)
library(tidyr)
library(purrr)


# calls data frame
# fills select na values
# creates list rom strings for mechanic, category, publisher
call_boardgame_data <- function() {

    # load data, this is relative to the root folder in the repository
    filename <- "data/app_data/board_game.csv"
    boardgame_data <- read_csv(filename)

    # convert NA value to 'Unknown' for specific columns
    repl_val <- "Unknown"
    # regex RYAN TO REPLACE WITH CORRECT REGEX HERE
    reg_use <- ","
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

# Filters the dataframe and returns the top n
call_boardgame_filter <- function(data, cat, mech, pub, n = NULL) {
    func_df_out <- data

    # check each category
    cat_check <- call_bool_series_and(data, "category", cat)
    mech_check <- call_bool_series_and(data, "mechanic", mech)
    publ_check <- call_bool_series_and(data, "publisher", pub)

    # remove rows from df that aren't matched
    func_df_out <- func_df_out %>%
        mutate(cat_check, mech_check, publ_check) %>%
        filter(cat_check, mech_check, publ_check) %>%
        select(-cat_check, -mech_check, -publ_check) %>%
        # sort by average rating
        arrange(desc(average_rating))

    # return number of entries is specified by user
    if (!is.null(n)) {
        func_df_out <- slice_head(func_df_out, n = n)
    }

    return(func_df_out)
}


# helper function to check where user input matches
# used in the `*and()` and `*or()` functions
check_list <- function(col_data, list_) {
    map(col_data, ~ (list_ %in% unlist(.x)))
}


# function which checks if conditions are met for all requirements
call_bool_series_and <- function(data, col_, list_) {

    # check if all of the values contain the user input list
    check <- check_list(data[[col_]], list_) %>%
        map(~all(.)) %>%
        unlist()

    # if there are no TRUE values in the entire list, switch all values to TRUE
    if (sum(check) == 0) check <- !check
    return(check)
}


# function which checks if one condition is met from requirements
call_bool_series_or <- function(data, col_, list_) {

    # check if one of the values matches the user input list
    check <- check_list(data[[col_]], list_) %>%
        map(~any(.)) %>%
        unlist()

    return(check)
}


# call from tab 1 of graph to filter data based on user selection
call_boardgame_radio <- function(data, col_, list_) {
    func_df_out <- data
    # subset based on user selection
    func_df_out <- func_df_out[call_bool_series_or(data, col_, list_), ]
    # call form group to add group column
    func_df_out <- form_group(func_df_out, col_, list_)
    # remove all entries that aren't part of a group
    fucn_df_out <- filter(func_df_out, !is.na(group))
    return(fucn_df_out)
}


# used in form group map call to check what should be assigned
form_group_helper <- function(data, list_) {
    if (length(data) == 0) {
        return(NA)
    }
    else if (length(list_) > 1 & all(data)) {
        return("All Selected")
    }
    else {
        return(list_[data])
    }

}


# form groupings based on user selection from `call_boardgame_radio`
form_group <- function(data, col_, list_) {

    # check if values contain the user input
    check <- check_list(data[[col_]], list_)

    # assign correct value
    output <- map(check, ~ (form_group_helper(.x, {{list_}}))) %>%
        unlist()

    # add new column to dataframe
    func_df_out <- data %>%
        mutate(group = output)

    return(func_df_out)
}


# provides group counts after `call_boardgame_radio()` is used
count_group <- function(data) {

    # create dataframe with counts for each category
    func_df_out <- data %>%
        count(year_published, group) %>%
        pivot_wider(names_from = group, values_from = n)

    # if `All Selected` exists, add counts to other categories
    if ("All Selected" %in% names(func_df_out)) {
        # remove na values from `All Selected`
        temp1 <- func_df_out$`All Selected` %>% replace_na(0)
        # select category columns
        temp2 <- func_df_out %>% select(-year_published, -`All Selected`)
        # add `All Selected to each column`
        new_df_out <- map_df(temp2, ~ {. + temp1})
        # create new output df by adding removed columns
        func_df_out <- new_df_out %>%
            mutate(year_published = func_df_out$year_published,
                   `All Selected` = func_df_out$`All Selected`) %>%
            select(year_published, everything())
    }

    return(func_df_out)
}


# return top 5 values based on the average for a given column
call_boardgame_top <- function(data, col_, year_in, year_out) {
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
subset_data <- function(data, col_ = "category") {
    func_df_out <- data %>% unnest(.data[[col_]])

    unique_out <- map(func_df_out[[col_]], ~ (unlist(.x))) %>%
        unlist() %>%
        unique() %>%
        na.omit()

    return(unique_out)
}


# used to remove columns prior to passing into plotting functions
remove_columns <- function(data) {
    reduced_data <- data %>% select(
        name,
        year_published,
        average_rating,
    )

    if ("group" %in% names(data)) {
        reduced_data <- mutate(reduced_data, group = data[["group"]])
    }
    return(reduced_data)
}
