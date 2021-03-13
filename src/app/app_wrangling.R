library(dplyr)
library(readr)
library(tidyr)

call_boardgame_data <- function(){
    
    # load dataframe
    filename <- 'data/app_data/board_game.csv'
    boardgame_data <- read_csv(filename)

    # convert NA value to 'Unknown' for specific columns
    repl_val <- 'Unknown'
    boardgame_data %>% replace_na(list(category = repl_val, mechanic = repl_val,
                                       publisher = repl_val))
    
    return(boardgame_data)
}

call_boardgame_filter <- function(data, cat, mech, pub, n = NULL){
    func_df_out <- data
    cat_check <- call_bool_series_and('category', cat, data)
    mech_check <- call_bool_series_and('mechanic', mech, data)
    publ_check <- call_bool_series_and('publisher', pub, data)
    
    func_df_out <- func_df_out %>%
        mutate(cat_check, mech_check, publ_check) %>%
        filter(cat_check, mech_check, publ_check) %>%
        select(-cat_check, -mech_check, -publ_check)
    
    if (!is.null(n)){
        func_df_out <- func_df_out %>% 
            arrange(desc(average_rating)) %>%
            slice_head(n = n)
        
        
    }
    return(func_df_out)
        
}

#cat_in = c('Economic', 'Negotiation')
#mech_in = list('Hand Management')
#pub_in = c('')

call_boardgame_radio <- function(data, col_, list_) {
    func_df_out <- data
    func_df_out <- func_df_out[call_bool_series_or(col_, list_, data),]
    func_df_out <- form_group(col_, list_, func_df_out)    
    fucn_df_out <- filter(func_df_out, !is.na({{col_}}))
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


call_bool_series_or <- function(col_, list_, data){
    output <- vector(mode = 'list', length = nrow(data))
    
    for (i in data[[col_]]) {
        strings <- strsplit(i, ",")
        unlisted_strings <- unlist(strings)
        check <- any(list_ %in% unlisted_strings)
        output[count] <- check
        count <- count + 1
    }
    output <- unlist(output)
    return(output)
    
}

    
call_bool_series_and <- function(col_, list_, data) {
    output <- vector(mode = 'list', length = nrow(data))
    count <- 1
    for (i in data[[col_]]) {
        strings <- strsplit(i, ",")
        unlisted_strings <- unlist(strings)
        check <- all(list_ %in% unlisted_strings)
        output[count] <- check
        count <- count + 1
    }
    output <- unlist(output)
    if (sum(output) == 0) {output = !output}
    return(output)
}


call_boardgame_top <- function(col_, year_in, year_out, data) {
    func_df_out <- data %>%
        filter(year_published >= year_in, year_published <= year_out) 
    
    func_df_out$explode_on <- strsplit(func_df_out[[col_]], ",")
    

    
    return(func_df_out)
}


subset_data <- function(data, col_='category') {
    func_df_out <- data %>% unnest({{col_}})
    
    output <- vector(mode = 'list', length = nrow(func_df_out))
    count <- 1
    
    for (i in func_df_out[[col_]]) {
        strings <- strsplit(i, ",")
        unlisted_strings <- unlist(strings)
        output[count] <- unlisted_strings
        count <- count + 1
    }
    unique_out <- unique(unlist(output))
    return(unique_out[!is.na(unique_out)])
}


remove_columns <- function(data) {
    reduced_data <- data %>% select(
        -X1,
        -game_id,
        -image,
        -max_players,
        -max_playtime,
        -min_age,
        -min_players,
        -min_playtime,
        -playing_time,
        -thumbnail,
        -artist,
        -category,
        -compilation,
        -designer,
        -expansion,
        -family,
        -mechanic,
        -publisher,
        -users_rated
    )
    
    return(reduced_data)
}
    
    
    
    



