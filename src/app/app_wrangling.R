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
    return(func_df_out[call_bool_series_or(col_, list_, data),])
}

form_group <- function(col_, list_, data) {
    func_df_out <- data
    output <- vector(mode = 'list', length = nrow(data))
    
    count <- 1
    for (i in data[[col_]]) {
        strings <- strsplit(i, ",")
        unlisted_strings <- unlist(strings)
        check <- (list_ %in% unlisted_strings)
        list_result <- list_[check]
        if (length(list_result) == 0) {list_result = NA}
        output[count] <- list_result
        count <- count + 1
    }
    output <- unlist(output)
    
    func_df_out <- func_df_out %>% 
        mutate(group = output)
    
    return(func_df_out)
    
    
}


# ASK RYAN WHY THIS HAS LIST_TO_STRING_
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
    
    
    
    



