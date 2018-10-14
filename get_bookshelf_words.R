# create get_bookshelf_words function
get_bookshelf_words <- function(bookshelf_text, bookshelf_ocr_data) {
        
        # find newline characters in bookshelf_text
        newline_locations <- as.data.frame(str_locate_all(string = bookshelf_text, pattern = "\n"))

        # create get_row_text function
        get_bookshelf_rows <- function(i) {
                
                # get current_newline_start_location
                if(i == 1) {
                        current_newline_start_location <- 1  
                } else {
                        current_newline_start_location <- newline_locations$end[i - 1] + 1
                }
                
                # get current_newline_end_location
                current_newline_end_location <- newline_locations$end[i] - 1
                
                # get current_row_text
                current_row_text <- str_sub(string = bookshelf_text, start = current_newline_start_location,
                                            end = current_newline_end_location)
                current_row_text
                
        }
        
        # run get_bookshelf_rows function
        bookshelf_rows <- map(.x = 1:nrow(newline_locations), .f = get_bookshelf_rows) %>% unlist() %>% as_tibble() %>% 
                rename(row_text = value) %>% mutate(row_id = row_number()) 

        # create list of just normal letters (regex [:alpha:] matches several unicode characters, so cant remove them)
        normal_letters <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o",
                            "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", " ")
        any_non_normal_letters_regex <- str_c(normal_letters, collapse = "|")
        any_non_normal_letters_regex <- str_c("[^", any_non_normal_letters_regex, "]")

        # tokenize row_text to get individual words, associated with their row of origin
        bookshelf_rows <- bookshelf_rows %>% 
                mutate(row_text = str_replace_all(string = row_text, 
                                                  pattern = regex(any_non_normal_letters_regex, ignore_case = TRUE), replacement = ""),
                       row_text = str_replace_all(string = row_text, pattern = regex("\\|"), replacement = "")) %>%
                filter(!(row_text %in% c("", " "))) %>%
                unnest_tokens(output = row_text, input = row_text, token = "words")
        
        # remove terms containing no alpha or numeric characters, 
        # remove punctuation from all terms, 
        # and drop terms = "" 
        bookshelf_ocr_data_clean <- bookshelf_ocr_data %>% 
                mutate(word = str_replace_all(string = word, 
                                              pattern = regex(any_non_normal_letters_regex, ignore_case = TRUE), replacement = ""),
                       word = str_replace_all(string = word, pattern = regex("\\|"), replacement = "")) %>%
                filter(!(word %in% c("", " ")))
        
        # bind_cols on bookshelf_ocr_data _clean and bookshelf_rows to compare words
        bookshelf_words <- bookshelf_rows %>% bind_cols(., bookshelf_ocr_data_clean)
        
        # remove words with only one letter
        bookshelf_words <- bookshelf_words %>% filter(nchar(row_text) > 1)
        
        # create loop_through_bbox_coordinates function
        extract_bbox_coordinates <- function(.x) {
                
                # assign current_bbox
                current_bbox <- .x
                
                # get current_bbox_coord_locations
                current_bbox_coord_locations <- as.data.frame(str_locate_all(string = current_bbox, pattern = ","))
                current_bbox_coord_locations <- current_bbox_coord_locations %>% select(start) %>%
                        mutate(start = start + 1) %>% bind_rows(tibble(start = 1), .) %>%
                        mutate(coord_name = c("start_bbox_top_left_x", "start_bbox_top_left_y", 
                                              "start_bbox_bottom_right_x", "start_bbox_bottom_right_y")) %>%
                        spread(key = coord_name, value = start)
                
                # get current_bbox_coordinates
                current_bbox_top_left_x <- str_sub(string = current_bbox, 
                                                   start = current_bbox_coord_locations$start_bbox_top_left_x[1],
                                                   end = current_bbox_coord_locations$start_bbox_top_left_y[1] - 2)
                
                current_bbox_top_left_y <- str_sub(string = current_bbox, 
                                                   start = current_bbox_coord_locations$start_bbox_top_left_y[1],
                                                   end = current_bbox_coord_locations$start_bbox_bottom_right_x[1] - 2)
                
                current_bbox_bottom_right_x <- str_sub(string = current_bbox, 
                                                       start = current_bbox_coord_locations$start_bbox_bottom_right_x[1],
                                                       end = current_bbox_coord_locations$start_bbox_bottom_right_y[1] - 2)
                
                current_bbox_bottom_right_y <- str_sub(string = current_bbox, 
                                                       start = current_bbox_coord_locations$start_bbox_bottom_right_y[1],
                                                       end = nchar(current_bbox))
                
                current_bbox_coordinates <- tibble(bbox_top_left_x = as.numeric(current_bbox_top_left_x),
                                                   bbox_top_left_y = as.numeric(current_bbox_top_left_y),
                                                   bbox_bottom_right_x = as.numeric(current_bbox_bottom_right_x),
                                                   bbox_bottom_right_y = as.numeric(current_bbox_bottom_right_y))
                current_bbox_coordinates
        }
        
        # create get_bbox_coordinates function
        get_bbox_coordinates <- function(bookshelf_words_tbl) {
                
                # get bbox_coordinates
                bbox_coordinates <- map_dfr(.x = bookshelf_words_tbl$bbox, .f = extract_bbox_coordinates)
                
                # bind bbox_coordinates to bookshelf_words_tbl
                bookshelf_words_tbl %>% bind_cols(., bbox_coordinates)
        }
        
        # run get_bbox_coordiantes function and return output
        get_bbox_coordinates(bookshelf_words)
}


############################################################################


# test get_bookshelf_words
# bookshelf_text <- bookshelf_text6
# bookshelf_ocr_data <- bookshelf_ocr6
# get_bookshelf_words(bookshelf_text = bookshelf_text2, bookshelf_ocr_data = bookshelf_ocr2)
