# use bbox
# http://kba.cloud/hocr-spec/1.2/  # hOCR format
# the values are with reference to the the top-left corner of the document image and measured in pixels
# the order of the values are x0 y0 x1 y1 = "left top right bottom"

# location of "the"
# bookshelf_bbox_coord <- bookshelf_ocr %>% filter(word == "THE") %>% pull(bbox)
bookshelf_bbox_coord <- bookshelf_ocr2 %>%
        filter(str_detect(string = word, pattern = regex("sam", ignore_case = TRUE))) %>% pull(bbox)
# bookshelf_bbox_coord <- bookshelf_ocr2 %>%
#         filter(str_detect(string = word, pattern = regex("hamilton", ignore_case = TRUE))) %>% pull(bbox)
bookshelf_bbox <- unlist(str_split(string = bookshelf_bbox_coord, pattern = ","))
bookshelf_bbox

# visually see location of "the"
bookshelf_image_draw <- image_draw(bookshelf_image2)
rect(bookshelf_bbox[1], bookshelf_bbox[2], bookshelf_bbox[3], bookshelf_bbox[4], 
     border = "red", lty = "dashed", lwd = 10)
dev.off()
print(bookshelf_image_draw)

# inspect nearby words
bookshelf_ocr %>% 
        separate(bbox, into = c("bbox_coord_1", "bbox_coord_2", "bbox_coord_3", "bbox_coord_4"), sep = ",") %>%
        mutate(bbox_coord_1 = as.numeric(bbox_coord_1), bbox_coord_2 = as.numeric(bbox_coord_2),
               bbox_coord_3 = as.numeric(bbox_coord_3), bbox_coord_4 = as.numeric(bbox_coord_4)) %>%
        # filter(bbox_coord_1 > 80, bbox_coord_3 < 300) %>%
        filter(bbox_coord_2 < 1519, bbox_coord_4 > 1408) %>%
        # arrange(desc(confidence)) %>% 
        data.frame()

# look for word "wind" or "ind"
bookshelf_bbox_coord <- bookshelf_ocr %>% 
        filter(str_detect(string = word, pattern = regex("ind", ignore_case = TRUE))) %>% pull(bbox)
bookshelf_bbox <- unlist(str_split(string = bookshelf_bbox_coord, pattern = ","))
bookshelf_bbox

# visually see location of "ind"
bookshelf_image2 <- image_draw(bookshelf_image)
rect(bookshelf_bbox[1], bookshelf_bbox[2], bookshelf_bbox[3], bookshelf_bbox[4], border = "red", lty = "dashed", lwd = 5)
dev.off()
print(bookshelf_image2)


##############################################################################
#############################################################################
#################################################################################


# add row variable

# inspect
bookshelf_text2
bookshelf_ocr2
bookshelf_ocr2 %>% data.frame()
bookshelf_ocr2 %>% filter(str_detect(string = word, pattern = "\n"))

# find newline characters in bookshelf_text and concatenate last five terms and concatenate as a unique id

# find newline characters in bookshelf_text
newline_locations <- as.data.frame(str_locate_all(string = bookshelf_text2, pattern = "\n"))
newline_locations

# test row text
# row_text <- str_sub(string = bookshelf_text2, start = newline_locations$end[2] + 1,
#                           end = newline_locations$start[3] - 1)
# row_text

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
        current_row_text <- str_sub(string = bookshelf_text2, start = current_newline_start_location,
                                    end = current_newline_end_location)
        current_row_text
        
}

# run get_bookshelf_rows function
bookshelf_rows <- map(.x = 1:nrow(newline_locations), .f = get_bookshelf_rows) %>% unlist() %>% as_tibble() %>% 
        rename(row_text = value) %>% mutate(row_id = row_number()) 
bookshelf_rows

# create list of just normal letters (regex [:alpha:] matches several unicode characters, so cant remove them)
# str_replace_all("Samâ€œ", pattern = regex("[^[:alpha:]]"), replacement = "")
normal_letters <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o",
                    "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", " ")
any_non_normal_letters_regex <- str_c(normal_letters, collapse = "|")
any_non_normal_letters_regex <- str_c("[^", any_non_normal_letters_regex, "]")
any_non_normal_letters_regex
# str_replace_all("Samâ€œ", pattern = regex(any_non_normal_letters_regex, ignore_case = TRUE), replacement = "")

# tokenize row_text to get individual words, associated with their row of origin
bookshelf_rows <- bookshelf_rows %>% 
        mutate(row_text = str_replace_all(string = row_text, 
                                          pattern = regex(any_non_normal_letters_regex, ignore_case = TRUE), replacement = ""),
               row_text = str_replace_all(string = row_text, pattern = regex("\\|"), replacement = "")) %>%
        filter(row_text != "") %>%
        unnest_tokens(output = row_text, input = row_text, token = "words")
bookshelf_rows
bookshelf_rows %>% data.frame()

# remove terms containing no alpha or numeric characters, 
# remove punctuation from all terms, 
# and drop terms = "" 
bookshelf_ocr2_clean <- bookshelf_ocr2 %>% 
        mutate(word = str_replace_all(string = word, 
                                      pattern = regex(any_non_normal_letters_regex, ignore_case = TRUE), replacement = ""),
               word = str_replace_all(string = word, pattern = regex("\\|"), replacement = "")) %>%
        filter(word != "")
bookshelf_ocr2_clean
bookshelf_ocr2_clean %>% data.frame()
bookshelf_ocr2 %>% slice(1:100) %>% data.frame()

# pad bookshelf_ocr_data_clean
bookshelf_ocr_data_clean_pad <- tibble(word = rep(NA, times = 455 - 288), confidence = rep(NA, times = 455 - 288),
                                       bbox = rep(NA, times = 455 - 288))
bookshelf_ocr_data_clean_w_pad <- bookshelf_ocr_data_clean %>% bind_rows(., bookshelf_ocr_data_clean_pad)

# pad bookshelf_rows
bookshelf_rows_pad <- tibble(row_text = rep(NA, times = 371 - 370), row_id = rep(NA, times = 371 - 370))
bookshelf_rows_w_pad <- rbind(bookshelf_rows, bookshelf_rows_pad)

# bind_cols on bookshelf_ocr2_clean and bookshelf_rows to compare words
# bookshelf_words <- bookshelf_rows %>% bind_cols(., bookshelf_ocr_data_clean_w_pad)
# bookshelf_words <- bookshelf_rows %>% bind_cols(., bookshelf_ocr_clean)
bookshelf_words <- bookshelf_rows_w_pad %>% bind_cols(., bookshelf_ocr_data_clean)
bookshelf_words %>% slice(50:100) %>% data.frame()
bookshelf_words %>% tail(50)
bookshelf_words %>% data.frame()
bookshelf_words

# remove words with only one letter
bookshelf_words <- bookshelf_words %>% filter(nchar(row_text) > 1)
bookshelf_words

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
        
        current_bbox_coordinates <- tibble(bbox_top_left_x = current_bbox_top_left_x,
                                           bbox_top_left_y = current_bbox_top_left_y,
                                           bbox_bottom_right_x = current_bbox_bottom_right_x,
                                           bbox_bottom_right_y = current_bbox_bottom_right_y)
        current_bbox_coordinates
}

# create get_bbox_coordinates function
get_bbox_coordinates <- function(bookshelf_words_tbl) {
        
        # get bbox_coordinates
        bbox_coordinates <- map_dfr(.x = bookshelf_words_tbl$bbox, .f = extract_bbox_coordinates)
        
        # bind bbox_coordinates to bookshelf_words_tbl
        bookshelf_words_tbl %>% bind_cols(., bbox_coordinates)
}

# test get_bbox_coordinates
bookshelf_words_tbl <- bookshelf_words
current_bbox <- bookshelf_words_tbl %>% slice(1) %>% select(bbox)
get_bbox_coordinates(bookshelf_words)


##################################################################
##################################################################
##################################################################



# test equivalency of bbox before-after image resizing
# note that "hamilton" gets fairly consistent bbox ratio between image 1 and 2
# but "wind" is less consistent, in part because bottom_right_x and y are off due to rectangle covering blank space
# even still though, "wind" top_left_y is off a bit
bookshelf_output1 %>% filter(row_text == "hamilton")
bookshelf_output1 %>% filter(row_text == "hamilton") %>% draw_bbox(bbox_tbl = ., image_number = 1)
# convert image 1 bbox to approximate image2 bbox and draw on image 2
bookshelf_output1 %>% filter(row_text == "hamilton") %>% 
        mutate_at(.vars = vars(bbox_top_left_x, bbox_bottom_right_x), .funs = funs(. * .67)) %>% 
        mutate_at(.vars = vars(bbox_top_left_y, bbox_bottom_right_y), .funs = funs(. * .43)) %>% 
        draw_bbox(bbox_tbl = ., image_number = 2)

bookshelf_output2 %>% filter(row_text == "hamilton")
bookshelf_output2 %>% filter(row_text == "hamilton") %>% draw_bbox(bbox_tbl = ., image_number = 2)

# compare ratio of bbox coordinates
685 / 1024
254 / 601
891 / 1337
306 / 680

# convert "hamilton" bbox_coordinates from image1 to image2
1024 * .67
601 * .43
1337 * .67
680 * .43

bookshelf_output1 %>% filter(row_text == "wind")
bookshelf_output1 %>% filter(row_text == "wind") %>% draw_bbox(bbox_tbl = ., image_number = 1)
# convert image 1 bbox to approximate image2 bbox and draw on image 2
bookshelf_output1 %>% filter(row_text == "wind") %>% 
        mutate_at(.vars = vars(bbox_top_left_x, bbox_bottom_right_x), .funs = funs(. * .67)) %>% 
        mutate_at(.vars = vars(bbox_top_left_y, bbox_bottom_right_y), .funs = funs(. * .43)) %>% 
        draw_bbox(bbox_tbl = ., image_number = 2)

bookshelf_output2 %>% filter(row_text == "wind")
bookshelf_output2 %>% filter(row_text == "wind") %>% draw_bbox(bbox_tbl = ., image_number = 2)

# compare ratio of bbox coordinates
18 / 27
854 / 1501
174 / 223
906 / 1580

# compare bookshelf_image1 with bookshelf_image4, which has image_resize and image_grayscale only, but not image_trim
# it is convertable at a constant ratio for x and y coord; one coord is off ratio a bit due to larger bbox
# converting image_trim requires addition/subtraction, not scalar by ratio
bookshelf_output1 %>% filter(row_text == "name")
bookshelf_output1 %>% filter(row_text == "name") %>% draw_bbox(bbox_tbl = ., image_number = 1)
# convert image 1 bbox to approximate image2 bbox and draw on image 2
bookshelf_output1 %>% filter(row_text == "name") %>% 
        mutate_at(.vars = vars(bbox_top_left_x, bbox_bottom_right_x), .funs = funs(. * .67)) %>% 
        mutate_at(.vars = vars(bbox_top_left_y, bbox_bottom_right_y), .funs = funs(. * .43)) %>% 
        draw_bbox(bbox_tbl = ., image_number = 2)

bookshelf_output4 %>% filter(row_text == "name")
bookshelf_output4 %>% filter(row_text == "name") %>% draw_bbox(bbox_tbl = ., image_number = 4)

# get ratio
20 / 30
931 / 1396
178 / 220
984 / 1475

# convert image 1 bbox to approximate image2 bbox and draw on image 4
bookshelf_output1 %>% filter(row_text == "name") %>% 
        mutate_at(.vars = vars(bbox_top_left_x, bbox_bottom_right_x), .funs = funs(. * .67)) %>% 
        mutate_at(.vars = vars(bbox_top_left_y, bbox_bottom_right_y), .funs = funs(. * .67)) %>% 
        draw_bbox(bbox_tbl = ., image_number = 4)


#####################


# test conversion
bookshelf_output1
bookshelf_output7

# convert bbox from image7 to image 1
# sincee image 7 has been trimmed though, the bbox coord are relative to the trimmed version
# so we need to add back on the trimmed amount to convert bbox from image7 to image1 coordinates
bookshelf_output7 %>% filter(row_text == "hamilton")
bookshelf_output7 %>% filter(row_text == "hamilton") %>% draw_bbox(bbox_tbl = ., image_number = 7)
bookshelf_output7 %>% filter(row_text == "hamilton") %>% draw_bbox(bbox_tbl = ., image_number = 1)


bookshelf_image1
# format width height colorspace matte filesize density
# 1   JPEG  3000   4000       sRGB  TRUE        0   72x72
bookshelf_output1 %>% filter(row_text == "hamilton") %>% draw_bbox(bbox_tbl = ., image_number = 1)


bookshelf_image7_trimmed
# format width height colorspace matte filesize density
# 1   JPEG  3000   3623       sRGB FALSE        0   72x72

bookshelf_image7
# format width height colorspace matte filesize density
# 1    PNG  2000   2415       sRGB FALSE  3932540 300x300
bookshelf_output7 %>% filter(row_text == "hamilton")
bookshelf_output7_trimmed %>% filter(row_text == "hamilton")

bookshelf_output7 %>% filter(row_text == "hamilton") %>% draw_bbox(bbox_tbl = ., image_number = 1)


##############################################################################


# working test of converting bbox to account for difference of trim() only

# convert bbox from image7_trimmed to plot correctly on image7
# should only require a re-scaling, since only diff btw image7 and image7_trimmed is resize() and grayscale()
# image7_trimmed got trim(), but did not get resize() or grayscale()
dim(bookshelf_output7)
dim(bookshelf_output7_trimmed)

hamilton_7 <- bookshelf_output7 %>% filter(row_text == "hamilton")
hamilton_7 

hamilton_7_trimmed <- bookshelf_output7_trimmed %>% filter(row_text == "hamilton") 
hamilton_7_trimmed

hamilton_7 %>% bind_cols(hamilton_7_trimmed) %>% glimpse()

# get ratio needed to multiply 
rescale_ratio <- hamilton_7_trimmed %>% bind_cols(., hamilton_7) %>% 
        mutate(bbox_top_left_x_ratio = bbox_top_left_x1 / bbox_top_left_x,
               bbox_top_left_y_ratio = bbox_top_left_y1 / bbox_top_left_y,
               bbox_bottom_right_x_ratio = bbox_bottom_right_x1 / bbox_bottom_right_x,
               bbox_bottom_right_y_ratio = bbox_bottom_right_y1 / bbox_bottom_right_y) %>%
        # select(bbox_top_left_x_ratio, bbox_top_left_y_ratio, 
        #       bbox_bottom_right_x_ratio, bbox_bottom_right_y_ratio) %>%
        summarize(avg_ratio = (bbox_top_left_x_ratio + bbox_top_left_y_ratio + 
                                         bbox_bottom_right_x_ratio + bbox_bottom_right_y_ratio) / 4) %>% 
        pull(avg_ratio)
rescale_ratio

# check rescale
hamilton_7_trimmed %>%  
        mutate(bbox_top_left_x_adj = bbox_top_left_x * rescale_ratio,
               bbox_top_left_y_adj = bbox_top_left_y * rescale_ratio,
               bbox_bottom_right_x_adj = bbox_bottom_right_x * rescale_ratio,
               bbox_bottom_right_y_adj = bbox_bottom_right_y * rescale_ratio) %>%
        select(contains("adj"))

hamilton_7

# plot rescale
hamilton_7_trimmed %>%  
        mutate(bbox_top_left_x = bbox_top_left_x * rescale_ratio,
               bbox_top_left_y = bbox_top_left_y * rescale_ratio,
               bbox_bottom_right_x = bbox_bottom_right_x * rescale_ratio,
               bbox_bottom_right_y = bbox_bottom_right_y * rescale_ratio) %>%
        draw_bbox(bbox_tbl = ., image_number = 7)

bookshelf_image7_trimmed


#################################################################################


# convert bbox from image1 (no pre-processing) to image3 (only image_trim(fuzz = 40), and grayscale())

# inspect
bookshelf_image1
image_info(bookshelf_image1)

bookshelf_image3
image_info(bookshelf_image3)

bookshelf_output1 %>% select(row_text) %>% data.frame()
bookshelf_output3 %>% select(row_text) %>% data.frame()

# in order to know how many pixels trimmed from top and left sides, need to have mutual reference point
# to do this, overlay reference text in center of each image, perform any image_trim(),
# then find coordinates to reference text
# change in x coords should give number of pixels trimmed from left, change in y gives pixels trimmed from top

# get arguments for image_composite
reference_width <- image_info(bookshelf_image1) %>% mutate(reference_width = width * .1) %>% pull(reference_width)
x_offset <- image_info(bookshelf_image1) %>% mutate(x_offset = width * .3) %>% pull(x_offset)
y_offset <- image_info(bookshelf_image1) %>% mutate(y_offset = height * .5) %>% pull(y_offset)
offset <- geometry_point(x = x_offset, y = y_offset)

# get reference image (image_annotate didn't seem to have enough border for good ocr)
reference_plot <- starwars %>% ggplot(data = ., aes(x = mass)) + geom_histogram() + ggtitle("REF 123") 
reference_plot
ggsave(filename = "reference_plot.png", plot = reference_plot)
image_read(path = "reference_plot.png") %>% 
        image_crop(image = ., geometry = geometry_area(width = 260, height = 75, x_off = 100, y_off = 10)) %>%
        image_write(image = ., path = "reference_image.png")
reference_image <- image_read(path = "reference_image.png")
reference_image %>% image_info()
reference_image <- reference_image %>% 
        image_resize(image = ., geometry = geometry_size_pixels(width = reference_width, height = reference_width))

# get ocr
bookshelf_image1_reference_ocr <- bookshelf_image1 %>% 
        image_composite(image = ., composite_image = reference_image, offset = offset) %>% ocr(.)

# get ocr_data
bookshelf_image1_reference_ocr_data <- bookshelf_image1 %>% 
        image_composite(image = ., composite_image = reference_image, offset = offset) %>% ocr_data(.)
bookshelf_image1_reference_ocr_data %>% filter(str_detect(string = word, pattern = "123"))
bookshelf_image1_reference_ocr_data %>% data.frame()

bookshelf_image1 %>% 
        image_convert(type = 'Grayscale') %>%
        image_trim(fuzz = 40) 

# get row for "name"
hamilton1 <- bookshelf_output1 %>% filter(row_text == "hamilton")
hamilton1

hamilton3 <- bookshelf_output3 %>% filter(row_text == "hamilton")
hamilton3

# convert image1 bbox to display on image4


#################################################################################3

# test bbox
bookshelf_image_draw <- image_draw(bookshelf_image1)
rect(1000, 1000, 3000, 4000, 
     border = "red", lty = "dashed", lwd = 10)
dev.off()
print(bookshelf_image_draw)


