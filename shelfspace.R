library(tesseract)
library(magick)
library(dplyr)
library(pdftools)
library(dplyr)
library(stringr)
library(tidytext)
library(purrr)
library(rlang)
library(tidyr)
library(stringi)
library(hunspell)
library(ggplot2)
library(readr)

# https://cran.r-project.org/web/packages/tesseract/vignettes/intro.html#preprocessing_with_magick
# http://kba.cloud/hocr-spec/1.2/  # hOCR format

setwd("C:/Users/Stephen/Desktop/R/shelfspace")
list.files()


# create bookshelf_image
bookshelf_image1 <- image_read("bookshelf.jpg")
print(bookshelf_image1)

# create bookshelf_image2
image_read("bookshelf.jpg") %>% image_resize("2000x") %>%
        image_convert(type = 'Grayscale') %>%
        image_trim(fuzz = 40) %>%
        image_write(image = ., path = "bookshelf_image2.png", format = 'png', density = '300x300')

bookshelf_image2 <- image_read("bookshelf_image2.png")
print(bookshelf_image2)

# create bookshelf_image3 without image_resize
image_read("bookshelf.jpg") %>% 
        image_convert(type = 'Grayscale') %>%
        image_trim(fuzz = 40) %>%
        image_write(image = ., path = "bookshelf_image3.png", format = 'png', density = '300x300')

bookshelf_image3 <- image_read("bookshelf_image3.png")
print(bookshelf_image3)

# create bookshelf_image4 without fuzz
image_read("bookshelf.jpg") %>% image_resize("2000x") %>%
        image_convert(type = 'Grayscale') %>%
        image_write(image = ., path = "bookshelf_image4.png", format = 'png', density = '300x300')

bookshelf_image4 <- image_read("bookshelf_image4.png")
print(bookshelf_image4)

# create bookshelf_image5 without resize or fuzz
image_read("bookshelf.jpg") %>% 
        image_convert(type = 'Grayscale') %>%
        image_write(image = ., path = "bookshelf_image5.png", format = 'png', density = '300x300')

bookshelf_image5 <- image_read("bookshelf_image5.png")
print(bookshelf_image5)

# create bookshelf_image6 without resize or grayscale
image_read("bookshelf.jpg") %>% 
        image_trim(fuzz = 40) %>%
        image_write(image = ., path = "bookshelf_image6.png", format = 'png', density = '300x300')

bookshelf_image6 <- image_read("bookshelf_image6.png")
print(bookshelf_image6)

# create bookshelf_image7 which is same as image2, but image_trim is done first to measure trim, then resize and gray
bookshelf_image7_trimmed <- image_read("bookshelf.jpg") %>% image_trim(fuzz = 40)
bookshelf_image7_trimmed
bookshelf_image7_trimmed %>% image_resize("2000x") %>%
        image_convert(type = 'Grayscale') %>%
        image_write(image = ., path = "bookshelf_image7.png", format = 'png', density = '300x300')

bookshelf_image7 <- image_read("bookshelf_image7.png")
print(bookshelf_image7)

# create bookshelf_image6 without resize or grayscale
image_read("bookshelf.jpg") %>% 
        image_resize("5000x") %>% image_convert(type = "Grayscale") %>%
        image_write(image = ., path = "bookshelf_image8.png", format = 'png', density = '300x300')

bookshelf_image8 <- image_read("bookshelf_image8.png")
print(bookshelf_image8)


##################################################################33


# bookshelf_ocr_text
# bookshelf_text <- ocr("test_docs/bookshelf.jpg", engine = tesseract("eng"))
bookshelf_text1 <- bookshelf_image1 %>% ocr(.)
bookshelf_text1
print(bookshelf_text1)
cat(bookshelf_text1)

# bookshelf_text2
bookshelf_text2 <- bookshelf_image2 %>% ocr(.)
bookshelf_text2
print(bookshelf_text2)
cat(bookshelf_text2)

# bookshelf_text3
bookshelf_text3 <- bookshelf_image3 %>% ocr(.)
bookshelf_text3
print(bookshelf_text3)
cat(bookshelf_text3)

# bookshelf_text4
bookshelf_text4 <- bookshelf_image4 %>% ocr(.)
bookshelf_text4
print(bookshelf_text4)
cat(bookshelf_text4)

# bookshelf_text5
bookshelf_text5 <- bookshelf_image5 %>% ocr(.)
bookshelf_text5
print(bookshelf_text5)
cat(bookshelf_text5)

# bookshelf_text6
bookshelf_text6 <- bookshelf_image6 %>% ocr(.)
bookshelf_text6
print(bookshelf_text6)
cat(bookshelf_text6)

# bookshelf_text7
bookshelf_text7 <- bookshelf_image7 %>% ocr(.)
bookshelf_text7
print(bookshelf_text7)
cat(bookshelf_text7)

# bookshelf_text7_trimmed
bookshelf_text7_trimmed <- bookshelf_image7_trimmed %>% ocr(.)
bookshelf_text7_trimmed
print(bookshelf_text7_trimmed)
cat(bookshelf_text7_trimmed)

# bookshelf_text7
bookshelf_text8 <- bookshelf_image8 %>% ocr(.)
bookshelf_text8
print(bookshelf_text8)
cat(bookshelf_text8)

# write example file
tibble(text = bookshelf_text1) %>% write_csv(., path = "bookshelf_text1.csv")


################################################################


# get bookshelf_ocr data
# bookshelf_ocr <- ocr_data("test_docs/bookshelf.jpg", tesseract("eng"))
bookshelf_ocr1 <- bookshelf_image1 %>% ocr_data(.)
bookshelf_ocr2 <- bookshelf_image2 %>% ocr_data(.)
bookshelf_ocr3 <- bookshelf_image3 %>% ocr_data(.) 
bookshelf_ocr4 <- bookshelf_image4 %>% ocr_data(.) 
bookshelf_ocr5 <- bookshelf_image5 %>% ocr_data(.) 
bookshelf_ocr6 <- bookshelf_image6 %>% ocr_data(.) 
bookshelf_ocr7 <- bookshelf_image7 %>% ocr_data(.) 
bookshelf_ocr7_trimmed <- bookshelf_image7_trimmed %>% ocr_data(.) 
bookshelf_ocr8 <- bookshelf_image8 %>% ocr_data(.) 



# inspect
bookshelf_ocr1 %>% arrange(desc(confidence)) %>% slice(1:100) %>% data.frame()
bookshelf_ocr2 %>% arrange(desc(confidence)) %>% slice(1:100) %>% data.frame()
bookshelf_ocr3 %>% arrange(desc(confidence)) %>% slice(1:100) %>% data.frame()
bookshelf_ocr4 %>% arrange(desc(confidence)) %>% slice(1:100) %>% data.frame()
bookshelf_ocr5 %>% arrange(desc(confidence)) %>% slice(1:100) %>% data.frame()
bookshelf_ocr6 %>% arrange(desc(confidence)) %>% slice(1:100) %>% data.frame()
bookshelf_ocr7 %>% arrange(desc(confidence)) %>% slice(1:100) %>% data.frame()
bookshelf_ocr8 %>% arrange(desc(confidence)) %>% slice(1:100) %>% data.frame()

# write example file
write_csv(bookshelf_ocr1, "bookshelf_ocr1.csv")


##########################################################################
##########################################################################
##########################################################################


# load get_bookshelf_words function
source("get_bookshelf_words.R")
source("draw_bbox.R")

# inspect variations in image pre-processing
# 1, 2, and 5 seem most promising (1 = no processing, 2 = resizing, fuzz, grayscale, 5 = just grayscale)
bookshelf_output1 <- get_bookshelf_words(bookshelf_text = bookshelf_text1, bookshelf_ocr_data = bookshelf_ocr1)
bookshelf_output1 %>% select(row_text, row_id, word) %>% data.frame()
# write_csv(bookshelf_output1, "bookshelf_output1.csv")

bookshelf_output2 <- get_bookshelf_words(bookshelf_text = bookshelf_text2, bookshelf_ocr_data = bookshelf_ocr2)
bookshelf_output2 %>% select(row_text, row_id, word) %>% data.frame()
# write_csv(bookshelf_output2, "bookshelf_output2.csv")

bookshelf_output3 <- get_bookshelf_words(bookshelf_text = bookshelf_text3, bookshelf_ocr_data = bookshelf_ocr3)
bookshelf_output3 %>% select(row_text, row_id, word) %>% data.frame()
# write_csv(bookshelf_output3, "bookshelf_output3.csv")

bookshelf_output4 <- get_bookshelf_words(bookshelf_text = bookshelf_text4, bookshelf_ocr_data = bookshelf_ocr4)
bookshelf_output4 %>% select(row_text, row_id, word) %>% data.frame()
# write_csv(bookshelf_output4, "bookshelf_output4.csv")

bookshelf_output5 <- get_bookshelf_words(bookshelf_text = bookshelf_text5, bookshelf_ocr_data = bookshelf_ocr5)
bookshelf_output5 %>% select(row_text, row_id, word) %>% data.frame()
# write_csv(bookshelf_output5, "bookshelf_output5.csv")

bookshelf_output6 <- get_bookshelf_words(bookshelf_text = bookshelf_text6, bookshelf_ocr_data = bookshelf_ocr6)
bookshelf_output6 %>% select(row_text, row_id, word) %>% data.frame()
# write_csv(bookshelf_output6, "bookshelf_output6.csv")

bookshelf_output7 <- get_bookshelf_words(bookshelf_text = bookshelf_text7, bookshelf_ocr_data = bookshelf_ocr7)
bookshelf_output7 %>% select(row_text, row_id, word) %>% data.frame()
# write_csv(bookshelf_output7, "bookshelf_output7.csv")

bookshelf_output7_trimmed <- get_bookshelf_words(bookshelf_text = bookshelf_text7_trimmed, 
                                                 bookshelf_ocr_data = bookshelf_ocr7_trimmed)
bookshelf_output7_trimmed %>% select(row_text, row_id, word) %>% data.frame()
# write_csv(bookshelf_output7_trimmed, "bookshelf_output7_trimmed.csv")

bookshelf_output8 <- get_bookshelf_words(bookshelf_text = bookshelf_text8, bookshelf_ocr_data = bookshelf_ocr8)
bookshelf_output8 %>% select(row_text, row_id, word) %>% data.frame()
write_csv(bookshelf_output8, "bookshelf_output8.csv")


####################################################################


# read in bookshelf_output
# bookshelf_output1 <- read_csv("bookshelf_output1.csv")
# bookshelf_output2 <- read_csv("bookshelf_output2.csv")
# bookshelf_output3 <- read_csv("bookshelf_output3.csv")
# bookshelf_output4 <- read_csv("bookshelf_output4.csv")
# bookshelf_output5 <- read_csv("bookshelf_output5.csv")
# bookshelf_output6 <- read_csv("bookshelf_output6.csv")
# bookshelf_output7 <- read_csv("bookshelf_output7.csv")
# bookshelf_output7_trimmed <- read_csv("bookshelf_output7_trimmed.csv")


###################################################################
###################################################################
##################################################################


# group bookshelf_words into bookshelf_terms (including alternates)

source("draw_bbox.R")

# inspect row_id frequency (not very useful)
bookshelf_output1 %>% ggplot(aes(x = row_id)) + geom_bar()

# create get_bookshelf_titles function
bookshelf_output1

get_bookshelf_titles <- function(grouping_var, data) {
        
        # get current_row_tbl
        current_row_tbl <- data
        
        # concatenate to get current_term
        current_term <- current_row_tbl %>% arrange(bbox_top_left_x) %>% pull(row_text) %>% str_c(string = ., collapse = " ")
        
        # get new bbox for current_term
        current_bbox_top_left_x <- current_row_tbl %>% arrange(bbox_top_left_x) %>% slice(1) %>%
                pull(bbox_top_left_x)
        current_bbox_top_left_y <- current_row_tbl %>% arrange(bbox_top_left_y) %>% slice(1) %>%
                pull(bbox_top_left_y)
        current_bbox_bottom_right_x <- current_row_tbl %>% arrange(desc(bbox_bottom_right_x)) %>% slice(1) %>%
                pull(bbox_bottom_right_x)
        current_bbox_bottom_right_y <- current_row_tbl %>% arrange(desc(bbox_bottom_right_y)) %>% slice(1) %>%
                pull(bbox_bottom_right_y)
        
        # series pattern for how many alternate titles resulting from original term count
        # 2 terms = 1 alternate, 3 terms = 3 alternates, 4 terms = 6 alternates, 5 terms = 10 alternates, etc
        # #alternates(x) for term-count x = x-1 + #alternates(x-1)
}

bookshelf_output1 %>% mutate(grouping_var = row_id) %>% group_by(grouping_var) %>% nest() %>%
        map(.x = ., .f = get_bookshelf_titles)

# test get_bookshelf_titles
current_row_tbl <- bookshelf_output1 %>% filter(row_id == 19)
current_row_tbl



######################################################################


# create alternate_row_groups

