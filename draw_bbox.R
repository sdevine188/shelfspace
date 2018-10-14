# create draw_bbox function
draw_bbox <- function(bbox_tbl, image_number) {
        
        # assign bookshelf_image
        bookshelf_image <- str_c("bookshelf_image", image_number)
        bookshelf_image <- eval(parse(text = bookshelf_image))
        
        # draw rectangle and print image
        bookshelf_image_draw <- image_draw(bookshelf_image)
        rect(bbox_tbl$bbox_top_left_x[1], bbox_tbl$bbox_top_left_y[1], bbox_tbl$bbox_bottom_right_x[1], 
             bbox_tbl$bbox_bottom_right_y[1], border = "red", lty = "dashed", lwd = 5)
        dev.off()
        print(bookshelf_image_draw) 
}

# test
# bookshelf_output1 %>% filter(row_text == "hamilton") %>% draw_bbox(bbox_tbl = ., image_number = 1)
# bookshelf_output2 %>% filter(row_text == "hamilton") %>% draw_bbox(bbox_tbl = ., image_number = 2)
# bookshelf_output3 %>% filter(row_text == "hamilton") %>% draw_bbox(bbox_tbl = ., image_number = 3)
