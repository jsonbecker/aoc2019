library(stringr)
library(purrr)
library(magrittr)
image <- readLines('day_8.txt')

calculate_area <- function(width, height) {
    width * height
}

image_layer_size <- calculate_area(25, 6)

image_into_layers <- unlist(str_extract_all(image, paste0('.{1,', image_layer_size,'}')))

count_of_value <- function(layer, pattern) {
    length(unlist(str_match_all(layer, pattern)))
}

number_of_0s <- map_int(image_into_layers, count_of_value, '0')
least_0s_layer <- image_into_layers[which.min(number_of_0s)]
count_of_value(least_0s_layer, '1') * count_of_value(least_0s_layer, '2')

image_layers <- map(image_into_layers, strsplit, '') %>%
                map(unlist) %>%
                map(matrix, nrow = 6, ncol = 25) %>%
                map(unlist)

# end state
all_0s_1s <- function(MAT) {
  v <- as.vector(MAT)
  all(v %in% c("0", "1"))
}

image <- matrix(2, nrow = 25, ncol = 6)
map(image_layers, function(x) {
    which(as.vector(x) %in% c("0"))
}

build_image <- function(layers, final_image, layer_number = 1) {
     if (all_0s_1s(final_image)) {
         return(final_image)
     }
     layer <- image_into_layers[layer_number]
     layer <- layer %>% strsplit('') %>% unlist()
     transparent_positions <- which(!final_image %in% c('0','1'))
     color_in_this_layer_positions <- which(layer %in% c('0','1'))
     new_pixels_positions <- intersect(transparent_positions, color_in_this_layer_positions)
     final_image[new_pixels_positions] <- layer[new_pixels_positions]
     layer_number <- layer_number + 1
     build_image(layers, final_image, layer_number)
}

build_image(image_into_layers, rep(2, 25*6))