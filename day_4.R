library(purrr)
library(dplyr)
library(magrittr)
library(stringr)

break_up <- function(password){
    password <- as.character(password)
    unlist(strsplit(password, ''))
}
has_double <- function(password){
    str_detect(string = password, pattern = '(\\d)\\1{1}')    
}

# has_double <- function(password){
    
#     single_digits <- map(password, break_up)
#     map_lgl(single_digits, 
#       function(z){
#           any(pmap_lgl(data.frame(x = 1:(length(z) - 1), 
#                                   y = 2:length(z)),
#               ~ z[.x] == z[.y]))
#       }
#     )
# }

has_exclusive_double <- function(password) {
    
    single_digits <- map(password, break_up)

}

monotonic <- function(password) {
    single_digits <- map(password, break_up)
    map_lgl(single_digits, function(z){
        identical(z, sort(z))
    })
}

possible_passwords <- data.frame(passwords = seq(372037L, 905157L, 1L))

# separate filter steps because monotonic is the faster check and eliminates
# most options
possible_passwords %>%
filter(monotonic(passwords)) %>%
filter(has_double_reg(passwords))

has_double_not_trip <- function(password){
    map_lgl(password, function(x) {
          str_extract_all(x, '(\\d)\\1{1,}') %>% 
          unlist() %>% 
          str_count() %>%
          `%in%`(2) %>%
          any()
      }
    )
}


possible_passwords %>%
filter(monotonic(passwords)) %>%
filter(has_double_reg(passwords)) %>%
filter(has_double_not_trip(passwords))