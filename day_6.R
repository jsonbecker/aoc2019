library(readr)
library(purrr)
orbits <- read_delim('day_6_input.txt', delim = ')', col_names = FALSE)
names(orbits) <- c('parent','child')

satellites <- unique(c(orbits$parent, orbits$child))

count_all_orbits <- function(sattelite, orbits, orbit_count = 0) {
    if (sattelite == 'COM') {
      return(orbit_count)
    }
    orbit_count <- orbit_count + 1
    next_sattelite <- orbits[orbits[['child']] == sattelite,]$parent
    count_all_orbits(next_sattelite, orbits, orbit_count)
}

reduce(map_dbl(satellites, ~ count_all_orbits(.x, orbits)), sum)

orbit_path <- function(sattelite, orbits, path = NA) {
    if (!is.na(path)){
      path <- paste(path, sattelite, sep = '-') 
    } else {
      path <- sattelite
    }
    if (sattelite == 'COM') {
        return(path)
    }
    next_sattelite <- orbits[orbits[['child']] == sattelite,]$parent
    orbit_path(next_sattelite, orbits, path)
}

closest_common_sattelite <- function(path1, path2) {
    path1 <- unlist(stringr::str_split(path1, '-'))
    path2 <- unlist(stringr::str_split(path2, '-'))
    shortest <- if (length(path1) < length(path2)) {
         path1
    } else {
         path2
    }
    longest <- if (length(path1) > length(path2)) {
        path1
    } else {
        path2
    }
    common_path <- map_lgl(shortest, ~.x %in% longest)
    shortest[which.max(common_path)]
}

lcd <- closest_common_sattelite(orbit_path('YOU', orbits), 
                                orbit_path('SAN', orbits))

# -2 at the end because not between YOU and SAN but between what YOU
# orbits and what SAN orbits, reducing the trips by from each.
(count_all_orbits('YOU',orbits) - count_all_orbits(lcd, orbits)) + 
(count_all_orbits('SAN', orbits) - count_all_orbits(lcd, orbits)) - 2