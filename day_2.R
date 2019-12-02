# This input has the values in 1 and 2 replaced as needed to solve part 1.
input <- c(1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,19,5,23,2,9,23,27,1,5,27,31,1,5,31,35,1,35,13,39,1,39,9,43,1,5,43,47,1,47,6,51,1,51,13,55,1,55,9,59,1,59,13,63,2,63,13,67,1,67,10,71,1,71,6,75,2,10,75,79,2,10,79,83,1,5,83,87,2,6,87,91,1,91,6,95,1,95,13,99,2,99,13,103,1,103,9,107,1,10,107,111,2,111,13,115,1,10,115,119,1,10,119,123,2,13,123,127,2,6,127,131,1,13,131,135,1,135,2,139,1,139,6,0,99,2,0,14,0)


step_function <- function(input) {
  for (i in seq(1, length(input), 4)) {
    op <- input[i]
    if (op == 99 | i == length(input)) {
      return(input)
    }
    # fucking index 1
    x_pos <- input[i + 1] + 1 
    y_pos <- input[i + 2] + 1
    res_pos <- input[i + 3] + 1
    result <- ifelse(op == 1, 
                     input[x_pos] + input[y_pos],
                     ifelse(op == 2,
                            input[x_pos] * input[y_pos], NA))
    input[res_pos] <- result
  }
  input
}

# I don't want to refactor for their terminology quite yet, so here's
# my part 2 process.

# First, provide a fresh input:
library(magrittr)
library(purrr)
initial_memory_state <- c(1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,19,5,23,2,9,23,27,1,5,27,31,1,5,31,35,1,35,13,39,1,39,9,43,1,5,43,47,1,47,6,51,1,51,13,55,1,55,9,59,1,59,13,63,2,63,13,67,1,67,10,71,1,71,6,75,2,10,75,79,2,10,79,83,1,5,83,87,2,6,87,91,1,91,6,95,1,95,13,99,2,99,13,103,1,103,9,107,1,10,107,111,2,111,13,115,1,10,115,119,1,10,119,123,2,13,123,127,2,6,127,131,1,13,131,135,1,135,2,139,1,139,6,0,99,2,0,14,0)

# Just calculate all possible results, because I'm lazy.
possible_parameters <- expand.grid(noun = seq(0L,99L), verb = seq(0L,99L))

pmap_dbl(possible_parameters, function(noun,verb) {
   initial_memory_state[2] <- noun
   initial_memory_state[3] <- verb 
   step_function(initial_memory_state)[1]
  }
) %>%
bind_cols(possible_parameters, results = .) %>%
filter(results == 19690720) %>%
mutate(answer = 100 * noun + verb) %$%
answer 

