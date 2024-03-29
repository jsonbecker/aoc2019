run_program <- function(memory_data, output, current_position) {
  posop <- memory_data[current_position]
  if (str_count(posop) <= 2) {
      op <- posop
      pos <- '000'
  } else {
      op <- as.numeric(substring(posop, str_length(posop) - 1, str_length(posop)))
      pos <- str_pad(substring(posop, 1, str_length(posop) - 2), 3, 'left', 0)
  }
  if (op == 99) {
      #print(memory_data)
      return(output)
  }
  if (op %in% c(1,2,7,8)){
      x <- memory_data[current_position + 1]
      y <- memory_data[current_position + 2]
      where <- memory_data[current_position + 3]
  } else if (op == 3){
      x <- readline(prompt = 'User input:')
      where <- memory_data[current_position + 1]
  } else if (op == 4) {
      # the first plus one is to shift to where the out put position is relative to
      # the 4 op code. The second + 1 is because R indexes at 1.
      output <- append(output, memory_data[memory_data[current_position + 1] + 1])
  } else if (op %in% c(5,6)){
      x <- memory_data[current_position + 1]
      y <- memory_data[current_position + 2]
  } else {
      stop('fuck off')
  }
  position_params <- str_split_fixed(stringi::stri_reverse(pos), '', 3)
  if(op %in% c(1, 2, 5, 6, 7, 8)) {
    if(position_params[1]==0){
        x <- memory_data[x + 1]
    }
    if(position_params[2]==0) {
        y <- memory_data[y + 1]
    }
  }
  if (op == 1){
      result <- x + y
      memory_data[where + 1] <- result
      current_position <- current_position + 4
  } else if (op == 2) {
      result <- x * y
      memory_data[where + 1] <- result
      current_position <- current_position + 4
  } else if (op == 3) {
      memory_data[where + 1] <- x
      current_position <- current_position + 2
  } else if (op == 4) {
      current_position <- current_position + 2
  } else if (op == 5) {
      if (x != 0) {
          current_position <- y + 1
      } else {
          current_position <- current_position + 3
      }
  } else if (op == 6){
      if (x == 0){
          current_position <- y + 1
      } else {
          current_position <- current_position + 3
      }
  } else if (op == 7) {
      if (x < y){
          memory_data[where + 1] <- 1
      } else {
          memory_data [where + 1] <- 0
      }
      current_position <- current_position + 4
  } else if (op == 8){
      if (x == y) {
          memory_data[where + 1] <- 1
      } else {
          memory_data [where + 1] <- 0
      }
      current_position <- current_position + 4
  } else {
      stop('fuck off')
  }
  print(memory_data)
  print('\n\n\n')
  print(current_position)
  run_program(as.numeric(memory_data), output, current_position)
}
initial_memory <- c(3,225,1,225,6,6,1100,1,238,225,104,0,1102,31,68,225,1001,13,87,224,1001,224,-118,224,4,224,102,8,223,223,1001,224,7,224,1,223,224,223,1,174,110,224,1001,224,-46,224,4,224,102,8,223,223,101,2,224,224,1,223,224,223,1101,13,60,224,101,-73,224,224,4,224,102,8,223,223,101,6,224,224,1,224,223,223,1101,87,72,225,101,47,84,224,101,-119,224,224,4,224,1002,223,8,223,1001,224,6,224,1,223,224,223,1101,76,31,225,1102,60,43,225,1102,45,31,225,1102,63,9,225,2,170,122,224,1001,224,-486,224,4,224,102,8,223,223,101,2,224,224,1,223,224,223,1102,29,17,224,101,-493,224,224,4,224,102,8,223,223,101,1,224,224,1,223,224,223,1102,52,54,225,1102,27,15,225,102,26,113,224,1001,224,-1560,224,4,224,102,8,223,223,101,7,224,224,1,223,224,223,1002,117,81,224,101,-3645,224,224,4,224,1002,223,8,223,101,6,224,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,8,226,677,224,102,2,223,223,1005,224,329,1001,223,1,223,1108,677,226,224,102,2,223,223,1006,224,344,101,1,223,223,108,677,226,224,102,2,223,223,1006,224,359,101,1,223,223,7,677,226,224,102,2,223,223,1005,224,374,101,1,223,223,1007,226,677,224,102,2,223,223,1005,224,389,101,1,223,223,8,677,677,224,102,2,223,223,1006,224,404,1001,223,1,223,1007,677,677,224,1002,223,2,223,1006,224,419,101,1,223,223,1108,677,677,224,1002,223,2,223,1005,224,434,1001,223,1,223,1107,226,677,224,102,2,223,223,1005,224,449,101,1,223,223,107,226,226,224,102,2,223,223,1006,224,464,101,1,223,223,1108,226,677,224,1002,223,2,223,1005,224,479,1001,223,1,223,7,677,677,224,102,2,223,223,1006,224,494,1001,223,1,223,1107,677,226,224,102,2,223,223,1005,224,509,101,1,223,223,107,677,677,224,1002,223,2,223,1006,224,524,101,1,223,223,1008,677,677,224,1002,223,2,223,1006,224,539,101,1,223,223,7,226,677,224,1002,223,2,223,1005,224,554,101,1,223,223,108,226,226,224,1002,223,2,223,1006,224,569,101,1,223,223,1008,226,677,224,102,2,223,223,1005,224,584,101,1,223,223,8,677,226,224,1002,223,2,223,1005,224,599,101,1,223,223,1007,226,226,224,1002,223,2,223,1005,224,614,101,1,223,223,1107,226,226,224,1002,223,2,223,1006,224,629,101,1,223,223,107,677,226,224,1002,223,2,223,1005,224,644,1001,223,1,223,1008,226,226,224,1002,223,2,223,1006,224,659,101,1,223,223,108,677,677,224,1002,223,2,223,1005,224,674,1001,223,1,223,4,223,99,226)

run_program(initial_memory, output = c(), current_position = 1)

