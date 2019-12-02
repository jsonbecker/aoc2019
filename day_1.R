# Note: I tried to use httr::GET and url to source
# my input but it's unique per user and I didn't
# want to take the time to figure out how to do the
# auth, so I have taken my input and pasted it here.
# Reminder: vertical cursors in vim Ctrl + V for
# visual block, highlight, then I, type in line 1
# what you want, then enter normal mode.

module_masses <- c(
   139301
  ,84565
  ,124180
  ,133902
  ,138726
  ,62665
  ,142967
  ,95598
  ,118044
  ,73234
  ,76476
  ,51634
  ,71582
  ,63619
  ,148430
  ,134733
  ,101537
  ,101140
  ,144543
  ,102233
  ,62048
  ,128633
  ,130113
  ,92531
  ,73820
  ,54964
  ,103485
  ,96364
  ,104119
  ,121954
  ,79215
  ,99235
  ,120179
  ,69237
  ,145584
  ,79193
  ,50684
  ,146481
  ,67783
  ,112741
  ,85024
  ,62298
  ,54083
  ,137704
  ,116561
  ,76862
  ,81410
  ,96341
  ,89992
  ,132926
  ,97955
  ,74751
  ,147553
  ,121496
  ,113303
  ,119671
  ,120871
  ,114278
  ,125628
  ,144275
  ,78826
  ,87092
  ,65883
  ,87517
  ,93974
  ,55358
  ,100922
  ,113304
  ,115728
  ,144556
  ,91728
  ,86367
  ,55283
  ,101841
  ,55454
  ,140703
  ,70706
  ,98173
  ,106920
  ,126984
  ,148960
  ,77909
  ,128304
  ,140036
  ,81044
  ,141419
  ,126770
  ,52787
  ,115783
  ,128647
  ,125986
  ,124506
  ,113935
  ,142203
  ,106404
  ,78433
  ,146573
  ,68575
  ,63563
  ,115616)

fuel_requirements <- function(module_mass) {
  floor(module_mass / 3) - 2 
}

sum(fuel_requirements(module_masses)))

# Part 2 requires recursion, but R being vectorized makes
# this a bit weird.
library(purrr)

fuel_requirements_recursive <- function(module_mass) {
  # first case
  fuel <- keep(fuel_requirements(module_mass), ~ .x > 0)
  # set up accumulator
  level_fuel <- fuel
  while (length(level_fuel) > 0) {
    level_fuel <- keep(fuel_requirements(level_fuel), ~ .x > 0)
    fuel <- append(fuel, level_fuel)
  }
  reduce(fuel, sum)
}

fuel_requirements_recursive(module_masses)

