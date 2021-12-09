library(tidyverse)
F_blues <- tibble(chord = c("F7","Bb7", "F7",  "Cmin7", "F7", "Bb7", "Bb7", "F7", "F7", "Gmin7", "C7", "F7", "F7"),
                length_beats = as.integer(c(4, 4, 4, 2, 2, 4, 4, 4, 4, 4, 4, 4, 4)))
F_blues <- F_blues %>% mutate(length_ticks = length_beats*4) %>%
  mutate(parsed = map(chord, parkR::parse_chord)) %>%
  tidyr::unnest(cols = parsed)
F_blues$onset_ticks <- cumsum(F_blues$length_ticks)
F_blues$onset_ticks <- F_blues$onset_ticks - F_blues$onset_ticks[1]
F_blues$beat <- (floor(F_blues$onset_ticks/4) %%4 ) + 1
F_blues$bar <- floor(F_blues$onset_ticks/16) + 1

usethis::use_data(F_blues, overwrite = TRUE)
