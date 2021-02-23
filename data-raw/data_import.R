F_blues <- tibble(chord = c("F7","Bb7", "F7",  "Cmin7", "F7", "Bb7", "Bb7", "F7", "F7", "Gmin7", "C7", "F7", "F7"),
                length_beats = as.integer(c(4, 4, 4, 2, 2, 4, 4, 4, 4, 4, 4, 4, 4)))
F_blues <- F_blues %>%
  mutate(length_ticks = length_beats * 4) %>%
  mutate(parsed = map(chord, parse_chord)) %>%
  sollogenerator:::set_format("lead_sheet")

usethis::use_data(F_blues, overwrite = TRUE)
