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

omnibook_transforms <- read.csv("data-raw/omnibook_transforms.csv", sep = ";", stringsAsFactors = F) %>%
  as_tibble() %>%
    mutate(pos = 1:nrow(.))
wjd_transforms <- read.csv("data-raw/wjd_transforms.csv", sep = ";", stringsAsFactors = F) %>% as_tibble()
#wjd_meta <- read.csv("data-raw/wjd_metadata.csv", sep = ";", stringsAsFactors = F) %>% as_tibble()
esac_transforms <- read.csv("data-raw/esac_transforms.csv", sep = ";", stringsAsFactors = F) %>% as_tibble()
#esac_meta <- read.csv("data-raw/esac_meta.csv", sep = ";", stringsAsFactors = F) %>% as_tibble()
usethis::use_data(omnibook_transforms, overwrite = TRUE)
usethis::use_data(wjd_transforms, overwrite = TRUE)
usethis::use_data(esac_transforms, overwrite = TRUE)


omnibook_meta <- read.csv("data-raw/omnibook_meta.csv", sep = ";", stringsAsFactors = F) %>%
  as_tibble() %>%
  mutate(pos = 1:nrow(.))
wjd_meta <- read.csv("data-raw/wjd_meta.csv", sep = ";", stringsAsFactors = F) %>% as_tibble()
esac_meta <- read.csv("data-raw/esac_meta.csv", sep = ";", stringsAsFactors = F) %>% as_tibble()
#esac_meta <- read.csv("data-raw/esac_meta.csv", sep = ";", stringsAsFactors = F) %>% as_tibble()
usethis::use_data(omnibook_meta, overwrite = TRUE)
usethis::use_data(wjd_meta, overwrite = TRUE)
usethis::use_data(esac_meta, overwrite = TRUE)

