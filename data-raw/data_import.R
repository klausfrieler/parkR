library(tidyverse)
# omnibook_transforms <- read.csv("data-raw/omnibook_transforms.csv", sep = ";", stringsAsFactors = F) %>%
#   as_tibble() %>%
#     mutate(pos = 1:nrow(.))
# wjd_transforms <- read.csv("data-raw/wjd_transforms.csv", sep = ";", stringsAsFactors = F) %>% as_tibble()
# #wjd_meta <- read.csv("data-raw/wjd_metadata.csv", sep = ";", stringsAsFactors = F) %>% as_tibble()
# esac_transforms <- read.csv("data-raw/esac_transforms.csv", sep = ";", stringsAsFactors = F) %>%
#   as_tibble() %>%
#   select(-chords_raw) %>%
#   select(id, everything())
# #esac_meta <- read.csv("data-raw/esac_meta.csv", sep = ";", stringsAsFactors = F) %>% as_tibble()
# usethis::use_data(omnibook_transforms, overwrite = TRUE)
# usethis::use_data(wjd_transforms, overwrite = TRUE)
# usethis::use_data(esac_transforms, overwrite = TRUE)
#
#
# omnibook_meta <- read.csv("data-raw/omnibook_meta.csv", sep = ";", stringsAsFactors = F) %>%
#   as_tibble() %>%
#   mutate(pos = 1:nrow(.))
# wjd_meta <- read.csv("data-raw/wjd_meta.csv", sep = ";", stringsAsFactors = F) %>% as_tibble()
# esac_meta <- read.csv("data-raw/esac_meta.csv", sep = ";", stringsAsFactors = F) %>% as_tibble()
# #esac_meta <- read.csv("data-raw/esac_meta.csv", sep = ";", stringsAsFactors = F) %>% as_tibble()
# usethis::use_data(omnibook_meta, overwrite = TRUE)
# usethis::use_data(wjd_meta, overwrite = TRUE)
# usethis::use_data(esac_meta, overwrite = TRUE)

