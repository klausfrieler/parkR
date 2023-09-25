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

read_beat_tracks <- function(){
  beat_fn <- list.files("C:/MeloSpyGUI/data/converted/", pattern = "*beat", full.names = T)
  beat_tracks <- map_dfr(beat_fn, function(fn){
    beat <- read.csv(fn, sep = ";", stringsAsFactors = F) %>%
      as_tibble() %>%
      mutate(id = str_replace(basename(fn), "_beattrack.csv", ".sv"),
             chord_change = chord != lag(chord, default = ""))
    beat
    }) %>%
    mutate(melid = as.integer(factor(id)),
           beat_id = sprintf("%s_%s_%s_%s", melid, chorus_id, bar, beat))
  return(beat_tracks)
}

fill_chord_stream <- function(chord_stream){
  l <- length(chord_stream)
  if(l == 0){
    stop()
  }
  if(is.na(chord_stream[1])){
    chord_stream[1] <- "X"
  }
  #browser()
  chord_idz <- which(!is.na(chord_stream))
  if(length(chord_idz) == 0){
    browser()
    return(chord_stream)
  }
  durs <- diff(c(chord_idz, l + 1))
  chords <- rep(chord_stream[chord_idz], durs)
  chords[chords == "X"] <- NA
  chords
}

key_analysis_single_beattrack <- function(beat_track){
  ids <- unique(beat_track$id)
  if(length(ids) > 1) stop()
  beat_track <- beat_track  %>%
    group_by(chorus_id, form) %>%
    mutate(beat_id_form = sprintf("%s_%s", form, 1:n())) %>%
    ungroup()
  form_parts <- unique(beat_track$form)
  form_ka <-map_dfr(form_parts, function(fp){
    #browser()
    tmp <- beat_track %>%
      filter(form == fp) %>%
      filter(chorus_id == min(chorus_id)) %>%
      mutate(chord_change = chord != lag(chord, default = "")) %>%
      filter(chord_change)
    ka <- tmp %>% pull(chord) %>% key_analysis(chord_stream = .)
    ka  <- bind_cols(tmp %>% select(beat_id, beat_id_form),
                     ka %>% select(local_scale_degree, local_key)) %>% mutate(form = fp)
    ka
  })
  ret <- beat_track %>%
    left_join(form_ka %>% select(local_scale_degree, local_key, beat_id_form), by = "beat_id_form") %>%
    mutate(local_scale_degree = fill_chord_stream(local_scale_degree),
           local_key = fill_chord_stream(local_key),
           beat) %>%
    select(-beat_id_form)
  ret[ret$chord == "NC" & !is.na(ret$local_scale_degree),]$local_scale_degree <- NA
  ret[ret$chord == "NC" & !is.na(ret$local_key),]$local_key <- NA
  if(nrow(ret %>% filter(chord != "NC", is.na(local_scale_degree))) > 1){
    browser()
  }
  #browser()
  ret
}

key_analysis_all_beattracks <-function(beat_tracks){
  ids <- unique(beat_tracks$id)
  map_dfr(ids, function(i){
    messagef("Annotation %s", i)
    key_analysis_single_beattrack(beat_tracks %>% filter(id == i))
  })
}
cpc_cdpcx_map <-  list("0" = "1",
                       "1" = "-",
                       "2" = "2",
                       "3" = list("min" = "3",
                                  "7"   = "B",
                                  "maj" = "B"),
                       "4" = list("min" = ">",
                                  "7"   = "3",
                                  "maj" = "3"),
                       "5" = "4",
                       "6" = "T",
                       "7" = "5",
                       "8" = "%",
                       "9" = "6",
                       "10" = list("min" = "7",
                                   "7"   = "7",
                                   "maj" = "<"),
                       "11" = list("min" = "L",
                                   "7"   = "L",
                                   "maj" = "7")
)
chord_type_map <- c("min" = "min",
                    "min6" = "min",
                    "min7" = "min",
                    "m7b5" = "min",
                    "o" = "min",
                    "o7" = "min",
                    "minmaj" = "min",
                    "minmaj7" = "min",
                    "6" = "maj",
                    "7" = "7",
                    "maj" = "maj",
                    "maj7" = "maj",
                    "NC" = NA)


recalc_cdpcx <- function(data = wjd_tpc){
  chord_dictionary <- tibble(
    chord = data$chord %>%
      unique()) %>%
    bind_cols(data$chord %>%
                unique() %>%
                parkR::parse_chord())

  tmp <- data %>%
    left_join(chord_dictionary %>%
                select(-pc, -bass, -bass_pc, -ext, chord_type = type), by = "chord") %>%
    mutate(cpc = (pitch - root_pc) %% 12)

  elements <- tmp %>%
    distinct(cpc, chord_type) %>%
    mutate(chord_type_short = chord_type_map[chord_type])

  elements$cdpcx <-
    map2_chr(as.character(elements$cpc), elements$chord_type_short, function(cpc, ct){
      #browser()
      if(is.na(ct)){
        return("X")
      }
      tmp <- cpc_cdpcx_map[[cpc]]
      if(length(tmp) > 1){
        return(tmp[[ct]])
      }
      else{
        return(tmp[[1]])
      }
    })
  tmp %>% select(-cdpcx) %>% left_join(elements %>% select(cpc, chord_type, cdpcx))
}
symm_diff <- function(s1, s2){
  union(setdiff(s1, s2), setdiff(s2, s1))
}
make_wjd_tpc <- function(){
  wjd_transforms <- jazzodata::wjd_transforms %>% mutate(event_id = 1:nrow(.))
  wjd_beats <- read_csv2("c:/MeloSpyGUI/analysis/feature+viz/wjd_beats.csv") %>%
    mutate(melid = as.integer(factor(id)),
           event_id = 1:nrow(.))
  wjd_beats[wjd_beats$chorus_id_raw < 0,]$chorus_id_raw <- wjd_beats[wjd_beats$chorus_id_raw < 0,]$chorus_id_raw + 1
  wjd_beats <- wjd_beats %>%
    group_by(id) %>%
    mutate(beat_id = sprintf("%d_%d_%d_%d", melid, chorus_id_raw, bar, beat)) %>%
    ungroup()
  browser()
  wjd_transforms <- wjd_transforms %>%
    left_join(wjd_beats %>%
                select(beat_id, event_id, melid),
              by = "event_id")
  wjd_transforms <- wjd_transforms %>%
    left_join(annotated_beat_tracks %>%
                select(beat_id, local_scale_degree, local_key),
              by = "beat_id")
  wjd_transforms
}
