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
      mutate(id = str_replace(basename(fn), "_beattrack.csv", ".sv")) %>%
      fix_inside_ncs()
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

get_chorus_subtypes <- function(beat_tracks){
  melids <- unique(beat_tracks$melid)
  map_dfr(melids, function(mid){
    streams <- beat_tracks %>%
      filter(melid == mid, chord_change) %>%
      group_by(chorus_id) %>%
      summarise(chord_stream = paste(chord, collapse = ","), .groups = "drop") %>%
      mutate(chorus_type = as.integer(factor(chord_stream)),
             melid = mid)
    streams
  })
}

explode_rle <- function(rle){
  rep(rle$values, rle$lengths)
}

fix_inside_ncs <- function(beat_track){
  #browser()
  if(("id" %in% names(beat_track)) && length(beat_track %>% pull(id) %>% unique()) > 1){
    stop("Only for single beat tracks")
  }
  chord_rle <- rle(beat_track$chord)
  ncs <- which(chord_rle$values == "NC")
  inside_ncs <- ncs[ncs > 1 & ncs < length(chord_rle$values)]
  if(length(inside_ncs) > 0){
    #browser()
    for(i in inside_ncs){
      chord_rle$values[i] <-   chord_rle$values[i - 1]
    }
    messagef("Fixed %d inside ncs for %s", length(inside_ncs), beat_track$id %>% unique())
    beat_track$chord <- explode_rle(chord_rle)
  }
  beat_track  %>%
    mutate(chord_change = chord != lag(chord, default = ""))

}

fix_all_inside_ncs <- function(beat_tracks){
  melids <- unique(beat_tracks$melid)
  map_dfr(melids, function(mid){
    #browser()
    fix_inside_ncs(beat_tracks %>% filter(melid == mid))
  })
}

check_beat_tracks <- function(beat_tracks){
  melids <- unique(beat_tracks$melid)
  map_dfr(melids, function(mid){
    #browser()
    tmp <- beat_tracks %>%
      filter(melid == mid,
             substr(form, 1,1 ) != "I",
             chorus_id > 0,
             bar > 0)
    if(nrow(tmp) == 0){
      return(tibble(melid = mid, chorus_consistency_bars = NA, chorus_consistency_chord = NA, state = "okay"))
    }
    #browser()
    chorus_consistency_bars <- tmp %>%
      count(chorus_id) %>%
       distinct(n) %>%
      nrow()
    #browser()
    chorus_consistency_chords <- tmp %>%
      group_by(chorus_id) %>%
      summarise(n_chords = n_distinct(chord), .groups= "drop") %>%
      count(n_chords) %>%
      nrow()

    tibble(melid = mid,
           chorus_consistency_bars = chorus_consistency_bars,
           chorus_consistency_chords =  chorus_consistency_chords,
           state = factor(
             chorus_consistency_bars <= 2 & chorus_consistency_chords <= 2,
             levels = c(FALSE, TRUE),
             labels = c("bad", "okay")))
  })
}

key_analysis_single_beattrack <- function(beat_track){
  ids <- unique(beat_track$id)
  if(length(ids) > 1) stop()
  beat_track <- beat_track  %>%
    mutate(chord_change = chord_change | chord != lag(chord, default = "") | chorus_id != lag(chorus_id, default = -999)) %>%
    group_by(chorus_id, form) %>%
    mutate(beat_id_form = sprintf("%s_%s", form, 1:n())) %>%
    ungroup()

  if(unique(beat_track$id) == "JohnColtrane_26=2_FINAL.sv"){
    beat_track <- beat_track %>% filter(bar > 1)
  }
  #consistent <- (check_beat_tracks(beat_track) %>% pull(state)) == "okay"
  consistent <- FALSE

  if(consistent){
    form_parts <- unique(beat_track$form)
    form_ka <- map_dfr(form_parts, function(fp){
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
      left_join(form_ka %>%
                  select(local_scale_degree, local_key, beat_id_form),
                by = "beat_id_form")
  }
  else{
    # tmp <- beat_track %>%
    #   mutate(chord_change = chord != lag(chord, default = "")) %>%
    #   filter(chord_change)
    # ka <- tmp %>% pull(chord) %>% key_analysis(chord_stream = .)
    # form_ka  <- bind_cols(tmp %>% select(beat_id, beat_id_form),
    #                  ka  %>% select(local_scale_degree, local_key))
    chorus_subtypes <- get_chorus_subtypes(beat_track)
    messagef("Analying melid = %s with %d sub types.", ids[1], n_distinct(chorus_subtypes$chorus_type))
    form_ka <- map_dfr(unique(chorus_subtypes$chorus_type), function(chtype){
      chids <- chorus_subtypes %>%
        filter(chorus_type == chtype) %>%
        pull(chorus_id)

      tmp <- beat_track %>%
        filter(chorus_id %in% chids) %>%
        #mutate(chord_change = chord != lag(chord, default = "")) %>%
        filter(chord_change)

      # ka <- tmp %>% pull(chord) %>% key_analysis(chord_stream = .)
      chord_stream <- chorus_subtypes %>%
        filter(chorus_type == chtype) %>%
        pull(chord_stream) %>% str_split(",")

      ka <- key_analysis(chord_stream = chord_stream[[1]])
      #browser()
      ka  <- map_dfr(chids, function(chid){
        bind_cols(tmp %>% select(beat_id, beat_id_form, chorus_id) %>% filter(chorus_id == chid),
                       ka %>% select(local_scale_degree, local_key))
      })
      #browser()
      ka
    })
    #browser()

    ret <- beat_track %>%
      left_join(form_ka %>%
                  select(local_scale_degree, local_key, beat_id),
                by = "beat_id")
  }
  ret <- ret %>%
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

key_analysis_all_beattracks <- function(beat_tracks){
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
  tmp %>% select(-cdpcx) %>% left_join(elements %>% select(cpc, chord_type, cdpcx), by  = c("cpc", "chord_type"))
}

symm_diff <- function(s1, s2){
  union(setdiff(s1, s2), setdiff(s2, s1))
}

make_wjd_transforms <- function(){
  chord_dictionary <- tibble(
    chord = annotated_beat_tracks$chord %>%
      unique()) %>%
    bind_cols(annotated_beat_tracks$chord %>%
                unique() %>%
                parkR::parse_chord())

  wjd_transforms <- read.csv("c:/MeloSpyGUI/analysis/feature+viz/wjd_transforms.csv", sep = ";", stringsAsFactors = F) %>%
    as_tibble() %>%
    select(-cpc_raw, -cdpcx_raw) %>%
    mutate(melid = as.integer(factor(id)),
           event_id = 1:nrow(.),
           beat_id = sprintf("%d_%d_%d", melid, bar, beat)) %>%
    rename(cpc_raw = cpc_raw_all,
           cdpcx_raw = cdpcx_raw_all)

  annotated_beat_tracks <- annotated_beat_tracks %>%
    mutate(beat_id = sprintf("%d_%d_%d", melid, bar, beat))

  wjd_transforms <- wjd_transforms %>%
    select(-chords_raw, -chord_types_raw) %>%
    left_join( annotated_beat_tracks %>%
                 select(beat_id, chord, local_scale_degree, local_key),
               by = "beat_id") %>%
    rename(phrase_begin = phrasbeg,
           phrase_end = phrasend,
           chorus_id = chorus_id_raw,
           form_name = form_labels_raw)

  wjd_transforms <- wjd_transforms %>%
    left_join(chord_dictionary %>% select(chord, chord_types_raw = type), by = "chord") %>%
    mutate(chords_raw = chord)

  wjd_transforms

}
make_wjd_tpc <- function(){
  wjd_transforms <- read.csv("c:/MeloSpyGUI/analysis/feature+viz/wjd_transforms.csv", sep = ";", stringsAsFactors = F) %>%
    as_tibble() %>%
    select(-cpc_raw, -cdpcx_raw) %>%
    mutate(melid = as.integer(factor(id)),
           event_id = 1:nrow(.),
           beat_id = sprintf("%d_%d_%d", melid, bar, beat),
           cpc_raw = cpc_raw_all,
           cdpcx = cdpcx_raw_all)
  browser()
  annotated_beat_tracks <- annotated_beat_tracks %>%
    mutate(beat_id = sprintf("%d_%d_%d", melid, bar, beat))

  wjd_transforms <- wjd_transforms %>%
    select(-chords_raw, -chord_types_raw) %>%
    left_join( annotated_beat_tracks %>%
                select(beat_id, chord, local_scale_degree, local_key),
              by = "beat_id") %>%
    set_names(str_remove(names(.), "s_raw")) %>%
    set_names(str_remove(names(.), "_raw")) %>%
  set_names(str_replace(names(.), "_ab", "_abs"))
  #browser()

  ret <- wjd_transforms %>%
    filter(!is.na(local_scale_degree)) %>%
    select(-cpc_all, -cdpcx_all) %>%
    recalc_cdpcx() %>%
    select(id, melid, beat_id, event_id, bar, beat, cdpcx, cpc, chord, local_scale_degree, local_key, tatum, period, division, form_name = form_label, everything()) %>%
    mutate(key_pc = str_split_fixed(local_key,"-", 2)[,1] %>% tone_name_to_pc(),
           tonic = str_split_fixed(local_key,"-", 2)[,1]) %>%
    left_join(jazzodata::wjd_meta %>%
                select(id, key, genre, performer, title, recordingyear, rhythmfeel, style, tempoclass, tonality_type),
              by = "id") %>%
    group_by(local_scale_degree) %>%
    mutate(n_pitches = n()) %>%
    ungroup()

  browser()
  ret
}
