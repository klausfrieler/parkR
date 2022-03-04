ticks_per_beat <- 4
ticks_per_bar <- 16
beats_per_bar <- 4

#' phrase_to_mcsv2
#'
#' This function format a generated phrase to mcsv2-like data fraem
#'
#' @param phrase_tbl (data frame) generated phrase data.frame
#' @param tempo (double scale) Tempo (bpm) of  the generated solo.
#' @param phrase_id (integer scalar) Id of phrase for coordination
#' @param chorus_id (integer scalar) Id of chorus for coordination
#' @return A solo data frame in MCSV2 format
#' @export
phrase_to_mcsv2 <- function(phrase_tbl, tempo = 120, phrase_id = 1, chorus_id = 1){
  #print(phrase_tbl)
  final <- tibble(
    bar = as.integer(floor(phrase_tbl$mpos / ticks_per_bar)) + 1,
    beat = as.integer(floor(phrase_tbl$mpos / ticks_per_beat) %% ticks_per_beat) + 1,
    tatum = as.integer(phrase_tbl$mpos %% ticks_per_beat) + 1
  )

  T <- 60/tempo
  final$beat_duration <- T
  final$division <- ticks_per_beat
  final$period <- beats_per_bar
  final$signature <- "4/4"
  final$phrase_id <- phrase_id
  final$phrase_begin <- 0
  final$phrase_begin[1] <- 1
  final$chorus_id <- chorus_id
  final$onset <- phrase_tbl$mpos / ticks_per_beat * T
  final$duration <- phrase_tbl$ioi * T/ticks_per_beat
  final$pitch <- phrase_tbl$pitch
  final$chord <- phrase_tbl$chord
  return(final[, c("onset", "duration", "period", "division", "bar", "beat", "tatum", "beat_duration", "signature", "pitch", "phrase_id", "phrase_begin", "chord", "chorus_id")])
}

#' chorus_to_mcsv2
#'
#' This function formats a generated chorus to mcsv2-compatible data fraem
#'
#' @param chorus_tbl (data frame) generated phrases in data.frame
#' @param tempo (double scale) Tempo (bpm) of  the generated solo.
#' @param chorus_id (integer scalar) Id of chorus for coordination
#' @return A solo data frame in MCSV2 format
#' @export
chorus_to_mcsv2 <- function(chorus_tbl, tempo = 120, chorus_id = 1){
  #print(phrase_tbl)
  purrr::map_dfr(unique(chorus_tbl$phrase_id), function(p_id){
    phrase_to_mcsv2(chorus_tbl %>% filter(phrase_id == p_id),
                    tempo = tempo, phrase_id = p_id, chorus_id = chorus_id)
  }) %>% set_format("mcsv2")
}

#' solo_to_mcsv2
#'
#' This function formats a generated solo to mcsv2-compatible data fraem
#'
#' @param solo_tbl (data frame) generated phrases in data.frame
#' @param tempo (double scale) Tempo (bpm) of  the generated solo.
#' @return A solo data frame in MCSV2 format
#' @export
solo_to_mcsv2 <- function(solo_tbl, tempo = 120){
  #print(phrase_tbl)
  purrr::map_dfr(unique(solo_tbl$chorus_id), function(c_id){
    chorus_to_mcsv2(solo_tbl %>% filter(chorus_id == c_id), tempo = tempo, chorus_id = c_id)
  }) %>% set_format("mcsv2")
}

#' solo_to_mcsv2
#'
#' This function formats a generated solo to mcsv2-compatible data fraem
#'
#' @param solo_tbl (data frame) generated phrases in data.frame
#' @param tempo (double scale) Tempo (bpm) of  the generated solo.
#' @return A solo data frame in MCSV2 format
#' @export
solo_to_lilypond <- function(solo_tbl, file = "solo.pdf", key = "c", tempo = 120, lead_sheet = NULL, ...){
  tmp <- solo_tbl %>%
    select(-c(type, mlu, phrase_id, chorus_id, chord)) %>%
    mutate(
           bar = floor(mpos / ticks_per_bar),
           beat = floor(mpos / ticks_per_beat) ,
           beat_pos = mpos %% ticks_per_beat,
           end_pos = mpos + ioi - 1,
           true_ioi = c(diff(mpos), NA),
           end_beat = floor(end_pos / ticks_per_beat) ,
           over_beat = beat != end_beat)
  #browser()

  if(tmp[1,]$beat_pos != 0){
    patch_row <- tmp[1,] %>% mutate(pitch = -1,
                                    ioi = mpos,
                                    mpos = 0,
                                    bar = 0,
                                    beat = 0, beat_pos = 0,
                                    end_pos = ioi,
                                    true_ioi = 5,
                                    end_beat = floor(ioi / ticks_per_beat),
                                    over_beat = ioi > 4)
    tmp <- bind_rows(patch_row, tmp)
  }
  #browser()

  tmp <- tmp %>% mutate(id = 1:nrow(.),
                        tabr_dur = ioi_to_tabr_dur(ioi))
  ##add rests
  tmp <- tmp %>%
    filter(ioi != true_ioi, mpos != 0) %>%
    mutate(pitch = -1,
           mpos = mpos + ioi,
           ioi = true_ioi - ioi,
           true_ioi = ioi,
           bar = floor(mpos / ticks_per_bar),
           beat = floor(mpos / ticks_per_beat) ,
           beat_pos = mpos %% ticks_per_beat,
           end_pos = mpos + ioi - 1,
           end_beat = floor(end_pos / ticks_per_beat) ,
           over_beat = beat != end_beat) %>%
    mutate(tabr_dur = ioi_to_tabr_dur(ioi)) %>%
    bind_rows(tmp)
  good_iois <- tmp %>% filter(tabr_dur != "split", !over_beat)
  bad_iois <- tmp %>% filter(tabr_dur == "split" | over_beat)

  bad_iois <-
    map_dfr(1:nrow(bad_iois), function(i){
    row <- bad_iois[i,]
    iois <- split_iois(row$ioi, row$beat_pos)
    bind_cols(tibble(ioi = iois,
                     tilde = c(rep("~", length(iois) -1), "")),
              row %>% select(-ioi))

  }) %>% mutate(tabr_dur =
                  ioi_to_tabr_dur(ioi),
                over_beat = F)

  tmp <- bind_rows(good_iois %>% mutate(tilde  = ""), bad_iois) %>%
    arrange(id, desc(pitch))
  tmp[tmp$pitch < 0,]$tilde <- ""
  if(nrow(tmp %>% filter(tabr_dur == "split" | over_beat)) > 0){
    warning("Somethings bad happend")
    browser()
  }
  notes <- sprintf("%s%s", get_tabr_note(tmp$pitch), tmp$tilde)
  chord_seq <- NULL
  attached_lead_sheet <- attr(solo_tbl, "lead_sheet")
  if(!is.null(attached_lead_sheet)){
    chord_seq <- setNames(4/attached_lead_sheet$duration, chord_to_lilypond(attached_lead_sheet$chord))
  }
  if(!is.null(lead_sheet)){
    chord_seq <- setNames(4/lead_sheet$duration, chord_to_lilypond(lead_sheet$chord))
  }
  browser()
  tabr::as_music(notes = notes, info = tmp$tabr_dur) %>%
    p() %>%
    track(tab = F) %>%
    score(chord_seq = chord_seq)  %>%
    tab(file = file, key = key, time = "4/4", tempo = sprintf("4 = %s", tempo))
}

ioi_to_tabr_dur <- function(ioi){
  ioi_map <- c("1" = "16",
               "2" = "8",
               "3" = "8.",
               "4" = "4",
               "6" = "4.",
               "8" = "2",
               "12" = "2.",
               "16" = "1")
  ret <- ioi_map[as.character(ioi)]
  ret[is.na(ret)] <- "split"
  ret
}

split_iois <- function(ioi, beat_pos){
  stopifnot(beat_pos < ticks_per_beat)
  if(ioi + beat_pos <= ticks_per_beat){
    return(ioi)
  }
  first <- ticks_per_beat - beat_pos
  iois <- first
  rest <- ioi - first
  while(rest > ticks_per_beat){
    iois <- c(iois, ticks_per_beat)
    rest <- rest - ticks_per_beat
  }
  c(iois, rest)
}

get_octave_str <- Vectorize(
  function(midi_pitch) {
    if(midi_pitch < 0 ){
      return("")
    }
    oct <- floor((midi_pitch - 60)/12)
    if(oct < 0){
      ret <- rep(",", abs(oct))
    }
    else
      if(oct > 0) {
        ret <- rep("'", oct)
      }
      else {
        ret <- ""
      }
    paste(ret, collapse = "")
  })

get_tabr_note <- Vectorize(function(midi_pitch){
  if(midi_pitch < 0 ){
    return("r")
  }
  pc_labels <- pc_labels_flat %>% str_replace("b", "_") %>% tolower()
  note_name <- pc_labels[midi_pitch %%12 + 1]
  oct <- get_octave_str(midi_pitch)
  sprintf("%s%s", note_name, oct)
})
