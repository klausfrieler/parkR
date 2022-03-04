g_counter <- 0

sample_iois <- function(size = 10, start = 1){

  last <- start - 3
  ret <- vector("numeric", size)
  ret[1] <- last
  if(size < 2){
    return(ret+3)
  }
  for(i in 2:size){
    ret[i] <- parkR::succ_ioiclass %>%
      filter(value == ret[i - 1]) %>%
      sample_n(1, replace = T) %>%
      pull(successor)
  }
  ret + 3
}





realize_arpeggio <- function(direction,length, start_pitch, chord, pitch_range = c(48, 84)){
  direction <- sign(direction)
  if(chord == "NC"){
    chord <- get_random_chord()
    #message(sprintf("[realize_arpeggio]: Found NC, substituting %s", chord))
  }
  chord <- parse_chord(chord)
  root_pc <- chord$pc
  chord_type <- chord$type
  octave <- floor((start_pitch - root_pc)/12)
  #printf("CT= %s PC=%s Octave= %s", chord_type,  root_pc, octave*12 + root_pc)
  #printf("Start pitch=%d Length= %d", start_pitch, length)
  arpeggio_pitches <- chord_definitions[[chord_type]]$arpeggio + 12 * octave + root_pc
  #print(arpeggio_pitches)
  pos <- which(start_pitch == arpeggio_pitches)
  pitches <- NULL
  if(length(pos) == 0){
    pos <- which.min(abs(arpeggio_pitches - start_pitch))
    pitches <- start_pitch
    messagef("ARPEGGIO: Added link at beginning")
  }
  #print(seq(pos, pos + direction * length - 1*direction, direction))
  if(length(pos) == 0 || length(pos + direction * length) == 0){
    browser()
  }
  pitches <- c(pitches, arpeggio_pitches[safe_seq(pos, pos + direction * length)])
  #print(paste(pc_labels_flat[pitches %% 12 + 1], collapse=" "))
  pitches
}

realize_chromatic <- function(direction, length, start_pitch, chord, pitch_range = c(48, 84)){
  pitches <- cumsum(c(start_pitch, rep(sign(direction), length)))
  pitches <- pitches[!is.na(pitches) & pitches >= pitch_range[1] & pitches <= pitch_range[2]]
  pitches

}

realize_diatonic <- function(direction, length, start_pitch, chord, pitch_range){
  direction <- sign(direction)

  if(chord == "NC"){
    chord <- get_random_chord()
    #message(sprintf("[realize_diatonic]: Found NC, substituting %s", chord))
  }

  chord_pitches <- get_scale_pitches(chord,
                                     min_pitch = 0,
                                     max_pitch = 127)
  #pretty_print_pitches(chord_pitches)
  pos <- which(start_pitch == chord_pitches)
  pitches <- NULL
  if(length(pos) == 0){
    pitches <- start_pitch
    pos <- which.min(abs(chord_pitches - start_pitch))
    #pretty_print_pitches(pitches)
    messagef("DIATONIC: Added link at beginning")
  }
  if(length(pos) == 0 || length(pos + direction * length) == 0){
    browser()
  }

  c(pitches, chord_pitches[safe_seq(pos, pos + direction * length)])

}

realize_direct <- function(value, start_pitch){
  if(is.character(value)){
    value <- value_to_vec(value)
  }
  cumsum(c(start_pitch, value))
}

realize_X <- function(value, start_pitch, chord){
  if(chord == "NC"){
    chord <- get_random_chord()
    #message(sprintf("[realize_X]: Found NC, substituting %s", chord))
  }
  find_best_match_to_chord(value, start_pitch, chord)
}

realize_atom <- function(row,
                         start_pitch,
                         start_ticks,
                         chord,
                         pitch_range = c(48, 84),
                         exclude_types = NULL){
  realize_wb_atom(type = row$type,
                  value = row$value,
                  direction = row$direction,
                  length = row$length,
                  start_pitch = start_pitch,
                  start_ticks = start_ticks,
                  chord = chord,
                  pitch_range = pitch_range,
                  exclude_types = exclude_types)
}


realize_wb_atom <- function(type,
                            value,
                            direction,
                            length,
                            start_pitch,
                            start_ticks,
                            chord,
                            pitch_range = c(48, 84),
                            exclude_types = NULL){
  if(!is.null(exclude_types) && (type %in% exclude_types)){
    #printf("Excluded %s", type)
    return(NULL)
  }
  messagef("Realizing atom '%s' with value %s, direction = %d, and length %d on pitch %d over chord %s",
                  type, value, direction, length, start_pitch, chord)
  rel_reg_pos <- relative_register_position(start_pitch, pitch_range)
  if(rel_reg_pos > .7 && direction > 0){
    if(type %in% c("C", "D", "A")) {
      direction <- (-direction)
      messagef("Changed direction from up to down")
    }
    else{
      start_pitch <- start_pitch - 7
      messagef(sprintf("Adjusted start pitch from %d to %d", start_pitch + 7, start_pitch))
    }

  }
  if(rel_reg_pos < .3 && direction < 0){
    if(type %in% c("C", "D", "A")) {
      direction <- (-direction)
      messagef("Changed direction from down to up")
    }
    else{
      start_pitch <- start_pitch + 7
      messagef("Adjusted start pitch from %d to %d", start_pitch - 7, start_pitch)
    }
  }

  if(type == "A"){
    pitches <- realize_arpeggio(direction, length, start_pitch, chord, pitch_range)
  }
  if(type == "J"){
    pitches <- realize_X(value, start_pitch, chord)
  }
  if(type == "F"){
    pitches <- realize_direct(value, start_pitch)
  }
  if(type == "T"){
    pitches <- realize_direct(value, start_pitch)
  }
  if(type == "D"){
    pitches <- realize_diatonic(direction, length, start_pitch, chord, pitch_range)
  }
  if(type == "C"){
    pitches <- realize_chromatic(direction, length, start_pitch, chord, pitch_range)
  }
  if(type == "X"){
    pitches <- realize_X(value, start_pitch, chord)
  }
  if(type == "L"){
    pitches <- realize_direct(value, start_pitch)
  }
  if(type == "R"){
    pitches <- realize_direct(value, start_pitch)
  }
  #print(type)
  safe_pitch_vector(pitches)

}

get_current_chord <- function(chord_sequence, start_ticks){
  #browser()
  current_chord <- chord_sequence[chord_sequence$onset_ticks <= start_ticks, ]
  messagef("Start ticks: %d, chord = %s", start_ticks, current_chord[nrow(current_chord),]$chord)
  current_chord[nrow(current_chord),]
}

get_start_pitch <- function(chord, pitch_range = c(48, 84)){
  if(chord == "NC"){
    chord <- get_random_chord()
    #message(sprintf("[get_start_pitch]: Found NC, substituting  %s", chord))
  }
  chord <- parse_chord(chord)
  #printf("Chord '%s'", chord)
  chord_type <- chord$type
  root_pc <- chord$pc
  octave <- floor(mean(pitch_range)/12)
  base_arp <- chord_definitions[[chord_type]]$arpeggio
  if(is.null(base_arp)){
    print(chord_type)
    browser()
  }
  base_arp <- base_arp[abs(base_arp) <= 12]
  candidates <-base_arp + 12 * octave + root_pc
  candidates <- candidates[candidates>=pitch_range[1]+5 & candidates<=pitch_range[2]-5]
  #print(candidates)
  safe_pitch_vector(sample(candidates, 1), pitch_range)
}

get_good_iois <- function(size, start_ticks){
  okay <- FALSE
  max_iter <- 10
  i <- 0
  while(!okay){
    i <- i + 1
    if(i > max_iter){
      return(list(iois = as.integer(sample_iois(size)), mpos = start_ticks))
    }
    iois <- as.integer(sample_iois(size)) + 1
    mpos <-  cumsum(c(start_ticks, iois))
    #print(mpos)
    num_offbeats <- sum(mpos %% 2 == 1)
    num_onbeats  <- sum(mpos %% 2 == 0)
    #print(mpos %% 2)
    okay <- num_onbeats >= 2 * num_offbeats
  }
  list(mpos = mpos, iois = iois)
}

get_iois_and_mpos <- function(pitches, start_ticks, mlu){
  #printf("MLU = %s, start_ticks = %d", mlu, start_ticks)
  if(mlu == "lick"){
    tmp <- get_good_iois(length(pitches), start_ticks)
    iois <- tmp$iois
  }
  else{
    line_dur <- ifelse(stats::runif(1) < .75, 1, 2)
    iois <- as.integer(rep(line_dur, length(pitches)))
    if(line_dur == 2){
      if(start_ticks %% 2 == 1){
        iois[1] <- 1
      }
    }
  }
  if(length(iois) == 1){
    mpos <- start_ticks
  }
  else {
    mpos <- cumsum(c(start_ticks, iois[1:(length(iois)-1)]))
  }
  #dmean <- mean(fill_diff(mpos) == iois, na.rm = T)
  list(iois = iois, mpos = mpos)
}

generate_phrase_over_chords <- function(lead_sheet,
                                        start_ticks,
                                        pitch_range = c(48, 84),
                                        mlu = c("line", "lick"),
                                        excludes = NULL,
                                        min_len = 2,
                                        max_len = 10){
  if(mlu == "line"){
    len <- parkR::phrase_length_dist %>% filter(n > 5)  %>% sample_n(1) %>% pull(n)
  }
  else{
    len <- parkR::phrase_length_dist %>% filter(n < 10)  %>% sample_n(1) %>% pull(n)
  }
  len <- min(max_len, max(min_len, len))
  ret <- list()
  okay <- FALSE
  start_chord <- get_current_chord(lead_sheet, start_ticks)

  #first find and generate start atom
  while(!okay){
    #browser()
    start_atom <- parkR::phrase_begin_dist %>% sample_n(1)
    #print(sprintf("Start: %s (%s)",  start$value, start$type))
    if(mlu == "line" && start_atom$type == "R"){
      next
    }
    start_pitch <- get_start_pitch(start_chord$chord, pitch_range)
    #print(sprintf("[%d] %s: %s", start_pitch, start$type, start$value))
    pitches <- realize_atom(row = start_atom,
                            start_pitch = start_pitch,
                            start_ticks = start_ticks,
                            chord = start_chord$chord,
                            pitch_range = pitch_range,
                            exclude_types = excludes)
    okay <- !is.null(pitches) && length(pitches) > 1 && abs(length(pitches) - start_atom$length) < 2
  }
  current_pitch <- pitches[length(pitches)]
  pitches <- pitches[1:(length(pitches)-1)]
  #pretty_print_pitches(pitches)
  tmp <- get_iois_and_mpos(pitches, start_ticks, mlu = mlu)
  current_ticks <- max(tmp$mpos) + last_n(tmp$iois, 1)
  messagef("Current ticks: %d, max mpos %d, last : %d", current_ticks, max(tmp$mpos), last_n(tmp$iois, 1))
  current_chord <- get_current_chord(lead_sheet, current_ticks)
  last <- start_atom
  ret <- list()
  ret[[1]] <- tibble(pitch = pitches,
                     mpos = tmp$mpos,
                     iois = tmp$iois,
                     type = start_atom$type,
                     chord = start_chord$chord)

  if(len < 2){
    return(ret[[1]])
  }

  #generate all other atoms
  for(i in 2:len){
    okay <- FALSE
    j <- 0
    #printf("Outer loop with %d (len %d)",i, len)
    while(!okay) {
      #print("==========")
      #printf("Loop...%d", j)
      j <- j +1
      if(j > 100){
        stop("Infinite loop generated")
      }
      candidate <- parkR::successor_dist %>%
        filter(type == last$type,
               direction == direction) %>%
        sample_n(1, replace=T)
      #print(candidate)
      #print(last)
      #print(sprintf("Last: %s (%s)",  last$value, last$type))
      #print(sprintf("New candidate: %s (%s)",  candidate$succ_value, candidate$succ_type))
      if(mlu == "line" && !(candidate$succ_type %in% c("D", "C", "F", "T", "A"))){
        next
      }
      if(candidate$succ_type == "L"){
        next
      }
      row <- candidate %>% select(type = succ_type,
                                  direction = succ_direction,
                                  length = succ_length,
                                  value = succ_value)
      #print(row)
      pitches <- realize_atom(row,
                              start_pitch = current_pitch,
                              start_ticks = current_ticks,
                              chord = current_chord$chord,
                              pitch_range = pitch_range,
                              exclude_types = excludes)
      if((length(pitches) <  (candidate$succ_length + 1)) && candidate$succ_type != "A"){
        #printf("Length %d, supposed: %d", length(pitches), candidate$succ_length + 1)
        #stop("Too less pitches")
      }
      okay <- !is.null(pitches) && length(pitches) > 1
    }
    last <- candidate
    current_pitch <- pitches[length(pitches)]
    #pretty_print_pitches(pitches)
    pitches <- pitches[1:(length(pitches) - 1)]
    #pretty_print_pitches(pitches)
    tmp <- get_iois_and_mpos(pitches, current_ticks, mlu = mlu)

    #printf("Current ticks: %d, max mpos %d, last : %d", current_ticks, max(tmp$mpos), last_n(tmp$iois, 1))
    current_chord <- get_current_chord(lead_sheet, current_ticks)
    current_ticks <- max(tmp$mpos) + last_n(tmp$iois, 1)

    ret[[i]] <- tibble(pitch = pitches,
                       mpos = tmp$mpos,
                       iois = tmp$iois,
                       type = row$type,
                       chord = current_chord$chord)
  }#
  #assign(sprintf("ret_%d", start_ticks), ret, globalenv())
  bind_rows(ret) %>% mutate(mlu = mlu)
}

#' generate_solo
#'
#' This function generaties a solo for a given lead sheet
#'
#' @param lead_sheet (data frame) Lead sheet
#' @param pitch_range (Integer vector of length 2) Minimum and maximum MIDI pitch for solo generation.
#' @param n_chorus (Integer scalar) Number of choruses to generate.
#' @param tempo (double scale) Tempo (bpm) of  the generated solo.
#' @param lick_to_line_ratio (double scalar) Ratio of licks to lines
#' @param excludes (character vector) List of WBA atoms not to use during generation
#' @return A solo data frame
#' @export
generate_solo <- function(lead_sheet,
                          n_chorus = 1,
                          pitch_range = c(48, 84),
                          tempo = 120,
                          lick_to_line_ratio = .5,
                          excludes = NULL){
  if(is.null(lead_sheet)){
    return(NULL)
  }
  start_mpos = 0
  ret <- list()
  current_ticks <- sample(0:7, 1)
  max_chorus_ticks <- sum(lead_sheet$length_ticks)
  max_ticks <- n_chorus * max_chorus_ticks
  phrase_id <- 1
  current_chorus_id <- 1
  if(max_ticks <= 16){
    message(sprintf("Lead sheet %s too short (rows = %d, max ticks = %d), bailing out",
                    lead_sheet$title[1], nrow(lead_sheet),
                    max_ticks))
    return(NULL)
  }
  while(current_ticks < (max_ticks - 16)){
    mlu <- ifelse(stats::runif(1) < lick_to_line_ratio, "lick", "line")
    if(phrase_id == 1){
      mlu <- "lick"
    }
    messagef("\n*** Generating phrase #%d of type %s", phrase_id, mlu)

    ret[[phrase_id]] <- generate_phrase_over_chords(lead_sheet,
                                                    current_ticks %% max_chorus_ticks,
                                                    mlu = mlu,
                                                    excludes = excludes)
    #browser()
    ret[[phrase_id]]$mpos <- ret[[phrase_id]]$mpos + (current_chorus_id - 1)*max_chorus_ticks
    ret[[phrase_id]]$phrase_id <- phrase_id
    ret[[phrase_id]]$chorus_id <- current_chorus_id
    phrase_break <- sample(parkr_options()$phrase_breaks, 1)
    current_ticks <- current_ticks + sum(ret[[phrase_id]]$iois) + phrase_break
    current_chorus_id <- floor(current_ticks/max_chorus_ticks) + 1
    #printf("Current ticks %d (max %d, chorus %d), current chorus: %d", current_ticks, max_ticks, max_chorus_ticks, current_chorus_id)
    phrase_id <- phrase_id + 1
  }
  solo <- bind_rows(ret) %>% set_format("solo_df") %>% filter(mpos <= max_ticks) %>% rename(ioi = iois)
  attr(solo, "lead_sheet") <- lead_sheet
  solo
}

#' make_many_solos
#'
#' This function generaties many one chorus solos from a given lead sheets
#'
#' @param n (Integer scalar) Number of solos to generate.
#' @param fname (Scalar character) path for saving solo files, used as a base name
#' @param lead_sheet (data frame) Lead sheet
#' @param lick_to_line_ratio (scalar double) Ratio of licks to lines
#' @param excludes (character vector) List of WBA atoms not to use during generation
#' @export
make_many_solos <- function(n,
                            fname="solos/solo",
                            lead_sheet = parkR::F_blues,
                            lick_to_line_ratio = .7,
                            excludes = NULL
                            ){
  ret <- list()
  for(i in 1:n){
    #seed<-sample(n)
    set.seed(i)
    tmp_name <- sprintf("%s%d.csv", fname, i)
    messagef("Writing to %s", tmp_name)
    generate_chorus(lead_sheet,
                    lick_to_line_ratio = lick_to_line_ratio,
                    excludes = excludes) %>%
      chorus_to_mcsv2 %>%
      write_mcsv2(tmp_name) -> ret[[i]]
  }
  invisible(ret)
}
