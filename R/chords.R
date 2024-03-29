#library(tidyverse)
#source("utils.R")
# pc_labels <-parkR::labels[["pc_labels"]]
# pc_labels_sharp <- labels[["pc_labels_sharp"]]
# pc_labels_flat <- labels[["pc_labels_flat"]]
# pc_labels_combined <- c(pc_labels_sharp, setdiff(pc_labels_flat, pc_labels_sharp))
note_names <- LETTERS[1:7]

ionian    <- c(0, 2, 4, 5, 7, 9, 11)
dorian    <- c(0, 2, 3, 5, 7, 9, 10)
phrygian  <- c(0, 1, 3, 5, 7, 8, 10)
lydian    <- c(0, 2, 4, 6, 7, 9, 11)
mixolydian  <- c(0, 2, 4, 5, 7, 9, 10)
mixolydian_sharp11  <- c(0, 2, 4, 6, 7, 9, 10)
aeolian  <- c(0, 2, 3, 5, 7, 8, 10)
lokrian  <- c(0, 1, 3, 5, 6, 8, 10)
altered <- c(0, 1, 3, 4, 6, 8, 10)
harmonic_minor  <- c(0, 2, 3, 5, 7, 8, 11)
melodic_minor  <- c(0, 2, 3, 5, 7, 9, 11)
htgt             <- c(0, 1, 3, 4, 6, 7, 9, 10)
gtht             <- c(0, 2, 3, 5, 6, 8, 9, 11)
harmonic_minor_5  <- c(0, 1, 4, 5, 7, 8, 10)
lydian_aug_9 <- c(0, 3, 4, 6, 7, 9, 10)
dom_7_tetra <- c(0, 4, 7, 10)
min_7_tetra <- c(0, 3, 7, 10)
maj_7_tetra <- c(0, 4, 7, 11)
dim_7_tetra <- c(0, 3, 6, 9)
hdim_7_tetra <- c(0, 3, 6, 10)
min_6_tetra <- c(0, 3, 6, 9)
minor_blues  <- c(0, 3, 5, 6, 7, 10)
major_blues <- c(0, 2, 3, 4, 7, 9)
mixo_blues  <- c(0, 2, 3, 4, 5, 7, 9, 10)
chromatic <- 0:11

#' @export
scales <- list("ionian"  = ionian,
               "major" = ionian,
               "dorian" = dorian,
               "phrygian" = phrygian,
               "lydian" =  lydian,
               "mixolydian" =  mixolydian,
               "mixolydian_sharp11" =  mixolydian_sharp11,
               "aeolian" =  aeolian,
               "lokrian" = lokrian,
               "altered" =   altered,
               "harmonic_minor"  = harmonic_minor,
               "melodic_minor" = melodic_minor,
               "htgt" = htgt,
               "gtht" = gtht,
               "harmonic_minor_5" = harmonic_minor_5,
               "lydian_aug_9" = lydian_aug_9,
               "dom_7_tetra" = dom_7_tetra,
               "min_7_tetra" = min_7_tetra,
               "maj_7_tetra"  = maj_7_tetra,
               "dim_7_tetra"  = dim_7_tetra,
               "hdim_7_tetra" = hdim_7_tetra,
               "min_6_tetra"  = min_6_tetra,
               "minor_blues" = minor_blues,
               "major_blues" = major_blues,
               "mixo_blues"  = mixo_blues,
               "minor" = aeolian,
               "chromatic" = chromatic
)
chord_definitions <- list()

chord_definitions[["maj7"]][["scales"]] <- list("ionian" = ionian,
                                                "lydian" = lydian,
                                                "maj_7_tetra" = maj_7_tetra)
#chord_definitions[["maj7"]][["scale_weights"]] <- c(.8, .2, 0)
chord_definitions[["maj7"]][["scale_weights"]] <- c(1.0, 0.0, 0.0)
chord_definitions[["maj7"]][["arpeggio"]] <- c(-12, -8, -5, -1, 0, 4, 7, 11, 12 + 2, 12 + 6, 12 + 9)

chord_definitions[["maj"]] <- chord_definitions[["maj7"]]
chord_definitions[["6"]] <- chord_definitions[["maj7"]]

chord_definitions[["min7"]][["scales"]] <- list("dorian" = dorian,
                                                "aeolian" = aeolian,
                                                "melodic_minor" = melodic_minor,
                                                "harmonic_minor" = harmonic_minor,
                                                "min_7_tetra" = min_7_tetra)
chord_definitions[["min7"]][["scale_weights"]] <- c(1., 0.0, 0.0, 0.0, 0.0)
chord_definitions[["min7"]][["arpeggio"]] <- c(-12, -9, -5, -2, 0, 3, 7, 10, 12 + 2, 12 + 5, 12 + 9)

chord_definitions[["min"]] <- chord_definitions[["min7"]]

chord_definitions[["minb6"]][["scales"]] <- list("aeolian" = aeolian,
                                                "harmonic_minor" = harmonic_minor)
chord_definitions[["minb6"]][["scale_weights"]] <- c(1., 0.0)
chord_definitions[["minb6"]][["arpeggio"]] <- c(-12, -9, -5, -3, 0, 3, 7, 8, 12 + 2, 12 + 5)

chord_definitions[["min6"]][["arpeggio"]] <- c(-12, -9, -5, -4, 0, 3, 7, 9, 12, 12 + 3)
chord_definitions[["min6"]][["scales"]] <- list("dorian" = dorian)
chord_definitions[["min6"]][["scale_weights"]] <- c(1.0)

chord_definitions[["minmaj7"]][["arpeggio"]] <- c(-12, -9, -5, -1, 0, 3, 7, 11, 12, 12 + 3)
chord_definitions[["minmaj7"]][["scales"]] <- list("harmonic_minor" = harmonic_minor)
chord_definitions[["minmaj7"]][["scale_weights"]] <- c(1.0)


chord_definitions[["7"]][["scales"]] <- list("mixolydian" = mixolydian,
                                             "mixolydian_sharp11" = mixolydian_sharp11,
                                             "altered" = altered,
                                             "dom_7_tetra" = dom_7_tetra,
                                             "maj_blues" = major_blues)

#chord_definitions[["7"]][["scale_weights"]] <- c(.8, .1, .1)
chord_definitions[["7"]][["scale_weights"]] <- c(0.5, 0.0, 0.0, 0.0, 0.5)
chord_definitions[["7"]][["arpeggio"]] <- c(-12, -8, -5, -2, 0, 4, 7, 10, 12 + 2)

chord_definitions[["m7b5"]][["arpeggio"]] <- c(-12, -9, -6, -2, 0, 3, 6, 10, 12 + 1, 12 + 5)
chord_definitions[["m7b5"]][["scales"]] <- list("lokrian" = lokrian,
                                                "phrygian" = phrygian,
                                                "hdim_7_tetra" = hdim_7_tetra)
#chord_definitions[["m7b5"]][["scale_weights"]] <- c(.8, .2)
chord_definitions[["m7b5"]][["scale_weights"]] <- c(0.5, 0.5, 0.0)

chord_definitions[["o7"]][["arpeggio"]] <- c(-12, -9, -6, -3, 0, 3, 6, 9, 12, 15)
chord_definitions[["o7"]][["scales"]] <- list("gtht" = gtht,
                                              "lyd#9" = lydian_aug_9,
                                              "dim_7_tetra" = dim_7_tetra,
                                              "dom_blues" = (major_blues + 6) %% 12)
#chord_definitions[["o7"]][["scale_weights"]] <- c(.8, .2)
chord_definitions[["o7"]][["scale_weights"]] <- c(1.0, 0.0, 0.0, 0.0)
chord_definitions[["o"]] <- chord_definitions[["o7"]]


chord_types <- c("minmaj7", "maj7", "min7", "min6","minb6", "min", "maj", "m7b5", "o7", "o", "7", "6")
extensions <- c("add9", "add3", "sus4", "sus",  "b5", "alt", "\\+", "9b", "9#", "9", "11#", "11", "13b", "13" )
triad_types <- c("min", "maj", "min", "min", "min", "min", "maj", "o", "o", "o", "maj", "maj")
names(triad_types) <- chord_types


cdpcx_map <- list()
cdpcx_map[["maj"]] <- c("1", "-", "2", "B", "3", "4", "T", "5", "%", "6", "<", "7")
cdpcx_map[["min"]] <- c("1", "-", "2", "3", ">", "4", "T", "5", "%", "6", "7", "L")
cdpcx_map[["dim"]] <- c("1", "-", "2", "3", ">", "4", "T", "5", "%", "6", "7", "L")
cdpcx_map[["dom"]] <- c("1", "-", "2", "B", "3", "4", "T", "5", "%", "6", "7", "L")
cdpcx_levels <- c("1", "2",  "3", "4",  "5", "6", "7", "-", "B", "%", "T",  ">", "<", "L")
chord_type_map <- c("minmaj7" = "min",
                    "maj7" = "maj",
                    "min7" = "min",
                    "min6" = "min",
                    "minb6" = "min",
                    "min" = "min",
                    "maj" = "maj",
                    "m7b5" = "min",
                    "o7" = "dim",
                    "o" = "dim",
                    "7" = "dom",
                    "6" = "maj")


get_chord_scale_summary <-function(){
  purrr::map_dfr(names(chord_definitions), function(n){
    entry <- chord_definitions[[n]]
    #browser()
    scales <- names(entry$scales)[entry$scale_weights > 0]
    scale_desc <- paste(purrr::map_chr(entry$scales[scales],
                                       function(s) sprintf("[%s]", paste(s, collapse = ", "))), collapse = ", ")
    tibble(chord_type = n,
           scales = scales %>%  paste(collapse = ", "),
           scale_desc = scale_desc)
  })
}

get_extensions <- function(chord_label){
  ret <- c()
  restore_m7b5 <- FALSE

  if(str_detect(chord_label, "m7b5")){
    restore_m7b5 <- TRUE
    chord_label <- str_remove(chord_label, "m7b5")
  }
  for(e in extensions){
    if(str_detect(chord_label, e)){
      ret <- c(ret, e)
      chord_label <- str_remove(chord_label, e)
      #printf("%s %s", paste(ret), chord_label)
    }

  }
  if(restore_m7b5){
    chord_label <- sprintf("%sm7b5", chord_label)
  }
  return(list(extensions = ret, chord_label = chord_label))
}

tone_name_to_pc <- Vectorize(function(tone){
  if(is.na(tone)) return(NA)
  if(tone == "Cb"){
    return(11L)
  }
  if(tone == "Fb"){
    return(4L)
  }
  flat_pc <- which(tone ==  parkR::labels$pc_labels_flat)
  sharp_pc <- which(tone ==  parkR::labels$pc_labels_sharp)
  if(length(flat_pc) != 0){
    pc <- flat_pc - 1
  }
  else if(length(sharp_pc) != 0){
    pc <- sharp_pc - 1
  }
  else {
    messagef("Invalid tone %s", tone)
    stop()
    return(NA)
  }
  as.integer(pc)
})

split_by_note_name <- function(chord_label){
  if(chord_label == "NC"){
    return(chord_label)
  }
  chord_label <- gsub("NC", "", chord_label)
  chord_label <- gsub("Fb", "E", chord_label)
  chord_label <- gsub("Fb", "E", chord_label)
  pos <- stringr::str_locate_all(chord_label, note_names)
  bass_pos <- stringr::str_locate_all(chord_label, "/")[[1]] %>% as_tibble() %>% mutate(start = start  + 1) %>% pull(start)
  #browser()
  pos <- purrr::map2_dfr(pos, note_names, function(x, y){
    ret <- as_tibble(x)
    if(nrow(ret) == 0) return(NULL)
    ret$note_name <- y
    ret
    } )
  if(nrow(pos) == 0){
    messagef("No note names detected")
    return(character())
  }
  start <-
    pos %>%
    arrange(start) %>%
    pull(start)
  start <- setdiff(start, bass_pos)
  if(length(start) > 1){
    end <- c(start[2:length(start)]-1, nchar(chord_label))
  }
  else{
    return(substr(chord_label, start, nchar(chord_label)))
  }
  purrr::map2_chr(start, end, function(x, y) substr(chord_label, x, y))
}

#' parse_chord
#'
#' This function parses a chord label into its constituents
#'
#' @param chord_label (character vector) chord labels to parse
#' @return data frame with columns \code{root} (tone name), \code{type} (basic chord type), \code{pc} (pitch class of root),
#' \code{bass} (extra bass note by slash chord), \code{bass_pc} (pitch class of ),
#' \code{ext} (string of comma separated extensions)
#' @export
parse_chord <- function(chord_label){
  type <- NA
  if(length(chord_label) > 1 ){
    #print((chord_label))
    return(purrr::map_dfr(chord_label, parse_chord))
  }
  if(toupper(chord_label) == "NC"){
    return(tibble(root = NA, type = "NC", pc = NA, bass = NA, bass_pc = NA, ext = ""))
  }
  #browser()
  bass <- strsplit(chord_label, "/")[[1]][2]
  chord_label <- strsplit(chord_label, "/")[[1]][1]
  chord_label <- gsub("dim7", "o7", chord_label)
  chord_label <- gsub("769", "7913", chord_label)
  chord_label <- gsub("AminD7", "Amin7", chord_label)
  chord_label <- gsub("maj7", "XXX", chord_label)
  chord_label <- gsub("j7", "XXX", chord_label)
  chord_label <- gsub("-", "YYY", chord_label)
  chord_label <- gsub("min", "YYY", chord_label)
  chord_label <- gsub("m", "YYY", chord_label)
  chord_label <- gsub("YYY", "min", chord_label)
  chord_label <- gsub("XXX", "maj7", chord_label)
  chord_label <- gsub("min7b5", "m7b5", chord_label)

  tmp <- get_extensions(chord_label)
  chord_label <- tmp$chord_label
  #browser()
  if(length(tmp$extensions) > 0 && !str_detect(chord_label, "[67]")){
    add <- str_extract("A", "[67]")
    chord_label <- sprintf("%s%s", chord_label, "7")
  }

  for(ct in chord_types){
    if(str_detect(chord_label, ct)){
      type <- ct
      chord_label <- str_remove(chord_label, type)
      #printf("%s %s", chord_label, type)
      break
    }
  }
  ext <- gsub("\\\\", "", paste0(tmp$extensions, collapse=","))
  if(is.na(type)) {
    if(!is.na(tone_name_to_pc(chord_label))){
      type <- "maj"
    }
    else {
      messagef("Parsing %s", chord_label)
      messagef("Unknown chord type in %s", chord_label)
      stop()
      return(NULL)
    }
  }
  pc <- tone_name_to_pc(chord_label)
  bass_pc <- tone_name_to_pc(bass)

  return(tibble(root = chord_label, type = type, pc = pc, root_pc = pc, bass = bass, bass_pc = bass_pc, ext = ext))
}
#' fast_parse_chord
#'
#' This function parses a chord label into its constituents as parse_chord, but optimized for speed for long vectors of chord labels
#'
#' @param chord_label (character vector) chord labels to parse
#' @return data frame with columns \code{root} (tone name), \code{type} (basic chord type), \code{pc} (pitch class of root),
#' \code{bass} (extra bass note by slash chord), \code{bass_pc} (pitch class of ),
#' \code{ext} (string of comma separated extensions)
#' @export
fast_parse_chord <- function(chord_label){
  chords <- unique(chord_label)
  parsed_chords <- parse_chord(chords) %>% mutate(chord = chords)
  tibble(chord = chord_label) %>% left_join(parsed_chords, by = "chord") %>% select(-chord)
}

chord_to_lilypond <- function(chord_labels, lengths = NULL){

  chord <- fast_parse_chord(chord_labels)
  roots <- str_replace_all(chord$root, "b", "es") %>%  str_replace_all("#", "is") %>% tolower() %>% str_replace("ees", "es")
  if(is.null(lengths)){
    chords <- sprintf("%s:%s", roots,  chord$type)

  }
  else {
    chords <- sprintf("%s%d:%s", roots, as.integer(floor(lengths)), chord$type)
  }
  chords
}
#' get_cdpcx
#'
#' This function transforms a set of (MIDI) pitches given a list of chords into the corresponding Extended Chordal Diatonic Pitch Classes (CDPCX)
#'
#' @param pitches (integer vector) pitches to transform
#' @param chord (character vector or tibble of parsed chords) chords to be used for calculating CDPCX classes, length must either match or it must be a single chord label
#' @return character vector of CDPCX values. (1-7 diatonic steps, - = b9, \% = b13, B = #9 blues third, T = tritone (#11), > major third over minor chord, < = minor seventh over major 7th chord, L = major seventh over chord with minor seventh
#' @export
get_cdpcx <- function(pitches, chord){
  #print(chord)
  if(length(chord) == 1){
    chord <- rep(chord, length(pitches))
  }
  else{
    if(length(chord) != length(pitches)){
      stop("Length of chords should be one or match length of pitches")
      }
  }
  if(all(is.character(chord))){
    chord <- fast_parse_chord(chord)
  }

  cpc <- (pitches - chord$pc) %% 12
  diatonic_type <- chord_type_map[chord$type]
  cpc[is.na(cpc)] <- 12

  cdpcx <- map2_chr(diatonic_type, cpc, ~{
    c(cdpcx_map[[.x]], NA)[.y + 1]
    })
  cdpcx

}

spread_pitches <- function(pc_set, root, min_pitch = 48, max_pitch = 84){
  tmp <- purrr::map(12*(-5:6), function(x) sort(root + x + pc_set)) %>% unlist
  tmp[tmp >= min_pitch & tmp <= max_pitch]
}

find_best_pitchset_match <- function(int_vector, start_pitch, pitch_vector, max_transposition = 2){
  if(is.character(int_vector)){
    int_vector <- value_to_vec(int_vector)
  }
  max_w <- 0
  ret <- NULL
  best_trans <- NA
  #print(pitch_vector)
  for(i in seq(-max_transposition, max_transposition)){
    #printf("==== Testing transposition %d ====", i)
    test_set <- cumsum(c(start_pitch+i, int_vector))
    #print(test_set)
    w <- length(intersect(pitch_vector, test_set))/length(test_set)
    #print(w)
    if(w >= max_w){
      max_w <- w
      ret <- test_set
      best_trans <- i
    }
  }
  #print(sprintf("Trans :%s, max_w: %.2f", best_trans, max_w))
  list(pitch_set = ret, trans = best_trans, match = max_w)
}

find_best_match_to_chord <- function(int_vector, start_pitch, chord, max_transposition = 2){
  #print(int_vector)
  #browser()
  if(is.character(int_vector)){
    int_vector <- value_to_vec(int_vector)
  }
  tmp <- get_scale_pitches(chord, sample = F, return_weights = T)
  if(is.null(tmp)){
    return(integer(0))
  }
  scales <- tmp$scales
  scale_weights <- tmp$weights
  best_trans <- max_transposition + 1
  pitch_set <- NULL
  best_match <- 0
  best_scale <- NA
  for(i in 1:length(scales)){
    #printf("=== Testing %s  ===", names(scales)[[i]])
    w <- scale_weights[[i]]
    tmp <- find_best_pitchset_match(int_vector, start_pitch,  pitch_vector = scales[[i]])
    tmp$match <- tmp$match * w
    #cat("Trans", tmp$trans, "Match", tmp$match, "\n")
    if(tmp$match >= best_match && abs(tmp$trans) < abs(best_trans)){
      best_match <- tmp$match
      best_trans <- tmp$trans
      best_scale <- names(scales)[[i]]
      #cat("Best trans", best_trans, "best_match", best_match, "BEst scale", best_scale,"\n")
    }
  }
  #cat("Final trans", best_trans, "FInal match", best_match, "Final scale", best_scale,"\n")
  cumsum(c(start_pitch + best_trans, int_vector))
}

weighted_sample <- function(sample_set, weights, size = 1){
  w <- as.integer(round(weights / sum(weights) * 100))
  #print(w)
  #print(sample_set)
  if(is.null(sample_set)){
    browser()
  }
  if(is.null(names(sample_set))){
    names(sample_set) <- 1: length(sample_set)
  }
  if(length(sample_set) != length(w)){
    browser()
  }
  tmp <- mapply(rep, names(sample_set), w) %>% unlist()
  #print(tmp)
  selected <- sample(tmp, size, replace = T)
  #print(selected)
  sample_set[[selected]]
}

get_scale_pitches <- function(chord, min_pitch = 48, max_pitch = 84, sample = T, return_weights = F){
  if(length(chord) > 1){
    return(lapply(chord, get_scale_pitches, min_pitch, max_pitch))
  }
  if(is.character(chord)){
    chord <- parse_chord(chord)
    if(is.null(chord) || nrow(chord) == 0 ){
      return(NULL)
    }
  }
  if(chord$type == "NC"){
    message("[get_scale_pitches] Found NC chord")
    return(NULL)
  }

  #print(chord)
  for(i in 1:nrow(chord)){
    ct <- chord[i, ]$type
    root <- chord[i,]$pc
    #print(sprintf("CT %s, root %d", ct, root))
    scales <- lapply(chord_definitions[[ct]]$scales,
                     function(x) spread_pitches(x, root, min_pitch, max_pitch))
    #print(chord_definitions[[ct]]$scales)
    names(scales) <- names(chord_definitions[[ct]]$scales)
  }
  if(sample){
    selected <- weighted_sample(names(scales), chord_definitions[[ct]]$scale_weights)
    #print(sprintf("Selected scale: %s" , selected))
    return(scales[[selected]])
    #selected
  }
  if(return_weights){
    return(list(scales = scales, weights = chord_definitions[[ct]]$scale_weights))
  }
  scales
}

get_arpeggio_pitches <- function(chord, min_pitch = 48, max_pitch = 84){
  if(length(chord) > 1){
    return(lapply(chord, get_arpeggio_pitches, min_pitch, max_pitch))
  }
  if(is.character(chord)){
    chord <- parse_chord(chord)
    if(is.null(chord) || nrow(chord) == 0 ){
      return(NULL)
    }
  }
  for(i in 1:nrow(chord)){
    ct <- chord[i, ]$type
    root <- chord[i,]$pc
    #print(sprintf("CT %s, root %d", ct, root))
    arpeggio <- lapply(chord_definitions[[ct]]$arpeggio,
                     function(x) spread_pitches(x, root, min_pitch, max_pitch))
  }
  arpeggio
  #selected <- weighted_sample(names(scales), chord_definitions[[ct]]$scale_weights)
  #print(sprintf("Selected scale: %s" , selected))
  #scales[[selected]]
  #selected
}

get_base_arpeggio_pc <- function(chord, n = NULL){
  pitches <- get_arpeggio_pitches(chord) %>% lapply(function(x) x %% 12) %>% unlist() %>% unique()
  if(!is.null(n)){
    n <- min(max(n, 0), length(pitches))
    pitches <- pitches[1:n]
  }
  pitches
}

get_common_scale <- function(chord_list, n = NULL, remove_duplicates = T ){
  pcs <- purrr::map(chord_list, get_base_arpeggio_pc, n) %>% unlist()
  if(remove_duplicates) pcs <- unique(pcs)
  pcs
}

shift_pc_set <- function(pc_set, shift){
  (pc_set + shift) %% 12
}

find_best_matching_scale <- function(pitch_set, root = pitch_set[1]){
  if(is.list(pitch_set)){
    #print((pitch_set))
    pitch_set <- purrr::reduce(pitch_set, union)
    #print(sort(pitch_set))
  }
  candidates <- purrr::map_int(scales, function(x) length(intersect(shift_pc_set(x, root), pitch_set))) %>% sort(decreasing = T)
  best = candidates[candidates == max(candidates)]
  list(raw = candidates,
       best = best,
       rel_match = as.numeric(best[1]/length(unique(pitch_set)))
  )
}

unroll_durations <- function(durations){
  tmp <- cumsum(durations)
  tmp <- c(0, tmp[1:(length(tmp)-1)])
  tmp
}

#' create_leadhseet_chord_db
#'
#' This function generates a lead sheet data from parsed chord data frame (irb or wjd_chord_db),
#'
#' @param db (chord data frame) Chord data frame to use
#' @param compid (integer or character scalar) If integer, then id of lead sheet in the chord data, if character, than title of song
#' @param name (character scalar) Currently unused
#' @param with_form (logical scalar) Flag if form shall be added
#' @return A lead sheet data frame
#' @export
create_leadsheet_from_chord_db <- function(db, compid, name = NULL, with_form = F){
  if(is.character(compid)){
    sheet <- db %>% filter(tolower(title) == tolower(!!compid)) %>%
      select(chord, duration, section, title, time, composer, date, compid, key)
  }
  else {
    sheet <- db %>% filter(compid == !!compid) %>% select(chord, duration, title, time, section, key)
  }
  if(nrow(sheet) == 1){
    return(NULL)
  }
  if(!nzchar(sheet$time[1])){
    time <- c("4", "4")
  }
  else{
    time <- strsplit(sheet$time[1], "/")[[1]]

  }
  #browser()
  period <- as.integer(time[1])
  denom <- as.integer(time[2])
  if(period == 6 && denom == 8){
    sheet$duration <- sheet$duration * 3/2
    period <- 3
    denom <- 4
  }
  ticks_per_beat <- 4/(denom/4)
  sheet <-
    sheet %>%
    mutate(length_ticks = duration * ticks_per_beat) %>%
    mutate(parsed = purrr::map(chord, parse_chord)) %>%
    tidyr::unnest(cols = parsed)
  sheet$onset_ticks <- unroll_durations(sheet$length_ticks)
  sheet$running_beat <- unroll_durations(sheet$duration)
  sheet$beat <- (sheet$running_beat %% period ) + 1
  sheet$bar <- floor(sheet$running_beat/period) + 1
  last_beat <- last_n(sheet$beat)
  last_dur <- last_n(sheet$duration)
  if((last_beat + last_dur) <= period){
    printf("Fixed last duration %d -> %d", last_dur, period - last_beat + 1)
    sheet[nrow(sheet),]$duration <- period - last_beat + 1

  }
  if(with_form){
    sheet$form <- form_from_sheet(sheet)
  }
  sheet %>% set_format("lead_sheet")
}


#' create_leadsheet_from_irb
#'
#' This function generates a lead sheet data from parsed iRealBook data (irb),
#'
#' @param compid (integer or character scalar) If integer, then id of lead sheet in the iRealBook data (data(irb)), if character, than title of song
#' @param name (character scalar) Currently unused
#' @param with_form (logical scalar) Flag if form shall be added
#' @return A leed sheet data frame
#' @export
create_leadsheet_from_irb <- function(compid, name = NULL, with_form = F){
  create_leadsheet_from_chord_db(parkR::irb, compid, name) %>% mutate(compid = compid)
}

#' create_leadsheet_from_wjd_db
#'
#' This function generates a lead sheet data from parsed WJD chord data (wjd_chord_db),
#'
#' @param compid (integer or character scalar) If integer, then id of lead sheet in the iRealBook data (data(irb)), if character, than title of song
#' @param name (character scalar) Currently unused
#' @param with_form (logical scalar) Flag if form shall be added
#' @return A leed sheet data frame
#' @export
create_leadsheet_from_wjd_db <- function(compid, name = NULL, with_form = F){
  if(!requireNamespace("jazzodata", quietly = T)){
    stop("Creating leadsheets von WJD requires 'jazzodata' package")
  }
  db <- parkR::wjd_chord_db %>%
    rename(chord = original, section = form_name) %>%
    left_join(jazzodata::wjd_meta %>%
                select(title, id, composer, date = recordingyear, key, time = signature), by = "id")
  create_leadsheet_from_chord_db(db, compid, name, with_form)
}

#' create_leadsheet
#'
#' This function generates a lead sheet data frame from a vector of chord symbols and beat lengths per chord
#'
#' @param name (character scalar) Currently unused
#' @param chords (logical scalar) Flag if form shall be added
#' @param length_beats (integer scalar) length of chord in beats
#' @return A leed sheet data frame
#' @export
create_leadsheet <- function(name, chords, length_beats){
  tmp <- tibble(chord = chords,
                  length_beats = length_beats)
  tmp <- tmp %>% mutate(length_ticks = length_beats * 4) %>%
    mutate(parsed = purrr::map(chord, parse_chord)) %>%
    tidyr::unnest(cols = c(parsed))
  tmp$onset_ticks <- cumsum(tmp$length_ticks)
  tmp$onset_ticks <- tmp$onset_ticks- tmp$onset_ticks[1]
  tmp$beat <- (floor(tmp$onset_ticks/4) %% 4 ) + 1
  tmp$bar <- floor(tmp$onset_ticks/16) + 1
  tmp %>% set_format("lead_sheet")
}

#' simulate_wjd
#'
#' This function simulates a full Weimar Jazz Database, based on lead sheet, chorus counts and tempo
#'
#' @param ids (integer vector) List of ids to create, ids shoudl be in the range 1 to 456, everything else will be ignored
#' @return A data frame of generated solos
#' @export
simulate_wjd <- function(ids = 1:456){
  wjd_meta <- jazzodata::wjd_meta %>% mutate(compid = as.integer(factor(id)))
  ids <- as.integer(ids)
  ids <- ids[ids >= 1  & ids <= 456 ]
  map_dfr(ids, function(cid) {
    #messagef("Generating %d...", cid)
    sheet <- create_leadsheet_from_wjd_db(compid = cid)
    if(!is.null(sheet)){
      md <- wjd_meta %>% filter(compid == cid)
      message(sprintf("(%d): Generating %s with %d choruses", cid, md$id, md$chorus_count))
      solo <- generate_solo(sheet, n_chorus = md$chorus_count, tempo = round(md$avgtempo))
      if(!is.null(solo)){
      solo <- solo %>%
        mutate(id = md$id,
               # compid = md$compid,
               # title = md$title,
               # n_chorus = md$chorus_count,
               # tempo = round(md$avgtempo),
               .before = 1)
      }
      solo
    }
    })
}

parse_wjd_bar <- function(bar_chords, bar_number = 1){
  #print(bar_chords)
  elements <- strsplit(bar_chords, " ")[[1]]
  elements <- elements[nchar(elements) > 0]
  #browser()
  elements <- purrr::map(elements, split_by_note_name)  %>% unlist()
  l <- length(elements)
  #print(l)
  if(l < 1 || l > 4){
    messagef("Invalid bar found: |%s|", bar_chords)
    stop()
  }
  duration <- list(4, c(2, 2), c(2, 1, 1), c(1, 1, 1, 1))[[l]]
  beat_pos <- list(1, c(1, 3), c(1, 3, 4), c(1, 2, 3, 4))[[l]]
  #browser()
  chords <- parse_chord(elements)
  chords$original <- elements
  chords$duration <- duration
  chords$beat_pos <- beat_pos
  chords$bar_number <- bar_number
  chords
}

parse_wjd_form_part <- function(form_part){
  #browser()
  elements <- strsplit(form_part, ":")[[1]]
  form_name <- elements[1]
  chords <- elements[2]
  chords <- gsub("||", "", chords, fixed = T)
  elements <- strsplit(chords, "\\|")[[1]]  %>% trimws()
  if(any(nchar(elements) == 0)){
    return(NULL)
  }
  parsed <- purrr::map2_dfr(elements, 1:length(elements), parse_wjd_bar)
  parsed$form_name <- form_name

  parsed
}

split_wjd_changes <- function(changes, id = ""){
  messagef("Splitting chords for %s", id)
  #browser()
  lines <- strsplit(changes, "\n")[[1]]
  parts <- purrr::map_dfr(lines, parse_wjd_form_part)
  #browser()
  offsets <-
    parts %>%
    group_by(form_name) %>%
    mutate(form_length = max(bar_number)) %>%
    ungroup() %>%
    distinct(form_name, form_length) %>%
    mutate(bar_offset = dplyr::lag(cumsum(form_length), default = 0))
  parts <-
    parts %>%
    left_join(offsets, by = "form_name") %>%
    mutate(bar_in_form = bar_number,
           bar_number  = bar_number + bar_offset) %>%
    select(-bar_offset) %>%
    mutate(id = id)
  #browser()
  parts

}

signed_mod <- function(x, n = 12){
  #print(x)
  if(length(x) == 0 || is.null(x) || all(is.na(x))){
    return(x)
  }
  r <- x %% n
  #browser()
  r[which(r > n/2)]  <- r[which(r > n/2)] - n
  r
}

create_sheet_from_wjd_changes <- function(changes, form_parts = NULL){
  purrr::map2_dfr(changes$chord_changes,
           changes$id,
           split_wjd_changes) %>%
    mutate(root_pc =  map_int(root, tone_name_to_pc)) %>%
    group_by(id) %>%
    mutate(root_movement = as.integer(c(signed_mod(diff(root_pc), 12), NA)),
           root_movement_to = as.integer(c(NA, signed_mod(diff(root_pc), 12))),
           next_chord = lead(original),
           next_type = lead(type),
           type_transition = sprintf("%d:%s->%s", root_movement, type, lead(type)),
           chord_transition = sprintf("%d:%s->%s", root_movement, original, lead(original)),
           pseudo_transition = original == lead(original)) %>%
    ungroup() %>%
    left_join(changes, by ="id")

}

#' export
get_random_chord <- function(chord_db = parkR::irb){
  if("root" %in% names(chord_db)){
    sprintf("%s%s", sample(chord_db$root, size = 1), sample(chord_db$type, size = 1))
  }
  else{
    sample(chord_db$chord, 1)
  }
}

expand_chord_changes <- function(split_changes,
                                 num_choruses = 1,
                                 max_bar = NULL,
                                 with_key_analysis = T,
                                 bars_in_solo = NULL){
  n_ids <- length(unique(split_changes$id))
  messagef("***Expanding %d chord changes", n_ids)
  browser()
  if(n_ids > 1){
    return(purrr::map_dfr(unique(split_changes$id), function(i){
      #messagef("### Recursive for %s", i)
      tmp <- expand_chord_changes(split_changes %>% filter(id == i))
      #messagef("### Recursive return for %s", i)
      tmp
    }))
  }
  if(nrow(split_changes) == 1){
    message("Single chord song detected")
    return(NULL)
  }
  chorus_length <- max(split_changes$bar_number)
  if(!is.null(max_bar)) {
    num_choruses <- floor(max_bar/chorus_length)
    if(max_bar/chorus_length != as.integer(max_bar/chorus_length)){
      num_choruses <- num_choruses + 1
    }
  }
  if(with_key_analysis){
    #browser()
    messagef("Enter key analysis (%d, %s)", length(unique(split_changes$id)), unique(split_changes$id))
    #print()
    ka <- key_analysis(chord_stream = split_changes$original)
    if(sum(ka$unique) == nrow(ka)){
      split_changes <- bind_cols(split_changes, ka %>% select(local_key, local_scale_degree, tonic, tonic_pc))
    }
    else{
      messagef("Could not find unique key analysis")
    }
  }

  one_chorus <-
    purrr::map_dfr(1:nrow(split_changes), function(row_id){
      t <- split_changes[row_id,]
      purrr::map_dfr(seq(t$beat_pos, t$beat_pos + t$duration - 1), function(b){
        t %>% select(-beat_pos) %>%  mutate(beat_pos = b)
      })
    })
  multi_choruses <- purrr::map_dfr(1:num_choruses, function(nc){
    one_chorus %>% mutate(bar_number = bar_number + (nc - 1) * chorus_length, chorus_id = nc)
  })
  if(!is.null(max_bar)) {
    multi_choruses <-   multi_choruses %>% filter(bar_number <= max_bar)
  }
  multi_choruses
}

create_wjd_local_key_annotations <- function(data = jazzodata::wjd_meta){
  ret <-
    map_dfr(unique(data$id), function(i){
    tmp <- data %>% filter(id == i)
    browser()
    parkR:::create_expanded_wjd_chord(tmp %>% select(id, chord_changes),
                                      num_choruses = tmp %>% pull(chorus_count),
                                      with_key_analysis = T)

  })
  browser()
  if(nrow(ret) == 0){
    messagef("Invalid expand")
    return(NULL)
  }
  ret %>%
    mutate(melid = as.integer(factor(id, levels = unique(data$id))),
           beat_id = sprintf("%s_%s_%s", melid, bar_number, beat_pos))

}
create_expanded_wjd_chord <- function(changes, num_choruses = NULL, max_bar = NULL, with_key_analysis  = F ){
  purrr::map2_dfr(changes$chord_changes,
           changes$id,
           split_wjd_changes) %>%
    left_join(changes, by ="id") %>%
    expand_chord_changes(num_choruses = num_choruses, max_bar = max_bar, with_key_analysis = with_key_analysis)

}

create_wjd_lead_sheet <- function(changes){
  purrr::map2_dfr(changes$chord_changes,
           changes$id,
           split_wjd_changes) %>%
    left_join(changes, by ="id")
}
