convert_scale_degree <- function(roman_number){
  roman_number <- tolower(roman_number)
  map(scale_degrees[["nomr"]])
}


scale_degrees <- list("norm" = c("I", "IIb", "II","IIIb", "III", "IV","IV#", "V", "VIb", "VI", "VIIb", "VII"),
                      "maj" = c("I", "IIb", "ii", "iii", "IV", "IV#", "V", "VIb", "vi", "VIIb", "viio"),
                      "min" = c("i", "IIb", "iio", "IIIb", "iv", "IV#", "V", "VIb", "vio", "VIIb", "viio"))
usethis::use_data(scale_degrees, overwrite = TRUE)

get_triad_type <- function(chord_type){
  triad_types[chord_type]
}

#' get_scale_degree_code
#'
#' @param chord_label (string) Valid chord symbol
#' @param key (string) valid key symbol
#' @param as_df (boolean) flag whether to return a data.frame or string vector
#'
#' @return
#' @export
get_scale_degree_code <- function(chord_label, key, as_df = FALSE){
    #browser()
  if(length(chord_label) > 1){
    ret <- map_chr(chord_label, function(ch) get_scale_degree_code(ch, key, as_df = as_df))
    return(ret)
  }

  key_letter <- str_split_fixed(key, "-", 2)[,1] %>% as.vector()
  key_pc <- str_split_fixed(key, "-", 2)[,1] %>% tone_name_to_pc() %>% as.vector()
  mode <- str_split_fixed(key, "-", 2)[,2] %>% as.vector()

  if(is.character(chord_label)){
    chord <- parse_chord(chord_label)
  }
  else{
    chord <- chord_label
  }
  ctype <- chord$type[1]
  pc_rel = (chord$pc[1] - key_pc) %% 12
  triad_type <- get_triad_type(ctype)
  scale_degree <- scale_degrees[["norm"]][pc_rel+1]
  if(triad_type == "min"){
    scale_degree <- tolower(scale_degree)
  }
  if(ctype == "m7b5"){
    scale_degree <- sprintf("%sm7b5", tolower(scale_degree))

  }
  else {
    if(str_detect(ctype, "7")){
      scale_degree <- sprintf("%s%s", scale_degree, ctype)
      scale_degree <- str_remove(scale_degree, "min")
      scale_degree <- str_remove(scale_degree, "ma")
    }
    else{
      if(triad_type == "o"){
        scale_degree <- tolower(scale_degree)
        scale_degree <- sprintf("%so", scale_degree)
      }

    }
  }
  if(as_df){
    tibble(tonic = key_letter, scale_degree = scale_degree)
  }
  else{
    sprintf("%s: %s", key_letter, scale_degree)
  }
}

add_key <- function(keys, mode, chord, offset = 0){
  if(mode == "X"){
    keys <- add_key(keys, "maj", chord, offset)
    keys <- add_key(keys, "min", chord, offset)
    return(keys)
  }
  if(offset == 0){
    key <- sprintf("%s-%s", chord$root[1], mode)
  }
  else{
    key <- sprintf("%s-%s", labels$pc_labels_flat[((chord$pc[1]  + offset) %% 12)  + 1], mode)
  }
  c(keys, key)
}

get_intrinsic_scale_degree <- function(chord_label, chord_pos = NULL){
  #browser()
  #print(chord_label)
  if(length(chord_label) > 1){
    ret <-
      map_dfr(1:length(chord_label), function(j){
        get_intrinsic_scale_degree(chord_label[j]) %>% mutate(chord_pos = j)
      })
    return(ret)
  }
  if(is.character(chord_label)){
    chord <- parse_chord(chord_label)
  }
  else{
    chord <- chord_label
  }
  ctype <- chord$type[1]
  keys <- c()
  #browser()
  if(ctype == "minmaj7"){
    keys <- add_key(keys, "min", chord)
  }
  if(ctype == "maj7" || ctype == "maj"){
    keys <- add_key(keys, "maj", chord)#Ij7
    keys <- add_key(keys, "maj", chord, offset = 7)#IVj7
    keys <- add_key(keys, "min", chord, offset = 4)#VIbj7
  }
  if(ctype == "maj"){
    keys <- add_key(keys, "X", chord, offset = 5)#V
    keys <- add_key(keys, "X", chord, offset = +2)#VIIb
  }
  if(ctype == "min7" || ctype == "min"){
    keys <- add_key(keys, "min", chord)
    keys <- add_key(keys, "maj", chord, offset = -2)#ii
    keys <- add_key(keys, "maj", chord, offset = +3)#vi
    keys <- add_key(keys, "min", chord, offset = -5)#iv
    keys <- add_key(keys, "maj", chord, offset = -4)#iii
    #keys <- add_key(keys, "blues", chord, offset = -2)#ii
  }
  if(ctype == "min6"){
    keys <- add_key(keys, "min", chord, offset = +7)#iiv6
    keys <- add_key(keys, "min", chord, offset = 0)#i6
  }
  if(ctype == "minb6"){
    keys <- add_key(keys, "min", chord, offset = 0)#ib6
    keys <- add_key(keys, "maj", chord, offset = -4)#I3
  }
  if(ctype == "m7b5"){
    keys <- add_key(keys, "min", chord, offset = -2)#ii7b5
  }
  if(ctype == "o7" || ctype == "o"){
    keys <- add_key(keys, "X", chord, offset = +1)
    keys <- add_key(keys, "X", chord, offset = +4)
    keys <- add_key(keys, "X", chord, offset = +7)
    keys <- add_key(keys, "X", chord, offset = +10)
    keys <- add_key(keys, "min", chord, offset = -1)
    keys <- add_key(keys, "maj", chord, offset = 0)
  }
  if(ctype == "7"){
    keys <- add_key(keys, "X", chord, offset = +5)
    keys <- add_key(keys, "X", chord, offset = -1)
    keys <- add_key(keys, "blues", chord, offset = 0)
    keys <- add_key(keys, "blues", chord, offset = -5)
    keys <- add_key(keys, "maj", chord, offset = 1)
  }
  if(ctype == "6"){
    keys <- add_key(keys, "maj", chord)#I6
  }
  if(length(keys) == 0){
    ret <- tibble(chord = "NC", key = NA,
           tonic = NA, scale_degree = NA,
           chord_root = NA,
           chord_type = "NC",
           chord_pc = NA,
           num_candidates = 0, tonic_pc = NA) %>%
           mutate(cand_id = 1:n(), chord_pos = !!chord_pos)
    return(ret)
  }
  map_dfr(keys, function(k){
    sc <- get_scale_degree_code(chord_label, k, as_df = T)
    bind_cols(tibble(chord = chord_label, key = k),
              sc,
              chord %>% select(chord_root = root, chord_type = type, chord_pc = pc),
              num_candidates = length(keys)) %>%
      mutate(tonic_pc = tone_name_to_pc(sc$tonic))
  }) %>% mutate( cand_id = 1:n(), chord_pos = !!chord_pos)
}

fuse_tonics <- function(chord_candidates){
  chord_pos <- unique(chord_candidates$chord_pos)
  max_pos <- max(chord_pos)

  map_dfr(min(chord_pos):(max(chord_pos)), function(cn){
    #browser()
    cand1 <- chord_candidates %>% filter(chord_pos == cn)  %>% pull(key)
    if (cn == max_pos){
      return(tibble(chord_pos = cn, key =  cand1[1], id = 1))
    }
    cand2 <- chord_candidates %>% filter(chord_pos == (cn + 1)) %>% pull(key)
    inter <- intersect(cand1, cand2)

    if(length(inter) == 0){
      return(tibble(chord_pos = cn, key =  cand1[1], id = 1))
    }
    bind_rows(
      tibble(chord_pos = cn, key =  inter, id = 1:length(inter)),
      tibble(chord_pos = cn + 1, key =  inter, id = 1:length(inter)))
  })
}

fuse_tonics2 <- function(chord_candidates){
  chord_pos <- unique(chord_candidates$chord_pos)
  max_pos <- max(chord_pos)
  map_dfr(min(chord_pos):max_pos, function(cn){
    cand1 <- chord_candidates %>% filter(chord_pos == cn)  %>% pull(key)
    if(cn == max_pos-1){
      cand2 <- chord_candidates %>% filter(chord_pos == (cn + 1)) %>% pull(key)
      inter <- intersect(cand1, cand2)
      if(length(inter) > 0){
       ret <- bind_rows(
         tibble(chord_pos = cn, key =  inter, id = 1:length(inter)),
         tibble(chord_pos = cn + 1, key =  inter, id = 1:length(inter)))
       return(ret)
      }
    }
    else if (cn == max_pos){
      return(tibble(chord_pos = cn, key =  cand1[1], id = 1))
    }
    cand2 <- chord_candidates %>% filter(chord_pos == (cn + 1)) %>% pull(key)
    cand3 <- chord_candidates %>% filter(chord_pos == (cn + 2)) %>% pull(key)
    inter <- intersect(cand1, intersect(cand2, cand3))

    if(length(inter) == 0){
      return(tibble(chord_pos = cn, key =  cand1[1], id = 1))
    }
    #browser()
    bind_rows(
      tibble(chord_pos = cn, key =  inter, id = 1:length(inter)),
      tibble(chord_pos = cn + 1, key =  inter, id = 1:length(inter)),
      tibble(chord_pos = cn + 2, key =  inter, id = 1:length(inter)))
  })
}

update_candidates <- function(key_analysis){
  key_analysis %>%
    group_by(chord_pos) %>%
    mutate(num_candidates = n(), cand_id = 1:n(), unique = num_candidates == 1) %>%
    ungroup() %>%
    arrange(chord_pos, cand_id)
}

print_analysis <- function(key_analysis){
  seq <- key_analysis %>% mutate(scale_annotation = sprintf("[(%s)%s: %s]", chord, tonic, scale_degree)) %>%
                            group_by(cand_id) %>%
                            summarise(analysis_sequence = paste(scale_annotation, collapse = " -> ")) %>%
    pull(analysis_sequence)
  printf("Tune: %s", key_analysis$title[1])
  print(seq)
}

uniquify_sequence <- function(key_analysis){
  #browser()
  non_uniques <- sum(!key_analysis$unique)
  if(non_uniques == 0){
    return(key_analysis)
  }
  if(sum(!key_analysis$unique) < nrow(key_analysis)){
    sections <- split_key_analysis_by_unique(key_analysis)
    if(nrow(sections) == 0){
      messagef("Nothing to unify, returning object")
      return(key_analysis)
    }
    ret <-
      map_dfr(1:nrow(sections), function(i){
        start <- sections[i,]$start
        end <- sections[i, ]$end
        #browser()
        unique_section <- sum(key_analysis[start:end,]$unique) > 0
        if(unique_section){
          ret <- key_analysis[start:end,]
        }
        else{
          ret <- uniquify_sequence(key_analysis[start:end,])
        }
        if(nrow(ret) == 0){
          browser()
          messagef("Uniquify failed")
          return(key_analysis[start:end,])
        }
        return(ret %>% update_candidates())
      })
    if(!sanity_check_key_analysis(ret, "Uniquify", as_bool = T)){
      browser()
    }
    return(ret %>% arrange(chord_pos, key))
  }
  #browser()
  key_analysis <- key_analysis %>%
    distinct(key, chord, chord_pos, .keep_all = T)  %>%
    mutate(has_tonic = rel_pc == 0)

  chord_count <-  n_distinct(key_analysis$chord_pos)

  tonic_stats <-
    key_analysis %>% group_by(key) %>% summarise(num_tonics = sum(rel_pc == 0), .groups = "drop")

  key_stats <- key_analysis%>%
    group_by(key) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(full_coverage = n == chord_count,
           is_major = str_detect(key, "maj")) %>%
    left_join(tonic_stats, by = "key") %>%
    arrange(desc(n), desc(full_coverage), desc(num_tonics), desc(is_major)) %>%
    mutate(cs = cumsum(n))

  stopifnot(nrow(key_stats) > 0)
  #browser()
  keys <- key_stats[1,]$key
  if(length(keys) == 0){
    browser()
    return(key_analysis)
  }
  key_analysis <- key_analysis %>%
    group_by(chord_pos) %>%
    mutate(uncovered = !any(key %in% !!keys)) %>%
    ungroup() %>%
    filter((key %in% keys) | uncovered) %>%
    select(-uncovered) %>%
    update_candidates()
  key_analysis
}

split_key_analysis_by_unique <- function(key_analysis){
  starts <-  c(1, rle(key_analysis$unique)$length) %>% cumsum()
  starts <- starts[1:(length(starts)-1)]
  ends <- rle(key_analysis$unique)$length %>% cumsum()
  tibble(start = starts, end = ends, len = end-starts)
}

sanity_check_key_analysis <- function(ka, label  = "", as_bool = F){
  if(any(is.na(ka$chord)) || any(diff(unique(ka$chord_pos))>1)){
    messagef("%s: Fail", label)
    #browser()
    if(as_bool)return(FALSE)
    return(ka)
  }
  if(as_bool) return(TRUE)
  return(ka)
}

get_reduction_factor <- function(key_analysis){
  num_chords <- max(key_analysis$chord_pos)
  num_keys <- n_distinct(key_analysis$key)
  rle_keys <- rle(key_analysis$key)$lengths %>% length()
  rle_mean_len <- rle(key_analysis$key)$lengths %>% mean()
  tibble(num_chords = num_chords,
         num_keys = num_keys,
         rle_keys = rle_keys,
         rle_mean_len = rle_mean_len,
         red = num_keys/num_chords,
         red_rle = rle_keys/num_chords,
         unique_ratio = mean(key_analysis$unique)
         )
}

find_II_V <- function(intrinsic_scale_degrees){
  isc <- intrinsic_scale_degrees %>% mutate(row_id = 1:nrow(intrinsic_scale_degrees))
  chord_pos <- unique(isc$chord_pos) %>% sort()
  chord_pos <- chord_pos[1:(length(chord_pos)-1)]
  isc$keep <- TRUE
  for(cp in chord_pos){
    #browser()
    supertonic <- isc %>%
      filter(chord_pos == cp) %>%
      filter(scale_degree %in% c("ii7", "ii*7"))
    if(nrow(supertonic) > 0){
      key_cand <- supertonic$key[1]
      fifth <- isc %>%
        filter(chord_pos == cp + 1) %>%
        filter(scale_degree %in% c("V7", "V", "IIb", "IIb7"), key == key_cand)
      if(nrow(fifth) > 0){
        #tibble(chord_pos = c(cp, cp + 1), cand_id = c(supertonic$cand_id[1], fifth$cand_id[1]))
        isc[isc$chord_pos == cp    & isc$cand_id != supertonic$cand_id[1], ]$keep <- FALSE
        isc[isc$chord_pos == cp +1 & isc$cand_id != fifth$cand_id[1], ]$keep <- FALSE
      }
    }
  }
  #browser()
  isc %>% filter(keep) %>% select(-keep) %>% update_candidates()
}

single_key_analysis <- function(chord_stream, fuse_length = 3){
  if(fuse_length == 3){
    cs <- chord_stream  %>%
      fuse_tonics2()
  }
  else{
    cs <- chord_stream  %>%
      fuse_tonics()
  }
  cs %>%
    distinct(.keep_all = T) %>%
    left_join(chord_stream, by = c("key", "chord_pos")) %>%
    distinct(.keep_all = T) %>%
    update_candidates() %>%
    mutate(rel_pc = (chord_pc - tonic_pc) %% 12) %>%
    arrange(chord_pos, key) %>%
    sanity_check_key_analysis(label = "Check point 1") %>%
    uniquify_sequence() %>%
    sanity_check_key_analysis(label = "Check point 2")

}
#' key_analysis
#'
#' This function parses the \code{chord} column from a lead sheet data frame or a vector of chord labels
#' into local keys and corresponding
#' local scale degree annotations (using Roman numerals)
#'
#' @param lead_sheet (data frame of class lead sheer) A lead sheet data frame or NULL, if NULL \code{chord_stream} is used
#' @param chord_stream (character string of chord symbols) A sequence of chords or NULL, if NULL \code{lead_sheet} is used, if both are NULL then an error is issued
#' @param remove_reps (logical scalar) Flag if repeated chords shall removed (default is \code{TRUE}). Currently disabled.
#' @param add_metadata (logical scalar) Flag if (lead sheet) metadata (lead sheet info, analysis stats, key stats, estimated overall key) shall be added to result as attributes (default is \code{TRUE}).
#' @return A key analysis data frame
#' @export
key_analysis <- function(lead_sheet = NULL, chord_stream = NULL, remove_reps = T, add_metadata = T){
  title <- ""
  compid <- NA
  single_chord <- F
  #browser()
  if(!is.null(lead_sheet)){
    chord_stream <-  get_intrinsic_scale_degree(lead_sheet %>% pull(chord))
    title <- unique(lead_sheet$title)
    compid <- unique(lead_sheet$compid)
  }
  else if(is.null(chord_stream)){
    stop("Must provide either lead sheet or chord stream")
  }
  else{
    single_chord <- length(chord_stream) == 1
    chord_stream <- get_intrinsic_scale_degree(chord_stream)
  }
  if(single_chord){
    ret <- chord_stream %>% filter((chord_pc - tonic_pc) %% 12 == 0)
    if(nrow(ret) == 0){
      chord_stream[1,]
    }
    return(ret %>% mutate(chord_pos = 1, unique = T) %>% rename(local_key = key, local_scale_degree = scale_degree))
  }
  if(remove_reps){
    #tmp <- rle(chord_stream)
    #chord_stream <- tmp$values
  }
  before <- nrow(chord_stream)

  chord_stream <- chord_stream %>% find_II_V()
  messagef("Before ii-V filter: %d, after: %d", before, nrow(chord_stream))
  messagef("Analyzing: '%s'[%s]", title, compid)
  max_iter <- 5
  pass <- chord_stream
  last_unique_ratio <- 0
  last_pass <- NULL
  for(i in 1:max_iter){
    #browser()
    pass2 <- single_key_analysis(pass, fuse_length = 2)
    pass3 <- single_key_analysis(pass, fuse_length = 3)
    red2 <- get_reduction_factor(pass2) %>%  mutate(fuse_length = 2)
    red3 <- get_reduction_factor(pass3) %>%  mutate(fuse_length = 3)
    #print(bind_rows(red2, red3))
    if(red2$rle_mean_len >= red3$rle_mean_len){
      messagef("Selecting fusion length 2")
      pass <- pass2
    }
    else{
      messagef("Selecting fusion length 3")
      pass <- pass3
    }
    #browser()
    unique_ratio <- mean(pass$unique)
    messagef("Pass %s: unique/ambiguous: %.2f/%.2f", i, unique_ratio, 1 - unique_ratio)
    if(unique_ratio >= 1.0 || last_unique_ratio >= unique_ratio){
      break
    }
    last_unique_ratio <- unique_ratio
    last_pass <- pass
  }
  #browser()
  if(add_metadata){
    key_stats <-  pass %>%
      mutate(key = str_replace(key, "blues", "maj")) %>%
      freq_table(key)
      attr(pass, "stats") <- get_reduction_factor(pass)
      attr(pass, "key_stats") <- key_stats
      attr(pass, "estimated_key") <- key_stats %>% top_n(1) %>% pull(key)
  }
  pass <- pass %>% rename(local_key = key, local_scale_degree = scale_degree)
  messagef("Final unique ratio: %.2f after %s iteration", unique_ratio, i )

  if(!is.null(lead_sheet) & add_metadata){
    pass <- pass %>%
      left_join(lead_sheet %>% rename(main_key = key), by = c("chord_pos", "chord"))  %>%
      select(section, bar, beat, chord, main_key, local_key, local_scale_degree)
    attr(pass, "metadata") <- lead_sheet %>% distinct(compid, title, composer)
  }
  class(pass) <- c(oldClass(pass), "key_analysis")
  browser()
  return(pass)
}

get_global_key <- function(lead_sheet = NULL, chord_stream = NULL, n_return = 1 ){
  if(!is.null(lead_sheet)){
    chord_stream <-  get_intrinsic_scale_degree(lead_sheet %>% pull(chord))
    title <- unique(lead_sheet$title)
    compid <- unique(lead_sheet$compid)
  }
  else if(is.null(chord_stream)){
    stop("Must provide either lead sheet or chord stream")
  }
  else{
    chord_stream <- get_intrinsic_scale_degree(chord_stream)
  }
  ka <- key_analysis(lead_sheet, chord_stream)
  ka  %>% pull(local_key) %>%
    str_replace("-", "") %>%
    str_replace("blues", "maj") %>%
    key_analysis(chord_stream = .) %>% freq_table(local_key) %>%
    top_n(n_return)
}

enhance_irb_chords <- function(fname){
  #irb <- read.csv("data/irb.csv", sep = ";", stringsAsFactors = F) %>% as_tibble()
  irb <- irb %>% group_by(compid) %>%
    mutate(chord_pos = 1:n(),
           chord_id = sprintf("%d-%s", compid, chord_pos)) %>%
    mutate(key_pc = str_split_fixed(key, "-", 2)[,1] %>% tone_name_to_pc() %>% as.vector(),
           pc_rel = (pc - key_pc) %% 12) %>%
    mutate(
           next_chord = lead(chord),
           over_next_chord = lead(chord, 2),
           next_type = lead(type),
           over_next_type = lead(type, 2),
           next_pc = lead(pc),
           over_next_pc = lead(pc, 2),
           next_pc_rel = lead(pc_rel),
           over_next_pc_rel = lead(pc_rel, 2)
    ) %>%
    mutate(root_movement = signed_mod(c(diff(pc), NA)),
           chord_bigram = sprintf("%s-%s", chord, next_chord),
           chord_trigram = sprintf("%s-%s-%s", chord, next_chord, over_next_chord),
           root_bigram = sprintf("%s-%s", pc, next_pc),
           root_trigram = sprintf("%s-%s-%s", pc, next_pc, over_next_pc),
           pc_rel_bigram = sprintf("%s-%s", pc_rel, next_pc_rel),
           pc_rel_trigram = sprintf("%s-%s-%s", pc_rel, next_pc_rel, over_next_pc_rel),
           type_bigram = sprintf("%s-%s", type, next_type),
           type_trigram = sprintf("%s-%s-%s", type, next_type, over_next_type),
    ) %>%
  ungroup()
  #assign("irb", irb, env = globalenv())
  irb
}

