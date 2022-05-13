safe_seq <- function(x, y, step = 1){
  if(x > y){
    ret <- seq(x, y, -1*step)
  }
  if(x < y){
    ret <- seq(x, y, step)
  }
  ret[ret > 0]
}

set_format <- function(obj, format = ""){
  attr(obj, "format") <- format
  return(obj)
}

get_format <- function(obj){
  attr(obj, "format")
}

add_format <- function(obj, format = ""){
  attr(obj, "format") <- c(format, attr(obj, "format"))
  return(obj)
}

is_solo_df <- function(obj){
  "solo_df" %in% attr(obj, "format")
}

is_mcsv2 <- function(obj){
  "mcsv2" %in% attr(obj, "format")
}

interval_vector_to_pitch <- function(int_vector, start_pitch = 60, root = NULL){
  if(is.character(int_vector)){
    int_vector <- value_to_vec(int_vector)
  }
  pitch_vec <- cumsum(c(start_pitch, int_vector))
  if(!is.null(root)){
    pitch_vec <-  (pitch_vec - root) %% 12
  }
  pitch_vec
}

select_by_id <- function(df, id = NULL, id_var = "id"){
  if (is.null(id)){
    return(df)
  }
  if (is.numeric(id)){
    id <- levels(factor(df[[id_var]]))[id]
  }
  df[df[[id_var]] %in% id,]
}


#' @export
pull_unique <- function(df, var){
  df %>% pull(!!enquo(var)) %>% unique()
}

#' @export
signed_modulo <- Vectorize(function(x, n){
  if(is.na(x)){
    return(NA)
  }
  t <- x %% n
  if(t > n/2){
    t <- t - n
  }
  t
})

#' @export
freq_table <- function(x, prop_var, sort = T) {
  prop_var  <- enquo(prop_var)

  tmp <- x %>%
    group_by( !!prop_var) %>%
    summarise(n = n(), .groups = "drop" ) %>%
    mutate(freq = n /sum(n))
  if(sort){
    tmp <- tmp %>% arrange(desc(n))
  }
  tmp
  #tmp %>% ggplot(aes_string(x = rlang::quo_text(prop_var), y= "freq")) + geom_col()
}

#' @export
freq2_table <- function(x, group_var, prop_var, sort = T) {
  group_var <- enquo(group_var)
  prop_var  <- enquo(prop_var)
  tmp <- x %>%
    group_by(!!group_var, !!prop_var) %>%
    summarise(n = n(), .groups = "drop" ) %>%
    mutate(freq = n /sum(n))
  if(sort){
    tmp <- tmp %>% arrange(desc(n))
  }
  tmp
  #tmp %>% ggplotf(aes_string(x = rlang::quo_text(prop_var), y= "freq")) + geom_col()
}

form_from_sections <- function(sections){
  r <- rle(sections)
  print(r)
  sprintf("%s-%d", r$values, r$lengths)
}

form_from_sheet <- function(sheet){
  sheet$section_start <- sheet$section != dplyr::lag(sheet$section)
  sheet$section_start[1] <- TRUE
  start_bars <- sheet[sheet$section_start,]$bar
  period <- as.integer(strsplit(sheet$time[1], "/")[[1]][1])
  printf("Title: %s, time %s", sheet$title[1], sheet$time[1])
  last_section <-
    sheet %>%
    filter(sheet$section == last_n(unique(sheet$section))) %>%
    summarise(last_section = sum(duration)/period) %>%
    pull(last_section)
  #last_dur_bar <- last_n(sheet$duration)/period
  #print(last_section)
  #print(start_bars)
  section_lengths <- c(diff(start_bars), last_section)
  section_labels <- rle(sheet %>% pull(section))$values
  #print(section_labels)
  #print(section_lengths)
  #print(floor(section_lengths) != section_lengths)
  if(any(floor(section_lengths) != section_lengths)){
    messagef("Error in sheet: %s", sheet$title[1])
    return("")
  }
  form <- sprintf("%s:%d", c(section_labels, "Total"), c(section_lengths, sum(section_lengths)))
  paste0(form, collapse = "|")
}

get_chord_change_marker <- function(solo, x, y){
  tmp <- as.integer(as.factor(solo$chords_raw))
  if(x == 1){
    return(c(1, as.integer(diff(tmp[seq(x,y)]) != 0) ))
  }
  as.integer(diff(tmp[seq(x -1, y)]) != 0)
}

get_chord_tone_links <- function(solo){
  solo$chord_tone <- solo$cdpcx_raw_all %in% c("1", "3", "5", "7")
  chord_tones_pos <- which(solo$chord_tone)
  purrr::map2_dfr(chord_tones_pos, lead(chord_tones_pos, 1), function(x, y){
    #browser()
    if(is.na(y)){
      return(NULL)
    }
    tibble(start = solo$cdpcx_raw_all[x],
           end = solo$cdpcx_raw_all[y],
           link = paste0(solo$cdpcx_raw_all[seq(x, y, 1)], collapse = ""),
           chord_changes = paste0(get_chord_change_marker(solo, x, y), collapse=""),
           num_chord_changes = sum(get_chord_change_marker(solo, x, y)),
           intervals = paste0(solo$int_raw[seq(x, y-1, 1)], collapse = ","),
           len = y -x +1)
  })
}

get_all_chord_tone_links <- function(data){
  solos <- unique(data$id)
  solo_links <- purrr::map_dfr(solos, function(x){
    tmp <- get_chord_tone_links(wjd_transforms %>% filter(id == x))
    if(nrow(tmp)){
      tmp$id <- x
      return(tmp)
    }
    else{
      return(NULL)
    }
    })
  solo_links
}

last_n <- function(x, n = 1){
  x[length(x) - n + 1]
}

pretty_print_pitches <- function(pitch_vector){
  octaves <- as.integer(floor((pitch_vector - 60) / 12 + 4))
  print(paste(pc_labels_flat[pitch_vector %% 12 +1], octaves, collapse = "-", sep = ""))
}

fill_diff <- function(x, fill = NA, post = T){
  if(post)return(c(diff(x), fill))
  return(c(fill, diff(x)))
}

safe_pitch_vector <- function(pitch_vector, pitch_range = c(48, 84)){
  if(is.null(pitch_vector) || length(pitch_vector) == 0){
    return(pitch_vector)
  }
  #print(pitch_vector)
  pitch_vector[!(is.na(pitch_vector)) & pitch_vector >= pitch_range[1] & pitch_vector <= pitch_range[2]]
}

relative_register_position <- function(pitch, pitch_range){
  span <- diff(pitch_range)
  np <- pitch - pitch_range[1]
  np/span
}

#' classify_int_pattern
#' Classifies interval pattern in a main type (cool/uncool) and a subtype (scale, repetition, other, trill)
#' @param pattern (string vector) Vector of interval patterns as comma-separated list of semi-tone intervals
#'
#' @return Data frame of classifications
#' @export
classify_int_pattern <- function(pattern){
  if(length(pattern) > 1){
    return(map_dfr(pattern, classify_int_pattern))
  }
  #browser()
  #printf("Classifying pattern %s", pattern)
  x <- value_to_vec(pattern)
  main_type <- "cool"
  main_type <- "other"
  #get rid of repetitions
  x_sz <- x[x != 0]
  #1. only repetition: uncool
  #print(x_sz)
  if(length(x_sz) <= 1){
    return(tibble(main_type =  "uncool", sub_type = "repetition"))
  }
  if(length(x_sz) < 4){
    return(tibble(main_type =  "cool", sub_type = "other"))
  }
  #get cumsum
  cs <- c(0, cumsum( x_sz))

  #print(cs)
  #get period of potential trill
  zero_pos <- which(cs == 0)
  #print(zero_pos)
  if(length(zero_pos) > 2){
    if(length(cs) %in% zero_pos || length(zero_pos) >= 3){

      periods <- unique(diff(zero_pos))
      #print(periods)
      if(length(periods) <= 1){
        if(length(cs)/length(zero_pos) <= periods[1]){
          #printf("l(x_sz) = %d, l(zp) = %d, r = %f, periods = %d", length(cs), length(zero_pos), length(cs)/length(zero_pos), periods[1])
          #print(cs[zero_pos[1]:(zero_pos[2]-1)])
          #print(cs[zero_pos[2]:(zero_pos[3]-1)])
          if(identical(cs[zero_pos[1]:(zero_pos[2] - 1)], cs[zero_pos[2]:(zero_pos[3] - 1)])){
            return(tibble(main_type =  "uncool", sub_type = "trill"))
          }

        }
      }
    }
  }
  #check for scales
  steps <- union(unique(abs(x)), c(1,2))
  #print(steps)
  is_scale <- length(steps) == 2
  if (is_scale){
    return(tibble(main_type =  "uncool", sub_type = "scale"))

  }
  tibble(main_type =  "cool", sub_type = "other")
}

