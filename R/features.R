easiness_descriptions <- tribble(
  ~label, ~name, ~explanation,
  #"N-gram Surprise", "surprise", "Surprise of an n-gram is the negative logarithm of its occurrence probability. The less frequent the n-gram, the higher the surprise.",
  "Extrinsic Difficulty", "surprise", "Defined as the surprise of an n-gram, which is the negative logarithm of its occurrence probability. The less frequent the n-gram, the higher the surprise.",
  "Extrinsic Difficulty (P)", "surprise_p", "Defined as the surprise of an n-gram, which is the negative logarithm of its occurrence probability with a specific performer",
  #  "N-gram Surprise (S)", "surprise_s", "Surprise of an n-gram is the negative logarithm of its occurrence probability within a specific solo",
  "Intrinsic Difficulty", "combined_easiness", "(N-scale) sum of N-scaled interval and pitch variety, direction changes and zig-zaggity",
  "Mean Interval Size", "mean_int_size", "Mean of the absolute values of the intervals in an n-gram",
  "Max. Interval Size" , "int_range", "Maximal absolute interval size in an n-gram",
  "Interval Variety", "int_variety", "Number of different intervals in the n-gram divided by n-gram length. If all intervals are different then this value is 1, if  all intervals are equal then this value is 1/N, where N is the n-gram length",
  "Pitch Variety", "pitch_variety", "Number of different pitches in the n-gram divided by n-gram length. If all pitches are different then this value is 1, if all pitches are equal then this value is 1/N, where N is the n-gram length",
  "Direction Changes", "dir_change", "Number of direction changes in the n-gram",
  "Zig-zaggity" , "mean_run_length", "The mean lenght of stretches of intervals in the same direction (ascending, descending, repeating).")


easiness_measures <- easiness_descriptions %>% pull(name)
names(easiness_measures) <- easiness_descriptions %>% pull(label)
stats_vars <- c(easiness_measures, "Number of notes" = "number_notes", "Phrase lengths" = "phrase_len")

get_stats_var_label <- function(var){
  names(stats_vars)[which(stats_vars == var)]
}

get_easiness_label <- function(easiness_var, standardize = FALSE){
  if(substr(easiness_var, 1, 2) == "z_"){
    easiness_var <- substr(easiness_var, 3, nchar(easiness_var))
    standardize <- F
  }
  label <- names(easiness_measures)[which(easiness_measures == easiness_var)]
  if(length(label) == 0){
    return(easiness_var)
  }
  if(standardize){
    label <- sprintf("%s (standardized)", label)
  }
  label
}
get_easiness_description <- function(easiness_var){
  easiness_descriptions %>% filter(name == easiness_var) %>% pull(explanation)
}

#' @export
fuzzyint_class <- Vectorize(
  function(x){
    class_vec <- list("0" = 0, "1" = 1, "2" = 1, "3" = 2, "4" = 2, "5" = 3, "6" = 3, "7" = 3)
    s <- sign(x)
    a <- abs(x)
    if(a > 7){
      return(s * 4)
    }
    return(s * class_vec[[as.character(a)]])

  })

#' export
n_gram_easiness <- function(pattern){
  if(length(pattern) > 1){
    return(map_dfr(pattern, n_gram_easiness))
  }
  #browser()
  v <- value_to_vec(pattern)
  mean_abs_int <- mean(abs(v))
  #int_path <- sum(abs(v))
  int_range <- max(abs(v))
  l <- length(v)
  r <- rle(sign(v))
  #browser()
  dir_change <- length(r$values) - 1
  mean_dir_change <- (length(r$values) - 1)/(l-1)
  mean_run_length <- 1 - mean(r$lengths)/l
  int_variety <- n_distinct(v)/l
  pitch_variety <- n_distinct(c(0, cumsum(v)))/(l+1)
  tibble(value = pattern,
         mean_int_size = mean_abs_int,
         #int_path = int_path,
         int_range = int_range,
         dir_change = dir_change,
         mean_dir_change = mean_dir_change,
         int_variety = int_variety,
         pitch_variety = pitch_variety,
         mean_run_length = mean_run_length)
}
