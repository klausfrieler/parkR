get_successor_dist <- function(data){
  g_counter <<- g_counter + 1

  successor_dist <- tibble(     type      = data$type,
                                direction = data$direction,
                                value     = data$value,
                                length    = data$length,
                                succ_type = lead(data$type, 1),
                                succ_direction = lead(data$direction, 1),
                                succ_length = lead(data$length, 1),
                                succ_value = lead(data$value, 1),
                                segment_id = data$segment_id,
                                succ_segment_id = lead(data$segment_id, 1))
  successor_dist %>% filter(!is.na(succ_type))
}

get_simple_successor_list <- function(data, var, reduce_frac = 0){
  ret <- tibble(value = data[, var], successor = lead(data[, var], 1))
  if(!is.null(reduce_frac) & reduce_frac > 0){
    ret <- ret %>% sample_frac(reduce_frac)
  }
  ret %>% filter(!is.na(value), !is.na(successor))
}

setup_distribrutions <- function(recalc = F){
  if(recalc){
    if(!requireNamespace("jazzodata", quietly = TRUE)){
      stop("Recalculation of distributions requries 'jazzodata' package.")
    }
    phrase_begin_dist <- WBA_df %>%
      filter(start == 1) %>%
      select(length, type, direction, value) %>%
      mutate(class_code = sprintf("%s%s%d",
                                  c("-", "=", "+")[direction+2],
                                  type, length))
    save(phrase_begin_dist, "data/phrase_begin_dist")
  }
  else{
    load("data/phrase_begin_dist.rda")
  }

  if(recalc){
    successor_dist <- WBA_df %>%
      tidyr::nest(-id, -phrase_id)
    ret <- list()
    for(i in 1:nrow(successor_dist)){
      ret[[i]] <- get_successor_dist(successor_dist[i,]$data[[1]])
    }
    successor_dist <- bind_rows(ret)
    save(successor_dist, "data/successor_dist")
  }
  else{
    load("data/successor_dist.rda")
  }

  if(recalc){
    succ_ioiclass <- get_simple_successor_list(jazzodata::wjd_transforms, "ioiclass_abs_raw", .01)
    save(succ_ioiclass, "data/succ_ioiclass")
  }
  else{
    load("data/succ_ioiclass.rda")

  }

  if(recalc){
    length_dist <- jazzodata::wjd_transforms %>%
      group_by(id) %>%
      summarise(num_phrases = max(phrase_id_raw))
    save(length_dist, "data/length_dist.rda")
  }
  else{
    load("data/length_dist.rda")
  }

  if(recalc){
    phrase_length_dist <- WBA_df %>%
      group_by(id) %>%
      mutate(rel_phrase_pos =(phrase_id-1)/(max(phrase_id)-1)) %>%
      group_by(id, phrase_id) %>%
      summarise( n = n(),
                 rpp = max(rel_phrase_pos)) %>%
      ungroup() %>%
      select(n, rpp)
    save(phrase_length_dist, "data/phrase_length_dist.rda")
  }
  else{
    load("data/phrase_length_dist.rda")

  }

}
