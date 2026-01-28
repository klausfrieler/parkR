#library(tidyverse)

bigram_encoder <- function(x, level = 1, sep = ""){
  #if(is.character(x)){
  #  x <- map_chr(x, function(y) strsplit(y, sep)[[1]][1])
  #}
  tmp <- tibble(pos = 1:length(x),
                x = x,
                y = lead(x),
                bigram = sprintf("%s,%s", x, y),
                bi_enc = as.integer(factor(bigram)) - 1,#zero based
                level = level) %>%
    #filter(!is.na(y)) %>%
    group_by(bi_enc) %>%
    mutate(n_xy = n()) %>%
    ungroup() %>%
    group_by(x) %>%
    mutate(n_x = n()) %>%
    ungroup() %>%
    group_by(y) %>%
    mutate(n_y = n()) %>%
    ungroup() %>%
    mutate(n = nrow(.), f_x = n_x/n, f_y = n_y/n, f_xy = n_xy/n, pmi = log(f_xy/f_x/f_y))

  tmp
}

build_bigram_stack <- function(x, max_level = 3, sd_threshold = 1.0, ids = NULL, add_values = T ){
  #browser()
  tictoc::tic()
  ret <- list()
  orig_x <- x
  if(is.character(x)){
    x <- as.integer(factor(x))
  }
  early_break <- FALSE
  benc <- bigram_encoder(as.integer(x), level = 1)
  ret[[1]] <- benc
  sd_xy <- sd(ret[[1]]$n_xy)
  messagef("Building level %d (sd = %.2f, ngrams = %d)", 1, sd_xy, n_distinct(ret[[1]]$bi_enc))
  if(!is.null(max_level) && max_level > 1){
    for(m in 2:max_level){
      ret[[m]] <- bigram_encoder(ret[[m-1]]$bi_enc, m)
      sd_xy <- sd(ret[[m]]$n_xy)
      messagef("Building level %d (sd = %.2f, ngrams = %d)", m, sd_xy, n_distinct(ret[[m]]$bi_enc))
      if(sd_xy <= sd_threshold){
        messagef("Threshold reached, finishing up.")
        ret[[m]] <- NULL
        early_break <- TRUE
        break
      }
    }
  }
  ret <- bind_rows(ret) %>%
    mutate(bigram_id = sprintf("%s-%s", bi_enc, level))
  if(add_values){
    ret <- ret %>%  add_ngram_values(orig_x)
  }
  #browser()
  if(!is.null(ids) ){
    if("id" %in% names(ids)){
        ids <- ids$id
    }
    messagef("Adding ids")
    max_level <- max(ret$level)
    ret <- ret %>% arrange(pos)
    ret$id <- rep(ids, each = max_level)
    # if(early_break){
    #   ret <- ret %>% arrange(pos)
    #   ret$id <- rep(ids, each = max_level)
    # }
    # else{
    #   #ret <- ret %>% left_join(tibble(pos = 1:length(ids), id = unique(ids)), by = "pos")
    # }
    messagef("Adding document frequencies")
    ret <- ret %>% group_by(bigram_id) %>% mutate(DF = n_distinct(id)) %>% ungroup()
  } else{
    ret$DF <- 1
  }

  tictoc::toc(func.toc = function(tic, toc, msg)  msg = sprintf("Building finished in %.3f sec.", toc - tic))
  ret %>% arrange(level, pos) %>% mutate(N = level + 1)
}

retrieve_ngram <- function(bigram_stack, bi_enc, level){
  #messagef("Retrieving %d, level = %d", bi_enc, level)
  #browser()
  elements <- bigram_stack %>% filter(level == !!level, bi_enc == !!bi_enc) %>% distinct(x, y)
  #messagef("Found bigram %s, %s", elements$x[1], elements$y[1])
  if(level == 1){
    bigram <- c(elements$x[1], elements$y[1])
    return(bigram)
  }
  left <- retrieve_ngram(bigram_stack, elements$x[1], level = level - 1)

  right <- retrieve_ngram(bigram_stack, elements$y[1], level = level - 1)
  c(left[1], right)

}

get_ngrams <- function(bi_enc = NULL, pos = NULL, level, bigram_stack = bbh, orig_data){
  if(is.null(pos)){
    stopifnot(!is.null(bi_enc))
    pos <- bigram_stack %>% filter(level == !!level, bi_enc %in% !!bi_enc) %>% pull(pos)
  }
  #return(    paste(orig_data[seq(pos[1], pos[1] + level)], collapse = ","))
  map_chr(pos, function(p){
    paste(orig_data[seq(p, p + level)], collapse = ",")
  }) %>% unique()
}

cover_seq <- function(bigram_stack, bi_enc, level, with_overlap = F){
  tictoc::tic()
  bigram_ids <- sprintf("%s-%s", bi_enc, level) %>% unique()
  min_pos <- min(bigram_stack$pos) - 1
  max_pos <- max(bigram_stack$pos)
  #bigram_stack <- bigram_stack %>% mutate(bigram_id = sprintf("%s-%s", bi_enc, level))
  #browser()
  positions <- bigram_stack %>%
    filter(bigram_id %in% bigram_ids) %>%
    select(pos, level, bigram_id) %>%
    arrange(desc(level), pos)

  if(nrow(positions) == 0){
    tmp <- tibble(pos = (1 + min_pos):max_pos,
                  covered = FALSE,
                  cover_ids =  "")
    bigram_stack %>% left_join(tmp, by = "pos")
    return(bigram_stack %>% left_join(tmp, by = "pos"))
  }
  covered <- rep(FALSE, max_pos - min_pos)
  cover_bigrams <- rep("", max_pos - min_pos)
  messagef("Covering with %d bigrams on %d positions (%d positions in total)", n_distinct(bigram_ids), max(bigram_stack$pos), nrow(positions))
  cover_count <<- 0
  map2(positions$pos, positions$level, function(p, l){
    if(sum(covered) != length(covered) && (p - min_pos + l <= length(covered))){
      idx <- seq(p - min_pos, p - min_pos + l)
      if(with_overlap || (sum(covered[idx]) == 0)){
        covered[idx] <<- TRUE
        cover_count <<- cover_count + l + 1
        #cover_bigrams[p] <<- paste(cover_bigrams[p], positions[positions$pos == p & positions$level == l,]$bigram_id, sep = ";")
        if(nchar(cover_bigrams[p - min_pos]) == 0){
          #browser()
          cover_bigrams[p - min_pos] <<- positions[positions$pos == p & positions$level == l,]$bigram_id
          #print(cover_bigrams)
          #messagef("Adding %s at pos %d (%s) on level %d",
          #        positions[positions$pos == p & positions$level == l,]$bigram_id,
          #        p, p-min_pos, l)
        }
        if(cover_count %% 10000 == 0){
          messagef("Covered %d positions (level = %d), Totally covered: %d (%.2f)", cover_count, l, sum(covered), sum(covered)/length(covered))
        }
      }
    }
  })
  messagef("Totally covered: %.2f",  sum(covered)/length(covered))
  tictoc::toc(func.toc = function(tic, toc, msg)  msg = sprintf("Covering finished in %.3f sec.", toc - tic))
  #browser()
  tmp <- tibble(pos = (1 + min_pos):max_pos,
                covered = covered,
                cover_ids =  gsub(pattern = "^;", "", cover_bigrams))
  bigram_stack %>% left_join(tmp, by = "pos")
}

cover_seq_all <- function(bigram_stack, min_n = 2, max_level = Inf, min_level = 1,  min_DF = 1, with_overlap = T, excess_n = 1){
  levels <- unique(pmax(min_level, pmin(max_level, bigram_stack$level)))
  #browser()
  messagef("Covering all sequences with levels [%d:%d] (overlap = %s)", min(levels), max(levels), with_overlap)
  bigram_stack$covered <- NULL
  bigram_stack$cover_ids <- NULL
  bs <- bigram_stack
  if("DF" %in% names(bs)){
    bs <- bs %>% filter(DF >= min_DF)
    #browser()
  }
  if("DF" %in% names(bs) && !is.na(excess_n)){
    bs <- bs %>% filter((n_xy - DF)/DF >= excess_n)
  }
  tmp <- bs %>%
    filter(level %in% levels, n_xy >= min_n) %>%
    arrange(desc(level), desc(n_xy), desc(pmi)) %>%
    distinct(bi_enc, level)
  cover_seq(bigram_stack, tmp$bi_enc, tmp$level, with_overlap)
}

get_all_covered_ngrams <- function(bs_covered, covered_id, orig_data){
  tmp <- bs_covered %>% filter(covered == covered_id) %>% distinct(bi_enc, level)
  if(nrow(tmp) == 0){
    return(character(0))
  }
  tmp_split <- strsplit(covered_id, "-")[[1]] %>% as.numeric()
  tmp <- bind_rows(tibble(bi_enc = tmp_split[[1]], level = tmp_split[[2]]), tmp)

  map2_dfr(tmp$bi_enc, tmp$level, function(x,y){
    tibble(bi_enc = x,
           level = y,
           value  = get_ngrams(bi_enc = x, level = y, bigram_stack = bs_covered, orig_data = orig_data))
  })
}

add_ngram_values <- function(bigram_stack, orig_data){
  messagef("Adding ngram values for %d rows", nrow(bigram_stack))
  max_level <- max(bigram_stack$level)
  orig_data <- c(orig_data, rep(NA, max_level))
  i <- 0
  value  <-
    map2_chr(bigram_stack$pos, bigram_stack$level, function(x,y){
      idx <- pmin(x:(x + y), length(orig_data))
      i <<- i + 1
      if(i %% 10000 == 0){
        messagef("Added %d (%.1f %%) values", i, round(100*i/nrow(bigram_stack), 1))
      }
      paste(orig_data[idx], collapse = ",")
    })

  bigram_stack$value <- value
  bigram_stack
}

get_all_subgrams <- function(bigram_stack, bi_enc, level, mode = "all"){
  pos <- bigram_stack %>% filter(bi_enc == !!bi_enc, level == !!level)
  n_xy <- unique(pos$n_xy)
  DF <- unique(pos$DF)

  pos <- pos %>% pull(pos)
  messagef("Retrieving %d, level = %d, n_xy = %d", bi_enc, level, n_xy)

  pos_df <- tibble(start = seq(pos[1], pos[1] + level), max_level = rev(0:level))
  mode <- match.arg(mode, choices = c("all", "proper", "inproper"))
  map2_dfr(pos_df$start, pos_df$max_level, function(p, l){
    if(mode == "inproper"){
      bigram_stack %>% filter(pos == p, level <= l, n_xy > !!n_xy)
    }
    else if(mode == "proper"){
      bigram_stack %>% filter(pos == p, level <= l, n_xy == !!n_xy)

    }
    else{
      bigram_stack %>% filter(pos == p, level <= l)

    }
  })
}

#' @export
ngram_analysis <- function(x,
                           max_level = Inf,
                           with_overlap = T,
                           min_level = 2,
                           min_n = 2,
                           min_DF = 1,
                           excess_n = 1,
                           ids = NULL){
  #if(is.null(ids)){
  #  if("id" %in% names(x) && is.dataframe(x)){
  #    ids <- x$ids
  #  }
  #}
  bs <- build_bigram_stack(x,
                           max_level = max_level,
                           sd_threshold = 0,
                           add_values = T,
                           ids = ids)
  #browser()

  #return(bs)
  bs <- cover_seq_all(bs,
                      min_n = min_n,
                      min_level = min_level,
                      max_level = max(bs$level),
                      min_DF = min_DF,
                      with_overlap = with_overlap,
                      excess_n = excess_n)
  #browser()
  bricks <- bs %>%
    filter(nchar(cover_ids)> 0, level == 1) %>%
    count(cover_ids) %>%
    arrange(desc(n)) %>%
    rename(bigram_id = cover_ids) %>%
    left_join(bs  %>% distinct(n_xy, bigram_id, value, x, y), by = "bigram_id") %>%
    mutate(d_n = n - n_xy) %>%
    filter(d_n < 0) %>%
    arrange(d_n)

  if(nrow(bricks) > 0){
    bricks <- bind_cols(bricks,
                        map_dfr(strsplit(bricks$bigram_id, "-"),
                                function(e) tibble(bi_enc = as.numeric(e[1]), level = as.numeric(e[2])))) %>%
      mutate(left = sprintf("%s-%s", x, level - 1), right = sprintf("%s-%s", y, level - 1))
    bricks_reduced <- bricks %>% filter((bigram_id %in% bricks$right) | (bigram_id %in% bricks$left))
    bricks_reduced2 <- bricks %>% filter(!(bigram_id %in% bricks$right) & !(bigram_id %in% bricks$left))

  }
  else{
    bricks <- NULL
    bricks_reduced <- NULL
    bricks_reduced2 <- NULL
  }

  list(bs = bs, bricks = bricks, bricks_reduced = bricks_reduced, bricks_reduced2 = bricks_reduced2)
}

get_tf_idf <- function(bigram_stack){
  N <- n_distinct(bigram_stack$id)
  n_d <- bigram_stack %>% group_by(id, bigram_id) %>%
    summarise(n_d = length(id)) %>%
    ungroup()
  #browser()
  bigram_stack <- bigram_stack %>% left_join(n_d, by = c("id", "bigram_id"))
  bigram_stack <- bigram_stack %>%
    group_by(id, level) %>%
    mutate(max_n_d = max(n_d)) %>%
    ungroup()

  #bigram_stack <- bigram_stack %>% left_join(max_n_d, by = c("id", "level"))
  #browser()
  bigram_stack <- bigram_stack %>% mutate(tf = n_d/max_n_d, idf = log(N/DF), tf_idf = tf * idf)

  bigram_stack
}

distribution_similarity <- function(x, y){
  if(length(x) == 0 || length(y) == 0){
    return(0)
  }
  joint <- c(x, y) %>% table()
  tx <- factor(x, levels = names(joint)) %>% table()
  ty <- factor(y, levels = names(joint)) %>% table()
  joint <- joint/sum(joint)
  tx <- tx/sum(tx)
  ty <- ty/sum(ty)
  tv <- 1 - .5 * sum(abs(tx - ty))
  if(tv < 0 || tv > 1){
    browser()
  }
  tv
}

get_pattern_similarity <- function(bigram_stack, id1, id2, max_level, group_var = "id"){

  messagef("Calculating similarity for %s <-> %s", id1, id2)
  pat1 <- bigram_stack %>% filter(!!sym(group_var) %in% id1)
  pat2 <- bigram_stack %>% filter(!!sym(group_var) %in% id2)
  levels <- unique(bigram_stack$level)
  map_dfr(levels[levels <= max_level], function(l){
    #browser()
    level1 <- pat1 %>%
      filter(level == l) #%>%
    #  count(bigram_id)
    level2 <- pat2 %>% filter(level == l)
    #not_in_1 <- setdiff(level2$bigram_id, level1$bigram_id)
    #not_in_2 <- setdiff(level1$bigram_id, level2$bigram_id)
    #level1 <- bind_rows(level1, tibble(bigram_id = not_in_1, n = 0))
    #level2 <- bind_rows(level2, tibble(bigram_id = not_in_2, n = 0))
    #n1 <- level1 %>% arrange(bigram_id) %>% pull(n)
    #n2 <- level2 %>% arrange(bigram_id) %>% pull(n)
    #d_n <- mean(abs(n1/sum(n1)-n2/sum(n2)))
    sim <- distribution_similarity(level1$bigram_id, level2$bigram_id)
    tibble(level = l, d_n = sim)

  })
}

get_pattern_similarity_matrix <- function(bigram_stack, levels, group_var = "id", sort_by_sim = F){
  idz <- cross2(unique(bigram_stack[[group_var]]),
                unique(bigram_stack[[group_var]]),
                .filter = function(x, y) ifelse(x <= y, TRUE, FALSE))
  if(missing(levels)){
    levels <- 1:max(bigram_stack$level)
  }
  map_dfr(levels, function(l){
    messagef("Cal sim mat for level %d", l)
    ret <-
    map_dfr(idz, function(x){
      if(x[[1]] != x[[2]]){
        bind_cols(tibble(group1 = x[[1]],
                         group2 = x[[2]]),
                  get_pattern_similarity(bigram_stack, x[[1]], x[[2]], max_level = l, group_var = group_var))
      }
    }) %>% group_by(level) %>%
      mutate(z_d_n = scale(d_n)) %>%
      ungroup() %>%
      group_by(group1, group2) %>%
      summarise(sim = mean(d_n), z_sim = mean(z_d_n), .groups = "drop")

  ret <- ret %>% rename(group1 = group2, group2 = group1) %>% bind_rows(ret)
  if(sort_by_sim){
    group_levels <- ret %>% group_by(group1) %>% summarise(s = mean(sim)) %>% arrange((s)) %>% pull(group1)

    ret %>% mutate(group1 = factor(group1, levels = group_levels), group2 = factor(group2, levels = group_levels)) %>%
      arrange(sim)
  }
  ret %>% mutate(max_level = l)
  })
}

get_all_pattern_sim_mats <- function(bigram_stack, max_level){

}

get_coverage <- function(bigram_stack, level, min_n, min_DF, with_overlap = T){
  max_pos <- max(bigram_stack$pos)
  tictoc::tic()
  loop <- cross3(level, min_n, min_DF)
  tictoc::toc()
  map_dfr(loop, function(x){
    level <- x[[1]]
    min_n <- x[[2]]
    min_DF <- x[[3]]
    tmp <- bigram_stack %>% filter(level == !!level, n_xy >= min_n, DF >= min_DF) %>%
      mutate(d = c(diff(pos), NA)) %>%
      select(pos, d, bigram_id) %>%
      mutate(gap = d - level)
    if(nrow(tmp) > 0){
      gaps <- tmp %>% filter(d > level) %>% summarise(gaps = sum(gap)) %>% pull(gaps)
      gaps <- gaps + min(tmp$pos) - 1
      tibble(level = level,
             min_n = min_n,
             min_DF = min_DF,
             overlap = with_overlap,
             gaps  = gaps,
             coverage = (max_pos - gaps)/max_pos)

    }
    else{
      NULL
    }

  })
}

get_coverage_by_set <- function(bigram_stack, bigram_set){
  total_pos <- bigram_stack %>% filter(level == 1) %>% nrow()
  tmp <- bigram_stack %>%
    filter(value %in% bigram_set$value) %>%
    distinct(pos, .keep_all = T)
  #browser()
  covered_pos <- map(1:nrow(tmp), function(i) seq(tmp[i,]$pos, tmp[i,]$pos + tmp[i,]$level)) %>%
    unlist()
  over_covered <- length(covered_pos)
  covered <- covered_pos %>%
    unique() %>%
    length()
  tibble(coverage = covered/total_pos, over_coverage = covered/over_covered)
}

#' @export
pull_unique <- function(data, var){
  data %>% pull({{var}}) %>% unique()
}

#' @export
pu <- pull_unique

get_all_prefixes <- function(bs, value, max_level = 2, min_freq = 2) {
  pat_str <- sprintf("%s$", value)
  base <- bs %>%
    filter(n_xy >= min_freq, str_detect(value, pat_str))
  if(nrow(base) == 0){
    return(character(0))
  }
  uni_probs <- bs %>% filter(level == 1) %>% parkR::freq_table(x)
  singles <- base %>%
    filter(level == 1, n_xy >= min_freq) %>%
    mutate(level = 0, n_xy = n_x, pmi = 1, value = x) %>%
    distinct(level, n_xy, pmi, value) %>%
    left_join(uni_probs %>% distinct(value = x, f_xy = freq)) %>%
    mutate(value = as.character(value))

  #browser()
  prefixes <- base %>%
    filter(level > 1, n_xy >= min_freq) %>%
    distinct(bi_enc = x, level) %>%
    mutate(level = level - 1)
  values <- bs %>%
    filter(n_xy >=  min_freq) %>%
    inner_join(prefixes, by = c("bi_enc", "level")) %>%
    distinct(level, n_xy, f_xy, pmi, value )


  values %>% bind_rows(singles) %>% arrange(level, desc(n_xy), value)
}

get_all_prefixes2 <- function(bs, value, min_freq = 2){
  pat_str <- sprintf("%s$", value)
  prefixes <-
    bs %>%
    filter(level > 1, n_xy >= min_freq ) %>%
    filter(str_detect(value, pat_str)) %>%
    distinct(value, level, n_xy) %>%
    pu(value) %>%
    value_to_vec() %>%
    map(~{paste(.x[-length(.x)], collapse = ",")})  %>%
    unlist()
  #browser()
  bs %>%
    filter(value %in% prefixes) %>%
    distinct(level, n_xy, f_xy, pmi, value )
}

get_last_element_from_value <- function(value){
  #browser()
  last <- function(x) x[[length(x)]]
  if(length(value) == 1){
    return(last(value_to_vec(value)))
  }
  sapply(value_to_vec(value, "character", collapse = F), (function(x) as.character(last(x))), simplify = T)
}

#' get_value_context
#' Retrieves all context n-grams for a value in a bigram stack
#' @param bs  A bigram_stack object
#' @param value (string) value in bigram stack
#' @param pre (integer) Length of context before value
#' @param post  (integer) Length of context after value
#' @param only_values (boolean) Flag, whether to return only unique context values
#'
#' @return Either a subset with context n-grams of the bigram stack or string of context values
#' @export
get_value_context <- function(bs, value, pre = 0, post = 0, only_values = F){
  pre <- max(0, pre)
  post = max(0, post)
  tmp <- bs %>% filter(value == !!value)
  if(nrow(tmp) == nrow(bs)){
    browser()
  }
  if(nrow(tmp) == 0){
    if(only_values){
      return(character(0))
    }
    return(tmp)
  }
  value_N = unique(tmp$N)
  N_min <- max(value_N - pre, 1)
  N_max <- min(value_N + post, max(bs$N))
  context_N <- min(value_N + pre + post, max(bs$N))
  value_pos <- tmp %>% pu(pos)
  start_pos <- pmax(value_pos - pre, 1)
  #ends <- pmin(value_pos + post, max(bs$pos))
  tmp <- bs %>% filter(N == context_N) %>% filter(pos %in% start_pos)
  if(only_values) tmp <- tmp %>% pu(value)
  tmp
}

predict_next <- function(bs, context, max_level = -1, alphabet = NULL){
  if(max_level < 0 || is.na(max_level) || is.infinite(max_level)){
    max_level <- max(bs$level)
  }
  #browser()
  if(is.character(context) & length(context) == 1) {
    tmp <- value_to_vec(context, "character", collapse = F)[[1]]
    if(str_detect(context, "\\(")){
      tmp <- sprintf("(%s)", tmp)
    }
  }
  else{
    tmp <- context
  }

  l <- length(tmp)
  if(length(tmp) > max_level){
    tmp <- tmp[(l - max_level + 1):l]
    l <- length(tmp)
    #messagef("Value too long for max_level, truncating to %s", paste(tmp, collapse = ","))
  }
  #browser()
  if(is.null(alphabet)){
    test <- tmp[l]
    abc <- bs %>% filter(level == 1) %>% pu(x) %>% as.character()
    if(!(tmp[l] %in% abc)){
      test <- integer(0)
    }
  }
  else{
    test <- which(alphabet == tmp[l])
  }

  if(length(test) == 0){
    stop(sprintf("Cannot predict from unknown symbol '%s'", tmp[l]))
  }

  singles <-  bs[bs$level == 1 & bs$x == test,] %>%
    distinct(n_xy, f_xy, level, local_context = x, value, pred = y) %>%
    mutate(value = as.character(value),
           pred = as.character(pred),
           local_context = as.character(local_context),
           log_p = -log2(f_xy))

  if(!is.null(alphabet)){
    singles <- singles %>%
      mutate(pred = alphabet[as.integer(pred)],
             local_context = alphabet[as.integer(local_context)])
  }

  ret <- NULL
  if(l > 1){
    ret <- map_dfr(seq(max(1, l - 1), 1), function(i){
      #browser()
      cur_val <- tmp[i:l] %>% paste(collapse = ",")
      cur_level <- l - i
      base <- bs[bs$value == cur_val,]
      #messagef("Testing: %s (i = %d, l = %d, cur_level = %d), found %d rows", cur_val, i, l, cur_level, nrow(base))
      if(nrow(base) == 0){
        return(NULL)
      }
      enc <- base %>% pu(bi_enc)
      bs[bs$x == enc & bs$level == cur_level + 1,] %>%
        distinct(n_xy, f_xy, level, value) %>%
        mutate(local_context = cur_val)
    })
    #browser()
    if(nrow(ret) > 0) ret <- ret %>% mutate(pred = get_last_element_from_value(value), log_p = -log2(f_xy))
  }

  bind_rows(singles, ret) %>%
    mutate(context = paste(tmp, collapse = ",")) %>%
    group_by(pred) %>%
    mutate(mean_log_p = mean(log_p),
           min_log_p = min(log_p)) %>%
    ungroup() %>%
    arrange(min_log_p, desc(level), desc(f_xy))

}

escape_search_pattern <- function(pattern){
  str_replace_all(pattern, "\\(", "\\\\(") %>%
    str_replace_all("\\|", "\\\\|") %>%
    str_replace_all("\\)", "\\\\)")
}

test_predictions <- function(bs, window_size= 5, num_windows = 10, offset = 0, max_order = 2, alphabet = NULL ){
  if(is.null(alphabet)){
    seq <- bs %>% filter(level == 1) %>% pull(x) %>% as.character()
  }
  else{
    seq <- alphabet[bs %>% filter(level == 1) %>% pull(x)]

  }

  map_dfr(1:num_windows, function(i){
    if(i + offset + window_size - 1 > length(seq)){
      return(NULL)
    }
    #browser()
    snip <- seq[(i + offset):(i + offset + window_size-1)]
    predictions <-  suppressWarnings(
      predict_next(bs, snip, max_level = max_order, alphabet = alphabet)
    )
    best <- predictions %>% filter(mean_log_p == min(mean_log_p))
    pred <- best %>% pu(pred)
    ic_pred <- best %>% pu(mean_log_p)
    truth <- seq[i + offset + window_size]
    ic_truth <- predictions %>% filter(pred == truth) %>% pu(mean_log_p)
    #browser()

    tibble(pos = i + offset + window_size,
           truth = truth,
           prediction = pred,
           ic_truth = ic_truth,
           ic_pred = ic_pred)
  }) %>%
    mutate(match = truth == prediction, diff_ic = ic_truth - ic_pred)
}


