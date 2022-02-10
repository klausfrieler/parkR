library(tidyverse)

messagef <- function(...) message(sprintf(...))
printf <- function(...) print(sprintf(...))

value_to_vec <- function(value_str){
  vec <- gsub("\\[", "", as.character(value_str))
  vec <- gsub("\\]", "", vec)
  vec <- gsub(" ", "", vec)
  as.integer(unlist(strsplit(vec, ",")))
}

# column_to_vector <- function(tbf, col_name) tbf[[col_name]]
# column_to_table   <- function(tbf, col_name) table(tbf[[col_name]])
# column_to_prop_table <- function(tbf, col_name) round(prop.table(table(tbf[[col_name]])) * 100, 1)

#words <- c("Diatonic Scales"="D", "Arpeggios"="A", "Figurations"="F", "L"="Link")
# long_names <- c("D" = "Diatonic (D)",
#                 "C" = "Chromatic (C)",
#                 "A" = "Arpeggio (A)",
#                 "J" = "Jump Arpeggio (J)",
#                 "F" = "Approach (F)",
#                 "L" = "Link (L)",
#                 "X" = "X Atom (X)",
#                 "T" = "Trill (T)",
#                 "R" = "Repetition (R)")

# directions <- c("Up" = "+",
#                 "Down"="-",
#                 "Oblique" = "")

int_span <- function(start, end){
  mapply(function(x, y){seq(x, y, 1)}, start, end) %>%
    unlist() %>%
    c() %>%
    unique() %>%
    sort()
}

vec_to_value <- function(int_vec){
  sprintf("[%s]", paste(int_vec, collapse=","))
}

add_overlaps <- function(data, type = c("all", "pre", "post")) {
  data <- data %>%
    mutate(pre_over = dplyr::lag(data$end)- data$start + 1)
  data <- data %>%
           mutate(post_over = dplyr::lead(data$pre_over))
  if(type[1] == "all"){
    return(data)
  }
  #data <- as.data.frame(data)
  if(type[2] == "pre"){
    return(data$pre_over)
  }
  if(type[3] == "post"){
    return(data$post_over)
  }
  stop("Unrecognized type")
}

get_gaps <- function(start, end, min_val = NULL, max_val = NULL){
  base <- int_span(start, end)
  #print(base)
  d <- diff(base)
  #print(d)
  gap_pos <- which(d > 1)
  #print(gap_pos)
  starts <- base[gap_pos] + 1
  ends   <- starts + d[gap_pos] - 2
  no_gaps <- length(starts) == 0
  left_border <- min(base)
  right_border <- max(base)
  #print(sprintf("Left:%d, right:%d", left_border, right_border))
  if(!is.null(min_val)){
    if(min_val < left_border){
      starts <- c(min_val, starts)
      ends   <- c(left_border - 1, ends)
    }
  }
  if(!is.null(max_val)){
    if(max_val > right_border){
      starts <- c(starts, right_border + 1)
      ends <- c(ends, max_val)
    }
  }
  tibble(length = ends - starts + 1, start = starts, end = ends)
}

get_arp_int_from_int <- function(x){
  if(length(x) > 1){
    return(sapply(x,  get_arp_int_from_int))
  }
  x <- x[1]
  if(!is.na(x) && abs(x) >=3 ){
    return(TRUE)
  }
  return(FALSE)
}

get_fuzzyint_from_int <- function(x){
  if(length(x) > 1){
    return(sapply(x,  get_fuzzyint_from_int))
  }
  x <- x[1]
  if (is.na(x)){
    return(NA)
  }
  if (x == 0){
    return(0)
  }
  s <- sign(x)
  a <- abs(x)
  if(a == 1 || a == 2){
    class <- 1
  }
  else if(a == 3 || a == 4){
    class <- 2
  }
  else if(a %in% 5:7){
    class <- 3
  }
  else{
    class <- 4
  }
  return(s *class)
}

get_rle_df <- function(int_vector){
  RLE <- rle(int_vector)
  l <- length(RLE$lengths)
  if(l == 1){
    start <- 1
    end   <- length(int_vector)
  }
  else{
    start <- cumsum(c(1, RLE$lengths[1:(l-1)]))
    end   <- cumsum(c(1, RLE$lengths[1:(l-1)])) + RLE$lengths-1
  }
  tibble(length = RLE$lengths,
         value = RLE$value,
         direction = sign(RLE$value),
         start = start,
         end   = end)
}

make_rle_df <- function(data, var){
  get_rle_df(data %>% pull(!!sym(var)))
}

values_from_positions <- function(int_vector, data){
  map2_chr(data$start, data$end, ~{vec_to_value(int_vector[.x:.y])})
}

directions_from_positions <- function(int_vector, data){
  map2_dbl(data$start, data$end, ~{sign(sum(int_vector[.x:.y]))})
}

classify_scale <- function(x){
  if(is.character(x)){
    x <- value_to_vec(x)
  }
  tt <- table(abs(x))
  stopifnot(length(tt) <= 2)
  if(length(tt) == 1){
    if( "1" %in% names(tt)){
      return("C")
    }
    return("D")
  }

  if(tt["1"] > (tt["2"] + 1)){
    return("C")
  }
  return("D")
}

find_chromatic_scales <- function(int_vector){
  scales <- get_rle_df(int_vector)
  scales <- scales %>% filter(abs(value) == 1, length>1)
  if(nrow(scales) == 0){
    return(NULL)
  }
  scales$type <- "C"
  scales$value <- values_from_positions(int_vector, scales)
  scales
}

find_diatonic_scales <- function(int_vector){
  scales <- get_rle_df(get_fuzzyint_from_int(int_vector))
  scales <- scales %>% filter(abs(value) == 1, length > 1)
  if(nrow(scales) == 0){
    return(NULL)
  }
  scales$type <- "D"
  scales$value <- values_from_positions(int_vector, scales)
  scales
}

split_scales <- function(int_vector){
  cs <- find_chromatic_scales(int_vector)
  #print(cs)
  #print(int_vector)
  if(is.null(cs)){
    return(NULL)
  }
  gaps <- get_gaps(cs$start, cs$end, max_val = length(int_vector), min_val=1)
  #print(gaps)
  #stop()
  if(nrow(gaps) == 0){
    return(cs)
  }
  ret <-list()
  for(i in 1:nrow(gaps)){
    row <- gaps[i,]
    #print(row)
    vec <- int_vector[row$start:row$end]
    #cat("Testing", vec, "\n")
    #print(row)
    if(row$length == 1){
      ret[[i]] <- tibble(length = 1,
                         value = vec_to_value(vec),
                         direction = sign(vec),
                         start = row$start,
                         end = row$end,
                         type = "X")
    }
    else{
      tmp <- find_diatonic_scales(vec)
      tmp$start <- tmp$start + row$start - 1
      tmp$end   <- tmp$end   + row$start - 1
      ret[[i]]  <- tmp
    }
  }
  bind_rows(cs, ret) %>% arrange(start)
}

find_scales <- function(int_vector){
  scales <- find_diatonic_scales(int_vector)
  if(is.null(scales)){
    return(NULL)
  }
  #cat("Found", nrow(scales), "rows for", int_vector, "\n")
  ret <- list()
  for(i in 1:nrow(scales)){
    row <- scales[i,]
    #print(row)
    splits <- split_scales(value_to_vec(row$value))
    if(!is.null(splits)){
      splits <- bind_rows(splits)
      splits$start <- splits$start + row$start -1
      splits$end   <- splits$end   + row$start -1
    }
    else {
      splits <- row
    }

    ret[[i]] <- splits
    #print(sprintf("Testing %s -> %d splits", row$value, nrow(splits)))
  }
  ret <- bind_rows(ret) %>% filter(type != "X")
  return(ret)
  ###### second approach#######
  #types <- purrrlyr::by_row(scales,
  #                          function(x) classify_scale(int_vector[x$start:x$end])) %>%
  #  tidyr::unnest(cols = c(.out)) %>%
  #  as.data.frame()
  #scales$type <- types[, ".out"]

  ##### old strategy #########
  #tmp <- get_rle_df(int_vector)
  #chromatics <- tmp %>% filter(abs(value) == 1, length > 1)
  #if(nrow(chromatics)){
  #  is <- intersect(scales$start, chromatics$start)
  #  positions <- mapply(function(x1,x2, y1, y2) x1 == x2 && y1== y2,
  #                      scales[scales$start %in% is, ]$start,
  #                      chromatics[chromatics$start %in% is, ]$start,
  #                      scales[scales$start %in% is, ]$end,
  #                      chromatics[chromatics$start %in% is, ]$end
  #                      )
  #  if(length(positions) && sum(positions)){
  #    positions <- is[positions]
  #    scales[scales$start %in% positions, ]$type<- "C"
  #  }
  #
  #}
  #scales
}

find_arpeggios <- function(int_vector){
  fuzzy_int_vector <- get_fuzzyint_from_int(int_vector)
  tmp <- get_rle_df(fuzzy_int_vector)
  tmp <- tmp %>% filter(abs(value) == 2, length>1)
  if(nrow(tmp) == 0){
    return(NULL)
  }
  tmp$type <- "A"
  tmp$value <- values_from_positions(int_vector, tmp)
  tmp
}


find_chords <- function(int_vector){
  arp_int_vector <- sign(int_vector)*get_arp_int_from_int(int_vector)
  tmp <- get_rle_df(arp_int_vector)
  tmp <- tmp %>% filter(value > 0, length>1)
  if(nrow(tmp) == 0){
    return(NULL)
  }
  tmp$type <- "J"
  tmp$value <- values_from_positions(int_vector, tmp)
  triads<- !as.logical(
    unlist(
      lapply(
        tmp$value,
        function(x) length(setdiff(unique(abs(value_to_vec(x))), c(3,4)))
        )
      )
  )
  #print(triads)
  if(length(triads) >0 && sum(triads)){
    tmp[triads,]$type <- "A"
  }
  tmp
}

find_trills <- function(int_vector){
  sum_vector <- int_vector + dplyr::lead(int_vector, 1)
  zero_crossings <- intersect(which(sum_vector == 0),
                              which(abs(int_vector) %in% c(1, 2, 3)))
  #print(zero_crossings)
  trills<-get_rle_df(sum_vector) %>% filter(value == 0, length > 0, start %in% zero_crossings)
  if(nrow(trills) == 0){
    return(NULL)
  }
  #if(length(zero_crossings) == 0){
  #  return(NULL)
  #}
  trills$length <- trills$length + 1
  trills$end <- trills$end + 1
  trills$type <- "T"
  trills$value <- values_from_positions(int_vector, trills)
  trills$direction <- 0
  return(trills)
  values <- values_from_positions(int_vector,
                                 tibble(start = zero_crossings,
                                        end = zero_crossings + 1))
  tibble(length = 2,
             type = "T",
             value = values,
             direction = sign(int_vector[zero_crossings]),
             start = zero_crossings,
             end   = zero_crossings + 1)
}

find_bigram <- function(int_vector, bigram, value = "F"){
  bigrams <- paste(int_vector, dplyr::lead(int_vector, 1), sep = " ")
  bigram <- paste(bigram, collapse=" ")
  pos <- which(bigrams == bigram)
  if(length(pos) == 0){
    return(NULL)
  }
  values <- values_from_positions(int_vector, tibble(start = pos, end = pos + 1))
  directions <- directions_from_positions(int_vector, tibble(start = pos, end = pos + 1))
  tibble(length = 2,
         type = "F",
         value = values,
         direction = directions,
         start = pos,
         end = pos + 1)
}

find_approaches <- function(int_vector){
  apps <-list()
  apps[["za_u"]] <- find_bigram(int_vector, c(-2, 1))
  apps[["za_d"]] <- find_bigram(int_vector, c( 2,-1))
  apps[["zb_u"]] <- find_bigram(int_vector, c(-3, 1))
  apps[["zb_d"]] <- find_bigram(int_vector, c( 3,-1))
  apps[["zc_u"]] <- find_bigram(int_vector, c(-3, 2))
  apps[["zc_d"]] <- find_bigram(int_vector, c( 3,-2))
  apps[["zd_u"]] <- find_bigram(int_vector, c( 4,-2))
  apps[["zd_d"]] <- find_bigram(int_vector, c(-4, 2))
  apps[["ze_u"]] <- find_bigram(int_vector, c(-1, 3))
  apps[["ze_d"]] <- find_bigram(int_vector, c( 1,-3))
  apps[["zf_u"]] <- find_bigram(int_vector, c(-2, 4))
  apps[["zf_d"]] <- find_bigram(int_vector, c( 2,-4))
  apps[["zg_u"]] <- find_bigram(int_vector, c(-2, 3))
  apps[["zg_d"]] <- find_bigram(int_vector, c( 2,-3))
  apps[["zh_u"]] <- find_bigram(int_vector, c(-1, 2))
  apps[["zh_d"]] <- find_bigram(int_vector, c( 1,-2))
  if(length(apps) == 0){
    return(NULL)
  }
  bind_rows(apps) %>% arrange(start)
}

get_overlaps <- function(class_df, int_vector){
  n <- nrow(class_df)
  ret <- NULL
  for(i in 1:(n-1)){
    s1 <- seq(class_df[i,]$start, class_df[i,]$end)
    s2 <- seq(class_df[i+1,]$start, class_df[i + 1,]$end)
    is <- intersect(s1, s2)
    value <- vec_to_value(int_vector[is])
    value1 <- vec_to_value(int_vector[s1])
    value2 <- vec_to_value(int_vector[s2])
    overlap <- class_df[i,]$end - class_df[i + 1,]$start + 1
    if(overlap){
      ret<-rbind(ret, tibble(end = class_df[i,]$end,
                             start = class_df[i + 1,]$start,
                             overlap_value = value,
                             value1 = value1,
                             value2 = value2,
                             overlap = overlap))
    }
  }
  return(ret)
}

fill_up_classes <- function(int_vector, class_df){
  l <- length(int_vector)
  if(is.null(class_df)  || nrow(class_df) == 0){
    prefix <- tibble(length = l,
                     type = "X",
                     direction = sign(sum(int_vector[1:l])),
                     value = vec_to_value(int_vector[1:l]),
                     start = 1,
                     end = l,
                     stringsAsFactors = F)
    return(prefix)
  }
  events <- int_span(class_df$start, class_df$end)
  min_e <- min(events)
  max_e <- max(events)

  #print("----HERE-----")
  gaps <- get_gaps(class_df$start, class_df$end, min_val = 1, max_val = length(int_vector))
  #print(gaps)
  #print(prefix)
  if(nrow(gaps) != 0){
    gaps$type <- "X"
    gaps$value <- values_from_positions(int_vector, gaps)
    gaps$direction <- directions_from_positions(int_vector, gaps)
  }
  else{
    gaps <- NULL
  }
  return(normalize(gaps))
}

find_repetitions <- function(int_vector){
  tmp <- get_rle_df(int_vector)
  repetitions <- tmp %>% filter(abs(value) == 0, length>1)
  if(nrow(repetitions)){
    repetitions$type <- "R"
    repetitions$value <- values_from_positions(int_vector, repetitions)
    repetitions <- repetitions[, c("length", "type", "value", "direction", "start", "end")]
    return(repetitions)
  }
  return(NULL)
}

normalize <- function(data){
  if(!is.null(data) && length(data) > 0){
    data <- data[, c("length", "type", "value", "direction", "start", "end")]
  }
  data
}

find_classes <- function(int_vector, debug = F){
  if(debug) cat("Testing", paste(int_vector, collapse = ","), "\n")
  repetitions <- find_repetitions(int_vector)
  #print(repetitions)
  scales <- find_scales(int_vector)
  #print(scales)
  arps <- find_chords(int_vector)
  #print(arps)
  trills <- find_trills(int_vector)
  #print(trills)
  approaches <- find_approaches(int_vector)
  #print(approaches)
  #print(length(int_vector))
  #print(unlist(lapply(list(repetitions, arps, scales, trills, approaches), nrow)))
  #print(length(approaches))
  #print(lapply(list(repetitions, arps, scales, trills, approaches), normalize))

  tmp <- bind_rows(lapply(list(repetitions,
                              arps,
                              scales,
                              trills,
                              approaches), normalize))
  if(nrow(tmp) > 0){
    tmp <- tmp %>% arrange(start)
  }
  gaps <- fill_up_classes(int_vector, tmp)
  #print(gaps)
  tmp <- rbind(tmp, gaps) %>% arrange(start)
  tmp <- add_overlaps(tmp, type = "all")
  return(tmp)
}

remove_rows <- function(data, rows){
  all_rows <- 1:nrow(data)
  left_over <- setdiff(all_rows, rows)
  data[left_over,]
}

remove_redundants <- function(data, debug = F){
  to_remove <- which(data$length == 2 &
                       data$pre_over == 1 &
                       data$post_over == 1 & data$type %in% c("F", "T"))
  i <- 0
  #print(data)
  limits <- 1 + length(to_remove)
  while(length(to_remove) > 0){
    i <- i + 1
    if(i >limits) stop("Found infinite loop while removing redundants")
    #print(sprintf("Removing row %d", to_remove[1]))
    data <- remove_rows(data, to_remove[1])
    data <- data %>% add_overlaps(type = "all")
    to_remove <- which(data$length == 2 &
                         data$pre_over == 1 &
                         data$post_over == 1 & data$type %in% c("F", "T"))
    #print(data)
  }
  #data <- data %>% add_overlaps(type="all")
  if(sum(abs(stats::na.omit(data$pre_over)) > 1) || sum(abs(stats::na.omit(data$post_over)) > 1)){
    print("Found invalid overlap")
    print(data)
    stop()
  }
  data
}

cut_row <- function(data, row_index, type=c("left", "right")){
  row <- data[row_index,]
  if(nrow(row) == 0){
    return(data)
  }
  if(row$length == 1){
    return(remove_rows(data, row_index))
  }
  vec <- value_to_vec(row$value)
  dir_flag <- 1
  if(type[1] == "left"){
    new_vec <- vec[2:length(vec)]
    row$start <- row$start+1
  } else{
    new_vec <- vec[1:(length(vec)-1)]
    row$end <- row$end -1
  }
  tmp <- find_classes(new_vec, debug=F)
  if(nrow(tmp)>1){
    stop("Found to long a row")
  }
  row$type <-tmp[1,]$type
  row$value <- vec_to_value(new_vec)
  row$direction <- sign(sum(new_vec))
  if(row$type == "X" && row$direction == 0){
    print(new_vec)
    stop()
  }
  row$length <- row$length-1
  data[row_index,] <- row
  data
}

get_two_rows <- function(data, row_index){
  data[row_index:(row_index + 1),]
}

show_overlaps <- function(data, thresh = 0, type = NULL, complete_idx = F){
  data %>%
    mutate(rows = 1:nrow(data)) %>%
    filter(post_over > thresh) -> overs
  if (!is.null(type)){
    overs <- overs %>% filter(type ==type)
  }
  overs <- overs %>%
    pull(rows)

  if(complete_idx){
    return(sort(unique(c(overs, overs + 1))))
  }
  return(sort(overs))
}

fuse_row_pair <- function(data, row_index){
  if(row_index < 1 || row_index > nrow(data)){
    stop("Invalid row index")
  }
  row1 <- data[row_index,]
  row2 <- data[row_index + 1,]
  type1 <- row1$type
  type2 <- row2$type
  if(type1 != type2) {
    return(data)
  }
  dir1 <- row1$direction
  dir2 <- row2$direction
  if(type1 != "X" && dir1 * dir2 < 0) {
    return(data)
  }
  new_row <- row1
  #print(sprintf("Fusing %s and %s", row1$value, row2$value))
  new_value <- c(value_to_vec(row1$value), value_to_vec(row2$value))
  #if(row_index == 14)print(new_value)
  #print((find_classes(new_value)))
  cc <- add_overlaps(normalize(find_classes(new_value)))
  #data[row_index,]$value <- new_value
  cc$start <- cc$start + row1$start - 1
  cc$end <-  cc$end + row1$start - 1
  data <- remove_rows(data, row_index)
  #print(data)
  data <- remove_rows(data, row_index)
  #print(data)

  data <- bind_rows(data, cc) %>% arrange(start)
  #print(data)
  #if(row_index == 14)stop()
  #stop()
  data %>% add_overlaps()
}

resolve_row_pair <- function(data, row_index){
  #browser()
  if(row_index <1 || row_index > nrow(data)){
    stop("Invalid row index")
  }
  #pat1 <- data[row_index,]$pattern
  #pat2 <- data[row_index+1,]$pattern
  #if(pat1 != pat2){
  #  warning("Different patterns")
  #  return(data)
  #}
  type1 <- data[row_index,]$type
  type2 <- data[row_index + 1,]$type
  dir1 <- data[row_index,]$direction
  dir2 <- data[row_index + 1,]$direction
  if(type1 == "F" && (type2 == "C" || type2 == "D" || type2 == "X" )){
    data <- cut_row(data, row_index, type = "right")
  }
  #if(type1 == "F" && (type2 == "C" || type2 == "D" || type2 == "X" )){
  #  data <- cut_row(data, row_index, type = "right")
  #}
  if(type1 == "F" && (type2 == "T" || type2 == "F")){
    data <- cut_row(data, row_index + 1, type = "left")
  }
  if(type1 == "J" && (type2 == "A" || type2 == "F")){
    data <- cut_row(data, row_index, type = "right")
  }
  if(type2 == "J" && (type1 == "A" || type1 == "F")){
    data <- cut_row(data, row_index + 1, type = "left")
  }
  if(type1 == "F" && type2 == "A"){
    data <- cut_row(data, row_index, type = "right")
  }
  if(type1 == "A" && type2 == "F"){
    data <- cut_row(data, row_index + 1, type = "left")
  }
  if((type1 == "D" || type1 == "C") && (type2 == "T" || type2 == "F")){
    data <- cut_row(data, row_index + 1, type = "left")
  }
  if(type1 == "T" && (type2 == "C" || type2 == "D" || type2 == "F" || type2 == "X") ){
    data <- cut_row(data, row_index, type = "right")
  }
  if(type1 == "X"){
    data <- cut_row(data, row_index, type = "right")
  }
  data %>% add_overlaps()
}

find_doubles <- function(vec){
  which(vec == lead(vec, 1))
}

resolve_overlaps <- function(data, debug = F){
  #browser()
  overs <- show_overlaps(data)
  if(length(overs) == 0){
    return(data)
  }
  i <- 0
  #print(overs)
  limit <- ifelse(debug, 1, 10) + length(overs)
  while(length(overs) > 0){
    i <- i + 1
    if(i > limit) {
      browser()
      stop("Infinite loop in resolving overlaps")
    }
    if(debug)cat("overs pre", overs, "\n")
    data <- resolve_row_pair(data, overs[1])
    if(debug)print(data)
    overs <- show_overlaps(data)
    if(debug)cat("overs post", overs, "\n")
  }
  #print(data)
  #print(data$type[find_doubles(data$type)])
  doubles <- find_doubles(data$type)
  if(length(doubles) == 0){
    return(data)
  }

  double_X <- doubles[which(data$type[doubles] == "X")]
  i <- 0
  limit <- ifelse(debug, 1, 10) + length(double_X)
  while(length(double_X) > 0){
    i <- i + 1
    if(i > limit) stop("Infinite loop in fusing rows")
      if(debug)cat("Fusing row", double_X[1], "\n")
      data <- fuse_row_pair(data, double_X[1])
      #if(debug) print(data)
      doubles <- find_doubles(data$type)
      #print(doubles)
      if(length(doubles) == 0){
        break
      }
      double_X <- doubles[which(data$type[doubles] == "X")]
      if(debug)cat("Post double_X", double_X, "\n")
  }
  data
}

count_overlap <- function(data){
  pre <- data$pre_over
  post <- data$post_over
  pre[is.na(pre)] <- 0
  post[is.na(post)] <- 0
  total_overlap <-pre + post
  table(total_overlap)
}

remove_overlaps <- function(data){
}

get_class_code <- function(data, type = c("full", "reduced", "short", "extended"), as_vector = F, with_overlap = T){
  if(is.character(data)){
    if(with_overlap){
      data <- find_classes(value_to_vec(data))
    }
    else{
      data <- find_wba(value_to_vec(data))
    }
  }
  else if(is.numeric(data)){
    if(with_overlap){
      data <- find_classes(data)
    }
    else{
      data <- find_wba(data)

    }
  }

  pre_over <- data$pre_over
  pre_over[is.na(pre_over)] <- 0
  overlaps_symbols <- c("", "_")
  collapse <- ""
  if(as_vector){
    collapse <- " "
  }
  if(type[1] == "extended"){
    ret <- paste(sprintf("%s%s%s%s",
                         (c("-", "", "+")[data$direction + 2]),
                         data$type,
                         data$value,
                         overlaps_symbols[pre_over + 1]),
                 collapse=collapse)
  }
  else if(type[1] == "full"){
    #browser()
    ret <- paste(sprintf("%s%s%d%s",
                         (c("-", "", "+")[data$direction + 2]),
                         data$type,
                         data$length,
                         overlaps_symbols[pre_over + 1]),
                 collapse = collapse)
  }
  else if (type[1] == "reduced"){
    ret<-paste(sprintf("%s%s%s",
                       (c("-", "", "+")[data$direction + 2]),
                       data$type,
                       overlaps_symbols[pre_over + 1]),
               collapse = collapse)
  }
  else if (type[1] == "short"){
    ret<-paste(sprintf("%s", data$type),
          collapse = collapse)
  }
  else {
    stop("Invalid type")
  }
  if(as_vector){
    ret <- unlist(strsplit(ret, collapse))
  }
  ret
}

get_class_code_raw <- function(data, type = c("full", "reduced", "short", "extended"), as_vector = F){
  collapse <- ""
  if(as_vector){
    collapse <- " "
  }
  if(type[1] == "extended"){
    ret <- paste(sprintf("%s%s%s",
                         (c("-", "", "+")[data$direction+2]),
                         data$type,
                         data$value),
                 collapse = collapse)
  }
  else if(type[1] == "full"){
    ret <- paste(sprintf("%s%s%d",
                         (c("-", "", "+")[data$direction+2]),
                         data$type,
                         data$length),
                 collapse = collapse)
  }
  else if (type[1] == "reduced"){
    ret<-paste(sprintf("%s%s",
                       (c("-", "", "+")[data$direction+2]),
                       data$type),
               collapse = collapse)
  }
  else if (type[1] == "short"){
    ret<-paste(sprintf("%s", data$type),
               collapse = collapse)
  }
  else {
    stop("Invalid type")
  }
  if(as_vector){
    ret <- unlist(strsplit(ret, collapse))
  }
  ret
}

compare_freq <- function(data1, data2, type = "F"){
  tt <- bind_rows(
    tibble(source = 1, value=data1[data1$type == type,]$value),
    tibble(source = 2, value=data2[data2$type == type,]$value))
  #print(tt)
  tt$value <- factor(tt$value)
  tt1 <- table(tt[tt$source == 1,]$value)
  tt2 <- table(tt[tt$source == 2,]$value)
  #print(prop.table(tt1)-prop.table(tt2))
  stats::chisq.test(tt1, p = tt2, rescale.p = T)

}

hash_wba_dataframe <- function(data){
  num_bytes <- 8
  if(nrow(data) > num_bytes){
    tl <- nrow(data) - num_bytes
    data <- data[1:num_bytes,]
    too_long <- TRUE
    print(sprintf("Too long by %d", tl))
  }
  hashes <- purrrlyr::by_row(data, function(x){
                          hash_wba(x$type, x$direction, x$length)}) %>%
    select(cols = c(.out)) %>%
    unlist() %>%
    as.integer()
  hashes <- rev(c(hashes, rep(0, num_bytes-length(hashes))))
  hashes <- (sum(hashes * 2^{seq(0, num_bytes * 8 - 1, 8)}))
  hashes
}

hash_wba <- function(type, direction, len){
  if(type == "Ax"){
    type <- "J"
  }
  type_val <- as.integer(factor(type, levels = c("A", "C", "D", "F", "J", "R", "T", "X"))) -1
  direction <- ifelse(direction > 0, 1, 0)
  len <- ifelse(len > 16, 16, len) - 1
  return(16 * (8 * direction + type_val) + len)
}

#' find_wba
#'
#' This function parses a interval vector (or corresponding string) into its WBA constituents
#'
#' @param x (integer vector or character string) interval sequence
#' @return Data frame with columns \code{length} (length of atom), \code{type} (WBA type),
#' \code{value} (interval string),
#' \code{direction} (direction of atom),
#' \code{start, end} (start and end position in sequence),
#' \code{pre_over, post_over} (amount of overlap to previous or following atom.)
#' @export
find_wba <- function(x, debug = F){
  if(is.character(x)){
    x <- value_to_vec(x)
  }
  x %>%
    find_classes(debug = debug) %>%
    remove_redundants(debug = debug) %>%
    resolve_overlaps(debug = debug)
}

#' find_wba_by_phrase
#'
#' This function parses an \code{int_raw} column of  a data frame into its WBA constituents separately by phrases and optional by id.
#'  Columns \code{id} and \code{phrase_id} must be present.
#'
#' @param data (data.frame) must contain int_raw, id and phrase_id columns,
#' @param id (string) string specifying id to use, if \code{NULL} the entire data frame is used.
#' @return Data frame with columns \code{length} (length of atom), \code{type} (WBA type),
#' \code{value} (interval string),
#' \code{direction} (direction of atom),
#' \code{start, end} (start and end position in sequence),
#' \code{pre_over, post_over} (amount of overlap to previous or following atom.)
#' @export
find_wba_by_phrase <- function(data, id = NULL){
  if(!is.null(id)){
    data <- data[data$id %in% id,]
    #message(sprintf("Checking '%s' with %d phrases", paste(id, collapse = ", "), length(unique(data$phrase_id))))
  }
  phrases <- unique(data$phrase_id)
  ret <- list()
  for(i in phrases){
    #print(sprintf("Checking phrase %d", i))
    int_vector <- data[data$phrase_id == i,]$int_raw %>% na.omit()
    #ignore last interval because it belongs to next phrase
    tmp <- find_wba(int_vector[1:(length(int_vector)-1)])
    tmp$phrase_id <- i
    ret[[i]] <- tmp
  }
  ret <- bind_rows(ret)
  if(!is.null(id)){
    ret$id <- id
    ret$id <- as.character(ret$id)
  }
  if(nrow(ret[ret$type == "X" & ret$length == 1,])){
    ret[ret$type == "X" & ret$length == 1,]$type <- "L"
  }
  ret
}


recalc_pattern_classes <- function(data){
  patterns <- lapply(unique(data$value), value_to_vec)
  patterns_classes <- purrr::map(patterns,
                                 function(x) tibble(pattern = vec_to_value(x),
                                                        find_wba(x)))
  #pattern_classes <<- purrr::map(patterns_CC, function(x) resolve_overlaps(remove_redundants(x)))
  patterns_classes %>% bind_rows()
}


