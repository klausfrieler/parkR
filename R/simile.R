library(tidyverse)
messagef <- function(...) message(sprintf(...))
pc_labels <- c("C", "C#/Db", "D", "D#/Eb", "E", "F", "F#/Gb", "G", "G#/Ab", "A", "Bb", "B")
pc_labels_sharp <- c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")
pc_labels_flat <- c("C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B")

asc <- function(x, n = 1){
  raw <- charToRaw(x)
  if(n < 0){
    n <- length(raw) + n + 1
  }
  if(n == 0){
    return(strtoi(raw, 16))
  }
  strtoi(raw, 16)[n]
}

#' @export
edit_dist <- function(s, t){
  adist(s,t)[1,1]
}

#' @export
edit_sim <- function(s, t){
  1 - edit_dist(s, t)/max(nchar(s), nchar(t))
}

#' @export
get_all_ngrams <- function(x, N = 3){
  l <- length(x) - N + 1
  stopifnot(l > 0)
  map_df(1:l, function(i){
    ngram <- x[i:(i + N - 1)]
    tibble(start = i, N = N, value = paste(ngram, collapse = ","))
  })
}

#as in  Müllensiefen & Frieler (2004)
ngrukkon <- function(x, y, N = 3){
  #browser()
  x <- get_all_ngrams(x, N = N) %>% pull(value)
  y <- get_all_ngrams(y, N = N) %>% pull(value)
  joint <- c(x, y) %>% table()
  tx <- factor(x, levels = names(joint)) %>% table()
  ty <- factor(y, levels = names(joint)) %>% table()
  1 - sum(abs(tx  - ty))/(length(x) + length(y))
}

#Krumhansl-Schmuckler algorithm
get_implicit_harmonies <- function(pitch_vec, segmentation = NULL, only_winner = T){
  ks_weights_major <- c(6.33, 2.23, 3.48, 2.33, 4.38, 4.09, 2.52, 5.19, 2.39, 3.66, 2.29, 2.88)
  ks_weights_minor <- c(6.33, 2.68, 3.52, 5.38, 2.60, 3.53, 2.54, 4.75, 3.98, 2.69, 3.34, 3.17)
  if(!is.null(segmentation)){
    if(length(segmentation) != length(pitch_vec)){
      stop("Segmentation must be of same length as pitch")
    }
    s <- unique(segmentation)
    return(
      map_dfr(s, function(x){
      #browser()
        pv <- pitch_vec[segmentation == x]
        tibble(segment = x, key = get_implicit_harmonies(pv, NULL, only_winner = only_winner) %>%   pull(key))
    })
    )

  }
  pitch_freq <- table(factor(pitch_vec  %% 12, levels = 0:11))
  correlations <-
    map_dfr(0:11, function(t){
      #browser()
      w_major <- cor.test(pitch_freq, ks_weights_major[((0:11 - t) %% 12) + 1]) %>% broom::tidy() %>% pull(estimate)
      w_minor <- cor.test(pitch_freq, ks_weights_minor[((0:11 - t) %% 12) + 1]) %>% broom::tidy() %>% pull(estimate)
      bind_rows(tibble(transposition = t,  match = w_major, type = "major", key = sprintf("%s-maj", pc_labels[t+1])),
                tibble(transposition = t,  match = w_minor, type = "minor", key = sprintf("%s-min", pc_labels[t+1])))
    }) %>% arrange(desc(match))
  #print(correlations)
  if(only_winner){
    return(correlations[1,])
  }
  correlations

}
bootstrap_implicit_harmonies <- function(pitch_vec, segmentation = NULL, sample_frac = .8, size = 10){
  if(!is.null(segmentation)){
    segments <- unique(segmentation)
    ret <-
      map_dfr(segments, function(seg){
        bootstrap_implicit_harmonies(pitch_vec[segmentation == seg],
                                     NULL,
                                     sample_frac = sample_frac,
                                     size = size) %>%
        mutate(segment = seg)
    })
    return(ret)
  }
  l <-length(pitch_vec)
  sample_size <- max(1, round(sample_frac * l))

  bs <-
    map_dfr(1:size, function(x){
    pv <- sample(pitch_vec, replace = T, sample_size)
    get_implicit_harmonies(pitch_vec = pv,  only_winner = T)
  })
  best_key <- bs %>% count(key) %>% arrange(desc(n)) %>% pull(key)
  bs %>% filter(key == best_key[1]) %>% head(1)
  #bs
}

classify_duration <- function(dur_vec, ref_duration = .5){
  rel_dur <- dur_vec/ref_duration
  rhythm_class <- rep(NA, length(rel_dur))
  rhythm_class[rel_dur > 0 & !is.na(rel_dur)] <- -2
  rhythm_class[rel_dur > 0.45] <- -1
  rhythm_class[rel_dur > 0.9] <- 0
  rhythm_class[rel_dur > 1.8] <- 1
  rhythm_class[rel_dur > 3.3] <- 2
  rhythm_class
}

#' @export
rhythfuzz <- function(dur_vec1, dur_vec2){
  edit_sim(intToUtf8(dur_vec1[!is.na(dur_vec1)] + 128),
           intToUtf8(dur_vec2[!is.na(dur_vec2)] + 128))
}

#' @export
edit_sim_vec <- function(vec1, vec2 = NULL){
  #browser()
  self_similarity <- FALSE
  if(is.null(vec2)){
    vec2 <- vec1
    self_similarity <- TRUE
  }
  n1 <- length(vec1)
  n2 <- length(vec2)
  if(is.character(vec1)){
    vec1 <- value_to_vec(vec1, simplify = n1 == 1 && n2 == 1)
  }
  if(is.character(vec2)){
    vec2 <- value_to_vec(vec2, simplify = n1 == 1 && n2 == 1)
  }
  stopifnot(all(sapply(vec1, is.integer), sapply(vec2, is.integer)))
  if(n1 == 1 && n2 == 1){
    ret <- edit_sim(intToUtf8(vec1[!is.na(vec1)] + 128), intToUtf8(vec2[!is.na(vec2)] + 128))
    return(ret)
  }
  ret <- matrix(1, nrow = n1, ncol = n2)
  if(self_similarity){
    for(i in 1:(n1 - 1)){
      v1 <- vec1[[i]]
      ret[i, i] <- 1
      for(j in (i + 1):n2){
        v2 <- vec2[[j]]
        ret[i, j] <-  edit_sim(intToUtf8(v1[!is.na(v1)] + 128), intToUtf8(v2[!is.na(v2)] + 128))
        ret[j, i] <- ret[i, j]
      }
    }
  }
  else{
    for(i in 1:n1){
      v1 <- vec1[[i]]
      for(j in 1:n2){
        v2 <- vec2[[j]]
        ret[i, j] <-  edit_sim(intToUtf8(v1[!is.na(v1)] + 128), intToUtf8(v2[!is.na(v2)] + 128))
      }
    }
  }
  ret
}

#' @export
harmcore <- function(pitch_vec1, pitch_vec2, segmentation1 = NULL, segmentation2 = NULL){
  #browser()
  implicit_harm1 <- get_implicit_harmonies(pitch_vec1, segmentation1) %>% pull(key)
  implicit_harm2 <- get_implicit_harmonies(pitch_vec2, segmentation2) %>% pull(key)
  print(implicit_harm1)
  print(implicit_harm2)

  common_keys <- levels(factor(union(implicit_harm1, implicit_harm2)))
  implicit_harm1 <- factor(implicit_harm1, levels = common_keys) %>% as.integer()
  implicit_harm2 <- factor(implicit_harm2, levels = common_keys) %>% as.integer()
  edit_sim(intToUtf8(implicit_harm1), intToUtf8(implicit_harm2))
}

#' @export
harmcore2 <- function(pitch_vec1, pitch_vec2, segmentation1 = NULL, segmentation2 = NULL){
  implicit_harm1 <- bootstrap_implicit_harmonies(pitch_vec1, segmentation1) %>% pull(key)
  implicit_harm2 <- bootstrap_implicit_harmonies(pitch_vec2, segmentation2) %>% pull(key)
  common_keys <- levels(factor(union(implicit_harm1, implicit_harm2)))
  implicit_harm1 <- factor(implicit_harm1, levels = common_keys) %>% as.integer()
  implicit_harm2 <- factor(implicit_harm2, levels = common_keys) %>% as.integer()
  edit_sim(intToUtf8(implicit_harm1), intToUtf8(implicit_harm2))
}

#little helper to calculate modus of simple vector
modus <- function(x){
  t <- table(x)
  as(names(t[t == max(t)]), class(x))

}

#find a list of candidates for best transpositions for two pitch vectors, based on basic stats
get_transposition_hints <- function(pitch_vec1, pitch_vec2){
  ih1 <- get_implicit_harmonies(pitch_vec1, only_winner = T)
  key1 <- ih1 %>% pull(key)
  pc1 <- ih1 %>% pull(transposition)
  ih2 <- get_implicit_harmonies(pitch_vec2, only_winner = T)
  pc2 <- ih2 %>% pull(transposition)
  key_diff <- (pc2 -  pc1) %% 12
  #messagef("Best key 1 = %s, best key 2 = %s, key diff = %d", key1, ih2 %>% head(1) %>% pull(key), key_diff )
  modus1 <- modus(pitch_vec1)
  modus2 <- modus(pitch_vec2)
  ret <- c(modus1 - modus2,
           round(mean(pitch_vec1)) - round(mean(pitch_vec2)),
           round(median(pitch_vec1)) - round(median(pitch_vec2)))
  octave_offset <- modus(round(ret/12))
  #messagef("Octave offset = %d", octave_offset)
  ret <- c(0, ret, octave_offset*12 + key_diff, octave_offset*12 + 12 - key_diff)
  unique(ret) %>% sort()

}

#' finds transposition that maximize raw edit distance of two pitch vectors
#' transposition in semitone of the *second* melody
#' @export
find_best_transposition <- function(pitch_vec1, pitch_vec2){
  trans_hints <- get_transposition_hints(pitch_vec1, pitch_vec2)
  sims <- map_dfr(trans_hints, function(x){
    #browser()
    tibble(transposition = x, sim = edit_dist(intToUtf8(pitch_vec1), intToUtf8(pitch_vec2 + x)))
  })
  sims %>% arrange(sim) %>% head(1) %>% pull(transposition)
}

#' Famous opti3 measure according to Müllensiefen & Frieler (2004)
#' @export
opti3 <- function(pitch_vec1, dur_vec1, pitch_vec2, dur_vec2, N = 3, use_bootstrap = F, classify_duration = T){
  pitch_vec1 <- round(pitch_vec1)
  pitch_vec2 <- round(pitch_vec2)
  v_ngrukkon <- ngrukkon(pitch_vec1, pitch_vec2, N = N)
  if(classify_duration){
    dur_vec1 <- classify_duration(dur_vec1)
    dur_vec2 <- classify_duration(dur_vec2)
  }
  v_rhythfuzz <- rhythfuzz(dur_vec1, dur_vec2)

  if(use_bootstrap){
    v_harmcore <- harmcore2(pitch_vec1, pitch_vec2)
  }
  else{
    v_harmcore <- harmcore(pitch_vec1, pitch_vec2)

  }
  opti3 <- 0.505 *  v_ngrukkon + 0.417  * v_rhythfuzz + 0.24  * v_harmcore - 0.146
  opti3 <- max(min(opti3, 1), 0)
  messagef("ngrukkon = %.3f, rhythfuzz = %.3f, harmcor = %.3f, opti3 = %.3f",
           v_ngrukkon, v_rhythfuzz, v_harmcore, opti3)
  opti3
}

#read a pYIN note track and make it nice
read_melody <- function(fname){
  melody <-
    read.csv(fname, header = F) %>%
    as_tibble() %>%
    rename(onset = V1, freq = V2, dur = V3) %>%
    mutate(pitch = round(freq_to_midi(freq)),
           ioi = c(diff(onset), NA),
           ioi_class = classify_duration(ioi))
  #browser()
  if(any(is.na(melody$pitch)) || any(is.infinite(melody$pitch))){
    stop("Warning: Melody (%s) contains invalid pitches", fname)
  }
  if(any(melody$ioi[!is.na(melody$ioi)] < .01)){
    stop("Warnings: Melody (%s) contains IOIs less than 1 ms, possibly no note track", fname)
  }
  melody
}

read_mcsv2 <- function(fname = "e:/Expertises/bachkoenig/cmp/live_is_life_nana.mcsv"){
  melody <- read.csv(fname, header = T, sep = ";", stringsAsFactors = F) %>%
    as_tibble() %>%
    mutate(ioi = c(diff(onset), NA),
           ioi_class = classify_duration(ioi),
           signature = str_extract(signature, "[0-9]+/[0-9]+"))
  melody
}

#' opti3 for melodies read by read_melody or read_mcsv2
#' returns sorted tibble of transpositions of melody2 and opti3 similarity
#' @export

opti3_df <- function(melody1, melody2, N = 3, use_bootstrap = F){
  trans_hints <- get_transposition_hints(melody1$pitch, melody2$pitch)
  v_rhythfuzz <- rhythfuzz(melody1$ioi_class, melody2$ioi_class)
  seg1 <- NULL
  seg2 <- NULL
  if("bar" %in% names(melody1)){
    seg1 <- melody1$bar
  }
  if("bar" %in% names(melody2)){
    seg2 <- melody2$bar
  }
  sims <- map_dfr(trans_hints, function(th){
    v_ngrukkon <- ngrukkon(melody1$pitch, melody2$pitch + th, N = N)
    if(use_bootstrap){
      v_harmcore <- harmcore2(melody1$pitch,
                              melody2$pitch + th,
                              segmentation1 = seg1,
                              segmentation2 = seg2)
    }
    else{
      v_harmcore <- harmcore(melody1$pitch,
                             melody2$pitch + th,
                             segmentation1 = seg1,
                             segmentation2 = seg2)
    }
    tibble(transposition = th,
           ngrukkon = v_ngrukkon,
           rhythfuzz = v_rhythfuzz,
           harmcore = v_harmcore,
           opti3 =  0.505 *  v_ngrukkon + 0.417  * v_rhythfuzz + 0.24  * v_harmcore - 0.146)
  })
  sims %>% arrange(desc(opti3))
}


#windowed version, shifts shorter melody along longer
#returns tibble of shift offsets and highest opti3 similarity
best_subsequence_similarity <- function(melody1, melody2){

  shorter <- melody1
  longer <- melody2
  swap <- "1 along 2"
  l1 <- nrow(melody1)
  l2 <- nrow(melody2)
  if(l2 < l1){
    shorter <- melody2
    longer <- melody1
    swap <- "2 along 1"

  }
  l1 <- nrow(shorter)
  l2 <- nrow(longer)
  d_l <- l2 - l1
  map_dfr(1:(d_l + 1), function(offset){
    #messagef("Offset %d", offset)
    longer_snip <- longer[seq(offset, offset + l1 - 1),]
    tibble(offset = offset - 1, sim  =  opti3_df(shorter, longer_snip) %>% head(1) %>% pull(opti3))
  }) %>% mutate(process = swap) %>% arrange(desc(sim))
}
