img_format <- "png"
default_text_size <- 12
default_color <- "darkslategrey"

default_color1  <-"lightblue4"
default_color2 <-"lightblue3"
default_color3 <-"lightblue2"
default_color4 <-"lightblue1"
jazzomat_red <- "indianred3"
jazzomat_gold <-"lightgoldenrod4"
jazzomat_blue <-"aquamarine4"

jazzomat_palette  <- list(
  set1 = c(default_color, default_color2, default_color3, default_color4),
  set2 = c(default_color, default_color2, default_color2, default_color),
  set3 = c(default_color,jazzomat_red, default_color2, default_color)
)

default_grid_color <- "gray64"

basic_theme <- "minimal"

labels <- list(
  cpc_labels = c("1", "b9", "9", "b3", "3", "11", "#11/b5", "5", "b13", "13", "7", "j7", "NA"),
  durclass_labels = c("Very Short", "Short", "Medium", "Long", "Very Long"),
  fuzzy_labels = c("Large Jump Down", "Jump Down","Leap Down","Step Down","Repetition","Step Up","Leap Up","Jump Up","Large Jump Up"),
  pc_labels = c("C", "C#/Db", "D", "D#/Eb", "E", "F", "F#/Gb", "G", "G#/Ab", "A", "Bb", "B"),
  pc_labels_sharp = c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"),
  pc_labels_flat = c("C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B"),
  metric_pos_4_4 = c("1", "", "", "", "", "", "1+", "", "", "", "", "", "2", "", "", "", "", "", "2+", "", "", "", "", "",  "3", "", "", "", "", "", "3+", "", "", "", "", "", "4", "", "", "", "", "","4+", "", "", "", "", "" ),
  main_idea = c("rhythm","lick","lick","line","melody","lick","lick","rhythm","line","line","line","line","line","line","line","line","line","line","rhythm","line","line","melody","line","line","line","melody","line","fragment","line","line","expressive","melody","expressive","line","line","lick","rhythm","rhythm","line","line","line","rhythm","rhythm","rhythm","rhythm","line","line","expressive","line","line","line","expressive","expressive","melody","line","expressive"),
  sub_ideas = c("rhythm-single-irregular","lick","lick","line-wavy-ascending","melody","lick","lick","rhythm-single-irregular","line-wavy-descending","line-tick-rabble","line-tick-rabble","line-tick-rabble","line-tick-rabble","line-tick-rabble","line-tick-rabble","line-descending","line-wavy-descending","line-tick-slide","rhythm-single-irregular","line-descending","line-wavy-ascending","melody","line-wavy-descending","line-wavy-ascending","line-wavy-concave","melody","line-wavy-convex","fragment","line-wavy-horizontal","line-wavy-ascending","expressive","melody","expressive","line-wavy-descending","line-ascending","lick","rhythm-multi-regular","rhythm-multi-regular","line-wavy-ascending","line-descending","line-wavy-descending","rhythm-multi-regular","rhythm-multi-regular","rhythm-multi-regular","rhythm-multi-regular","line-wavy-descending","line-wavy-ascending","expressive","line-wavy-horizontal","line-wavy-descending","line-wavy-ascending","expressive","expressive","melody","line-wavy-horizontal","expressive"),
  parsons_labels = c("Down", "Repeat", "Up"),
  cdpc_labels = c("1", "2", "3", "4", "5", "6", "7", "#9", "#7", "#11",  "b7", "#10"),
  cdpcx_labels = c("1", "2", "3", "4", "5", "6", "7", "#9", "#7", "#11",  "b7", "#10", "b9", "b13"),
  contour_labels =  c("Ascending", "Ascending-Horizontal", "Concave", "Convex", "Descending", "Descending-Horizontal", "Horizontal", "Horizontal-Ascending", "Horizontal-Descending"),
  redcontour_labels =  c("Ascending", "Descending",  "Convex", "Concave",  "Horizontal")

)

make_octave <- function(n){
  c(sprintf("C%s", as.character(n)), rep("", 11))
}

MIDI_labels <- NULL
MIDI_reduced_labels <- NULL
for (i in -1:10){
  MIDI_labels <- c(MIDI_labels, make_octave(i))
  MIDI_reduced_labels <- c(MIDI_reduced_labels, make_octave(i))
}
MIDI_labels[8 + 0:11 * 12] <- paste("G", -1:10, sep="")
MIDI_labels[3 + 0:11 * 12] <- paste("D", -1:10, sep="")
MIDI_labels[5 + 0:11 * 12] <- paste("E", -1:10, sep="")
MIDI_labels[6 + 0:11 * 12] <- paste("F", -1:10, sep="")
MIDI_labels[10 + 0:11 * 12] <- paste("A", -1:10, sep="")
MIDI_labels[12 + 0:11 * 12] <- paste("B", -1:10, sep="")

MIDI_full <- MIDI_labels
MIDI_full[9 + 0:11*12] <- paste("Ab", -1:10, sep="")
MIDI_full[2 + 0:11*12] <- paste("Db", -1:10, sep="")
MIDI_full[4 + 0:11*12] <- paste("Eb", -1:10, sep="")
MIDI_full[7 + 0:11*12] <- paste("Gb", -1:10, sep="")
MIDI_full[11 + 0:11*12] <- paste("Bb", -1:10, sep="")
#MIDI_full[12 + 0:11*12]<-paste("B", -1:10, sep="")

MIDI_reduced_labels[8 + 0:11*12] <- paste("G", -1:10, sep="")
MIDI_full <- MIDI_full[2:length(MIDI_full)]
MIDI_reduced_labels <- MIDI_reduced_labels[2:length(MIDI_reduced_labels)]

# names(MIDI_labels) <- (1:length(MIDI_labels))
# names(MIDI_reduced_labels) <- (1:length(MIDI_reduced_labels))
labels[["MIDI_note_names"]] <- MIDI_full
labels[["MIDI_note_names_reduced"]] <- MIDI_reduced_labels

usethis::use_data(labels, overwrite = T)

#' nice_cdpcx
#' Makes a nice (sorted, labeled) cdpcx factor
#' @param cdpcx_factor
#'
#' @return factor of vector
#' @export
nice_cdpcx <- function(cdpcx_factor){
  #Order and re-label raw CDPCX values for display
  cdpcx_factor <- factor(cdpcx_factor,
                         levels = c("1", "2", "3", "4", "5", "6", "7", "B", "L", "T", "<", ">", "-", "%"),
                         labels = labels$cdpcx_labels
                         )
  return(cdpcx_factor)
}

get_default_theme <- function(x_rotate = 0, keep_legend = F){
  if (basic_theme == "tufte"){
    t <- ggthemes::theme_tufte()
    t <- t + theme(strip.text=element_text(size=round(default_text_size*.75), hjust=0))
    t <- t + theme(panel.border=element_blank())
  }
  else if(basic_theme == "minimal"){
    t <- theme_minimal()
    t <- t + theme(strip.text=element_text(size=round(default_text_size*.75), hjust=0))
    t <- t + theme(panel.border=element_blank())
    t <- t + theme(panel.grid.major	= element_line(colour=default_grid_color, size=.3))
    t <- t + theme(panel.grid.minor	= element_blank())
  }
  else if(basic_theme == "bw"){
    t <- theme_bw()
    t <- t + theme(strip.text=element_text(size=round(default_text_size*.75), hjust=.5))
    t <- t + theme(strip.background = element_rect(fill = "white", color="white") )
  }
  else if(basic_theme == "few"){
    t <- ggthemes::theme_few()
    t <- t + theme(strip.text=element_text(size=round(default_text_size*.75), hjust=0))
  }
  t <- t + theme(text=element_text(size=default_text_size))
  t <- t + theme(axis.title.x = element_text(size = default_text_size, vjust = -.5))
  if (x_rotate != 0){
    t <- t + theme(axis.text.x = element_text(size=round(default_text_size *.85), angle=x_rotate, hjust=1))
  }
  else{
    t <- t + theme(axis.text.x = element_text(size=round(default_text_size *.85)))

  }
  t <- t + theme(plot.title=element_text(hjust=0))
  t <- t + theme(panel.spacing.x=unit(0.5, "cm"))
  t <- t + theme(panel.spacing.y=unit(0.5, "cm"))
  t <- t + theme(legend.title=element_text(size=default_text_size))
  #t <- t + theme(legend.title.align=1)
  t <- t + theme(legend.text=element_text(size=round(default_text_size*.75)))
  if(!keep_legend){
    t <- t + theme(legend.position="none")
  }
  t <- t + theme(legend.key.size=unit(0.5, "cm"))
  t <- t + theme(legend.key.width=unit(.1, "cm"))
  t
}

cpc_plot <- function(cpc_vec){
  q<- ggplot(data.frame(cpc = factor(cpc_vec, levels = 0:11, labels = 0:11)),
             aes(x = cpc, y =..count..))

  q <- q + geom_bar()
  q <- q + scale_x_discrete(drop = F)
  q <- q + get_default_theme()
  q
}

pitch_plot <- function(pitch_vec){
  q<- ggplot(data.frame(x = 1:length(pitch_vec),
                        pitch = pitch_vec),
             aes(x = x, y = pitch_vec))

  q <- q + geom_point()
  q <- q + geom_line()
  q <- q + get_default_theme()
  q
}

#' piano_roll
#'
#' This function produces a piano roll of a colo
#'
#' @param solo (solo data frame) MCSV2 formatted solo data.frame
#' @param by_chorus (logical scalar) Flag to add chorus facets
#' @export
piano_roll <- function(solo, by_chorus = T){
  if(get_format(solo) != "mcsv2"){
    messagef("Warning: piano_roll plot only for mcsv2 data")
    return(NULL)
  }
  q <- ggplot(solo, aes(x = onset, y = pitch, colour = factor(phrase_id %% 2)))
  q <- q + geom_point()
  q <- q + geom_line(aes(group = factor(phrase_id)))
  q <- q + get_default_theme()
  if(by_chorus){
    q <- q + facet_wrap(~chorus_id, ncol = 1, scales = "free_x")
  }
  q

}
#' cpc_plot_solo
#'
#' This function produces a bar plot of CPC values from a solo (MCSV2) data frame
#'
#' @param solo (solo data frame) MCSV2 formatted solo data.frame
#' @param by_chord (logical scalar) Flag to add chord facets
#' @export
cpc_plot_solo <- function(solo, by_chord = T){
  if(get_format(solo) != "mcsv2"){
    messagef("Warning: piano_roll plot only for mcsv2 data")
    return(NULL)
  }
  if(!("cpc" %in% names(solo))){
    solo  <- solo %>%
      mutate(cpc = (pitch - parse_chord(chord)$pc) %% 12)
  }
  solo <- solo %>% mutate(cpc = factor(cpc, levels = 0:11, labels = 0:11))
  q<- ggplot(solo, aes(x = cpc, y = ..count..))

  q <- q + geom_bar(fill = default_color)
  q <- q + scale_x_discrete(drop = F)
  q <- q + get_default_theme()
  if(by_chord){
    q <- q + facet_wrap(~chord, scales = "free")
  }
  q
}

phrase_over_form2 <- function(solo, beats, bar_unit = 4, size = 1, width=.5){
  #assumes a raw melody export with phrase_begin markers in solo
  #and a beat track export in beats
  min_p <- min(solo$pitch) - 10
  max_p <- max(solo$pitch) + 3
  #solo$form <- factor(solo$form, levels=unique(solo$form))
  #phrase_begins <- solo %>% filter(phrase_begin == 1) %>% mutate(ioi = c(diff(onset), NA))
  #no_form_parts <- length(levels(phrase_begins$form))
  #phrase_begins <- phrase_begins[!is.na(phrase_begins$ioi), ]
  q <- ggplot()
  q <- q + geom_point(data = solo, aes(x = onset,
                                       colour = phrase_id %% 2,
                                       y = pitch))
  q <- q + geom_path(data = solo, aes(x = onset,
                                      colour = phrase_id %% 2,
                                      y = pitch,
                                      group = phrase_id))
  q <- q + scale_x_continuous(breaks = beats[beats$beat == 1 & beats$bar %% bar_unit == 1,]$onset,
                              labels = beats[beats$beat == 1 & beats$bar %% bar_unit == 1,]$bar)
  q <- q + get_default_theme()
  q <- q + labs(x = "Bar number", y = "MIDI Pitch")
  #browser()
  tmp <- beats[beats$beat == 1 & beats$bar %% bar_unit == 1, c("onset", "form", "chorus_id")]
  tmp$onset_lag1 <- dplyr::lead(tmp$onset)
  tmp$min_p <- min_p
  tmp$max_p <- max_p
  tmp <- tmp[1:(nrow(tmp) - 1),]
  q  <- q + geom_rect(data = tmp, mapping = aes(xmin = onset,
                                                xmax = onset_lag1,
                                                ymin = min_p,
                                                ymax = max_p,
                                                fill = form),
                      alpha=.2)
  q  <- q + geom_text(data = tmp, mapping = aes(x = .5 * (onset + onset_lag1),
                                                y = min_p + 3,
                                                label = form))
  q  <- q + scale_fill_grey()
  q  <- q + facet_wrap(~chorus_id, ncol = 1, scales = "free_x")
  return(q)
}

add_geom_bar <- function(q, percentage = T, fill_var = NULL, palette = jazzomat_palette[["set1"]]){
  if (percentage){
    if (is.null(fill_var)){
      q <- q + geom_bar(aes(y = ..count../tapply(..count..,..PANEL..,sum)[..PANEL..]),
                        fill = palette[[1]],
                        position = position_dodge())
      q <- q + scale_y_continuous(labels = scales::percent, name = "Percentage (%)")
    }
    else{
      q <- q + geom_bar(aes_string(y = "..count../tapply(..count..,..PANEL..,sum)[..PANEL..]",
                                   fill = fill_var),
                        position = position_dodge())
      q <- q + scale_y_continuous(labels = scales::percent, name = "Percentage (%)")
      q <- q + scale_fill_manual(values = palette, name = "")
    }
  }
  else{
    if (is.null(fill_var)){
      q <- q + geom_bar(fill = palette[[1]],
                        position = position_dodge())
    }
    else{
      q <- q + geom_bar(aes_string(fill = fill_var),
                        position = position_dodge())
      q <- q + scale_fill_manual(values = palette[[1]], name = "")
    }
  }
  q

}
add_histogram <- function(q, percentage = T, binwidth = NULL, fill_var = NULL, colour = "black", palette = jazzomat_palette[["set1"]]){
  if (percentage){
    if (is.null(fill_var)){
      q <- q + geom_histogram(aes(y = ..count../tapply(..count.., ..PANEL..,sum)[..PANEL..]),
                              fill = palette[[1]],
                              colour = colour,
                              binwidth = binwidth,
                              position = position_dodge())
      q <- q + scale_y_continuous(labels = scales::percent, name = "Percentage (%)")
    }
    else{
      q <- q + geom_histogram(aes_string(y = "..count../tapply(..count..,..PANEL..,sum)[..PANEL..]",
                                         fill = fill_var),
                              colour = colour,
                              binwidth = binwidth,
                              position = position_dodge())
      q <- q + scale_y_continuous(labels = scales::percent, name = "Percentage (%)")
      q <- q + scale_fill_manual(values = palette, name = "")
    }
  }
  else{
    if (is.null(fill_var)){
      q <- q + geom_histogram(fill = palette[[1]],
                              colour = colour,
                              binwidth = binwidth,
                              position = position_dodge())
    }
    else{
      q <- q + geom_histogram(aes_string(fill = fill_var),
                              colour = colour,
                              binwidth = binwidth,
                              position = position_dodge())
      q <- q + scale_fill_manual(values = palette, name = "")
    }
  }
  q
}

#' get_cdpcx_hist
#' Plots a fancy cdpcx (extended chordal diatonic pitch class plot"
#' @param data (data frame) most contain cdpcx_col as variable with cdpcx data
#' @param id (string or integer) Optional set of ids to filter
#' @param colour_chromatic (boolean) Flag whether to colour chromatic pitch classes differently
#' @param percentage (boolen) Flag whether to use percentage scale on y-axis
#' @param fill_var (String) extra fill variable
#' @param cdpcx_col (string) column name where cdpcx data is stored, default cdpcx_raw_all
#'
#' @return A ggplot2 object
#' @export
cdpcx_hist <- function(data, id = NULL, colour_chromatic = T, percentage = T, fill_var = NULL, cdpcx_col = "cdpcx_raw_all"){
  tmp <- select_by_id(data, id)
  tmp <- tmp[tmp[[cdpcx_col]] != "X",]
  if (nrow(tmp) == 0){
    return(NULL)
    # tmp <- select_by_id(data, id)
    # tmp$nice_cdpcx <- nice_cdpcx(tmp[[cdpcx_col]])
    # colour_chromatic <- F
    # fill_var <- NULL
  }
  else{
    tmp$nice_cdpcx <- nice_cdpcx(tmp[[cdpcx_col]])
    tmp$chromatic <- factor(as.numeric(tmp$nice_cdpcx) > 7, labels = c("Diatonic", "Chromatic"))
    tmp$chromatic2 <- factor(paste(tmp[, fill_var], as.character(tmp$chromatic), sep = " - "))
  }
  q <- ggplot(tmp, aes(x = nice_cdpcx))
  if (colour_chromatic){
    if (is.null(fill_var)){
      q <- add_geom_bar(q, percentage, fill_var = "chromatic")
    }
    else{
      q <- add_geom_bar(q, percentage, fill_var = "chromatic2", palette = jazzomat_palette[["set2"]])
    }
  }
  else{
    q <- add_geom_bar(q, percentage, fill_var)
  }
  q <- q + get_default_theme() + theme(legend.position = "none")
  q <- q + scale_x_discrete(name = "Extended chordal diatonic pitch class",
                            labels = labels[["cdpcx_labels"]],
                            drop = FALSE)
  return (q)
}

#' pc_hist
#'
#' Plots a fancy pitch class plot
#' @param data (data frame) must contain "pc_col" as variable with pitch class data
#' @param id (string or integer) Optional set of ids to filter
#' @param percentage (boolean) Flag, whether to use percentage scale on y-axis
#' @param fill_var (string) extra fill variable
#' @param pc_col (string) column name where pitch class data is stored, defaults to "pc_raw"
#'
#' @return A ggplot2 object
#' @export
pc_hist <- function(data, id = NULL, percentage = T, fill_var = NULL, pc_col = "pc_raw"){
  tmp <- select_by_id(data, id)
  q <- ggplot(tmp, aes(x = factor(!!sym(pc_col), levels = 0:11)))
  q <- add_geom_bar(q, percentage = percentage, fill_var = fill_var)
  q <- q + get_default_theme() + theme(legend.position = "none")
  q <- q + scale_x_discrete(name = "Pitch Class", drop = FALSE, labels = labels[["pc_labels"]])
  q
}

#' pitch_hist
#'
#' Plots a fancy pitch  plot
#' @param data (data frame) must contain "pitch_col" as variable with (MIDI) pitch  data
#' @param id (string or integer) Optional set of ids to filter
#' @param reduced_labels (boolean) Flag, whether to use reduced pitch labels
#' @param percentage (boolean) Flag, whether to use percentage scale on y-axis
#' @param fill_var (string) extra fill variable
#' @param pitch_col (string) column name where pitch  data is stored, defaults to "pitch_raw"

#' @return A ggplot2 object
#' @export
pitch_hist <- function(data, id = NULL, reduced_labels = F, percentage = T, fill_var = NULL, pitch_col = "pitch_raw"){
  tmp <- select_by_id(data, id)
  min_p <- min(tmp[[pitch_col]])
  max_p <- max(tmp[[pitch_col]])
  q <- ggplot(tmp, aes(x = factor(!!sym(pitch_col), levels = min_p:max_p)))
  q <- add_geom_bar(q, percentage = percentage, fill_var = fill_var)
  q <- q + get_default_theme() + theme(legend.position = "none")
  if(reduced_labels){
    q <- q + scale_x_discrete(name = "Pitch", drop = FALSE, labels = labels[["MIDI_note_names_reduced"]][min_p:max_p])
  }
  else{
    q <- q + scale_x_discrete(name = "Pitch", drop = FALSE, labels = labels[["MIDI_note_names"]][min_p:max_p])

  }
  q
}

#' mcm_hist
#'
#' Plots a fancy Metrical Circle Map (N = 48)  plot
#' @param data (data frame) Must contain "mcm48_col" as column with MCM data
#' @param id (string or integer) Optional set of ids to filter
#' @param percentage (boolean) Flag, whether to use percentage scale on y-axis
#' @param fill_var (string) extra fill variable
#' @param mcm48_col (string) column name where MCM 48 data is stored, defaults to "mcm_48"
#' @param x_labels (string vector) Labels for x axis, defaults to 4/4 labels
#'
#' @return
#' @export
mcm_hist <- function(data, id = NULL, percentage = T, fill_var = NULL, mcm48_col = "mcm_48", x_labels = labels[["metric_pos_4_4"]]){
  tmp <- select_by_id(data, id)
  #tmp$full_beats <- factor(tmp$mcm_48 %% 12 == 0, labels=c("Offbeat", "Beat"))
  q <- ggplot(tmp, aes(x = factor(!!sym(mcm48_col), levels = c(0:47))))
  q <- q + scale_color_manual(values = c("white", "black"))
  q <- q +  get_default_theme() + theme(legend.position="none")
  q <- add_geom_bar(q, percentage, fill_var)
  q <- q +  scale_x_discrete(name = "Metrical Circle Map (N = 48)", drop = FALSE, labels = x_labels)
  return(q)

}

#' int_hist
#' Plots a fancy Semitone interval histogram
#' @param data (data frame) Must contain "int_col" as column with MCM data
#' @param id (string or integer) Optional set of ids to filter
#' @param percentage (boolean) Flag, whether to use percentage scale on y-axis
#' @param cut_off (integer) Cutoff the distribution beyond this value (default 25)
#' @param int_col (string) name of the column containing the interval data  (default "int_raw")
#'
#' @return ggplot2 object
#' @export
int_hist <- function(data, id = NULL, cut_off = 25, percentage = T, int_col = "int_raw"){
  tmp <- select_by_id(data, id)
  tmp <- tmp[!is.na(tmp[["int_col"]]),]
  tmp <- tmp[abs(tmp[["int_col"]]) < cut_off,]
  ext <- max(abs(min(tmp[["int_col"]])), abs(max(tmp[["int_col"]])))
  #cat("min=", min(tmp$int_raw), "max=",  max(tmp$int_raw), "\n")
  tmp[["int_col"]] <-  factor(tmp[["int_col"]], levels = -ext:ext)
  labels <- rep("", 49)
  if (ext > 14){
    marks = c(-24, -19, -12, -7, -5, -2, 0, 2, 7, 5, 12, 19, 24)
    #print  (ext)
    labels[marks + 25] = marks
    labels <- labels[(25 - ext):(25 + ext + 1)]
    #cat(ext, 25-ext, 25+ext+1, length(labels), "\n")
    #print(labels)
  }
  else{
    labels = -ext:ext
  }
  q <- ggplot(tmp, aes(x=!!sym(int_col)))
  q <- add_geom_bar(q, percentage)

  q <- q + get_default_theme() + theme(legend.position = "none")
  q <- q + scale_x_discrete(name = "Semitone Interval", drop = FALSE, labels = labels)
  q
}

#' int_hist
#' Plots a fancy Fuzzy interval histogram
#' @param data (data frame) Must contain "fuzzyint_col" as column with fuzzy interval data (integers -4 to 4)
#' @param id (string or integer) Optional set of ids to filter
#' @param percentage (boolean) Flag, whether to use percentage scale on y-axis
#' @param fill_var (string) Additional filler variable
#' @param fuzzyint_col (string) name of the column containing the interval data  (default "int_raw")
#'
#' @return ggplot2 object
#' @export
fuzzyint_hist <- function(data, id = NULL, percentage = T,  fill_var = NULL, fuzzyint_col = "fuzzyint_raw"){
  tmp <- select_by_id(data, id)
  tmp<-tmp[!is.na(tmp[["fuzzyint_col"]]),]

  q <- ggplot(tmp, aes(x = factor(!!sym(fuzzyint_col), levels = -4:4)))
  q <- add_geom_bar(q, percentage = percentage, fill_var = fill_var)

  q <- q + get_default_theme(x_rotate = 45) + theme(legend.position = "none")
  q <- q + scale_x_discrete(name="Fuzzy Interval", drop=FALSE, labels = labels$fuzzy_labels)
  q
}

#' durclass_hist
#' Plots a fancy duration class histogram
#' @param data (data frame) Must contain "fuzzyint_col" as column with duration clas data (integers from -2 (very short) to 2 (very long))
#' @param id (string or integer) Optional set of ids to filter
#' @param id_var (string or integer) Name of the column in data that contains the id var (allows grouping by different variables)
#' @param fuzzyint_col (string) name of the column containing the interval data  (default "int_raw")
#' @param legend.x, legend.y (numeric) position of the legend, for nice inserts.
#' @param percentage (boolean) Flag, whether to use percentage scale on y-axis
#' @param fill_var (string) Additional filler variable
#'
#' @return ggplot2 object
#' @export
durclass_hist <- function(data,
                          id = NULL,
                          id_var = "id",
                          durclass_col = "durclass_abs_raw",
                          durclass_col2 = "durclass_rel_raw",
                          cmp_labels = c("Absolute", "Relative"),
                          legend.x = .8,
                          legend.y = .8,
                          percentage = T,
                          fill_var = NULL){
  tmp <- select_by_id(data, id)
  if (is.null(fill_var)){
    #tmp <- reshape2::melt(tmp[, c(id_var, "durclass_rel_raw", "durclass_abs_raw")])
    tmp <- tmp %>%
      select(all_of(c(id_var, durclass_col, durclass_col2))) %>%
      pivot_longer(-!!sym(id_var))
    tmp$name <- factor(tmp$name, labels = cmp_labels)
    q <- ggplot(tmp %>% filter(!is.na(value)), aes(x = factor(value, levels = -2:2)))
    q <- add_geom_bar(q, percentage = percentage, fill_var = "name")
    q <- q + theme(legend.position = c(legend.x, legend.y), legend.title = element_blank())
  }
  else{
    tmp <- tmp %>% mutate(!!sym(durclass_col) := factor(!!sym(durclass_col), levels = -2:2))
    q <- ggplot(tmp, aes(x = !!sym(durclass_col)))
    q <- add_geom_bar(q, percentage = percentage, fill_var = fill_var)
    q <- q + theme(legend.position = c(legend.x, legend.y))

  }
  q <- q + get_default_theme(keep_legend = T)
  q <- q + scale_x_discrete(name = "Duration classes", drop = FALSE, labels = labels$durclass_labels)
  #q <- q + scale_fill_manual(values=jazzomat_pallette, name="", labels=c("Relative", "Absolute"))
  q
}
