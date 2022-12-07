library(tidyverse)

F_blues <- tibble(chord = c("F7","Bb7", "F7",  "Cmin7", "F7", "Bb7", "Bb7", "F7", "F7", "Gmin7", "C7", "F7", "F7"),
                  duration = as.integer(c(4, 4, 4, 2, 2, 4, 4, 4, 4, 4, 4, 4, 4)))
F_blues <- F_blues %>% mutate(length_ticks = duration * 4) %>%
  mutate(parsed = map(chord, parkR::parse_chord)) %>%
  tidyr::unnest(cols = parsed)
F_blues$onset_ticks <- cumsum(F_blues$length_ticks)
F_blues$onset_ticks <- F_blues$onset_ticks - F_blues$onset_ticks[1]
F_blues$beat <- (floor(F_blues$onset_ticks/4) %% 4 ) + 1
F_blues$bar <- floor(F_blues$onset_ticks/16) + 1

usethis::use_data(F_blues, overwrite = TRUE)

scale_degrees <- list("norm" = c("I", "IIb", "II","IIIb", "III", "IV","IV#", "V", "VIb", "VI", "VIIb", "VII"),
                      "maj" = c("I", "IIb", "ii", "iii", "IV", "IV#", "V", "VIb", "vi", "VIIb", "viio"),
                      "min" = c("i", "IIb", "iio", "IIIb", "iv", "IV#", "V", "VIb", "vio", "VIIb", "viio"))
usethis::use_data(scale_degrees, overwrite = TRUE)

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
usethis::use_data(jazzomat_palette, overwrite = T)


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
