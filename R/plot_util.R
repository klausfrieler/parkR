#library(cowplot)
#out_dir <- "../figs/new/"
#fig_dir <- "e:/projects/science/jazzomat/docs/conferences/JazzforschungD2018/paper/figs/"
img_format <- "png"
default_text_size<-12
default_color <- "darkslategrey"

#default_color  <-"lightblue4"
default_color2 <-"lightblue3"
default_color3 <-"lightblue2"
default_color4 <-"lightblue1"
jazzomat_red <- "indianred3"
#default_color3 <-"lightgoldenrod4"
#default_color4 <-"aquamarine4"
jazzomat_pallette = c(default_color, default_color2, default_color3, default_color4)
jazzomat_pallette2 = c(default_color, default_color2, default_color2, default_color)
jazzomat_pallette3 = c(default_color,jazzomat_red, default_color2, default_color)

default_grid_color <- "gray64"
basic_theme <- "minimal"

get_default_theme <- function(x_rotate = 0, keep_legend = F){
  if (basic_theme == "tufte"){
    t <- theme_tufte()
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
    t <- theme_few()
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

piano_roll <- function(tbl){
  if(get_format(tbl) != "mcv2"){
    messagef("Warning: piano_roll plot only for mcsv2 data")
    return(NULL)
  }
  q <- ggplot(tbl,
             aes(x = onset, y = pitch, colour = factor(phrase_id %% 2)))

  q <- q + geom_point()
  q <- q + geom_line(aes(group = factor(phrase_id)))
  q <- q + get_default_theme()
  q

}

cpc_plot2 <- function(tbl){
  tbl$cpc <- (tbl$pitch - parse_chord(tbl$chord)$pc) %% 12
  q<- ggplot(tbl,
             aes(x = factor(cpc, levels = 0:11, labels = 0:11), y = ..count..))

  q <- q + geom_bar(fill = default_color)
  q <- q + scale_x_discrete(drop=F)
  q <- q + get_default_theme()
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
  browser()
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
  q  <- q + facet_wrap(~chorus_id, ncol = 1, scale = "free_x")
  return(q)
}
