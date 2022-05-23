chisq.residuals <- function (tab, digits = 2, std = FALSE, raw = FALSE)
{
  if (all(std, raw))
    stop("Choose between standardized and raw residuals.")
  k = stats::chisq.test(tab)
  if (raw) {
    res <- k$observed - k$expected
  }
  else if (std) {
    res <- k$stdres
  }
  else {
    res <- k$residuals
  }
  round(res, digits)
}

#' ggassoc
#' Plots chisquare residuals for 2 x N tables, similar to vcd::asso
#' @param cont_tab (table) 2 x N Contingency table
#' @param x_lab (string) Label for x-axis
#' @param y_lab (string) Label for y-axis
#' @param subtitle (string) subtitle for plot
#' @param spread (numeric) Spread factor for boxes
#' @param text_size (integer) Text size
#' @param fill_colors (string) 2-element vector of color values for filling boxes, defaults to black and red
#'
#' @return ggplot2
#' @export
ggassoc <- function(cont_tab,
                    x_lab = "",
                    y_lab = "",
                    title = "Chi-Square Residuals",
                    subtitle = "",
                    spread = 2.25,
                    text_size = 3,
                    fill_colors = jazzomat_palette$set1[1:2],
                    std = F,
                    raw = T){
  names <- dimnames(cont_tab)
  if(length(names[[1]]) != 2){
    stop("Currently only available for 2 x N tables!")
  }
  if(is.null(x_lab) || nchar(x_lab) == 0){
    x_lab <- "target"
  }
  if(is.null(y_lab) || nchar(y_lab) == 0){
    y_lab <- "group"
  }
  browser()
  cont_tab <- cont_tab[,colSums(cont_tab) != 0]
  group_labels <- names[[1]]
  target_labels <- names[[2]]
  num_cats <- length(target_labels)
  resid_df <- chisq.residuals(cont_tab, std = std, raw = raw) %>%
    t() %>%
    as.data.frame() %>%
    set_names(x_lab, y_lab, "residual") %>%
    mutate(resid_norm = residual/max(abs(residual), na.rm = T),
           group = factor(!!sym(y_lab)))
  sum_stats  <-  (colSums(cont_tab)/sum(cont_tab)) %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    set_names("target_cat", "width") %>%
    mutate(width = sqrt(2 * width / max(width))) %>%
    mutate(xpos = cumsum(c(0, width[-length(width)])))
  max_resid <- max(abs(resid_df$resid_norm))
  #browser()
  resid_df <- resid_df %>%
    mutate(group_f = spread * (as.numeric(group) - 1)  -  spread/2,
           target_f = as.numeric(factor(!!sym(x_lab)))) %>%
    mutate(xmin = rep(sum_stats$xpos, 2),
           xmax = rep(sum_stats$xpos + sum_stats$width, 2),
           ymin = group_f,
           ymax = group_f  + resid_norm,
           sign_dev = residual > 0)
  #browser()
  if(nchar(subtitle) == 0){
    subtitle <- sprintf("%s x %s", x_lab, y_lab)
  }
  q <- resid_df %>% ggplot(aes())
  q <- q + geom_rect(data  = resid_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = sign_dev), color = "white")
  q <- q + theme_classic()
  q <- q + theme(legend.position = "none",
                 axis.line.x = element_blank(),
                 axis.line.y = element_blank(),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_text(size = 12, face = "bold"))
  q <- q + labs(x = "", y = "",
                subtitle = subtitle, title = title)
  q <- q + scale_fill_manual(values = fill_colors[1:2])
  q <- q + scale_x_continuous(breaks = NULL)
  q <- q + scale_y_continuous(breaks = c(-spread/2, spread/2), labels = group_labels)
  q <- q + geom_text(data = sum_stats,
                     aes(x = xpos + .5 * width,
                         y = .1 + .1 * (as.integer(factor(target_cat)) %% 3 - 1),
                         label = target_cat, hjust = 0.5, size = width ))
  q <- q +  scale_size(range = c(3, 5))
  q
}


ggassoc2 <- function(cont_tab,
                     x_lab = "",
                     y_lab = "",
                     title = "Chi-Square Residuals",
                     subtitle = "",
                     spread = 2.25,
                     text_size = 3,
                     fill_colors = jazzomat_palette$set1[1:2],
                     std = F,
                     raw = T){
  names <- dimnames(cont_tab)
  if(length(names[[1]]) != 2){
    stop("Currently only available for 2 x N tables!")
  }
  if(is.null(x_lab) || nchar(x_lab) == 0){
    x_lab <- "target"
  }
  if(is.null(y_lab) || nchar(y_lab) == 0){
    y_lab <- "group"
  }
  browser()
  cont_tab <- cont_tab[,colSums(cont_tab) != 0]
  group_labels <- names[[1]]
  target_labels <- names[[2]]
  num_cats <- length(target_labels)
  chisq <- chisq.test(cont_tab)
  sqrt_expected <- sqrt(chisq$expected)
  resid_df <- chisq.residuals(cont_tab, std = std, raw = raw) %>%
    t() %>%
    as.data.frame() %>%
    set_names(x_lab, y_lab, "residual") %>%
    mutate(resid_norm = residual/max(abs(residual), na.rm = T),
           group = factor(!!sym(y_lab)))
  sum_stats  <-  sqrt_expected %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    set_names("target_cat", "width") %>%
    mutate(width = width / max(width)) %>%
    mutate(xpos = cumsum(c(0, width[-length(width)])))
  max_resid <- max(abs(resid_df$resid_norm))
  #browser()
  resid_df <- resid_df %>%
    mutate(group_f = spread * (as.numeric(group) - 1)  -  spread/2,
           target_f = as.numeric(factor(!!sym(x_lab)))) %>%
    mutate(xmin = rep(sum_stats$xpos, 2),
           xmax = rep(sum_stats$xpos + sum_stats$width, 2),
           ymin = group_f,
           ymax = group_f  + resid_norm,
           sign_dev = residual > 0)
  #browser()
  if(nchar(subtitle) == 0){
    subtitle <- sprintf("%s x %s", x_lab, y_lab)
  }
  q <- resid_df %>% ggplot(aes())
  q <- q + geom_rect(data  = resid_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = sign_dev), color = "white")
  q <- q + theme_classic()
  q <- q + theme(legend.position = "none",
                 axis.line.x = element_blank(),
                 axis.line.y = element_blank(),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_text(size = 12, face = "bold"))
  q <- q + labs(x = "", y = "",
                subtitle = subtitle, title = title)
  q <- q + scale_fill_manual(values = fill_colors[1:2])
  q <- q + scale_x_continuous(breaks = NULL)
  q <- q + scale_y_continuous(breaks = c(-spread/2, spread/2), labels = group_labels)
  q <- q + geom_text(data = sum_stats,
                     aes(x = xpos + .5 * width,
                         y = .1 + .1 * (as.integer(factor(target_cat)) %% 3 - 1),
                         label = target_cat, hjust = 0.5, size = width ))
  q <- q +  scale_size(range = c(3, 5))
  q
}
