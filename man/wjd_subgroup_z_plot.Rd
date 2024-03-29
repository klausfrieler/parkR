% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_util.R
\name{wjd_subgroup_z_plot}
\alias{wjd_subgroup_z_plot}
\title{Generate z-Plots for a sunbset of the Weimar Jazz Database (or another dataset)}
\usage{
wjd_subgroup_z_plot(
  feature_frame = jazzpdata::wjd_features_hardcore,
  group_selector,
  features = jazzodata::hardcore_features,
  facet_var = NULL,
  ...
)
}
\arguments{
\item{feature_frame}{(data.frame) A data.frame containing the variables as defined in features, as well as group_selector, and, if requested, facet_var}

\item{group_selector}{(boolean) Marks the subgroup to display}

\item{features}{(vector of strings) List of feature variables (need to be contained in feature_frame)}

\item{facet_var}{(string) Variable name for facetting}

\item{...}{Extra arguments, passed to plot_z_values}
}
\value{
gg2plot object
}
\description{
Generate z-Plots for a sunbset of the Weimar Jazz Database (or another dataset)
}
