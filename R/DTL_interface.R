# library(httr)
# library(tidyverse)

#' DTL_similarity_search
#' Runs a similarity search by the DTL similarity search API
#' @param search_pattern (string vector) Array of search patterns, where a search pattern is a comma-separated list of search elements (depends on transformation)
#' @param transformation (string) melodic representation to search in. Admissible values are "interval", "pitch", and "fuzzyinterval"
#' @param database_names (string) comma separated list of melodic database to search in. Database names are "dtl" (DTL1000 data set), "wjd" (Weimar Jazz Database), "omnibook" (Charlie Parker Omnibook), and "esac" (Essen Folks Song collection).
#' @param metadata_filters (string) Metadata filters for the databases, comma separated list of entries <dbname>: {<filter>}
#' @param filter_category (integer as string) I have no idea what this means
#' @param minimum_similarity (numeric) Value between 0 and 1 as the minimal similarity value between a pattern and the search pattern.
#' @param max_edit_distance (integer) Value of maximal edit distance between search pattern and data base patterns.
#' @param max_length_difference (integer) Maximum length difference between search pattern and result pattern
#'
#' @return Search ID, to be used in DTL_get_results
#' @export
DTL_similarity_search <- function(search_pattern = "1,2,1,2,1,2,1,2",
                                  transformation = c("interval", "pitch", "fuzzyinterval"),
                                  database_names = "dtl,wjazzd,omnibook",
                                  metadata_filters = '{"dtl": {}, "wjazzd": {}, "esac": {}, "omnibook": {}}',
                                  filter_category = "0",
                                  minimum_similarity = 1.0,
                                  max_edit_distance = NA,
                                  max_length_difference = 0)
{
  transformation <- match.arg(transformation)
  url <- suppressWarnings(httr::modify_url("https://staging-dtl-pattern-api.hfm-weimar.de/", path = "/patterns/similar"))
  if(is.na(max_edit_distance)){
    max_edit_distance <- map_int(value_to_vec(search_pattern), length) %>% min()
  }
  messagef("[DTL API] Starting search for %s", search_pattern)
  resp <- suppressWarnings(httr::POST(url, body = list( n_gram = search_pattern,
                                 transformation = transformation,
                                 database_names = database_names,
                                 metadata_filters = metadata_filters,
                                 filter_category = filter_category,
                                 minimum_similarity = minimum_similarity,
                                 max_edit_distance = max_edit_distance,
                                 max_length_difference = max_length_difference, filter_category = 0),
               encode = "form"))
  #browser()
  #print(content(resp, "text"))
  if (httr::http_error(resp)) {
      messagef(
        "[DTL API]  Similarity Search  request failed [%s]\n%s\n<%s>",
        status_code(resp),
        "",#parsed$message,
        ""#parsed$documentation_url
        )
    return(NULL)
  }
  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  messagef("[DTL API] Retrieved search ID %s of for pattern %s", parsed$search_id, search_pattern)
  parsed$search_id
}

DTL_get_results <- function(search_id){
  url <- suppressWarnings(httr::modify_url("http://staging-dtl-pattern-api.hfm-weimar.de/", path = "/patterns/get"))
  #messagef("[DTL API] Retrieving results for search_id %s",  search_id)
  resp <- suppressWarnings(httr::GET(url, query = list(search_id  = search_id)))
  if (http_error(resp)) {
      messagef(
        "[DTL API]  Similarity Search  request failed [%s]\n%s\n<%s>",
        status_code(resp),
        "",#parsed$message,
        ""#parsed$documentation_url
      )
    return(NULL)
  }
  #print(content(resp, "text"))
  #browser()

  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  messagef("[DTL API] Retrieved %s lines for search_id %s", length(parsed), search_id)
  map_dfr(parsed, function(x){
    if(is.null(x$within_single_phrase)){
      x$within_single_phrase <- FALSE
    }
    #browser()
    as_tibble(x) %>% mutate(melid = as.character(melid))
  })
}

#' DTL_similarity_search_results
#' Runs a similarity search by the DTL similarity search API, same as DTL_similarity_search but returns data frame of results instead of result ID
#' @param search_pattern (string vector) Array of search patterns, where a search pattern is a comma-separated list of search elements (depends on transformation)
#' @param transformation (string) melodic representation to search in. Admissible values are "interval", "pitch", and "fuzzyinterval"
#' @param database_names (string) comma separated list of melodic database to search in. Database names are "dtl" (DTL1000 data set), "wjd" (Weimar Jazz Database), "omnibook" (Charlie Parker Omnibook), and "esac" (Essen Folks Song collection).
#' @param metadata_filters (string) Metadata filters for the databases, comma separated list of entries <dbname>: {<filter>}
#' @param filter_category (integer as string) I have no idea what this means
#' @param minimum_similarity (numeric) Value between 0 and 1 as the minimal similarity value between a pattern and the search pattern.
#' @param max_edit_distance (integer) Value of maximal edit distance between search pattern and data base patterns.
#' @param max_length_difference (integer) Maximum length difference between search pattern and result pattern
#'
#' @return Search ID, to be used in DTL_get_results
#' @export
DTL_similarity_search_results <- function(search_patterns = "1,2,1,2,1,2,1,2",
                                          transformation = c("interval", "pitch", "fuzzyinterval"),
                                          database_names = "dtl,wjazzd,omnibook",
                                          metadata_filters = '{"dtl": {}, "wjazzd": {}, "esac": {}, "omnibook": {}}',
                                          filter_category = "0",
                                          minimum_similarity = 1.0,
                                          max_edit_distance = NA,
                                          max_length_difference = 0){
  transformation <- match.arg(transformation)
  results <- tibble()
  if(is.na(max_edit_distance)){
    max_edit_distance <- map_int(value_to_vec(search_patterns), length) %>% min()
  }
  for(pattern in search_patterns){
    search_id <- DTL_similarity_search(pattern,
                                       transformation,
                                       database_names,
                                       metadata_filters,
                                       filter_category,
                                       minimum_similarity,
                                       max_edit_distance = max_edit_distance,
                                       max_length_difference = max_length_difference)
    if(is.null(search_id)){
      next
    }

    ret <- DTL_get_results(search_id)
    if(!is.null(ret) && nrow(ret) > 0){
      ret$search_pattern <- pattern
    }
    results <- bind_rows(results, ret)


  }
  #browser()
  if(nrow(results))
    results %>% distinct(melid, start, length, .keep_all = T)
}

#' DTL_similarity_search_results
#' Runs a similarity search by the DTL similarity search API, same as DTL_similarity_search_results but uses parallel request to fasten thinks up for large searches possibly with many search patterns.
#' @param search_pattern (string vector) Array of search patterns, where a search pattern is a comma-separated list of search elements (depends on transformation)
#' @param transformation (string) melodic representation to search in. Admissible values are "interval", "pitch", and "fuzzyinterval"
#' @param database_names (string) comma separated list of melodic database to search in. Database names are "dtl" (DTL1000 data set), "wjd" (Weimar Jazz Database), "omnibook" (Charlie Parker Omnibook), and "esac" (Essen Folks Song collection).
#' @param metadata_filters (string) Metadata filters for the databases, comma separated list of entries <dbname>: {<filter>}
#' @param filter_category (integer as string) I have no idea what this means
#' @param minimum_similarity (numeric) Value between 0 and 1 as the minimal similarity value between a pattern and the search pattern.
#' @param max_edit_distance (integer) Value of maximal edit distance between search pattern and data base patterns.
#' @param max_length_difference (integer) Maximum length difference between search pattern and result pattern
#'
#' @return Search ID, to be used in DTL_get_results
#' @export
DTL_similarity_search_results_fast <- function(search_patterns = "1,2,1,2,1,2,1,2",
                                               transformation = c("interval", "pitch", "fuzzyinterval"),
                                               database_names = "dtl,wjazzd,omnibook",
                                               metadata_filters = '{"dtl": {}, "wjazzd": {}, "esac": {}, "omnibook": {}}',
                                               filter_category = "0",
                                               minimum_similarity = 1.0,
                                               max_edit_distance = NA,
                                               max_length_difference = 0){
  transformation <- match.arg(transformation)
  if(is.na(max_edit_distance)){
    max_edit_distance <- map_int(value_to_vec(search_patterns), length) %>% min()
  }
  future::plan(multisession)
  results <- furrr:::future_map_dfr(search_patterns, function(pattern){
    search_id <- DTL_similarity_search(pattern,
                                       transformation,
                                       database_names,
                                       metadata_filters,
                                       filter_category,
                                       minimum_similarity,
                                       max_edit_distance = max_edit_distance,
                                       max_length_difference = max_length_difference)
    if(is.null(search_id)){
      return(tibble())
    }
    ret <- DTL_get_results(search_id)
    if(!is.null(ret) && nrow(ret) > 0 )ret$search_pattern <- pattern
    ret
  })
  #browser()
  results %>% distinct(melid, start, length, .keep_all = T)
}
