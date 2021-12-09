#' write_mcsv2
#'
#' This function write a MCSV2 formated data fraem to a file
#'
#' @param data (data frame) MCSV2 formatted solo data.frame
#' @param fname (character scalar) file path to write to.
#' @export
write_mcsv2 <- function(data, fname){
  header <- strsplit("onset;duration;period;division;bar;beat;tatum;beat_duration;signature;pitch;phrase_id;phrase_begin", ";") %>% unlist()
  missing <- setdiff(header, names(data))
  if(length(missing)> 0){
    stop(sprintf("Invalid MCSV2 data frame, missing: %s", paste(missing, sep = ", ", collapse = ", ")))
  }
  utils::write.table(data,
                     fname,
                     sep=";",
                     dec=".",
                     quote=F,
                     col.names=T,
                     row.names=F)
  return(data)
}
#onset;duration;period;division;bar;beat;tatum;beat_duration;signature;pitch;phrase_id;phrase_begin;chorus_id;form;key;idea
