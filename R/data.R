#' irb chords
#'
#' The IRealPro lead sheets
#' @name irb
#' @docType data
NULL

#' irb chords with derived information
#'
#' The IRealPro lead sheets + derived information
#' @name irb_extended
#' @docType data
NULL

#' WJD chords
#'
#' Chord changes & lead sheets extracted from the Weimar Jazz Database
#' @name wjd_chord_db
#' @docType data
NULL

#' WBA_df
#'
#' Database of WBA atoms extracted from the Weimar Jazz Database.
#' @name WBA_df
#' @docType data
NULL

#' wba_mla
#'
#' Database of WBA atoms and midlevel units (MLUs) extracted from the Weimar Jazz Database
#' @name wba_mla
#' @docType data
NULL


#' successor_dist
#'
#' First order Markov transitions of WBA atoms extracted from the Weimar Jazz Database
#' @name successor_dist
#' @docType data
NULL

#' succ_ioiclass
#'
#' First order Markov transitions of interonset interval classes extracted from the Weimar Jazz Database
#' @name succ_ioiclass
#' @docType data
NULL

#' phrase_begin_dist
#'
#' Distribution of WBA atoms at beginning of phrases extracted from the Weimar Jazz Database
#' @name phrase_begin_dist
#' @docType data
NULL

#' length_dist
#'
#' Disbtribution of number of phrases over WJD solos extracted from the Weimar Jazz Database
#' @name length_dist
#' @docType data
NULL

#' phrase_length_dist
#'
#' Disbtribution of phrase lengths and relative phrase positions extracted from the Weimar Jazz Database
#' @name phrase_length_dist
#' @docType data
NULL

#' F_blues
#'
#' Sample lead sheet of a sinple 12-bar jazz blues in F
#' @name F_blues
#' @docType data
NULL

#' instrument_ranges
#'
#' Tibble of instrument ranges, extracted from the Weimar Jazz Databse, min(imum), max(imum),  5% (q5) and 95% (q95) percentiles
#' @name instrument_ranges
#' @docType data
NULL


#' wjd_transforms
#'
#' Some basic raw features from the Weimar Jazz Database (WJD) (\code{"id, beat13, beatall, phrasbeg, phrasend, cdpcx_raw_all, cpc_raw_all, chord_types_raw, chords_raw, phrase_id_raw, mcm_48, pitch_raw, fuzzyint_raw, int_raw, ioiclass_abs_raw, ioiclass_rel_raw, parsons_raw, style"})
#' @name wjd_transforms
#' @docType data
NULL

#' wjd_features
#'
#' Some basic scalar features from the Weimar Jazz Database (WJD) (\code{""})
#' @name wjd_features
#' @docType data
NULL


#' wjd_meta
#'
#' Metadata for the  Weimar Jazz Database (WJD) solos (\code{"id, number_notes, avgtempo, chord_changes, chorus_count, composer, filename_sv, form, full_title, genre, harmony_template, instrument, key, label, lineup, mbzid, performer, recordbib, recordingdate, recordingyear, recordtitle, rhythmfeel, signature, solo_time, soloend, solopart, solostart, status, style, tempoclass, title, titleaddon, tonality_type"})
#' @name wjd_transforms
#' @docType data
NULL

#' omnibook_transforms
#'
#' Some basic raw features from the Weimar Jazz Database (WJD) (\code{"id, beat1, beat3, beatall, phrasbeg, phrasend, cdpcx_raw_all, cpc_raw_all, durclass_abs_raw, durclass_rel_raw, metrical_position, chord_types_raw, chords_raw, phrase_id_raw, mcm_48, pitch_raw, fuzzyint_raw, int_raw, ioiclass_abs_raw, ioiclass_rel_raw, pos"})
#' @name omnibook_transforms
#' @docType data
NULL

#' omnibook_meta
#'
#' Metadata for the  Charlie Parker Omnibook (CPO) solos (\code{"omnibook_meta"})
#' @name omnibook_meta
#' @docType data
NULL

#' esac_transforms
#'
#' Some basic raw features from the Essen Folk Song Database (EFSC) (\code{"id, int_raw, ioiclass_abs_raw, ioiclass_rel_raw, mcm_48"})
#' @name esac_transforms
#' @docType data
NULL

#' esac_meta
#'
#' Metadata for the  Essen Folk Song Database (EFSC) (\code{"id, cnr, collection, comment, esac_key, esac_title, function., melstring, region, signature, source, text, tunefamily, unit"})
#' @name esac_meta
#' @docType data
NULL

#' labels
#'
#' List of various useful music related labels
#' @name labels
#' @docType data
NULL
