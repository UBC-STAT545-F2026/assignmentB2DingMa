#' Complement a string of DNA sequence
#'
#' Replaces each base (letter) with its complementary base (e.g. ACGT -> TGCA). Input is automatically cleaned to contain only the four bases. Results may be returned in reverse, and in either lower or upper case
#'
#' @param seq string of DNA sequence (hence "seq") to be complemented (ACGT). May contain extra formatting (e.g. tab spacing and base numbers from NCBI), but will not be retained in the output. Does not support ambiguity codes (e.g. "N")
#' @param reverse boolean indicating whether to reverse the complementary sequence. Default = FALSE
#' @param upper boolean indicating whether to return results in upper case. Default = TRUE. FALSE returns result in lower case
#' @returns string of complement of input DNA sequence. May be reversed. If input does not contain the letters ACGT, an empty string will be returned.
#' @examples
#' simple_seq <- "Atggccggcaacgtgaagaagagctctggg"
#' complement_dna(simple_seq, upper = FALSE)
#' complement_dna(simple_seq, reverse = TRUE)
#' @export
complement_dna <- function(seq, reverse = FALSE, upper = TRUE) {
  # input checks
  stopifnot(is.character(seq))
  if (length(seq) > 1) {
    warning(glue::glue("expects a single string but received a character vector of length {length(seq)}, continuing with the first element"), immediate. = T)
    seq <- seq[1]
  }
  if (paste0(stringr::str_extract_all(seq, stringr::regex("[^ACGT]", ignore_case = T))[[1]], collapse = "") != "") {
    warning(glue::glue("characters outside of canonical bases (ACGT) detected, these are removed from input, please verify output"), immediate. = T)
  }

  # extract only valid bases from string and complement
  seq_mod <- stringr::str_extract_all(toupper(seq), "[ACGT]")[[1]]
  seq_mod <- chartr("ACTG", "TGAC", seq_mod)

  # lower/upper case resulting string
  if (upper) {
    seq_mod <- toupper(seq_mod)
  } else {
    seq_mod <- tolower(seq_mod)
  }

  # reverse string
  if (reverse) {
    seq_mod <- rev(seq_mod)
  }

  # collapse vector and check result emptiness
  seq_mod <- paste0(seq_mod, collapse = "")
  if (seq_mod == "") {
    warning("no bases found in input sequence, returning empty string", immediate. = T)
  }

  return(seq_mod)
}
