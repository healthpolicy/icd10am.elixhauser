#' Classify episodes using the Elixhauser Comorbidity Index
#'
#' @description
#' Applies the Elixhauser Comorbidity Index to admitted patient episodes coded
#' with ICD-10-AM. Each episode receives a total Elixhauser score and binary
#' flags for each of the 30 Elixhauser comorbidity categories.
#'
#' Diagnosis codes are matched at three levels of specificity: full code,
#' 4-character prefix, and 3-character prefix. This ensures codes that differ
#' only in trailing characters are correctly attributed to their comorbidity
#' category.
#'
#' Both wide-format (one row per episode, diagnoses in separate columns) and
#' long-format (separate diagnosis data frame) inputs are supported via the
#' `format` argument.
#'
#' @param df A data frame with one row per episode.
#' @param id The episode identifier column (unquoted name).
#' @param format Input format: `"long"` (default) or `"wide"`.
#' @param df_diag Long-format diagnosis data frame with one row per diagnosis.
#'   Required when `format = "long"`. Must contain the episode ID column and a
#'   diagnosis code column.
#' @param diag Name of the diagnosis code column in `df_diag` (quoted string).
#'   Default: `"diag"`. Used only when `format = "long"`.
#' @param diagno Name of the diagnosis sequence number column in `df_diag`
#'   (quoted string). Default: `"diagno"`. Used only when `format = "long"`.
#' @param diag_prefix Character prefix shared by all diagnosis columns in `df`
#'   (e.g. `"diag"` matches `diag1`, `diag2`, ...). Default: `"diag"`. Used
#'   only when `format = "wide"`.
#'
#' @return The input `df` with additional columns appended:
#'   \describe{
#'     \item{`elixhauser_score`}{Total Elixhauser comorbidity score (numeric).
#'       Note: weights can be negative (e.g. Valvular disease = -1), so the
#'       score may be negative.}
#'     \item{`el_*`}{One column per Elixhauser comorbidity category, prefixed
#'       `el_`. Contains the category weight (0 if absent).}
#'   }
#'   Episodes with no matching diagnoses receive a score of 0 and zeros for
#'   all condition columns.
#'
#' @references
#' Elixhauser A, Steiner C, Harris DR, Coffey RM (1998). Comorbidity measures
#' for use with administrative data. *Medical Care*, 36(1), 8--27.
#' \doi{10.1097/00005650-199801000-00004}
#'
#' Quan H, Sundararajan V, Halfon P, et al. (2005). Coding algorithms for
#' defining comorbidities in ICD-9-CM and ICD-10 administrative data.
#' *Medical Care*, 43(11), 1130--1139.
#' \doi{10.1097/01.mlr.0000182534.19832.83}
#'
#' @export
#'
#' @importFrom dplyr all_of bind_rows distinct filter group_by if_else left_join
#'   mutate rename select starts_with summarise across
#' @importFrom tidyr pivot_wider
#' @importFrom rlang .data abort as_name ensym sym
#' @importFrom stringr str_sub
#' @importFrom stats setNames
#' @importFrom icd10am.utils .to_long
#'
#' @examples
#' dfbase <- icd10am.utils::dfbase
#' dfdiag <- icd10am.utils::dfdiag
#'
#' # Long format
#' classify_elixhauser(
#'   df      = dfbase,
#'   id      = recordID,
#'   format  = "long",
#'   df_diag = dfdiag,
#'   diag    = "diag",
#'   diagno  = "diagno"
#' )
#'
#' # Wide format
#' dfwide <- icd10am.utils::dfwide
#' classify_elixhauser(
#'   df          = dfwide,
#'   id          = recordID,
#'   format      = "wide",
#'   diag_prefix = "diag"
#' )
classify_elixhauser <- function(
    df,
    id,
    format      = c("long", "wide"),
    df_diag     = NULL,
    diag        = "diag",
    diagno      = "diagno",
    diag_prefix = "diag"
) {
  format <- match.arg(format)
  id_str <- rlang::as_name(rlang::ensym(id))

  # --- Normalise to long format ---
  if (format == "wide") {
    converted <- .to_long(df, id = !!rlang::sym(id_str), diag_prefix = diag_prefix)
    df_diag <- converted$df_diag
    diag    <- "diag"
    diagno  <- "diagno"
  }

  if (is.null(df_diag)) {
    rlang::abort("`df_diag` must be supplied when `format = 'long'`.")
  }

  # --- Rename to standard internal names ---
  df_std <- df_diag |>
    dplyr::rename(
      episode_id = dplyr::all_of(id_str),
      diag       = dplyr::all_of(diag)
    )

  # Pre-deduplicate lookup tables to avoid many-to-many join warnings
  lookup_full <- char_elix_codes |>
    dplyr::filter(!is.na(.data$elixhauser)) |>
    dplyr::select("diag", "elixhauser", "elixhauser_x") |>
    dplyr::distinct()

  lookup_3 <- char_elix_codes |>
    dplyr::filter(!is.na(.data$diag3), nchar(.data$diag3) == 3L,
                  !is.na(.data$elixhauser)) |>
    dplyr::select("diag3", "elixhauser", "elixhauser_x") |>
    dplyr::distinct()

  lookup_4 <- char_elix_codes |>
    dplyr::filter(!is.na(.data$diag3), nchar(.data$diag3) == 4L,
                  !is.na(.data$elixhauser)) |>
    dplyr::select("diag3", "elixhauser", "elixhauser_x") |>
    dplyr::distinct()

  # --- Three-level diagnosis matching ---
  # Level 1: full code match
  # Some ICD-10-AM codes map to multiple Elixhauser categories; many-to-many is expected.
  df_match_full <- df_std |>
    dplyr::left_join(lookup_full, by = "diag",
                     relationship = "many-to-many") |>
    dplyr::filter(!is.na(.data$elixhauser)) |>
    dplyr::select("episode_id", "elixhauser", "elixhauser_x") |>
    dplyr::distinct()

  # Level 2: 3-character prefix match
  df_match_3 <- df_std |>
    dplyr::mutate(diag3 = stringr::str_sub(.data$diag, 1L, 3L)) |>
    dplyr::left_join(lookup_3, by = "diag3",
                     relationship = "many-to-many") |>
    dplyr::filter(!is.na(.data$elixhauser)) |>
    dplyr::select("episode_id", "elixhauser", "elixhauser_x") |>
    dplyr::distinct()

  # Level 3: 4-character prefix match
  df_match_4 <- df_std |>
    dplyr::mutate(diag3 = stringr::str_sub(.data$diag, 1L, 4L)) |>
    dplyr::left_join(lookup_4, by = "diag3",
                     relationship = "many-to-many") |>
    dplyr::filter(!is.na(.data$elixhauser)) |>
    dplyr::select("episode_id", "elixhauser", "elixhauser_x") |>
    dplyr::distinct()

  # Deduplicated weights lookup
  elix_lookup <- elix_weights |>
    dplyr::rename(elixhauser = "code") |>
    dplyr::distinct()

  # --- Combine levels, deduplicate, join weights ---
  df_matched <- dplyr::bind_rows(df_match_full, df_match_3, df_match_4) |>
    dplyr::distinct() |>
    dplyr::left_join(elix_lookup, by = "elixhauser") |>
    dplyr::mutate(elixhauser_x = paste0("el_", .data$elixhauser_x))

  # --- Compute total score per episode ---
  df_score <- df_matched |>
    dplyr::group_by(.data$episode_id) |>
    dplyr::summarise(
      elixhauser_score = sum(.data$weight, na.rm = TRUE),
      .groups = "drop"
    )

  # --- Pivot condition flags wide ---
  df_flags <- df_matched |>
    dplyr::select("episode_id", "elixhauser_x", "weight") |>
    dplyr::filter(!is.na(.data$weight)) |>
    dplyr::distinct() |>
    tidyr::pivot_wider(
      names_from  = "elixhauser_x",
      values_from = "weight",
      values_fill = 0,
      values_fn   = max
    )

  # --- Join back to episode data ---
  df |>
    dplyr::left_join(df_score, by = stats::setNames("episode_id", id_str)) |>
    dplyr::left_join(df_flags, by = stats::setNames("episode_id", id_str)) |>
    dplyr::mutate(
      elixhauser_score = dplyr::if_else(
        is.na(.data$elixhauser_score), 0, .data$elixhauser_score
      ),
      dplyr::across(dplyr::starts_with("el_"), ~ dplyr::if_else(is.na(.x), 0, .x))
    )
}
