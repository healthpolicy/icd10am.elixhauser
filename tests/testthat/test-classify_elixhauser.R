dfbase <- icd10am.utils::dfbase
dfdiag <- icd10am.utils::dfdiag
dfwide <- icd10am.utils::dfwide

test_that("classify_elixhauser returns expected columns in long format", {
  result <- classify_elixhauser(
    df      = dfbase,
    id      = recordID,
    format  = "long",
    df_diag = dfdiag,
    diag    = "diag",
    diagno  = "diagno"
  )

  expect_true("elixhauser_score" %in% names(result))
  expect_true(any(startsWith(names(result), "el_")))
  expect_equal(nrow(result), nrow(dfbase))
})

test_that("classify_elixhauser wide format produces same scores as long format", {
  # Derive long-format inputs from dfwide so IDs match
  df_episodes <- dfwide[, c("recordID", "ageyears", "agemonths")]

  diag_cols <- grep("^diag[0-9]+$", names(dfwide), value = TRUE)
  df_diag_long <- tidyr::pivot_longer(
    dfwide[, c("recordID", diag_cols)],
    cols      = dplyr::all_of(diag_cols),
    names_to  = "diagno",
    values_to = "diag"
  ) |>
    dplyr::mutate(diagno = as.integer(gsub("diag", "", diagno))) |>
    dplyr::filter(!is.na(diag), diag != "")

  result_long <- classify_elixhauser(
    df      = df_episodes,
    id      = recordID,
    format  = "long",
    df_diag = df_diag_long,
    diag    = "diag",
    diagno  = "diagno"
  )

  result_wide <- classify_elixhauser(
    df          = dfwide,
    id          = recordID,
    format      = "wide",
    diag_prefix = "diag"
  )

  scores_long <- result_long[order(result_long$recordID), "elixhauser_score"][[1]]
  scores_wide <- result_wide[order(result_wide$recordID), "elixhauser_score"][[1]]

  expect_equal(scores_long, scores_wide)
})

test_that("classify_elixhauser gives score 0 for episodes with no matching diagnoses", {
  empty_diag <- data.frame(
    recordID = dfbase$recordID[1],
    diag     = "ZZZZZ",
    diagno   = 1L
  )

  result <- classify_elixhauser(
    df = dfbase[1, ], id = recordID, format = "long",
    df_diag = empty_diag, diag = "diag", diagno = "diagno"
  )

  expect_equal(result$elixhauser_score, 0)
})

test_that("classify_elixhauser el_ columns are numeric", {
  result <- classify_elixhauser(
    df = dfbase, id = recordID, format = "long",
    df_diag = dfdiag, diag = "diag", diagno = "diagno"
  )

  el_cols <- names(result)[startsWith(names(result), "el_")]
  expect_true(length(el_cols) > 0)
  expect_true(all(sapply(result[el_cols], is.numeric)))
})

test_that("classify_elixhauser produces non-zero scores for known comorbid episodes", {
  result <- classify_elixhauser(
    df = dfbase, id = recordID, format = "long",
    df_diag = dfdiag, diag = "diag", diagno = "diagno"
  )

  expect_true(any(result$elixhauser_score != 0))
})
