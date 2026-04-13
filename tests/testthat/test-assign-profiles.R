test_that("assigning profiles (table design) in Round 1 works", {
  set.seed(123)

  possible_profiles <- tibble::tibble(tidyr::crossing(att1 = c(0:4),
                                                      att2 = c(0:4),
                                                      att3 = c(0:4),
                                                      att4 = c(0:4),
                                                      att5 = c(0:4),
                                                      att6 = c(0:4),
                                                      att7 = c(0:4)))

  obs <- runif(nrow(possible_profiles), 1, 10000)

  observed <- possible_profiles |>
    dplyr::mutate(n = obs,
                  n = dplyr::case_when(n < 1000 ~ NA,
                                       TRUE ~ n)) |>
    dplyr::filter(!is.na(n)) |>
    dplyr::mutate(pct = n / sum(n))

  final_assignments <- assign_profiles(
    num_assignment_groups = 5L,
    table_configuration = list(panelists_per_table = 4L,
                               proportion_of_shared_profiles = .67),
    possible_profiles = possible_profiles,
    observed = observed,
    range_of_profiles = c(5L, 10L, 15L, 20L, 25L),
    profiles_per_level = 3L,
    output_dir = testthat::test_path("data")
  )

  observed <- final_assignments$observed
  final_assignments <- final_assignments$profile_sampling

  # did output save correctly
  testthat::expect_equal(
    final_assignments,
    suppressMessages(
      readr::read_csv(
        testthat::test_path("data/profile_assignments.csv")
      )
    )
  )

  # correct number of columns
  testthat::expect_equal(ncol(final_assignments), 13)
  # correct number of rows
  testthat::expect_equal(nrow(final_assignments), 150)
  # total attributes are correct
  testthat::expect_equal(final_assignments |>
                           dplyr::distinct(.data$total) |>
                           dplyr::pull(),
                         c(5, 10, 15, 20, 25))
  # number of each attribute total is correct
  testthat::expect_equal(final_assignments |>
                           dplyr::pull(.data$total),
                         rep(c(5, 10, 15, 20, 25), each = 30))
  # column names are correct
  testthat::expect_equal(colnames(final_assignments),
                         c(glue::glue("att{1:7}"), "total", "table",
                           glue::glue("rater{1:4}")))
  # every rater assigned correct number of profiles
  testthat::expect_equal(final_assignments |>
                           dplyr::select(dplyr::starts_with("rater")) |>
                           tidyr::pivot_longer(cols = dplyr::everything(),
                                               names_to = "rater",
                                               values_to = "assignment") |>
                           dplyr::filter(.data$assignment == 1) |>
                           dplyr::count(.data$rater) |>
                           dplyr::pull(.data$n),
                         rep(75, 4))
  # all attribute totals are increment of the range of profiles
  testthat::expect_equal(final_assignments |>
                           dplyr::mutate(increment = .data$total %% 5) |>
                           dplyr::pull(.data$increment),
                         rep(0, nrow(final_assignments)))
  # assignments are in correct format
  testthat::expect_contains(c(0, 1),
                            final_assignments |>
                              dplyr::select(dplyr::starts_with("rater")) |>
                              tidyr::pivot_longer(cols = dplyr::everything(),
                                                  names_to = "rater",
                                                  values_to = "assignment") |>
                              dplyr::distinct(.data$assignment) |>
                              dplyr::pull())
  # attribute scores are in correct format
  testthat::expect_contains(c(0:4),
                            final_assignments |>
                              dplyr::select(dplyr::starts_with("att")) |>
                              tidyr::pivot_longer(cols = dplyr::everything(),
                                                  names_to = "att",
                                                  values_to = "att_score") |>
                              dplyr::distinct(.data$att_score) |>
                              dplyr::pull())
})

test_that("assigning profiles (rater design) in Round 1 works", {
  set.seed(123)

  possible_profiles <- tibble::tibble(tidyr::crossing(att1 = c(0:4),
                                                      att2 = c(0:4),
                                                      att3 = c(0:4),
                                                      att4 = c(0:4),
                                                      att5 = c(0:4),
                                                      att6 = c(0:4),
                                                      att7 = c(0:4)))

  obs <- runif(nrow(possible_profiles), 1, 10000)

  observed <- possible_profiles |>
    dplyr::mutate(n = obs,
                  n = dplyr::case_when(n < 1000 ~ NA,
                                       TRUE ~ n)) |>
    dplyr::filter(!is.na(n)) |>
    dplyr::mutate(pct = n / sum(n))

  final_assignments <- assign_profiles(num_assignment_groups = 5L,
                                       table_configuration = NULL,
                                       possible_profiles = possible_profiles,
                                       observed = observed,
                                       range_of_profiles =
                                         c(5L, 10L, 15L, 20L, 25L),
                                       profiles_per_level = 3L,
                                       output_dir = testthat::test_path("data"))

  observed <- final_assignments$observed
  final_assignments <- final_assignments$profile_sampling

  # did output save correctly
  testthat::expect_equal(
    final_assignments,
    suppressMessages(
      readr::read_csv(
        testthat::test_path("data/profile_assignments.csv")
      )
    )
  )

  # correct number of columns
  testthat::expect_equal(ncol(final_assignments), 13)
  # correct number of rows
  testthat::expect_equal(nrow(final_assignments), 55)
  # total attributes are correct
  testthat::expect_equal(final_assignments |>
                           dplyr::distinct(.data$total) |>
                           dplyr::pull(),
                         c(5, 10, 15, 20, 25))
  # number of each attribute total is correct
  testthat::expect_equal(final_assignments |>
                           dplyr::pull(.data$total),
                         rep(c(5, 10, 15, 20, 25), each = 11))
  # column names are correct
  testthat::expect_equal(colnames(final_assignments),
                         c(glue::glue("att{1:7}"), "total",
                           glue::glue("rater{1:5}")))
  # every rater assigned correct number of profiles
  testthat::expect_equal(final_assignments |>
                           dplyr::select(dplyr::starts_with("rater")) |>
                           tidyr::pivot_longer(cols = dplyr::everything(),
                                               names_to = "rater",
                                               values_to = "assignment") |>
                           dplyr::filter(.data$assignment == 1) |>
                           dplyr::count(.data$rater) |>
                           dplyr::pull(.data$n),
                         rep(15, 5))
  # minimum number of profiles seen by all raters
  testthat::expect_gte(final_assignments |>
                         dplyr::select(dplyr::starts_with("rater")) |>
                         tibble::rowid_to_column("prof_num") |>
                         tidyr::pivot_longer(cols = -c("prof_num"),
                                             names_to = "rater",
                                             values_to = "assignment") |>
                         dplyr::filter(.data$assignment == 1) |>
                         dplyr::count(.data$prof_num) |>
                         dplyr::filter(.data$n == 5) |>
                         nrow(),
                       1)
  # all attribute totals are increment of the range of profiles
  testthat::expect_equal(final_assignments |>
                           dplyr::mutate(increment = .data$total %% 5) |>
                           dplyr::pull(.data$increment),
                         rep(0, nrow(final_assignments)))
  # assignments are in correct format
  testthat::expect_contains(c(0, 1),
                            final_assignments |>
                              dplyr::select(dplyr::starts_with("rater")) |>
                              tidyr::pivot_longer(cols = dplyr::everything(),
                                                  names_to = "rater",
                                                  values_to = "assignment") |>
                              dplyr::distinct(.data$assignment) |>
                              dplyr::pull())
  # attribute scores are in correct format
  testthat::expect_contains(c(0:4),
                            final_assignments |>
                              dplyr::select(dplyr::starts_with("att")) |>
                              tidyr::pivot_longer(cols = dplyr::everything(),
                                                  names_to = "att",
                                                  values_to = "att_score") |>
                              dplyr::distinct(.data$att_score) |>
                              dplyr::pull())
})

test_that("assign profiles -- error messages work", {
  set.seed(123)

  possible_profiles <- tibble::tibble(tidyr::crossing(att1 = c(0:4),
                                                      att2 = c(0:4),
                                                      att3 = c(0:4),
                                                      att4 = c(0:4),
                                                      att5 = c(0:4),
                                                      att6 = c(0:4),
                                                      att7 = c(0:4)))

  obs <- runif(nrow(possible_profiles), 1, 10000)

  observed <- possible_profiles |>
    dplyr::mutate(n = obs,
                  n = dplyr::case_when(n < 1000 ~ NA,
                                       TRUE ~ n)) |>
    dplyr::filter(!is.na(n)) |>
    dplyr::mutate(pct = n / sum(n))

  # test incorrect arguments
  err <- rlang::catch_cnd(assign_profiles(
    num_assignment_groups = 5,
    table_configuration = list(panelists_per_table = 4L,
                               proportion_of_shared_profiles = .67),
    possible_profiles = possible_profiles,
    observed = observed,
    range_of_profiles = c(5L, 10L, 15L, 20L, 25L),
    profiles_per_level = 3L,
    output_dir = testthat::test_path("data")
  ))
  testthat::expect_s3_class(err, "rlang_error")
  testthat::expect_match(
    err$message,
    "must be an integer."
  )

  err <- rlang::catch_cnd(
    assign_profiles(
      num_assignment_groups = 5L,
      table_configuration = c(panelists_per_table = 4L,
                              proportion_of_shared_profiles = .67),
      possible_profiles = possible_profiles,
      observed = observed,
      range_of_profiles = c(5L, 10L, 15L, 20L, 25L),
      profiles_per_level = 3L,
      output_dir = testthat::test_path("data")
    )
  )
  testthat::expect_s3_class(err, "rlang_error")
  testthat::expect_match(
    err$message,
    "must be a list."
  )

  err <- rlang::catch_cnd(
    assign_profiles(
      num_assignment_groups = 5L,
      table_configuration = list(panelits_per_table = 4L,
                                 proportion_of_shared_profiles = .67),
      possible_profiles = possible_profiles,
      observed = observed,
      range_of_profiles = c(5L, 10L, 15L, 20L, 25L),
      profiles_per_level = 3L,
      output_dir = testthat::test_path("data")
    )
  )
  testthat::expect_s3_class(err, "rlang_error")
  testthat::expect_match(
    err$message,
    paste0(
      "must be a list containing `panelists_per_table` and ",
      "`proportion_of_shared_profiles`."
    )
  )

  err <- rlang::catch_cnd(
    assign_profiles(
      num_assignment_groups = 5L,
      table_configuration =
        list(panelists_per_table = 4L,
             proportion_of_shared_profiles = .67,
             panelists_per_table = 3L),
      possible_profiles = possible_profiles,
      observed = observed,
      range_of_profiles = c(5L, 10L, 15L, 20L, 25L),
      profiles_per_level = 3L,
      output_dir = testthat::test_path("data")
    )
  )
  testthat::expect_s3_class(err, "rlang_error")
  testthat::expect_match(
    err$message,
    "must be a list of length 2."
  )

  err <- rlang::catch_cnd(
    assign_profiles(
      num_assignment_groups = 5L,
      table_configuration = list(panelists_per_table = 4,
                                 proportion_of_shared_profiles = .67),
      possible_profiles = possible_profiles,
      observed = observed,
      range_of_profiles = c(5L, 10L, 15L, 20L, 25L),
      profiles_per_level = 3L,
      output_dir = testthat::test_path("data")
    )
  )
  testthat::expect_s3_class(err, "rlang_error")
  testthat::expect_match(
    err$message,
    "must be an integer."
  )

  err <- rlang::catch_cnd(
    assign_profiles(
      num_assignment_groups = 5L,
      table_configuration = list(panelists_per_table = 4L,
                                 proportion_of_shared_profiles = 1.1),
      possible_profiles = possible_profiles,
      observed = observed,
      range_of_profiles = c(5L, 10L, 15L, 20L, 25L),
      profiles_per_level = 3L,
      output_dir = testthat::test_path("data")
    )
  )
  testthat::expect_s3_class(err, "rlang_error")
  testthat::expect_match(
    err$message,
    "must be a double value between 0 and 1."
  )

  err <- rlang::catch_cnd(
    assign_profiles(
      num_assignment_groups = 5L,
      table_configuration = list(panelists_per_table = 4L,
                                 proportion_of_shared_profiles = -0.1),
      possible_profiles = possible_profiles,
      observed = observed,
      range_of_profiles = c(5L, 10L, 15L, 20L, 25L),
      profiles_per_level = 3L,
      output_dir = testthat::test_path("data")
    )
  )
  testthat::expect_s3_class(err, "rlang_error")
  testthat::expect_match(
    err$message,
    "must be a double value between 0 and 1."
  )

  err <- rlang::catch_cnd(
    assign_profiles(
      num_assignment_groups = 5L,
      table_configuration = list(panelists_per_table = 4L,
                                 proportion_of_shared_profiles = "a"),
      possible_profiles = possible_profiles,
      observed = observed,
      range_of_profiles = c(5L, 10L, 15L, 20L, 25L),
      profiles_per_level = 3L,
      output_dir = testthat::test_path("data")
    )
  )
  testthat::expect_s3_class(err, "rlang_error")
  testthat::expect_match(
    err$message,
    "must be a double value between 0 and 1."
  )

  err <- rlang::catch_cnd(
    assign_profiles(
      num_assignment_groups = 5L,
      table_configuration =
        list(panelists_per_table = 4L,
             proportion_of_shared_profiles = .67),
      possible_profiles = possible_profiles,
      observed = observed,
      observed_count_label = 5,
      range_of_profiles = c(5L, 10L, 15L, 20L, 25L),
      profiles_per_level = 3L,
      output_dir = testthat::test_path("data")
    )
  )
  testthat::expect_s3_class(err, "rlang_error")
  testthat::expect_match(
    err$message,
    "must be a character string."
  )

  err <- rlang::catch_cnd(
    assign_profiles(
      num_assignment_groups = 5L,
      table_configuration = list(panelists_per_table = 4L,
                                 proportion_of_shared_profiles = .67),
      possible_profiles = possible_profiles,
      observed = observed,
      range_of_profiles = c(5, 10L, 15L, 20L, 25L),
      profiles_per_level = 3L,
      output_dir = testthat::test_path("data")
    )
  )
  testthat::expect_s3_class(err, "rlang_error")
  testthat::expect_match(
    err$message,
    "must be a vector of integer values."
  )

  err <- rlang::catch_cnd(
    assign_profiles(
      num_assignment_groups = 5L,
      table_configuration = list(panelists_per_table = 4L,
                                 proportion_of_shared_profiles = .67),
      possible_profiles = possible_profiles,
      observed = observed,
      range_of_profiles = list(5L),
      profiles_per_level = 3L,
      output_dir = testthat::test_path("data")
    )
  )
  testthat::expect_s3_class(err, "rlang_error")
  testthat::expect_match(
    err$message,
    "must be a vector of integer values."
  )

  err <- rlang::catch_cnd(
    assign_profiles(
      num_assignment_groups = 5L,
      table_configuration = list(panelists_per_table = 4L,
                                 proportion_of_shared_profiles = .67),
      possible_profiles = possible_profiles,
      observed = observed,
      range_of_profiles = c(5L, 10L, 15L, 20L, 25L),
      profiles_per_level = 3,
      output_dir = testthat::test_path("data")
    )
  )
  testthat::expect_s3_class(err, "rlang_error")
  testthat::expect_match(
    err$message,
    "must be an integer."
  )
})

test_that("assign profiles -- few attributes work (table design)", {
  set.seed(1234)

  possible_profiles <- tibble::tibble(tidyr::crossing(att1 = c(0:4),
                                                      att2 = c(0:4),
                                                      att3 = c(0:4),
                                                      att4 = c(0:4)))

  obs <- runif(nrow(possible_profiles), 1, 10000)

  observed <- possible_profiles |>
    dplyr::mutate(n = obs,
                  n = dplyr::case_when(n < 1000 ~ NA,
                                       TRUE ~ n)) |>
    dplyr::filter(!is.na(n)) |>
    dplyr::mutate(pct = n / sum(n))

  final_assignments <- assign_profiles(
    num_assignment_groups = 5L,
    table_configuration =
      list(panelists_per_table = 4L,
           proportion_of_shared_profiles = .67),
    possible_profiles = possible_profiles,
    observed = observed,
    range_of_profiles = c(5L, 10L, 15L, 20L, 25L),
    profiles_per_level = 3L,
    output_dir = testthat::test_path("data")
  )

  observed <- final_assignments$observed
  final_assignments <- final_assignments$profile_sampling

  # did output save correctly
  testthat::expect_equal(
    final_assignments,
    suppressMessages(
      readr::read_csv(
        testthat::test_path("data/profile_assignments.csv")
      )
    )
  )

  # correct number of columns
  testthat::expect_equal(ncol(final_assignments), 10)
  # correct number of rows
  testthat::expect_equal(nrow(final_assignments), 68)
  # total attributes are correct
  testthat::expect_equal(final_assignments |>
                           dplyr::distinct(.data$total) |>
                           dplyr::pull(),
                         c(5, 10, 15))
  # number of each attribute total is correct
  testthat::expect_equal(final_assignments |>
                           dplyr::pull(.data$total),
                         c(rep(5, 30), rep(10, 30), rep(15, 8)))
  # column names are correct
  testthat::expect_equal(colnames(final_assignments),
                         c(glue::glue("att{1:4}"), "total", "table",
                           glue::glue("rater{1:4}")))
  # every rater assigned correct number of profiles
  testthat::expect_equal(final_assignments |>
                           dplyr::select(dplyr::starts_with("rater")) |>
                           tidyr::pivot_longer(cols = dplyr::everything(),
                                               names_to = "rater",
                                               values_to = "assignment") |>
                           dplyr::filter(.data$assignment == 1) |>
                           dplyr::count(.data$rater) |>
                           dplyr::pull(.data$n),
                         c(38, 38, 38, 38))
  # all attribute totals are increment of the range of profiles
  testthat::expect_equal(final_assignments |>
                           dplyr::mutate(increment = .data$total %% 5) |>
                           dplyr::pull(.data$increment),
                         rep(0, nrow(final_assignments)))
  # assignments are in correct format
  testthat::expect_contains(c(0, 1),
                            final_assignments |>
                              dplyr::select(dplyr::starts_with("rater")) |>
                              tidyr::pivot_longer(cols = dplyr::everything(),
                                                  names_to = "rater",
                                                  values_to = "assignment") |>
                              dplyr::distinct(.data$assignment) |>
                              dplyr::pull())
  # attribute scores are in correct format
  testthat::expect_contains(c(0:4),
                            final_assignments |>
                              dplyr::select(dplyr::starts_with("att")) |>
                              tidyr::pivot_longer(cols = dplyr::everything(),
                                                  names_to = "att",
                                                  values_to = "att_score") |>
                              dplyr::distinct(.data$att_score) |>
                              dplyr::pull())
})

test_that("assign profiles -- few attributes work (rater design)", {
  set.seed(1234)

  possible_profiles <- tibble::tibble(tidyr::crossing(att1 = c(0:4),
                                                      att2 = c(0:4),
                                                      att3 = c(0:4),
                                                      att4 = c(0:4)))

  obs <- runif(nrow(possible_profiles), 1, 10000)

  observed <- possible_profiles |>
    dplyr::mutate(n = obs,
                  n = dplyr::case_when(n < 1000 ~ NA,
                                       TRUE ~ n)) |>
    dplyr::filter(!is.na(n)) |>
    dplyr::mutate(pct = n / sum(n))

  final_assignments <- assign_profiles(num_assignment_groups = 5L,
                                       table_configuration = NULL,
                                       possible_profiles = possible_profiles,
                                       observed = observed,
                                       range_of_profiles =
                                         c(5L, 10L, 15L, 20L, 25L),
                                       profiles_per_level = 3L,
                                       output_dir = testthat::test_path("data"))

  observed <- final_assignments$observed
  final_assignments <- final_assignments$profile_sampling

  # did output save correctly
  testthat::expect_equal(
    final_assignments,
    suppressMessages(
      readr::read_csv(
        testthat::test_path("data/profile_assignments.csv")
      )
    )
  )

  # correct number of columns
  testthat::expect_equal(ncol(final_assignments), 10)
  # correct number of rows
  testthat::expect_equal(nrow(final_assignments), 26)
  # total attributes are correct
  testthat::expect_equal(final_assignments |>
                           dplyr::distinct(.data$total) |>
                           dplyr::pull(),
                         c(5, 10, 15))
  # number of each attribute total is correct
  testthat::expect_equal(final_assignments |>
                           dplyr::pull(.data$total),
                         c(rep(5, 11), rep(10, 11), rep(15, 4)))
  # column names are correct
  testthat::expect_equal(colnames(final_assignments),
                         c(glue::glue("att{1:4}"), "total",
                           glue::glue("rater{1:5}")))
  # every rater assigned correct number of profiles
  testthat::expect_equal(final_assignments |>
                           dplyr::select(dplyr::starts_with("rater")) |>
                           tidyr::pivot_longer(cols = dplyr::everything(),
                                               names_to = "rater",
                                               values_to = "assignment") |>
                           dplyr::filter(.data$assignment == 1) |>
                           dplyr::count(.data$rater) |>
                           dplyr::pull(.data$n),
                         c(8, 8, 8, 7, 7))
  # minimum number of profiles seen by all raters
  testthat::expect_gte(final_assignments |>
                         dplyr::select(dplyr::starts_with("rater")) |>
                         tibble::rowid_to_column("prof_num") |>
                         tidyr::pivot_longer(cols = -c("prof_num"),
                                             names_to = "rater",
                                             values_to = "assignment") |>
                         dplyr::filter(.data$assignment == 1) |>
                         dplyr::count(.data$prof_num) |>
                         dplyr::filter(.data$n == 5) |>
                         nrow(),
                       1)
  # all attribute totals are increment of the range of profiles
  testthat::expect_equal(final_assignments |>
                           dplyr::mutate(increment = .data$total %% 5) |>
                           dplyr::pull(.data$increment),
                         rep(0, nrow(final_assignments)))
  # assignments are in correct format
  testthat::expect_contains(c(0, 1),
                            final_assignments |>
                              dplyr::select(dplyr::starts_with("rater")) |>
                              tidyr::pivot_longer(cols = dplyr::everything(),
                                                  names_to = "rater",
                                                  values_to = "assignment") |>
                              dplyr::distinct(.data$assignment) |>
                              dplyr::pull())
  # attribute scores are in correct format
  testthat::expect_contains(c(0:4),
                            final_assignments |>
                              dplyr::select(dplyr::starts_with("att")) |>
                              tidyr::pivot_longer(cols = dplyr::everything(),
                                                  names_to = "att",
                                                  values_to = "att_score") |>
                              dplyr::distinct(.data$att_score) |>
                              dplyr::pull())
})
