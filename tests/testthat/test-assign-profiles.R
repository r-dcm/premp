test_that("assigning profiles (group design) in Round 1 works", {
  set.seed(123)

  possible_profiles <- tibble::tibble(tidyr::crossing(
    att1 = c(0:4),
    att2 = c(0:4),
    att3 = c(0:4),
    att4 = c(0:4),
    att5 = c(0:4),
    att6 = c(0:4),
    att7 = c(0:4)
  )) |>
    tibble::rowid_to_column("profile_id")

  obs <- runif(nrow(possible_profiles), 1, 10000)

  observed <- possible_profiles |>
    dplyr::mutate(n = obs, n = dplyr::case_when(n < 1000 ~ NA, TRUE ~ n)) |>
    dplyr::filter(!is.na(n))

  final_assignments <- assign_profiles(
    group_configuration = list(
      num_assignment_groups = 5L,
      panelists_per_group = 4L,
      shared_within = 3L,
      shared_across = 1L
    ),
    observed = observed,
    included_total_skills_mastered = c(5L, 10L, 15L, 20L, 25L),
    profiles_per_total_skills_mastered = 3L,
    output_dir = testthat::test_path("data")
  )

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
  testthat::expect_equal(ncol(final_assignments), 3)
  # correct number of rows
  testthat::expect_equal(nrow(final_assignments), 300)
  # column names are correct
  testthat::expect_equal(
    colnames(final_assignments),
    c("profile_id", "group", "rater_id")
  )
  # every rater assigned correct number of profiles
  testthat::expect_equal(
    final_assignments |>
      dplyr::count(.data$rater_id) |>
      dplyr::pull(.data$n),
    rep(75, 4)
  )
})

# stopped fixing unit tests here
test_that("assigning profiles (rater design) in Round 1 works", {
  set.seed(123)

  possible_profiles <- tibble::tibble(tidyr::crossing(
    att1 = c(0:4),
    att2 = c(0:4),
    att3 = c(0:4),
    att4 = c(0:4),
    att5 = c(0:4),
    att6 = c(0:4),
    att7 = c(0:4)
  )) |>
    tibble::rowid_to_column("profile_id")

  obs <- runif(nrow(possible_profiles), 1, 10000)

  observed <- possible_profiles |>
    dplyr::mutate(n = obs, n = dplyr::case_when(n < 1000 ~ NA, TRUE ~ n)) |>
    dplyr::filter(!is.na(n))

  final_assignments <- assign_profiles(
    group_configuration = NULL,
    panelist_configuration = list(
      num_panelists = 5L,
      shared_across = 1L
    ),
    observed = observed,
    included_total_skills_mastered = c(5L, 10L, 15L, 20L, 25L),
    profiles_per_total_skills_mastered = 3L,
    output_dir = testthat::test_path("data")
  )

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
  testthat::expect_equal(ncol(final_assignments), 2)
  # correct number of rows
  testthat::expect_equal(nrow(final_assignments), 75)
  # column names are correct
  testthat::expect_equal(
    colnames(final_assignments),
    c("profile_id", "rater_id")
  )
  # every rater assigned correct number of profiles
  testthat::expect_equal(
    final_assignments |>
      dplyr::count(.data$rater_id) |>
      dplyr::pull(.data$n),
    rep(15, 5)
  )
  # minimum number of profiles seen by all raters
  testthat::expect_gte(
    final_assignments |>
      dplyr::count(.data$rater_id) |>
      dplyr::filter(.data$n == 5) |>
      nrow(),
    0
  )
})

test_that("assigning profiles (rater design) in Round 2 works", {
  set.seed(123)

  possible_profiles <- tibble::tibble(tidyr::crossing(
    att1 = c(0:4),
    att2 = c(0:4),
    att3 = c(0:4),
    att4 = c(0:4),
    att5 = c(0:4),
    att6 = c(0:4),
    att7 = c(0:4)
  )) |>
    tibble::rowid_to_column("profile_id")

  obs <- runif(nrow(possible_profiles), 1, 10000)

  observed <- possible_profiles |>
    dplyr::mutate(n = obs, n = dplyr::case_when(n < 1000 ~ NA, TRUE ~ n)) |>
    dplyr::filter(!is.na(n))

  round_1_assignments <- assign_profiles(
    group_configuration = NULL,
    panelist_configuration = list(
      num_panelists = 5L,
      shared_across = 1L
    ),
    observed = observed,
    included_total_skills_mastered = c(5L, 10L, 15L, 20L, 25L),
    profiles_per_total_skills_mastered = 3L,
    output_dir = testthat::test_path("data")
  )

  final_assignments <- assign_profiles(
    group_configuration = NULL,
    panelist_configuration = list(
      num_panelists = 5L,
      shared_across = 1L
    ),
    observed = observed,
    included_total_skills_mastered = c(5L, 10L, 15L, 20L, 25L),
    profiles_per_total_skills_mastered = 2L,
    previously_rated = round_1_assignments,
    output_dir = testthat::test_path("data")
  )

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
  testthat::expect_equal(ncol(final_assignments), 2)
  # correct number of rows
  testthat::expect_equal(nrow(final_assignments), 50)
  # column names are correct
  testthat::expect_equal(
    colnames(final_assignments),
    c("profile_id", "rater_id")
  )
  # every rater assigned correct number of profiles
  testthat::expect_equal(
    final_assignments |>
      dplyr::count(.data$rater_id) |>
      dplyr::pull(.data$n),
    rep(10, 5)
  )
  # minimum number of profiles seen by all raters
  testthat::expect_gte(
    final_assignments |>
      dplyr::count(.data$rater_id) |>
      dplyr::filter(.data$n == 10) |>
      nrow(),
    5
  )
})

test_that("assign profiles -- error messages work", {
  set.seed(123)

  possible_profiles <- tibble::tibble(tidyr::crossing(
    att1 = c(0:4),
    att2 = c(0:4),
    att3 = c(0:4),
    att4 = c(0:4),
    att5 = c(0:4),
    att6 = c(0:4),
    att7 = c(0:4)
  )) |>
    tibble::rowid_to_column("profile_id")

  obs <- runif(nrow(possible_profiles), 1, 10000)

  observed <- possible_profiles |>
    dplyr::mutate(n = obs, n = dplyr::case_when(n < 1000 ~ NA, TRUE ~ n)) |>
    dplyr::filter(!is.na(n))

  # test incorrect arguments
  err <- rlang::catch_cnd(assign_profiles(
    group_configuration = list(
      num_assignment_groups = 5,
      panelists_per_group = 4L,
      shared_within = 3L,
      shared_across = 1L
    ),
    observed = observed,
    included_total_skills_mastered = c(5L, 10L, 15L, 20L, 25L),
    profiles_per_total_skills_mastered = 3L,
    output_dir = testthat::test_path("data")
  ))
  testthat::expect_s3_class(err, "rlang_error")
  testthat::expect_match(
    err$message,
    "must be an integer."
  )

  err <- rlang::catch_cnd(
    assign_profiles(
      group_configuration = c(
        num_assignment_groups = 5L,
        panelists_per_group = 4L,
        shared_within = 2L,
        shared_across = 1L
      ),
      observed = observed,
      included_total_skills_mastered = c(5L, 10L, 15L, 20L, 25L),
      profiles_per_total_skills_mastered = 3L,
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
      group_configuration = list(
        num_assignment_groups = 5L,
        panelits_per_group = 4L,
        shared_within = 2L,
        shared_profiles = 1L
      ),
      observed = observed,
      included_total_skills_mastered = c(5L, 10L, 15L, 20L, 25L),
      profiles_per_total_skills_mastered = 3L,
      output_dir = testthat::test_path("data")
    )
  )
  testthat::expect_s3_class(err, "rlang_error")
  testthat::expect_match(
    err$message,
    paste0(
      "must be a list containing `num_assignment_groups`, ",
      "`panelists_per_group`, `shared_within`, and ",
      "`shared_across`."
    )
  )

  err <- rlang::catch_cnd(
    assign_profiles(
      group_configuration = list(
        num_assignment_groups = 5L,
        shared_within = 2L,
        shared_across = 1L
      ),
      observed = observed,
      included_total_skills_mastered = c(5L, 10L, 15L, 20L, 25L),
      profiles_per_total_skills_mastered = 3L,
      output_dir = testthat::test_path("data")
    )
  )
  testthat::expect_s3_class(err, "rlang_error")
  testthat::expect_match(
    err$message,
    "must be a list of length 4."
  )

  err <- rlang::catch_cnd(
    assign_profiles(
      group_configuration = list(
        num_assignment_groups = 5L,
        panelists_per_group = 4,
        shared_within = 2L,
        shared_across = 1L
      ),
      observed = observed,
      included_total_skills_mastered = c(5L, 10L, 15L, 20L, 25L),
      profiles_per_total_skills_mastered = 3L,
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
      group_configuration = list(
        num_assignment_groups = 5L,
        panelists_per_group = 4L,
        shared_within = 1.1,
        shared_across = 1L
      ),
      observed = observed,
      included_total_skills_mastered = c(5L, 10L, 15L, 20L, 25L),
      profiles_per_total_skills_mastered = 3L,
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
      group_configuration = list(
        num_assignment_groups = 5L,
        panelists_per_group = 4L,
        shared_within = 2L,
        shared_across = 1.1
      ),
      observed = observed,
      included_total_skills_mastered = c(5L, 10L, 15L, 20L, 25L),
      profiles_per_total_skills_mastered = 3L,
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
      group_configuration = list(
        num_assignment_groups = 5L,
        panelists_per_group = 4L,
        shared_within = 2L,
        shared_across = 1L
      ),
      observed = observed,
      observed_count_label = 5,
      included_total_skills_mastered = c(5L, 10L, 15L, 20L, 25L),
      profiles_per_total_skills_mastered = 3L,
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
      group_configuration = list(
        num_assignment_groups = 5L,
        panelists_per_group = 4L,
        shared_within = 2L,
        shared_across = 1L
      ),
      observed = observed,
      included_total_skills_mastered = c(5, 10L, 15L, 20L, 25L),
      profiles_per_total_skills_mastered = 3L,
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
      group_configuration = list(
        num_assignment_groups = 5L,
        panelists_per_group = 4L,
        shared_within = 2L,
        shared_across = 1L
      ),
      observed = observed,
      included_total_skills_mastered = list(5L),
      profiles_per_total_skills_mastered = 3L,
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
      group_configuration = list(
        num_assignment_groups = 5L,
        panelists_per_group = 4L,
        shared_within = 2L,
        shared_across = 1L
      ),
      observed = observed,
      included_total_skills_mastered = c(5L, 10L, 15L, 20L, 25L),
      profiles_per_total_skills_mastered = 3,
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
      group_configuration = list(
        num_assignment_groups = 5L,
        panelists_per_group = 4L,
        shared_within = 2L,
        shared_across = 1L
      ),
      observed = observed |>
        dplyr::mutate(
          n = dplyr::case_when(n < 2500 ~ NA, TRUE ~ n)
        ),
      included_total_skills_mastered = c(5L, 10L, 15L, 20L, 25L),
      profiles_per_total_skills_mastered = 3L,
      output_dir = testthat::test_path("data")
    )
  )
  testthat::expect_s3_class(err, "rlang_error")
  testthat::expect_match(
    err$message,
    "should only include profiles that were observed."
  )
})

test_that("assign profiles -- few attributes work (group design)", {
  set.seed(1234)

  possible_profiles <- tibble::tibble(tidyr::crossing(
    att1 = c(0:4),
    att2 = c(0:4),
    att3 = c(0:4),
    att4 = c(0:4)
  )) |>
    tibble::rowid_to_column("profile_id")

  obs <- runif(nrow(possible_profiles), 1, 10000)

  observed <- possible_profiles |>
    dplyr::mutate(n = obs, n = dplyr::case_when(n < 1000 ~ NA, TRUE ~ n)) |>
    dplyr::filter(!is.na(n))

  final_assignments <- assign_profiles(
    group_configuration = list(
      num_assignment_groups = 5L,
      panelists_per_group = 4L,
      shared_within = 2L,
      shared_across = 1L
    ),
    observed = observed,
    included_total_skills_mastered = c(5L, 10L, 15L, 20L, 25L),
    profiles_per_total_skills_mastered = 3L,
    output_dir = testthat::test_path("data")
  )

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
  testthat::expect_equal(ncol(final_assignments), 3)
  # correct number of rows
  testthat::expect_equal(nrow(final_assignments), 152)
  # column names are correct
  testthat::expect_equal(
    colnames(final_assignments),
    c("profile_id", "group", "rater_id")
  )
  # every rater assigned correct number of profiles
  testthat::expect_equal(
    final_assignments |>
      dplyr::count(.data$rater_id) |>
      dplyr::pull(.data$n),
    c(38, 38, 38, 38)
  )
})

test_that("assign profiles -- few attributes work (rater design)", {
  set.seed(1234)

  possible_profiles <- tibble::tibble(tidyr::crossing(
    att1 = c(0:4),
    att2 = c(0:4),
    att3 = c(0:4),
    att4 = c(0:4)
  )) |>
    tibble::rowid_to_column("profile_id")

  obs <- runif(nrow(possible_profiles), 1, 10000)

  observed <- possible_profiles |>
    dplyr::mutate(n = obs, n = dplyr::case_when(n < 1000 ~ NA, TRUE ~ n)) |>
    dplyr::filter(!is.na(n))

  final_assignments <- assign_profiles(
    group_configuration = NULL,
    panelist_configuration = list(
      num_panelists = 5L,
      shared_across = 1L
    ),
    observed = observed,
    included_total_skills_mastered = c(5L, 10L, 15L, 20L, 25L),
    profiles_per_total_skills_mastered = 3L,
    output_dir = testthat::test_path("data")
  )

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
  testthat::expect_equal(ncol(final_assignments), 2)
  # correct number of rows
  testthat::expect_equal(nrow(final_assignments), 38)
  # column names are correct
  testthat::expect_equal(
    colnames(final_assignments),
    c("profile_id", "rater_id")
  )
  # every rater assigned correct number of profiles
  testthat::expect_equal(
    final_assignments |>
      dplyr::count(.data$rater_id) |>
      dplyr::pull(.data$n),
    c(8, 8, 8, 7, 7)
  )
})
