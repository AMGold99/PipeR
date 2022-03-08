# specify directory of yearly pivot tables
research_dir <- file.path(getwd(),
                          "piper analysis",
                          "survey",
                          "time series",
                          "research")




# specify paths of all .csv files in that directory
research_path <- fs::dir_ls(research_dir,
                            glob = "*.csv")




# create panel dataset from pivot tables
time_series_research <- purrr::map_dfr(paths, ~ readr::read_csv(.x, id = "path")) |>

  tidyr::extract(path, into = "year", "(\\d{4})") |>

  dplyr::relocate(year, .after = dplyr::last_col()) |>

  tidyr::pivot_longer(cols = !c(Value, year),
                      names_to = "Ethnicity",
                      values_to = "Amount") |>

  dplyr::mutate(year = as.integer(year))





####------DONE----------####

done_dir <- file.path(getwd(),
                      "piper analysis",
                      "survey",
                      "time series",
                      "done")
done_path <- fs::dir_ls(done_dir,
                        glob = "*.csv")

time_series_done <- purrr::map_dfr(paths_done, ~ readr::read_csv(.x, id = "path")) |>

  tidyr::extract(path, into = "year", "(\\d{4})") |>

  dplyr::relocate(year, .after = dplyr::last_col()) |>

  tidyr::pivot_longer(cols = !c(Value, year),
                      names_to = "Ethnicity",
                      values_to = "Amount") |>

  dplyr::mutate(year = as.integer(year))
