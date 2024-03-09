cohortNameReference = "cohort_1"
cohortNameComparator = c("cohort_2", "cohort_3")

table <- timing1 |>
  visOmopResults::splitAll() |>
  dplyr::filter(.data$cohort_name_reference %in% .env$cohortNameReference) |>
  dplyr::filter(.data$cohort_name_comparator %in% .env$cohortNameComparator)

counts <- table |>
  dplyr::filter(variable_name != "diff_days") |>
  dplyr::mutate(variable_name = gsub(" ", "_", .data$variable_name)) |>
  tidyr::pivot_wider(names_from = variable_name, values_from = estimate_value) |>
  dplyr::select(dplyr::all_of(c("result_id", "cdm_name", "cohort_name_reference",
                                "cohort_name_comparator", "number_records", "number_subjects")))

table |>
  dplyr::filter(.data$variable_name == "diff_days") |>
  dplyr::select(dplyr::all_of(c("result_id", "cdm_name", "cohort_name_reference", "cohort_name_comparator",
                                "variable_name", "estimate_name", "estimate_value", "variable_name"))) |>
  dplyr::mutate(
    estimate_value = as.numeric(.data$estimate_value),
    label = paste0(.data$cdm_name, "; ", .data$cohort_name_reference, "; ", .data$cohort_name_comparator),
    quantile = as.numeric(gsub("q", "", .data$estimate_name))/100
  ) |>
  dplyr::left_join(
    counts
  ) |>
  dplyr::group_by(.data$cdm_name, .data$cohort_name_reference, .data$cohort_name_comparator) |>
  dplyr::mutate(density = .data$quantile - dplyr::lag(.data$quantile),
                density = dplyr::if_else(is.na(.data$density),
                                         dplyr::lead(.data$density), .data$density),
                density = )



table <- lapply(split(table,seq(nrow(table))),
       function(tib){
         dplyr::tibble(
           x = seq(from = -10000, to = 10000, length.out = 1000) + tib$mean,
           y = dnorm(seq(tib$min-10000, tib$max+10000, length.out = 1000), tib$mean, tib$sd)
         ) |>
           dplyr::cross_join(tib)
       }) |>
  dplyr::bind_rows()

ggData <- dplyr::tibble(
  cohort_name_reference = cohortNameReference,
  cohort_name_comparator = cohortNameComparator
) |>
  dplyr::left_join(
    table,
    by = c("cohort_name_reference", "cohort_name_comparator")
  ) |>
  dplyr::mutate(y_pos = max(y)) |>
  dplyr::group_by(.data$cdm_name, .data$cohort_name_reference, .data$cohort_name_comparator) |>
  dplyr::mutate(
    y_pos = .data$y_pos*dplyr::cur_group_id(),
    y = .data$y + .data$y_pos
  ) |>
  dplyr::ungroup()


median_data <- ggData |>
  dplyr::group_by(.data$cdm_name, .data$cohort_name_reference, .data$cohort_name_comparator) |>
  dplyr::mutate(y_points = abs(.data$median - .data$x)) |>
  dplyr::arrange(.data$y_points) |>
  dplyr::mutate(id = dplyr::row_number()) |>
  dplyr::filter(id %in% 1:2)
  dplyr::filter((.data$median - .data$x) == min(.data$median - .data$x))



ggData |>
  ggplot2::ggplot(ggplot2::aes(x = .data$x, y = .data$y, fill = label)) +
  ggplot2::geom_line() +
  ggplot2::geom_polygon(alpha = 0.5) +
  ggplot2::geom_hline(yintercept = unique(ggData$y_pos)) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
  ggplot2::geom_line(ggplot2::aes(x = .data$median, y = .data$y),
                     data =)


