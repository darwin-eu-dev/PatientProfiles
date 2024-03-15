summariseSmd <- function(result,
                         absolute = FALSE,
                         missing = 0,
                         onlySameGroup = FALSE,
                         onlySameStrata = FALSE) {
  # CONTINUOUS
  # smd = (mean1 - mean2) / sqrt(sd1 * sd1 + sd2 * sd2)
  # DISCRETE
  # percentage:
  # smd = (p1 - p2) / sqrt((p1*(1-p1) + p2*(1-p2))/2)
  # multiple levels:
  # see asmd categorical
  # https://support.sas.com/resources/papers/proceedings12/335-2012.pdf
  # asmdCategorical
  return(xx)
}

asmdCategorical <- function(x, variables = NULL, groupName = "group", weight = NULL) {
  if (is.null(variables)) {
    variables <- colnames(x)[!(colnames(x) %in% c(groupName, weight))]
  }
  if (is.null(weight)) {
    x <- x %>% dplyr::mutate(weight = 1)
  } else {
    x <- x %>% dplyr::rename("weight" = dplyr::all_of(weight))
  }
  x <- x %>%
    dplyr::rename("group" = dplyr::all_of(groupName)) %>%
    dplyr::select("group", "weight", dplyr::all_of(variables))
  lab <- unique(x$group)
  if (length(lab) != 2) {
    stop("Number of labels in group column different from 2.")
  }
  x <- x %>%
    dplyr::mutate(group = dplyr::if_else(.data$group == .env$lab[1], 1, 0))
  denominator <- x %>%
    dplyr::group_by(.data$group) %>%
    dplyr::tally(wt = .data$weight, name = "denominator")
  result <- NULL
  for (k in 1:length(variables)) {
    y <- x %>%
      dplyr::rename("label" = dplyr::all_of(variables[k])) %>%
      dplyr::group_by(.data$group,.data$label) %>%
      dplyr::tally(wt = .data$weight) %>%
      dplyr::right_join(denominator, by = "group") %>%
      dplyr::mutate(percentage = dplyr::if_else(
        is.na(.data$n), 0, .data$n/.data$denominator
      )) %>%
      dplyr::select("label", "group", "percentage") %>%
      tidyr::pivot_wider(names_from = "group", values_from = "percentage", values_fill = 0)
    TT <- y[["1"]]
    CC <- y[["0"]]
    result <- result %>% dplyr::union_all(dplyr::tibble(
      variable = variables[k],
      asmd = asmdFromPercentage(TT, CC)
    ))
  }
  result <- result %>% mutate(asmd_type = "categorical")
  return(result)
}

asmdFromPercentage <- function(TT, CC) {
  if (length(TT) == 1) {
    return(NA)
  } else {
    TT <- TT[-1]
    CC <- CC[-1]
    n <- length(TT)
    vect <- TT - CC
    TT1 <- matrix(rep(TT, n), nrow = n, byrow = TRUE)
    TT2 <- matrix(rep(TT, n), nrow = n, byrow = FALSE)
    CC1 <- matrix(rep(CC, n), nrow = n, byrow = TRUE)
    CC2 <- matrix(rep(CC, n), nrow = n, byrow = FALSE)
    S <- (TT1*TT2 + CC1*CC2) / 2
    diag(S) <- (TT*(1-TT) + CC*(1-CC)) / 2
    asmd <- as.numeric(sqrt(vect %*% solve(S) %*% vect))
    return(asmd)
  }
}
