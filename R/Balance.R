# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of CohortMethod
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# @author Observational Health Data Sciences and Informatics
# @author Patrick Ryan
# @author Marc Suchard
# @author Martijn Schuemie

computeMeansPerGroup <- function(cohorts, cohortMethodData) {

  hasStrata <- "stratumId" %in% colnames(cohorts)

  if (hasStrata) {
    stratumSize <- cohorts %>%
      group_by(.data$stratumId, .data$treatment) %>%
      count() %>%
      ungroup()
  }

  if (hasStrata && any(stratumSize %>% pull(.data$n) > 1)) {
    # Variable strata sizes detected: weigh by size of strata set
    w <- stratumSize %>%
      mutate(weight = 1/.data$n) %>%
      inner_join(cohorts, by = c("stratumId", "treatment")) %>%
      select(.data$rowId, .data$treatment, .data$weight)

    # Normalize so sum(weight) == 1 per treatment arm:
    wSum <- w %>%
      group_by(.data$treatment) %>%
      summarize(wSum = sum(.data$weight, na.rm = TRUE)) %>%
      ungroup()

    cohortMethodData$w <- w %>%
      inner_join(wSum, by = "treatment") %>%
      mutate(weight = .data$weight / .data$wSum) %>%
      select(.data$rowId, .data$treatment, .data$weight)

    # By definition:
    sumW <- 1

    # Note: using abs() because due to rounding to machine precision number can become slightly negative:
    result <- cohortMethodData$covariates %>%
      inner_join(cohortMethodData$w, by = c("rowId")) %>%
      group_by(.data$covariateId, .data$treatment) %>%
      summarise(sum = sum(.data$covariateValue, na.rm = TRUE),
                mean = sum(.data$weight * .data$covariateValue, na.rm = TRUE),
                sumSqr = sum(.data$weight * .data$covariateValue^2, na.rm = TRUE),
                sumWSqr = sum(.data$weight^2, na.rm = TRUE)) %>%
      mutate(sd = sqrt(abs(.data$sumSqr - .data$mean^2) * sumW/(sumW^2 - .data$sumWSqr))) %>%
      ungroup() %>%
      select(.data$covariateId, .data$treatment, .data$sum, .data$mean, .data$sd) %>%
      collect()

    cohortMethodData$w <- NULL
  } else {
    cohortCounts <- cohorts %>%
      group_by(.data$treatment) %>%
      count()

    result <- cohortMethodData$covariates %>%
      inner_join(select(cohorts, .data$rowId, .data$treatment), by = "rowId") %>%
      group_by(.data$covariateId, .data$treatment) %>%
      summarise(sum = sum(.data$covariateValue, na.rm = TRUE),
                sumSqr = sum(.data$covariateValue^2, na.rm = TRUE)) %>%
      inner_join(cohortCounts, by = "treatment") %>%
      mutate(sd = sqrt((.data$sumSqr - (.data$sum^2/.data$n))/.data$n),
             mean = .data$sum/.data$n) %>%
      ungroup() %>%
      select(.data$covariateId, .data$treatment, .data$sum, .data$mean, .data$sd) %>%
      collect()
  }
  target <- result %>%
    filter(.data$treatment == 1) %>%
    select(.data$covariateId, sumTarget = .data$sum, meanTarget = .data$mean, sdTarget = .data$sd)

  comparator <- result %>%
    filter(.data$treatment == 0) %>%
    select(.data$covariateId, sumComparator = .data$sum, meanComparator = .data$mean, sdComparator = .data$sd)

  result <- target %>%
    full_join(comparator, by = "covariateId") %>%
    mutate(sd = sqrt((.data$sdTarget^2 + .data$sdComparator^2)/2)) %>%
    select(!c(.data$sdTarget, .data$sdComparator))

  return(result)
}

#' Compute covariate balance before and after matching and trimming
#'
#' @description
#' For every covariate, prevalence in treatment and comparator groups before and after
#' matching/trimming are computed. When variable ratio matching was used the balance score will be
#' corrected according the method described in Austin et al (2008).
#'
#' @template CohortMethodData
#'
#' @param population         A data frame containing the people that are remaining after matching
#'                           and/or trimming.
#' @param subgroupCovariateId  Optional: a covariate ID of a binary covariate that indicates a subgroup of
#'                             interest. Both the before and after populations will be restricted to this
#'                             subgroup before computing covariate balance.
#' @details
#' The population data frame should have the following three columns:
#'
#' - rowId (numeric): A unique identifier for each row (e.g. the person ID).
#' - treatment (integer): Column indicating whether the person is in the target (1) or comparator (0) group.
#' - propensityScore (numeric): Propensity score.
#'
#' @return
#' Returns a tibble describing the covariate balance before and after matching/trimming.
#'
#' @references
#' Austin, P.C. (2008) Assessing balance in measured baseline covariates when using many-to-one
#' matching on the propensity-score. Pharmacoepidemiology and Drug Safety, 17: 1218-1225.
#'
#' @export
computeCovariateBalance <- function(population, cohortMethodData, subgroupCovariateId = NULL) {
  ParallelLogger::logTrace("Computing covariate balance")
  start <- Sys.time()

  if (!is.null(subgroupCovariateId)) {
    subGroupCovariate <- cohortMethodData$covariates %>%
      filter(.data$covariateId == subgroupCovariateId) %>%
      collect()

    if (nrow(subGroupCovariate) == 0) {
      stop("Cannot find covariate with ID ", subgroupCovariateId)
    }

    tempCohorts <- cohortMethodData$cohorts %>%
      collect() %>%
      filter(.data$rowId %in% subGroupCovariate$rowId)

    if (nrow(tempCohorts) == 0) {
      stop("Cannot find covariate with ID ", subgroupCovariateId, " in population before matching/trimming")
    }

    sumTreatment <- sum(tempCohorts$treatment)
    if (sumTreatment == 0 || sumTreatment == nrow(tempCohorts)) {
      stop("Subgroup population before matching/trimming doesn't have both target and comparator")
    }

    tempCohortsAfterMatching <- population %>%
      filter(.data$rowId %in% subGroupCovariate$rowId)

    if (nrow(tempCohortsAfterMatching) == 0) {
      stop("Cannot find covariate with ID ", subgroupCovariateId, " in population after matching/trimming")
    }
    sumTreatment <- sum(tempCohortsAfterMatching$treatment)
    if (sumTreatment == 0 || sumTreatment == nrow(tempCohortsAfterMatching)) {
      stop("Subgroup population before matching/trimming doesn't have both target and comparator")
    }

    cohortMethodData$tempCohorts <- tempCohorts %>%
      select(.data$rowId, .data$treatment)

    cohortMethodData$tempCohortsAfterMatching <- tempCohortsAfterMatching %>%
      select(.data$rowId, .data$treatment, .data$stratumId)
  } else {
    cohortMethodData$tempCohorts <- cohortMethodData$cohorts %>%
      select(.data$rowId, .data$treatment)

    cohortMethodData$tempCohortsAfterMatching <- population %>%
      select(.data$rowId, .data$treatment, .data$stratumId)
  }
  on.exit(cohortMethodData$tempCohorts <- NULL)
  on.exit(cohortMethodData$tempCohortsAfterMatching <- NULL, add = TRUE)

  beforeMatching <- computeMeansPerGroup(cohortMethodData$tempCohorts, cohortMethodData)
  afterMatching <- computeMeansPerGroup(cohortMethodData$tempCohortsAfterMatching, cohortMethodData)

  colnames(beforeMatching)[colnames(beforeMatching) == "meanTarget"] <- "beforeMatchingMeanTarget"
  colnames(beforeMatching)[colnames(beforeMatching) == "meanComparator"] <- "beforeMatchingMeanComparator"
  colnames(beforeMatching)[colnames(beforeMatching) == "sumTarget"] <- "beforeMatchingSumTarget"
  colnames(beforeMatching)[colnames(beforeMatching) == "sumComparator"] <- "beforeMatchingSumComparator"
  colnames(beforeMatching)[colnames(beforeMatching) == "sd"] <- "beforeMatchingSd"
  colnames(afterMatching)[colnames(afterMatching) == "meanTarget"] <- "afterMatchingMeanTarget"
  colnames(afterMatching)[colnames(afterMatching) == "meanComparator"] <- "afterMatchingMeanComparator"
  colnames(afterMatching)[colnames(afterMatching) == "sumTarget"] <- "afterMatchingSumTarget"
  colnames(afterMatching)[colnames(afterMatching) == "sumComparator"] <- "afterMatchingSumComparator"
  colnames(afterMatching)[colnames(afterMatching) == "sd"] <- "afterMatchingSd"
  balance <- beforeMatching %>%
    full_join(afterMatching, by = "covariateId") %>%
    inner_join(collect(cohortMethodData$covariateRef), by = "covariateId") %>%
    mutate(beforeMatchingStdDiff = (.data$beforeMatchingMeanTarget - .data$beforeMatchingMeanComparator)/.data$beforeMatchingSd,
           afterMatchingStdDiff = (.data$afterMatchingMeanTarget - .data$afterMatchingMeanComparator)/.data$afterMatchingSd)

  balance$beforeMatchingStdDiff[balance$beforeMatchingSd == 0] <- 0
  balance$afterMatchingStdDiff[balance$beforeMatchingSd == 0] <- 0
  balance <- balance[order(-abs(balance$beforeMatchingStdDiff)), ]
  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste("Computing covariate balance took", signif(delta, 3), attr(delta, "units")))
  return(balance)
}

#' Create a scatterplot of the covariate balance
#'
#' @description
#' Create a scatterplot of the covariate balance, showing all variables with balance before and after
#' matching on the x and y axis respectively. Requires running `computeCovariateBalance` first.
#'
#' @return
#' A ggplot object. Use the [ggplot2::ggsave] function to save to file in a different
#' format.
#'
#' @param balance     A data frame created by the `computeCovariateBalance` funcion.
#' @param absolute    Should the absolute value of the difference be used?
#' @param threshold   Show a threshold value for after matching standardized difference.
#' @param title       The main title for the plot.
#' @param fileName    Name of the file where the plot should be saved, for example 'plot.png'. See the
#'                    function `ggsave` in the ggplot2 package for supported file formats.
#' @param beforeLabel Label for the x-axis.
#' @param afterLabel  Label for the y-axis.
#' @param showCovariateCountLabel  Show a label with the number of covariates included in the plot?
#' @param showMaxLabel Show a label with the maximum absolute standardized difference after matching/stratification?
#'
#' @export
plotCovariateBalanceScatterPlot <- function(balance,
                                            absolute = TRUE,
                                            threshold = 0,
                                            title = "Standardized difference of mean",
                                            fileName = NULL,
                                            beforeLabel = "Before matching",
                                            afterLabel = "After matching",
                                            showCovariateCountLabel = FALSE,
                                            showMaxLabel = FALSE) {
  beforeLabel <- as.character(beforeLabel)
  afterLabel <- as.character(afterLabel)
  if (absolute) {
    balance$beforeMatchingStdDiff <- abs(balance$beforeMatchingStdDiff)
    balance$afterMatchingStdDiff <- abs(balance$afterMatchingStdDiff)
  }
  limits <- c(min(c(balance$beforeMatchingStdDiff, balance$afterMatchingStdDiff), na.rm = TRUE),
              max(c(balance$beforeMatchingStdDiff, balance$afterMatchingStdDiff), na.rm = TRUE))
  plot <- ggplot2::ggplot(balance,
                          ggplot2::aes(x = .data$beforeMatchingStdDiff, y = .data$afterMatchingStdDiff)) +
    ggplot2::geom_point(color = rgb(0, 0, 0.8, alpha = 0.3), shape = 16) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::ggtitle(title) +
    ggplot2::scale_x_continuous(beforeLabel, limits = limits) +
    ggplot2::scale_y_continuous(afterLabel, limits = limits)
  if (threshold != 0) {
    plot <- plot + ggplot2::geom_hline(yintercept = c(threshold,
                                                      -threshold), alpha = 0.5, linetype = "dotted")
  }
  if (showCovariateCountLabel || showMaxLabel) {
    labels <- c()
    if (showCovariateCountLabel) {
      labels <- c(labels, sprintf("Number of covariates: %s", format(nrow(balance), big.mark = ",", scientific = FALSE)))
    }
    if (showMaxLabel) {
      labels <- c(labels, sprintf("%s max(absolute): %.2f", afterLabel, max(abs(balance$afterMatchingStdDiff), na.rm = TRUE)))
    }
    dummy <- data.frame(text = paste(labels, collapse = "\n"))
    plot <- plot + ggplot2::geom_label(x = limits[1] + 0.01, y = limits[2], hjust = "left", vjust = "top", alpha = 0.8, ggplot2::aes(label = text), data = dummy, size = 3.5)

  }
  if (!is.null(fileName)) {
    ggplot2::ggsave(fileName, plot, width = 4, height = 4, dpi = 400)
  }
  return(plot)
}

.truncRight <- function(x, n) {
  nc <- nchar(x)
  x[nc > (n - 3)] <- paste("...",
                           substr(x[nc > (n - 3)], nc[nc > (n - 3)] - n + 1, nc[nc > (n - 3)]),
                           sep = "")
  x
}

#' Plot variables with largest imbalance
#'
#' @description
#' Create a plot showing those variables having the largest imbalance before matching, and those
#' variables having the largest imbalance after matching. Requires running
#' `computeCovariateBalance` first.
#'
#' @return
#' A ggplot object. Use the [ggplot2::ggsave] function to save to file in a different
#' format.
#'
#' @param balance        A data frame created by the `computeCovariateBalance` funcion.
#' @param n              (Maximum) count of covariates to plot.
#' @param maxNameWidth   Covariate names longer than this number of characters are truncated to create
#'                       a nicer plot.
#' @param title          Optional: the main title for the plot.
#' @param fileName       Name of the file where the plot should be saved, for example 'plot.png'. See
#'                       the function `ggsave` in the ggplot2 package for supported file formats.
#' @param beforeLabel    Label for identifying data before matching / stratification / trimming.
#' @param afterLabel     Label for identifying data after matching / stratification / trimming.
#'
#' @export
plotCovariateBalanceOfTopVariables <- function(balance,
                                               n = 20,
                                               maxNameWidth = 100,
                                               title = NULL,
                                               fileName = NULL,
                                               beforeLabel = "before matching",
                                               afterLabel = "after matching") {
  n <- min(n, nrow(balance))
  beforeLabel <- as.character(beforeLabel)
  afterLabel <- as.character(afterLabel)
  topBefore <- balance[order(-abs(balance$beforeMatchingStdDiff)), ]
  topBefore <- topBefore[1:n, ]
  topBefore$facet <- paste("Top", n, beforeLabel)
  topAfter <- balance[order(-abs(balance$afterMatchingStdDiff)), ]
  topAfter <- topAfter[1:n, ]
  topAfter$facet <- paste("Top", n, afterLabel)
  filtered <- rbind(topBefore, topAfter)

  data <- tibble::tibble(covariateId = rep(filtered$covariateId, 2),
                         covariate = rep(filtered$covariateName, 2),
                         difference = c(filtered$beforeMatchingStdDiff, filtered$afterMatchingStdDiff),
                         group = rep(c(beforeLabel, afterLabel), each = nrow(filtered)),
                         facet = rep(filtered$facet, 2),
                         rowId = rep(nrow(filtered):1, 2))
  filtered$covariateName <- .truncRight(as.character(filtered$covariateName), maxNameWidth)
  data$facet <- factor(data$facet, levels = c(paste("Top", n, beforeLabel), paste("Top", n, afterLabel)))
  data$group <- factor(data$group, levels = c(beforeLabel, afterLabel))
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data$difference,
                                             y = .data$rowId,
                                             color = .data$group,
                                             group = .data$group,
                                             fill = .data$group,
                                             shape = .data$group)) +
    ggplot2::geom_point() +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                          rgb(0, 0, 0.8, alpha = 0.5))) +
    ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                           rgb(0, 0, 0.8, alpha = 0.5))) +
    ggplot2::scale_x_continuous("Standardized difference of mean") +
    ggplot2::scale_y_continuous(breaks = nrow(filtered):1, labels = filtered$covariateName) +
    ggplot2::facet_grid(facet ~ ., scales = "free", space = "free") +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 7),
                   axis.title.y = ggplot2::element_blank(),
                   legend.position = "top",
                   legend.direction = "vertical",
                   legend.title = ggplot2::element_blank())
  if (!is.null(title)) {
    plot <- plot + ggplot2::ggtitle(title)
  }
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 10, height = max(2 + n * 0.2, 5), dpi = 400)
  return(plot)
}

