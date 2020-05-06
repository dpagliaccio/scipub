#' Create Table1 of group summary with stats for scientific publication
#'
#' The `FullTable1` function can be used to create a Table1 for
#' scientific publication. This is intended to summarize demographic
#' and other variables (`vars`) split by a grouping variable (`strata`)
#' from an input dataset (`data`).
#' Continuous variables will be summarized as mean (SD)
#' and tested across groups using t-test or anova (for 3+ level `strata`).
#' Categorical variables will be summarized as N (%)
#' and tested across groups as chi-squared.
#' Effect sizes for group differences will be calculated as Cohen's d,
#'  partial eta-squared, Odds Ratio, Cramer's V depending on the test.
#' Requires `tidyverse` and `stats` libraries.
#' @param data The input dataset (will be converted to tibble).
#' @param strata The grouping variable of interest (converted to factor),
#'  if NULL will make one column table.
#' @param vars A list of variables to summarize, e.g. c("Age","sex","WASI").
#' @param var_names An optional list to rename the variable colnames in the
#' output table, e.g. c("Age (years)","Sex","IQ"). Must match `vars` in length.
#' If not supplied, `vars` will be printed as is.
#' @param factor_vars An optional list of variables from `vars` to use
#' as class factor, e.g. c("sex"). Note that any character, factor, or
#' logical class variables will be summarized as categorical by default.
#' @param round_n The number of decimal places to round output to (default=2).
#' @param es_col Include a column for effect size
#' of group difference? (default=T).
#' @param p_col Include a column for p-value of group difference? (default=T).
#' @param stars Where to include stars indicating
#' significance of group differences.
#' Options: "col"=separate column (default), "name"= append to variable nam,
#' "stat"= append to group difference statistic, "none" for no stars.
#' @param html Format as html in viewer or not (default=F, print in console),
#'  needs library(htmlTable) installed.
#' @return Output Table 1
#' @import 	dplyr
#' @import 	purrr
#' @importFrom 	stats anova aov chisq.test complete.cases fisher.test sd setNames t.test
#' @import 	stringr
#' @import 	tibble
#' @import 	tidyr
#' @import 	tidyselect
#' @export
#' @examples
<<<<<<< HEAD
=======
<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> 67503dc3c4d50df205c17f314d15b4e747486d20
>>>>>>> b98dde24e700dc06f480dc9490ad086530a8450b
#'   FullTable1(data = diamonds[(diamonds$cut == "Ideal" | diamonds$cut == "Good") & (diamonds$color == "D" |   diamonds$color == "J"), ],
#'    vars = c("cut", "depth", "price"), strata = "color")
#'   FullTable1(data = diamonds[(diamonds$cut == "Ideal" | diamonds$cut == "Good") & (diamonds$color == "D" | diamonds$color == "G" |  diamonds$color == "J"), ],
#'    vars = c("cut", "depth", "price"), strata = "color", stars ="name", p_col=FALSE, html=TRUE)
#'   FullTable1(data = diamonds[(diamonds$cut == "Ideal" | diamonds$cut == "Good"), ],
#'    vars = c("cut", "depth", "price"), html=TRUE)
<<<<<<< HEAD
=======
<<<<<<< HEAD
=======
=======
#' diamonds %>%
#'   filter(cut == "Ideal" | cut == "Good") %>%
#'   filter(color == "D" | color == "J") %>%
#'   FullTable1(data = ., vars = c("cut", "depth", "price"), strata = "color")
#' diamonds %>%
#'   filter(cut == "Ideal" | cut == "Good") %>%
#'   filter(color == "D" | color == "G" | color == "J") %>%
#'   FullTable1(data = ., vars = c("cut", "depth", "price"), strata = "color", stars ="name", p_col=F, html=T)
#' diamonds %>%
#'   filter(cut == "Ideal" | cut == "Good") %>%
#'   FullTable1(data = ., vars = c("cut", "depth", "price"), html=T)
>>>>>>> d610a440547e813385cf39238680c43df9e3e862
>>>>>>> 67503dc3c4d50df205c17f314d15b4e747486d20
>>>>>>> b98dde24e700dc06f480dc9490ad086530a8450b

FullTable1 <- function(data, strata=NULL, vars = NULL,
                       var_names = vars, factor_vars = NULL,
                       round_n = 2, es_col = c(TRUE, FALSE), p_col = c(TRUE, FALSE),
                       stars = c("col", "name", "stat", "none"), html = c(FALSE, TRUE)) {

  es <- p <- Stat <- Variable <- sig <- NULL

  # set df to tibble
  data <- dplyr::as_tibble(data)

  # if null strata
  if (is.null(strata)) {
    es_col = FALSE
    p_col = FALSE
    stars = "none"
    strata <- "onecol"
    data$onecol <- "Sample"
    data$onecol <- as.factor(data$onecol)
  }




  # if vars is missing, use all except strata
  if (is.null(vars)) {
    vars <- names(data)[!names(data) %in% strata]
    var_names <- vars
  }

  # Check var_names
  if (length(var_names) != length(vars)) {
    stop("length of var_names does not match length of vars", call. = F)
  }

  # subset columns to relevant data
  data_edit <- data %>% dplyr::select(tidyselect::all_of(c(vars, strata)))
  # convert any listed factors to factor just in case
  data_edit <- data_edit %>% dplyr::mutate_at(.vars = c(factor_vars, strata), .funs = list(factor))

  # convert any character to factor & warn
  if (ncol(data_edit %>% dplyr::select_if(is.character)) > 0) {
    warning(paste0("Character class variables converted to factor: ", stringr::str_c(names(data_edit %>% dplyr::select_if(is.character)), sep = " ", collapse = ",")), call. = F)
    data_edit <- data_edit %>% dplyr::mutate_if(is.character, .funs = list(factor))
  }
  # convert any logical to factor & warn
  if (ncol(data_edit %>% dplyr::select_if(is.logical)) > 0) {
    warning(paste0("Logical class variables converted to factor: ", stringr::str_c(names(data_edit %>% dplyr::select_if(is.logical)), sep = " ", collapse = ",")), call. = F)
    data_edit <- data_edit %>% dplyr::mutate_if(is.logical, .funs = list(factor))
  }
  # convert any ordered to factor & warn
  if (ncol(data_edit %>% dplyr::select_if(is.ordered)) > 0) {
    warning(paste0("Ordered class variables converted to factor: ", stringr::str_c(names(data_edit %>% dplyr::select_if(is.ordered)), sep = " ", collapse = ",")), call. = F)
    data_edit <- data_edit %>% dplyr::mutate_if(is.ordered, factor, ordered = FALSE)
  }
  # convert any remaining variables with only 2 distinct options to factor & warn
  if (ncol(data_edit %>% dplyr::select_if(function(col) is.factor(col) == F & dplyr::n_distinct(col, na.rm = T) == 2)) > 0) {
    warning(paste0("Variables with only two distinct values converted to factor: ", stringr::str_c(names(data_edit %>% dplyr::select_if(function(col) is.factor(col) == F & dplyr::n_distinct(col, na.rm = T) == 2)), sep = " ", collapse = ",")), call. = F)
    data_edit <- data_edit %>% dplyr::mutate_if(function(col) is.factor(col) == F & dplyr::n_distinct(col, na.rm = T) == 2, factor)
  }

  # get list of factors
  factor_vars <- names(data_edit %>% dplyr::select_if(is.factor) %>% dplyr::select(-c(tidyselect::all_of(strata))))

  # drop any missing strata
  if (strata != "onecol") {
    if (sum(is.na(data[[strata]])) > 0) {
      warning(paste0("N=", sum(is.na(data[[strata]])), " missing/NA in grouping variable: ", strata), call. = F)
      data_edit <- data_edit %>% tidyr::drop_na(tidyselect::all_of(strata))
    }
  }
  # check if all one type of variable
  type <- dplyr::case_when(length(factor_vars) == 0 ~ "numeric", length(factor_vars) == length(vars) ~ "factor", TRUE ~ "mixed")





  # create sub-function
  # datafile = data_edit;groupvar = strata;outcome = "Age"
  grouptests <- function(datafile, groupvar, outcome, ...) {
    perc <- NULL
    y <- datafile[[outcome]]
    x <- datafile[[groupvar]]
    # set group names with N
    grplvl <- stringr::str_c(levels(datafile[[groupvar]]), " (N=", (datafile %>%
                                                                      dplyr::group_by_at(groupvar) %>%
                                                                      dplyr::select(tidyselect::all_of(groupvar)) %>%
                                                                      dplyr::tally())$n, ")", sep = "")


    tableout <- c("Variable", grplvl, "Stat", "p", "sig", "es") %>%
<<<<<<< HEAD
      purrr::map_dfc(stats::setNames, object = list(character())) %>%
      tibble::add_row()
=======
<<<<<<< HEAD
      purrr::map_dfc(stats::setNames, object = list(character())) %>%
      tibble::add_row()
=======
<<<<<<< HEAD
      purrr::map_dfc(stats::setNames, object = list(character())) %>%
      tibble::add_row()
=======
      purrr::map_dfc(setNames, object = list(character())) %>%
      tibble:::add_row()
>>>>>>> d610a440547e813385cf39238680c43df9e3e862
>>>>>>> 67503dc3c4d50df205c17f314d15b4e747486d20
>>>>>>> b98dde24e700dc06f480dc9490ad086530a8450b

    # IF NUMERIC
    if (is.numeric(y)) {
      tableout$Variable <- var_names[which(vars == outcome)]

      # get mean (SD) per group
      tableout[, grplvl] <- as.data.frame(t((datafile %>%
                                               dplyr::group_by_at(groupvar) %>%
                                               dplyr::select_at(outcome) %>%
                                               dplyr::summarise_all(list(mean = mean, sd = stats::sd), na.rm = TRUE) %>%
                                               dplyr::mutate_at(c("mean", "sd"), round, round_n) %>%
                                               tidyr::unite("col", mean, sd, sep = " (") %>%
                                               dplyr::mutate(col = stringr::str_c(col, ")")))[2]))

      # IF 2 LEVEL
      if (length(levels(x)) == 2) {
        testtype <- ifelse(type == "mixed", "t=", "")
        # calcualte t-test & p-value
        tableout$Stat <- paste0(testtype, format(round(-1 * stats::t.test(y ~ x)$statistic, round_n), nsmall = round_n), sep = "") # -1* to flip so t direction is g2>g1
        p <- stats::t.test(y ~ x)$p.value

        # calculate effect size - cohens d
        estype <- ifelse(type == "mixed", "d=", "")
        tableout$es <- paste0(estype, format(round((datafile %>%
                                                      dplyr::group_by_at(groupvar) %>%
                                                      dplyr::select_at(outcome) %>%
                                                      dplyr::summarise_all(list(mean = mean, sd = stats::sd), na.rm = TRUE) %>%
                                                      dplyr::mutate(d = (mean[2] - mean[1]) / (sqrt((sd[2]^2 + sd[1]^2) / 2))))[[1, "d"]], round_n), nsmall = round_n))
        # IF MORE THAN 2 LEVELS
      } else if (length(levels(x)) > 2) {
        testtype <- ifelse(type == "mixed", "F=", "")
        # calcualte anova & p-value
        tableout$Stat <- paste0(testtype, format(round(summary(stats::aov(y~x))[[1]][[4]][1], round_n), nsmall = round_n), sep = "")
        p <- summary(stats::aov(y~x))[[1]][[5]][1]

        # calculate effect size - cohens d
        estype <- ifelse(type == "mixed", paste0("\u03B7", "2="), "")
        tableout$es <- paste0(estype, format(round(((stats::anova(stats::aov(y~x))[1, 2] / sum(stats::anova(stats::aov(y~x))[,2]))), round_n),nsmall = round_n))
      }




      # IF FACTOR
    } else {

      if (groupvar != "onecol") {
        p <- stats::chisq.test(y, x)$p.value
        testtype <- ifelse(type == "mixed", paste0("\u03C7", "2="), "")
        tableout$Stat <- paste0(testtype, format(round(stats::chisq.test(y, x)$statistic, round_n), nsmall = round_n), sep = "")
      }

      # IF 2 LEVEL
      if (length(levels(y)) == 2) {
        lvl2 <- levels(y)[2]
        tableout$Variable <- paste0(var_names[which(vars == outcome)], " (", lvl2, ")")
        # get N (%)
        tableout[, grplvl] <- as.data.frame(t((datafile %>%
                                                 tidyr::drop_na(tidyselect::all_of(outcome)) %>%
                                                 dplyr::group_by_at(c(groupvar, outcome)) %>%
                                                 dplyr::select_at(outcome) %>%
                                                 tally() %>%
                                                 dplyr::ungroup() %>%
                                                 dplyr::group_by_at(c(groupvar)) %>%
                                                 dplyr::mutate(perc = 100 * n / sum(n)) %>%
                                                 dplyr::mutate_at(c("perc"), round, round_n) %>%
                                                 tidyr::unite("col", n, perc, sep = " (") %>%
                                                 dplyr::mutate(col = stringr::str_c(col, "%)")) %>%
                                                 dplyr::filter(base::get(outcome) == lvl2))$col))



        # calculate effect size - odds ratio
        if (length(levels(x)) == 2) {
          estype <- ifelse(type == "mixed", "OR=", "")
          tableout$es <- paste0(estype, format(round(stats::fisher.test(x, y)$estimate, round_n), nsmall = round_n))
        } else if (length(levels(x)) > 2) {
          # calculate effect size - cramer v
          estype <- ifelse(type == "mixed", "V=", "")
          tableout$es <- paste0(estype, format(round(sqrt((stats::chisq.test(y, x)$statistic) / (sum(stats::complete.cases(cbind(y, x))) * stats::chisq.test(y, x)$parameter)), round_n), nsmall = round_n))
        }

        # IF MORE THAN 2 LEVELS
      } else {
        ## TO DO - multi line summary n % # tidyr::unite("col",outcome, col, sep = " = ") %>%summarize(V3 = toString(col)) #nest
        tableout[2:(1 + length(levels(y))), grplvl] <- datafile %>%
          tidyr::drop_na(tidyselect::all_of(outcome)) %>%
          dplyr::group_by_at(c(groupvar, outcome)) %>%
          dplyr::select_at(outcome) %>%
          tally() %>%
          dplyr::ungroup() %>%
          dplyr::group_by_at(c(groupvar)) %>%
          dplyr::mutate(perc = 100 * n / sum(n)) %>%
          dplyr::mutate_at(c("perc"), round, round_n) %>%
          tidyr::unite("col", n, perc, sep = " (") %>%
          dplyr::mutate(col = stringr::str_c(col, "%)")) %>%
          tidyr::spread(groupvar,"col") %>%
          dplyr::select(-c(tidyselect::all_of(outcome)))

        # calculate effect size - cramer v
        estype <- ifelse(type == "mixed", "V=", "")
        tableout$es <- paste0(estype, format(round(sqrt((stats::chisq.test(y, x)$statistic) / (sum(stats::complete.cases(cbind(y, x))) * stats::chisq.test(y, x)$parameter)), round_n), nsmall = round_n))

      }
    }

    # set p for all test types
    if (groupvar != "onecol") {
      tableout$sig <- ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", "")))
      tableout$p <- ifelse(p < .001, "<.001",
                           ifelse(p < .01, sub(format(round(p, 3), nsmall = 3), pattern = "0.", replacement = "."),
                                  sub(format(round(p,2), nsmall = 2), pattern = "0.", replacement = ".")))
    }

    return(tableout)
  }






  # iterate on all variables
  finaltable <- do.call("rbind", lapply(vars, function(x) grouptests(datafile = data_edit, groupvar = strata, outcome = x)))




  # FINAL FORMATTING
  # remove es if not requested
  if (!es_col[1]) {
    finaltable <- finaltable %>% dplyr::select(-c(es))
  }
  # remove p if not requested
  if (!p_col[1]) {
    finaltable <- finaltable %>% dplyr::select(-c(p))
  }
  # remove p if not requested
  if (strata == "onecol") {
    finaltable <- finaltable %>% dplyr::select(-c(Stat))
  }

  if (stars[1] == "name") {
    finaltable <- finaltable %>% tidyr::unite(Variable, Variable, sig, sep = " ")
  } else if (stars[1] == "stat") {
    finaltable <- finaltable %>% tidyr::unite(Stat, Stat, sig, sep = " ")
  } else if (stars[1] == "none") {
    finaltable <- finaltable %>% dplyr::select(-c(sig))
  }

  # caption
  missing_n <- data_edit %>%
    dplyr::select(tidyselect::all_of(vars)) %>%
    purrr::set_names(var_names) %>%
    dplyr::mutate_all(is.na) %>%
    dplyr::summarise_all(sum) %>%
    dplyr::select_if(function(sum) sum > 0)
  missingness <- ifelse(ncol(missing_n) > 0,
                        paste0(stringr::str_c("N=", missing_n, " missing ", colnames(missing_n), ". "), collapse = "")
                        , "")


  caption <- paste0(
    "Note. ",
    ifelse(sum(is.na(data[[strata]])) > 0, paste0("N=", sum(is.na(data[[strata]]))
                                                  ," excluded for missing group variable. "), ""),
    missingness,
    ifelse(strata == "onecol","","*p<.05, **p<.01, ***p<.001"))

  # check if htmlTable installed
  if (html[1] == T & !requireNamespace("htmlTable", quietly = TRUE)) {
    warning("library(htmlTable) is needed for HTML format output, please install and try again")
    html <- FALSE
  }

  if (html[1] == T) {
    return(print(htmlTable::htmlTable(finaltable, useViewer=T, rnames = FALSE, caption=caption, pos.caption="bottom")))
  } else {
    return(list(table=noquote(as.data.frame(finaltable,row.names = NULL)), caption=caption))
  }

}
