#' Create correlation table (with stars for significance)
#'  for scientific publication
#'
#' The `correltable` function can be used to create correlation
#' table (with stars for significance) for scientific publication
#' This is intended to summarize correlations between (`vars`)
#'  from an input dataset (`data`).
#' Correlations are based on `stats::cor`, `use` and `method`
#'  follow from that function.
#' Stars indicate significance: `*p<.05, **p<.01, ***p<.001`
#' For formatting, variables can be renamed, numbers can be rounded,
#'  upper or lower triangle only can be selected (or whole matrix),
#'   and empty columns/rows can be dropped if using triangles.
#' For more compact columns, variable names can be numbered in the
#'  rows and column names will be corresponding numbers.
#' If only cross-correlation between two sets of variables is desired
#'  (no correlations within a set of variables), `vars2` and `var_names` can be used.
#' This function will drop any non-numeric variables by default.
#' Requires `tidyverse` and `stats` libraries.
#' @param data The input dataset.
#' @param vars A list of the names of variables to correlate,
#'  e.g. c("Age","height","WASI"), if NULL, all variables in `data` will be used.
#' @param var_names An optional list to rename the `vars` colnames
#'  in the output table, e.g. c("Age (years)","Height (inches)","IQ").
#'   Must match `vars` in length. If not supplied, `vars` will be printed as is.
#' @param vars2 If cross-correlation between two sets of variables
#'  is desired, add a second list of  variables to correlate with
#'   `vars`; Overrides `tri`, `cutempty`, and `colnum`.
#' @param var_names2 An optional list to rename the `vars2` colnames
#'  in the output table If not supplied, `vars2` will be printed as is.
#' @param method Type of correlation to calculate c("pearson", "spearman"),
#'  based on `stats::cor`, default = "pearson".
#' @param use  Use pairwise.complete.obs or restrict to complete cases
#'  c("pairwise", "complete"), based on `stats::cor`, default = "pairwise".
#' @param round_n The number of decimal places to round all output to (default=2).
#' @param tri Select output formatting c("upper", "lower","all");
#'  KEEP the upper triangle, lower triangle, or all values, default ="upper.
#' @param cutempty If keeping only upper/lower triangle with `tri`,
#'  cut empty row/column, default=FALSE.
#' @param colnum For more concise column names, number row names and
#'  just use corresponding numbers as column names, default=FALSE, if TRUE overrides cutempty.
#' @param html Format as html in viewer or not (default=F, print in console),
#'  needs library(htmlTable) installed.
#' @return Output Table 1
#' @import 	dplyr
#' @import 	forcats
#' @import 	purrr
#' @importFrom 	stats aov chisq.test complete.cases na.omit pf pt t.test
#' @import 	stringr
#' @import 	tidyselect
#' @export
#' @examples
#' correltable(data = diamonds)
#' correltable(data = diamonds, vars = c("carat", "depth", "price"), var_names = c("Carat", "Depth", "Price ($)"), tri = "lower", colnum = TRUE)
#' correltable(data = diamonds, vars = c("carat", "depth", "price"), var_names = c("Carat", "Depth", "Price ($)"), vars2 = c("x", "y", "z"))
#' correltable(
#'   data = diamonds[(diamonds$cut == "Ideal" | diamonds$cut == "Good") & (diamonds$color == "D" | diamonds$color == "G" | diamonds$color == "J"), ],
#'   vars = c("cut", "carat", "depth", "color", "price", "x", "y", "z"), var_names = c("cut", "Carat", "Depth", "Color", "Price ($)", "x", "y", "z"), cutempty = TRUE, html = TRUE
#' )
correltable <- function(data, vars = NULL, var_names = vars,
                        vars2 = NULL, var_names2 = vars2,
                        method = c("pearson", "spearman"), use = c("pairwise", "complete"), round_n = 2,
                        tri = c("upper", "lower", "all"), cutempty = c(FALSE, TRUE),
                        colnum = c(FALSE, TRUE), html = c(FALSE, TRUE)) {
  # remove duplicates
  var_names <- var_names[!duplicated(vars)]
  vars <- vars[!duplicated(vars)]

  var_names2 <- var_names2[!duplicated(vars2)]
  vars2 <- vars2[!duplicated(vars2)]


  # combine vars to start
  varsall <- c(vars, vars2)
  var_namesall <- c(var_names, var_names2)


  # if vars is missing,
  if (is.null(varsall)) {
    varsall <- names(data)
    var_namesall <- varsall
  }

  # Check var_names
  if (length(var_names) != length(vars)) {
    stop("length of var_names does not match length of vars", call. = F)
  }
  if (length(var_names2) != length(vars2)) {
    stop("length of var_names2 does not match length of vars2", call. = F)
  }




  # select data
  x <- data %>% dplyr::select(tidyselect::all_of(varsall))

  # if complete cases - remove all na
  if (stringr::str_detect(use[1], "complete")) {
    miss <- NROW(x[!complete.cases(x), ])
    x <- x[complete.cases(x), ]
  }

  # #check for non-numeric and convert with warning...
  if (ncol(x %>% dplyr::select_if(is.numeric)) != ncol(x)) {
    warning(paste0("Converting non-numeric columns to factor: ", stringr::str_c(colnames(x %>% dplyr::select_if(purrr::negate(is.numeric))), sep = " ", collapse = ",")), call. = F)

    numvars <- names(x %>% dplyr::select_if(is.numeric))
    factorvars <- names(x %>% dplyr::select_if(purrr::negate(is.numeric)))

    orig <- x %>%
      dplyr::mutate_if(purrr::negate(is.numeric), factor, ordered = FALSE) %>%
      dplyr::mutate_if(purrr::negate(is.numeric), forcats::fct_relevel)
    f <- x %>%
      dplyr::select_if(purrr::negate(is.numeric)) %>%
      dplyr::mutate_if(purrr::negate(is.numeric), factor, ordered = FALSE)

      factortwo <- names(f %>% dplyr::select_if(function(col) dplyr::n_distinct(col) == 2))
      factormore <- names(f %>% dplyr::select_if(function(col) dplyr::n_distinct(col) > 2))

      x[, factorvars] <- 1

      # var_namesall <- var_namesall[match(colnames(x %>% select_if(is.numeric)), varsall)]
      # varsall <- varsall[match(colnames(x %>% select_if(is.numeric)), varsall)]
      # x <- x %>% select_if(is.numeric)
    } else {
      numvars <- varsall
      factorvars <- NULL
    }



    # correlate numeric
    r <- suppressWarnings(stats::cor(x, use = use[1], method = method[1]))
    # get n
    n <- t(!is.na(x)) %*% (!is.na(x))
    # calc t-stat
    t <- (r * sqrt(n - 2)) / sqrt(1 - r^2)
    # calc p-value
    p <- -2 * expm1(pt(abs(t), (n - 2), log.p = TRUE))
    p[p > 1] <- 1


    # round, format, and add stars
    rmat <- matrix(paste(sub(x = format(round(r, round_n), round_n), pattern = "0.", replacement = "."),
      ifelse(p < .001,
        "***",
        ifelse(p < .01,
          "**",
          ifelse(p < .05,
            "*",
            ""
          )
        )
      ),
      sep = ""
    ),
    nrow = ncol(x), ncol = ncol(x)
    )

    #  if factor variables exist do t/F/chi
    if (!is.null(factorvars)) {

      # merge from rmat above
      mergemat <- as.data.frame(rmat, row.names = varsall, stringsAsFactors = FALSE)
      colnames(mergemat) <- varsall


      # if 2 level factors - do t-test
      if (!is.null(factortwo) & !is.null(numvars)) {
        tmpt <- mapply(function(n, f) paste0("t=", format(round(stats::t.test(get(n) ~ get(f), data = orig, na.action = na.omit)$statistic, round_n), round_n)), rep(numvars, length(factortwo)), rep(factortwo, length(numvars)))
        tmpp <- mapply(function(n, f) stats::t.test(get(n) ~ get(f), data = orig)$p.value, rep(numvars, length(factortwo)), rep(factortwo, length(numvars)))
        tmpt <- mapply(function(n, f) paste0("t=", format(round(t.test(get(n) ~ get(f), data = orig, na.action = na.omit)$statistic, round_n), round_n)), rep(numvars, length(factortwo)), rep(factortwo, length(numvars)))
        tmpp <- mapply(function(n, f) t.test(get(n) ~ get(f), data = orig)$p.value, rep(numvars, length(factortwo)), rep(factortwo, length(numvars)))
        tmat <- matrix(paste(tmpt, ifelse(tmpp < .001,
          "***",
          ifelse(tmpp < .01,
            "**",
            ifelse(tmpp < .05,
              "*",
              ""
            )
          )
        ), sep = ""),
        ncol = length(numvars), nrow = length(factortwo)
        )
        mergetmat <- as.data.frame(tmat, row.names = factortwo, stringsAsFactors = FALSE)
        colnames(mergetmat) <- numvars

        mergemat[factortwo, numvars] <- mergetmat[factortwo, numvars]
        mergemat[numvars, factortwo] <- t(mergetmat[factortwo, numvars])
      }



      # if 3+ level factors - do aov
      if (!is.null(factormore) & !is.null(numvars)) {
        fmat <- mapply(function(n, f) paste0("F=", format(round(summary(stats::aov(get(n) ~ get(f), data = orig, na.action = na.omit))[[1]]$"F value"[1], round_n), round_n)), rep(numvars, length(factormore)), rep(factormore, length(numvars)))
        fmatp <- mapply(function(n, f) summary(stats::aov(get(n) ~ get(f), data = orig))[[1]]$"Pr(>F)"[1], rep(numvars, length(factormore)), rep(factormore, length(numvars)))
        fmatp <- mapply(function(n, f) summary(aov(get(n) ~ get(f), data = orig))[[1]]$"Pr(>F)"[1], rep(numvars, length(factormore)), rep(factormore, length(numvars)))
        fmat <- matrix(paste(fmat, ifelse(fmatp < .001,
          "***",
          ifelse(fmatp < .01,
            "**",
            ifelse(fmatp < .05,
              "*",
              ""
            )
          )
        ), sep = ""),
        ncol = length(numvars), nrow = length(factormore)
        )
        mergefmat <- as.data.frame(fmat, row.names = factormore, stringsAsFactors = FALSE)
        colnames(mergefmat) <- numvars

        mergemat[factormore, numvars] <- mergefmat[factormore, numvars]
        mergemat[numvars, factormore] <- t(mergefmat[factormore, numvars])
      }


      # if more than one factor var, test chi2
      if (length(factorvars) > 1) {
        chix <- mapply(function(n1, n2) paste0("\u03C7", "2=", format(round(stats::chisq.test(orig[[n1]], orig[[n2]])$statistic, round_n), round_n)), factorvars, rev(factorvars))
        chip <- mapply(function(n1, n2) stats::chisq.test(orig[[n1]], orig[[n2]])$p.value, factorvars, rev(factorvars))
        chix <- mapply(function(n1, n2) paste0("\u03C7", "2=", format(round(chisq.test(orig[[n1]], orig[[n2]])$statistic, round_n), round_n)), factorvars, rev(factorvars))
        chip <- mapply(function(n1, n2) chisq.test(orig[[n1]], orig[[n2]])$p.value, factorvars, rev(factorvars))
        chix <- matrix(paste(chix, ifelse(chip < .001,
          "***",
          ifelse(chip < .01,
            "**",
            ifelse(chip < .05,
              "*",
              ""
            )
          )
        ), sep = ""),
        ncol = length(factorvars), nrow = length(factorvars)
        )


        mergechi <- as.data.frame(chix, row.names = factorvars, stringsAsFactors = FALSE)
        colnames(mergechi) <- factorvars

        mergemat[factorvars, rev(factorvars)] <- mergechi
      }


      # convert back to matrix
      rmat <- as.matrix(mergemat)
    }




    # remove diagonal
    diag(rmat) <- "-"



    # if vars2 supplied - override other...
    if (!is.null(vars2)) {
      colnum <- FALSE
      cutempty <- FALSE
      tri <- "all"
    }





    # rename
    if (colnum[1]) {
      rownames(rmat) <- paste(seq_len(length(varsall)), var_namesall, sep = ". ")
      colnames(rmat) <- seq_len(length(varsall))
    } else {
      rownames(rmat) <- var_namesall
      colnames(rmat) <- var_namesall
    }


    ## pick upper/lower triangle of correlation matrix to keep
    if (tri[1] == "upper") {
      rmat[lower.tri(rmat, diag = TRUE)] <- ""
      # & !colnum[1]
      if (cutempty[1]) {
        rmat <- rmat[, -1] # rmat <- rmat[-ncol(x),]
      }
    }
    if (tri[1] == "lower") {
      rmat[upper.tri(rmat, diag = TRUE)] <- ""
      if (cutempty[1]) {
        rmat <- rmat[-ncol(x), ]
      }
    }
    # if vars2 supplied - cut to cross-correl
    if (!is.null(vars2)) {
      rmat <- rmat[seq_len(length(vars)), (length(vars) + 1):length(varsall)]
    }



    missing_n <- data %>%
      dplyr::select(tidyselect::all_of(varsall)) %>%
      purrr::set_names(var_namesall) %>%
      dplyr::mutate_all(is.na) %>%
      dplyr::summarise_all(sum) %>%
      dplyr::select_if(function(sum) sum > 0)
    missingness <- ifelse(ncol(missing_n) > 0,
      paste0(stringr::str_c("N=", missing_n, " missing ", colnames(missing_n), ". "), collapse = ""),
      ""
    )


    caption <- paste0(
      "Note. This table presents ", stringr::str_to_title(method[1]), " correlation coefficients with ",
      ifelse(stringr::str_detect(use[1], "complete"),
        paste0("list-wise deletion (N=", NROW(x), ifelse(miss > 0, paste0(", missing ", miss, " cases"), ""), ")"),
        paste0("pairwise deletion. ", missingness)
      ),
      ifelse(is.null(factorvars),
        "",
        paste0(" Group differences for continuous and categorical variables are indicated by t-statistic/ANOVA F and chi-squared, respectively.")
      ),
      " *p<.05, **p<.01, ***p<.001"
    )




    # check if htmlTable installed
    if (html[1] == TRUE & !requireNamespace("htmlTable", quietly = TRUE)) {
      warning("library(htmlTable) is needed for HTML format output, please install and try again")
      html <- FALSE
    }

    if (html[1] == TRUE) {
      return(print(htmlTable::htmlTable(rmat,
        useViewer = TRUE, caption = caption, pos.caption = "bottom"
      )))
    } else {
      return(list(table = noquote(rmat), caption = caption))
    }
  }

