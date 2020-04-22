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
#' For more compact coumns, variable names can be numbered in the
#'  rows and column names will be corresponding numbers.
#' If only cross-correlation between two sets of variables is desired
#'  (no correlations within a set of variables), `vars2` and `var_names` can be used.
#' This function will drop any non-numeric variables by default.
#' Requires `tidyverse` and `stats` libraries.
#' @param data The input dataset.
#' @param vars A list of the names of variables to correlate,
#'  e.g. c("Age","hght","WASI"), if NULL, all variables in `data` will be used.
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
#'  KEEP the upper triangle, lower triange, or all values, defualt ="upper.
#' @param cutempty If keeping only upper/lower triangle with `tri`,
#'  cut empty row/column, default=FALSE.
#' @param colnum For more concise column names, number row names and
#'  just use corresponding numbers as column names, default=FALSE, if TRUE overrides cutempty.
#' @return Output Table 1
#' @import tidyverse
#' @import stats
#' @export
#' @examples
#' correltable(data=diamonds)
#' correltable(data=diamonds, vars= c("carat","depth","price"),var_names= c("Carat","Depth","Price ($)"),tri="lower", colnum=T)
#' correltable(data=diamonds, vars= c("carat","depth","price"),var_names= c("Carat","Depth","Price ($)"),vars2= c("x","y","z"))
#' write.table(correltable(data=diamonds),quote = F,sep = "\t")
#' write.csv(correltable(data=diamonds))

#add somoething to handle factor as t-test/chi?! temp replace with zero to make cor run?
#output formatting#result=c("none", "html", "latex")

#correltable(data=DTI_merge_clean_winsor, vars=c("DTI_meanFD_good","DTI_meanFA_good","DTI_SNR_good","DTI_SNR_good"))
#data=DTI_merge_clean_winsor; vars=c("DTI_meanFD_good","DTI_meanFA_good","DTI_SNR_good"); var_names = vars;
#vars2=NULL; var_names2=vars2;method=c("pearson", "spearman"); use=c("pairwise","complete");round_n=2;tri=c("upper", "lower","all"); cutempty= c(FALSE,TRUE); colnum= c(FALSE,TRUE)

correltable <- function(data, vars=NULL, var_names=vars,
                       vars2=NULL, var_names2=vars2,
                       method=c("pearson", "spearman"), use=c("pairwise", "complete"), round_n=2,
                       tri=c("upper", "lower", "all"), cutempty= c(FALSE, TRUE), colnum= c(FALSE, TRUE)) {
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




  #select data
  x <- data %>% dplyr::select(all_of(varsall))

  #check for non-numeric and remove with warning...
  if (ncol(x %>% select_if(is.numeric)) != ncol(x)) {
    warning(paste0("Removing non-numeric columns: ", stringr::str_c(colnames(x %>% select_if(negate(is.numeric))), sep = " ", collapse = ",")), call. = F)
  var_namesall <- var_namesall[match(colnames(x %>% select_if(is.numeric)), varsall)]
  varsall <- varsall[match(colnames(x %>% select_if(is.numeric)), varsall)]
  x <- x %>% select_if(is.numeric)
  }

  #if complete cases - remove all na
  if (str_detect(use[1],"complete")) {
    miss <- NROW(x[!complete.cases(x),])
    x <- x[complete.cases(x),]
  }


  #correlate
  r <- stats::cor(x, use = use[1], method = method[1])
  #get n
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
                                    ""))), sep = ""),
         nrow = ncol(x), ncol = ncol(x))
  # remove diagonal
  diag(rmat) <- "-"



  # if vars2 supplied - override other...
  if (!is.null(vars2)) {
    colnum <- FALSE
    cutempty <- FALSE
    tri <- "all"
  }



  #rename
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
    #& !colnum[1]
    if (cutempty[1]) {
      rmat <- rmat[, -1] #rmat <- rmat[-ncol(x),]
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
    dplyr::select(all_of(varsall)) %>%
    purrr::set_names(var_namesall) %>%
    dplyr::mutate_all(is.na) %>%
    dplyr::summarise_all(sum) %>%
    dplyr::select_if(function(sum) sum > 0)
  missingness <- ifelse(ncol(missing_n) > 0,
                         paste0(missing_n %>%
                                  stringr::str_c("N=", ., " missing ", colnames(.), ". "), collapse = "")
                         , "")


  caption <- paste0(
    "Note. This table presents ", stringr::str_to_title(method[1]), " correlation coefficients with ",
    ifelse(str_detect(use[1], "complete"),
           paste0("list-wise deletion (N=", NROW(x), ifelse(miss > 0, paste0(", missing ", miss, " cases"), ""), ")"),
           paste0("pairwise deletion. ", missingness)),
    " *p<.05, **p<.01, ***p<.001")


  return(list(noquote(rmat), caption))
}
