pkgname <- "scipub"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('scipub')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("FullTable1")
### * FullTable1

flush(stderr()); flush(stdout())

### Name: FullTable1
### Title: Create Table1 of group summary with stats for scientific
###   publication
### Aliases: FullTable1

### ** Examples

FullTable1(
  data = psydat,
  vars = c("Age", "Height", "depressT"), strata = "Sex"
)
FullTable1(
  data = psydat,
  vars = c("Age", "Height", "depressT"), strata = "Sex"
)
FullTable1(
  data = psydat, vars = c("Age", "Sex", "Height", "depressT"),
  var_names = c("Age (months)", "Sex", "Height (inches)", "Depression T"),
  strata = "Income", stars = "name", p_col = FALSE
)



cleanEx()
nameEx("apastat")
### * apastat

flush(stderr()); flush(stdout())

### Name: apastat
### Title: Format simple statistic test results for scientific publication
### Aliases: apastat

### ** Examples

apastat(stats::cor.test(psydat$Age, psydat$Height))
apastat(stats::t.test(Height ~ Sex, data = psydat))
apastat(stats::lm(data = psydat, Height ~ Age + Sex))
apastat(stats::lm(data = psydat, Height ~ Age + Sex), var = "Age")




cleanEx()
nameEx("correltable")
### * correltable

flush(stderr()); flush(stdout())

### Name: correltable
### Title: Create correlation table (with stars for significance) for
###   scientific publication
### Aliases: correltable

### ** Examples

correltable(data = psydat)
correltable(
  data = psydat, vars = c("Age", "Height", "iq"),
  tri = "lower", html = TRUE
)
correltable(
  data = psydat, vars = c("Age", "Height", "iq"),
  var_names = c("Age (months)", "Height (inches)", "IQ"),
  tri = "upper", colnum = TRUE, html = TRUE
)
correltable(
  data = psydat, vars = c("Age", "Height", "iq"),
  var_names = c("Age (months)", "Height (inches)", "IQ"),
  vars2 = c("depressT", "anxT"),
  var_names2 = c("Depression T", "Anxiety T"), html = TRUE
)



cleanEx()
nameEx("winsorZ")
### * winsorZ

flush(stderr()); flush(stdout())

### Name: winsorZ
### Title: Winsorize outliers based on z-score cutoff to next most extreme
###   non-outlier value
### Aliases: winsorZ

### ** Examples

winsorZ(psydat$iq)
## Not run: 
##D psydat %>%
##D   dplyr::select(c(iq, anxT)) %>%
##D   map(winsorZ)
##D psydat %>% mutate_at(c("iq", "anxT"), list(~ winsorZ(.)))
##D psydat %>% mutate_if(is.double, list(~ winsorZ(.)))
## End(Not run)




cleanEx()
nameEx("winsorZ_find")
### * winsorZ_find

flush(stderr()); flush(stdout())

### Name: winsorZ_find
### Title: Identify outliers based on z-score cutoff that are Winsorized by
###   the 'winsorZ' function
### Aliases: winsorZ_find

### ** Examples

summary(winsorZ_find(psydat$iq))
## Not run: 
##D psydat %>% mutate_at(c("iq", "anxT"), list(out = ~ winsorZ_find(.)))
## End(Not run)




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
