# scipub

This package contains functions for summarizing data
    for scientific publication. This includes making a "Table 1"
    to summarize demographics across groups, correlation tables
    with significance indicated by stars, and extracting formatted
    statistical summarizes from simple tests for in-text notation.
    The package also includes functions for Winsorizing data based
    on a Z-statistic cutoff.
    
Functions:
    apastat - Format simple statistic test results for scientific publication
    correltable - Create correlation table (with stars for significance)  for scientific publication
    FullTable1 - Create Table1 of group summary with stats for scientific publication
    winsorZ_find - Identify outliers based on z-score cutoff that are Winsorized by the `winsorZ` function
    winsorZ - Winsorize outliers based on z-score cutoff to next most extreme non-outlier value
