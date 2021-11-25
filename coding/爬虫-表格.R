library(rvest)

births <- read_html("https://www.ssa.gov/oact/babynames/numberUSbirths.html")
html_table(html_nodes(births, "table")[[2]])