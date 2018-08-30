library(shiny)
library(shinydashboard)
library(distcomp)
library(tidyverse)
library(gridExtra)
library(bibtex)
library(DT)

paper_citation <- bibtex::read.bib(system.file("extdata", "distcomp.bib", package = "distcomp"))

intro_line_1 <- 'An data schema application for the tools described in '
intro_line_2 <- a(href = sprintf("https://doi.org/%s", paper_citation$doi),
                  sprintf("%s et. al., Journal of Statistical Software %s(%s) %s.",
                          paper_citation$author[1],
                          paper_citation$volume,
                          paper_citation$number,
                          paper_citation$year))

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
}

cols <- gg_color_hue(3L)

