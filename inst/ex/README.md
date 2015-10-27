## About the contents of this directory

This directory contains the two examples discussed in the paper, along
with the data and transcripts of R sessions. They are provided for
reference.

### Update on version 0.25.4

Version 0.25.1 of `distcomp` relied on imports from `httr` version
below 0.6.x. Since then, package `httr` has been updated to version
1.x using a new `curl` library and `distcomp` has now been updated to
account for them. The changes are pretty minor but the parameters for
the `httr::POST` requests that `distcomp` makes are different: the SSL
options are now named with underscores and the parameter values are
now not `logical` but `integer`, with `0L` for `FALSE` and `1L` for
`TRUE`. All our examples work as before (see the examples in the `doc`
directory). The transcripts here have been updated for version 0.25.4
of `distcomp` with session information along with some improvements.

## Table of Contents


|Name                       | Description                                                              |
|---------------------------|--------------------------------------------------------------------------|
|`README.Rmd`, `README.Rmd` |  This file plus generated Markdown                                       |
|                           |                                                                          |
|`Rprofile`                 |  Example Rprofile for use with distcomp for setting up workspace for     |
|                           |  running examples locally with `opencpu` R package                       |
|                           |                                                                          |
|`STCoxSetup.Rout`          |  R Transcript for defining computation, setting up worker sites, and     |
|                           |  master process using shiny apps.                                        |
|`STCoxDefn.rds`            |  The generated definition file for Stratified Cox Example                |
|`STCoxMaster.R`            |  Master R code produced by webapp; see transcript `STCoxSetup.Rout`      |
|`STCoxMaster.Rout`         |  R Transcript executing `STCoxMaster.R`                                  |
|                           |                                                                          |
|`SVDSetup.Rout`            |  R Transcript for defining computation, setting up worker sites, and     |
|                           |  master process using shiny apps.                                        |
|`SVDDefn.rds`              |  The generated definition file for SVD Example                           |
|`SVDMaster.R`              |  Master R code produced by webapp; see transcript `SVDSetup.Rout`        |
|`SVDMaster.Rout`           |  R Transcript executing `SVDMaster.R`                                    |
|                           |                                                                          |
|`svd-example.R`            |  Plain R code for svd example                                            |
|`svd-example.Rout`         |  R Transcript executing `svd-example.R` including all 5 singular values  |
|`svd-site1.csv`            |  Data for Site 1 in svd example                                          |
|`svd-site2.csv`            |  Data for Site 2 in svd example                                          |
|`svd-site3.csv`            |  Data for Site 3 in svd example                                          |
|                           |                                                                          |
|`uis-example.R`            |  Plain R code for uis example                                            |
|`uis-example.Rout`         |  R Transcript executing `uis-example.R`                                  |
|`uis-site1.csv`            |  Data for Site 1 in uis example                                          |
|`uis-site2.csv`            |  Data for Site 2 in uis example                                          |
|`uis.RDS`                  |  UIS data for comparing aggregated results                               |
