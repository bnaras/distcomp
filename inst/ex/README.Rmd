## About the contents of this directory

This directory contains the two examples discussed in the paper, along
with the data and transcripts of R sessions. They are provided for
reference.

### Update on version 0.25.2

Version 0.25.1 of `distcomp` relied on imports from `httr` version
below 0.6.x. Since then, package `httr` has been updated to version
1.x using a new `curl` library and `distcomp` has now been updated to
account for them. The changes are pretty minor but the parameters for
the `httr::POST` requests that `distcomp` makes are different: the SSL
options are now named with underscores and the parameter values are
now not `logical` but `integer`, with `0L` for `FALSE` and `1L` for
`TRUE`. All our examples work as before (see the examples in the `doc`
directory), but the transcripts here are still from version 0.25.1 of
`distcomp`. In version 0.26.x of `distcomp`, we will provide updated
transcripts (with session information) along with some new
improvements.

## Table of Contents


|Name                      | Description                                                              |
|--------------------------|------------------------------------------------------------------------- |
|`README.md`               |  This file                                                               |
|                          |                                                                          |
|`STCoxDefn.Rout`          |  R Transcript that produced `STCoxDefn.rds`                              |
|`STCoxDefn.rds`           |  The definition file for Stratified Cox Example                          |
|`STCoxMasterSetup.Rout`   |  R Transcript that produced `STCoxTest.R`                                |
|`STCoxSite1Setup.Rout`    |  R Transcript to set up Site 1 for STCoxTest                             |
|`STCoxSite2Setup.Rout`    |  R Transcript to set up Site 2 for STCoxTest                             |
|`STCoxTest.R`             |  Master R code produced by webapp; see transcript `STCoxMasterSetup.Rout`|
|`STCoxTest.Rout`          |  R Transcript executing `STCoxTest.R`                                    |
|`                         |                                                                          |
|`SVDDefn.Rout`            |  R Transcript that produced `SVDDefn.rds`                                |
|`SVDDefn.rds`             |  The definition file for SVD Example                                     |
|`SVDMasterSetup.Rout`     |  R Transcript that produced `SVDTest.R`                                  |
|`SVDSite1Setup.Rout`      |  R Transcript to set up Site 1 for SVDTest                               |
|`SVDSite2Setup.Rout`      |  R Transcript to set up Site 2 for SVDTest                               |
|`SVDSite3Setup.Rout`      |  R Transcript to set up Site 3 for SVDTest                               |
|`SVDTest.R`               |  Master R code produced by webapp; see transcript `STCoxMasterSetup.Rout`|
|`SVDTest.Rout`            |  R Transcript executing `SVDTest.R`                                      |
|`                         |                                                                          |
|`svd-example.R`           |  Manually created svd example                                            |
|`svd-example.Rout`        |  R Transcript executing `svd-example.R`                                  |
|`svd-site1.csv`           |  Data for Site 1 in svd example                                          |
|`svd-site2.csv`           |  Data for Site 2 in svd example                                          |
|`svd-site3.csv`           |  Data for Site 3 in svd example                                          |
|`                         |                                                                          |
|`uis-example.R`           |  Manually created uis example                                            |
|`uis-example.Rout`        |  R Transcript executing `uis-example.R`                                  |
|`uis-site1.csv`           |  Data for Site 1 in uis example                                          |
|`uis-site2.csv`           |  Data for Site 2 in uis example                                          |
|`uis.RDS`                 |  UIS data for comparing aggregated results                               |
