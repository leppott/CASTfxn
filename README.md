README-CASTfxn
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

    #> Last Update: 2020-09-09 09:56:24

Suite of functions for the Causal Assessment Screening Tool (CAST).
Includes Shiny app.

## Badges

[![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://GitHub.com/leppott/CASTfxn/graphs/commit-activity)
[![GitHub
license](https://img.shields.io/github/license/leppott/CASTfxn.svg)](https://github.com/leppott/CASTfxn/blob/master/LICENSE)
[![Travis-CI Build
Status](https://travis-ci.org/leppott/CASTfxn.svg?branch=master)](https://travis-ci.org/leppott/CASTfxn)
[![GitHub
issues](https://img.shields.io/github/issues/leppott/CASTfxn.svg)](https://GitHub.com/leppott/CASTfxn/issues/)

[![GitHub
release](https://img.shields.io/github/release/leppott/CASTfxn.svg)](https://GitHub.com/leppott/CASTfxn/releases/)
[![Github all
releases](https://img.shields.io/github/downloads/leppott/CASTfxn/total.svg)](https://GitHub.com/leppott/CASTfxn/releases/)

## Installation

Requires the use of devtools (or another package) to install from
GitHub.

If using devtools need to add an extra line of code for R v3.6.
<https://github.com/r-lib/devtools/issues/1939>

``` r
Sys.setenv("TAR" = "internal")
```

Vignettes are also not installed by default. The additional parameters
in install\_github are used to ensure the install happens if there is an
existing install and to install the vignettes.

``` r
devtools::install_github("leppott/CASTfxn", force=TRUE, build_vignettes=TRUE)
```

## Purpose

Functions to aid the data analysis and drive the functionality of the
Causal Assessment Screening Tool.

## Status

In development.

## Usage

By those using the CAST and familiar with causal assessment.

## Documentation

Vignette and install guide are planned for the future.

## Help

Functions in the package have a help file with extra documentation.
There is also a vignette with descriptions and examples of all functions
in the `CASTfxn` library.

``` r
# To get help on a function
# library(CASTfxn) # the library must be loaded before accessing help
?CASTfxn
```

To see all available functions in the package use the command below.

``` r
# To get index of help on all functions
# library(CASTfxn) # the library must be loaded before accessing help
help(package="CASTfxn")
```
