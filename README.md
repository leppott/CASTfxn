README-CASTfxn
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

    #> Last Update: 2019-05-16 14:34:39

Suite of functions for the Causal Assessment Screening Tool
(CAST).

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

If using devtools need to add an extra line of code (see below).
<https://github.com/r-lib/devtools/issues/1939>

Vignettes are also not installed by default. The additional parameters
in install\_github are used to install the vignettes.

``` r
Sys.setenv("TAR" = "internal")
devtools::install_github("leppott/CASTfnx")
```

## Purpose

Functions to aid the data analysis and drive the functionality of the
Causal Assessment Screening Tool.

## Status

In development.

## Usage

By those using the CAST.

## Documentation

Vignette and install guide are planned for the future.

## Help

Every function has a help file with a working example. There is also a
vignette with descriptions and examples of all functions in the
`CASTfxn` library.

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
