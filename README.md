
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bitfield <a href='https://github.com/luckinet/bitfield/'><img src='man/figures/logo.svg' align="right" height="200" /></a>

<!-- badges: start -->
<!-- [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/)](https://cran.r-project.org/package=) -->
<!-- [![DOI](https://zenodo.org/badge/DOI/)](https://doi.org/) -->

[![R-CMD-check](https://github.com/luckinet/bitfield/workflows/R-CMD-check/badge.svg)](https://github.com/luckinet/bitfield/actions)
[![codecov](https://codecov.io/gh/luckinet/bitfield/branch/master/graph/badge.svg?token=hjppymcGr3)](https://codecov.io/gh/luckinet/bitfield)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- [![](http://cranlogs.r-pkg.org/badges/grand-total/)](https://cran.r-project.org/package=) -->
<!-- badges: end -->

## Overview

This package is designed to build sequences of bits for any setting
where large amounts of non-complex data have to be stored in an
efficient way. This can also be useful when documenting the metadata of
any tabular dataset by collecting information throughout the dataset
creation process. The resulting data structure is referred to as a “[bit
field](https://en.wikipedia.org/wiki/Bit_field)”, which can be stored as
a sequence of 0s and 1s, or as an integer, reducing the size of the
contained information drastically. This is commonly used in MODIS
dataproducts to document layer quality.

Think of a bit as a switch representing off and on states. A combination
of a pair of bits can store four states, and n bits can accommodate 2^n
states. In R, integers are typically 32-bit values, allowing a single
integer to store 32 switches (called `flags` here) and 2^32 states.
These states could be the outcomes of functions returning boolean values
(each using one bit) or functions returning a small set of cases (using
the corresponding number of bits).

In essence, `bitfield` allows you to capture a diverse range of
information into a single value, like a column in a table or a raster
layer accompanying a modelled gridded dataset. This is beneficial not
only for reporting quality metrics, provenance, or other metadata but
also for simplifying the reuse of complex ancillary data in (semi)
automated script-based workflows.

## Installation

Install the official version from CRAN:

``` r
# install.packages("bitfield")
```

Install the latest development version from github:

``` r
devtools::install_github("luckinet/bitfield")
```

## Examples

``` r
library(bitfield)

library(dplyr, warn.conflicts = FALSE); library(CoordinateCleaner); library(stringr)
```

Let’s first build an example dataset

``` r
input <- tibble(x = sample(seq(23.3, 28.1, 0.1), 10),
                y = sample(seq(57.5, 59.6, 0.1), 10),
                commodity = rep(c("soybean", "maize"), 5),
                yield = rnorm(10, mean = 10, sd = 2),
                year = rep(2021, 10))

validComm <- c("soybean", "maize")
```

And make it have some unordinary values

``` r
input$x[5] <- 259
input$x[9] <- 0
# input$x[10] <- "23.546"
input$y[10] <- NA_real_
input$y[9] <- 0
input$commodity[c(3, 5)] <- c(NA_character_, "honey")
input$year[c(2:3)] <- c(NA, "2021r")

kable(input)
```

|     x |    y | commodity |     yield | year  |
|------:|-----:|:----------|----------:|:------|
|  23.9 | 58.7 | soybean   | 13.433170 | 2021  |
|  23.7 | 59.5 | maize     |  7.702426 | NA    |
|  24.5 | 57.6 | NA        | 11.300058 | 2021r |
|  24.0 | 58.3 | maize     |  8.922000 | 2021  |
| 259.0 | 57.7 | honey     |  9.911468 | 2021  |
|  24.7 | 58.2 | maize     | 10.292415 | 2021  |
|  26.8 | 58.9 | soybean   |  7.211000 | 2021  |
|  26.4 | 59.2 | maize     |  7.509846 | 2021  |
|   0.0 |  0.0 | soybean   |  6.627607 | 2021  |
|  23.5 |   NA | maize     | 10.584207 | 2021  |

The first step is in creating what is called registry in `bitfield`.
This registry captures all the information required to build the
bitfield

``` r
newRegistry <- bf_create(width = 12, length = dim(input)[1])
```

1.  The `width =` specifies how many bits are in the registry.
2.  The `lenght =` specifies how long the output table is. This is
    usually taken from an input.
3.  The `name =` specifies the label of the registry, which becomes very
    important when publishing, because registry and output table are
    stored in different files and it must be possible to unambiguously
    associate them to one another.

Then, individual bit flags need to be grown by specifying a mapping
function and which position of the bitfield should be modified. To help
with growing bits, various aspects are important to keep in mind

1.  if your mapping function returns a boolean value, the bit flags will
    be `FALSE == 0` and `TRUE == 1`.
2.  if your mapping function returns cases, they will be assigned a
    sequence of numbers that are encoded by their respective binary
    representation, i.e. if there are 3 cases (which takes up 2 bits),
    the bit flags will be `case 1 = 00`, `case 2 == 01` and
    `case 3 == 10`, and so on.

``` r
newRegistry <- newRegistry %>%
  # tests for coordinates ...
  bf_grow(flags = bf_na(x = input, test = "x"),
          pos = 1, registry = .) %>%
  bf_grow(flags =  bf_range(x = input, test = "x", min = -180, max = 180),
          pos = 2, registry = .) %>%
  bf_grow(flags = bf_decimals(x = input, test = "x"),
          pos = 10:11, registry = .) %>%

  # ... or override NA test
  bf_grow(flags = bf_range(x = input, test = "y", min = -90, max = 90),
          pos = 3, na_val = FALSE, registry = .)  %>%

  # test for matches with an external vector
  bf_grow(flags = bf_match(x = input, test = "commodity", against = validComm),
          pos = 4, na_val = FALSE, registry = .) %>%
  
  # define cases
  bf_grow(flags = bf_case(x = input, exclusive = FALSE, 
                          high = yield > 11, 
                          medium = yield < 11 & yield > 9, 
                          small = yield < 9), 
          pos = 8:9, registry = .)
```

It is also possible to use other functions that return flags, where it
is required to provide a name and a concise yet expressive description,
which is otherwise automatically provided by the `bf_*` function. Then
you need to keep in mind:

3.  chose name and description so that they reflect the outcome of the
    mapping function. If the function tests whether a value is `NA` and
    returns `TRUE` if the value is `NA`, the name and description should
    indicate that the bit flag is `TRUE == 1` when an `NA` value has
    been found.
4.  A concise rule to name flags should follow the same rule used by the
    `bf_*` functions, where the functional aspect is followed by the
    variable that is tested, for example `distinct_x_y` when columns `x`
    and `y` shall have distinct values.

``` r
newRegistry <- newRegistry %>%
  # use external functions, such as from CoordinateCleaner ...
  bf_grow(flags = cc_equ(x = input, lon = "x", lat = "y", value = "flagged"), 
          name = "distinct_x_y", desc = c("x and y coordinates are not identical, NAs are FALSE"),
          pos = 5, na_val = FALSE, registry = .) %>%
  
  # ... or stringr ...
  bf_grow(flags = str_detect(input$year, "r"), 
          name = "flag_year", desc = c("year values do have a flag, NAs are FALSE"),
          pos = 6, na_val = FALSE, registry = .) %>%
  
  # ... or even base R
  bf_grow(flags = !is.na(as.integer(input$year)), 
          name = "valid_year", desc = c("year values are valid integers"),
          pos = 7, registry = .)
#> Testing equal lat/lon
#> Flagged NA records.
#> Warning in bf_grow(flags = !is.na(as.integer(input$year)), name = "valid_year",
#> : NAs durch Umwandlung erzeugt
```

The resulting strcuture is basically a record of all the things that are
grown on the bitfield, but so far nothing has happened

``` r
newRegistry
```

Finally the bitfield needs to be combined (note: input datasets have
been stored into the environment `qb_env`). This will result in an
output table (with one column that has the name `QB`).

``` r
(intBit <- bf_combine(registry = newRegistry))
#>  [1] 735 927 695 479 341 863 991 991 463 843
```

As mentioned above, the bitfield is a record of things, which is
required to decode the quality bit (similar to a key). Together with the
legend, the bit flags can then be converted back to human readable text
or used in any downstream workflow.

``` r
bitfield <- bf_unpack(x = intBit, registry = newRegistry, sep = "-")
#> # A tibble: 9 × 4
#>   name            flags pos   description                                       
#>   <chr>           <int> <chr> <chr>                                             
#> 1 not_na_x            2 1     the values in column 'x' do not contain any NAs   
#> 2 range_x             2 2     the values in column 'x' range between [-180,180] 
#> 3 range_y             2 3     the values in column 'y' range between [-90,90]   
#> 4 match_commodity     2 4     the values in column 'commodity' are contained in…
#> 5 distinct_x_y        2 5     x and y coordinates are not identical, NAs are FA…
#> 6 flag_year           2 6     year values do have a flag, NAs are FALSE         
#> 7 valid_year          2 7     year values are valid integers                    
#> 8 cases               3 8:9   the values are split into the following cases [1:…
#> 9 decimals            2 10:11 the values in 'x' have 0|1 decimals

# -> prints legend by default, which is also available in bf_env$legend

input %>% 
  bind_cols(bitfield) %>% 
  kable()
```

|     x |    y | commodity |     yield | year  | bf_int | bf_binary           |
|------:|-----:|:----------|----------:|:------|-------:|:--------------------|
|  23.9 | 58.7 | soybean   | 13.433170 | 2021  |    735 | 1-1-1-1-1-0-1-10-10 |
|  23.7 | 59.5 | maize     |  7.702426 | NA    |    927 | 1-1-1-1-1-0-0-11-10 |
|  24.5 | 57.6 | NA        | 11.300058 | 2021r |    695 | 1-1-1-0-1-1-0-10-10 |
|  24.0 | 58.3 | maize     |  8.922000 | 2021  |    479 | 1-1-1-1-1-0-1-11-00 |
| 259.0 | 57.7 | honey     |  9.911468 | 2021  |    341 | 1-0-1-0-1-0-1-01-00 |
|  24.7 | 58.2 | maize     | 10.292415 | 2021  |    863 | 1-1-1-1-1-0-1-01-10 |
|  26.8 | 58.9 | soybean   |  7.211000 | 2021  |    991 | 1-1-1-1-1-0-1-11-10 |
|  26.4 | 59.2 | maize     |  7.509846 | 2021  |    991 | 1-1-1-1-1-0-1-11-10 |
|   0.0 |  0.0 | soybean   |  6.627607 | 2021  |    463 | 1-1-1-1-0-0-1-11-00 |
|  23.5 |   NA | maize     | 10.584207 | 2021  |    843 | 1-1-0-1-0-0-1-01-10 |

## Bitfields for other data-types

This example here shows how to compute quality bits for tabular data,
but this technique is especially helpful for raster data. To keep this
package as simple as possible, no specific methods for rasters were
developed (so far), they instead need to be converted to tabular form
and joined to the attributes or meta data that should be added to the
QB, for example like this

``` r
library(terra)

raster <- rast(matrix(data = 1:25, nrow = 5, ncol = 5))

input <- values(raster) %>% 
  as_tibble() %>% 
  rename(values = lyr.1) %>% 
  bind_cols(crds(raster), .)

# from here we can continue creating a bitfield and growing bits on it just like shown above...

# ... and then converting it back to a raster
QB_rast <- crds(raster) %>% 
  bind_cols(QB_int) %>% 
  rast(type="xyz", crs = crs(raster), extent = ext(raster))
```

# To Do

- write registry show method
- include MD5 sum for a bitfield and update it each time the bitfield is
  grown further
