
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

Let’s first load an example dataset

``` r
input <- example_data

# these data have various deviations:
input$x                                          # invalid coordinate value (259)
#>  [1]  25.3  27.9  27.8  27.0 259.0  27.3  26.1  26.5   0.0  25.7
input$y                                          # improbable coordinate value (0,0)
#>  [1] 59.5 58.1 57.8 59.2 58.7 59.1 58.4 59.0  0.0   NA
input$commodity                                  # missing or mislabeled terms ("honey")
#>  [1] "soybean" "maize"   NA        "maize"   "honey"   "maize"   "soybean"
#>  [8] "maize"   "soybean" "maize"
input$yield                                      # too big range?!
#>  [1] 11.192915 11.986793 13.229386  9.431376 12.997422  8.548882 11.276921
#>  [8] 10.640715  9.010452 13.169897
input$year                                       # flags (*r)
#>  [1] "2021"  NA      "2021r" "2021"  "2021"  "2021"  "2021"  "2021"  "2021" 
#> [10] "2021"

validComm <- c("soybean", "maize")               # some valid category terms

kable(input)
```

|     x |    y | commodity |     yield | year  |
|------:|-----:|:----------|----------:|:------|
|  25.3 | 59.5 | soybean   | 11.192915 | 2021  |
|  27.9 | 58.1 | maize     | 11.986793 | NA    |
|  27.8 | 57.8 | NA        | 13.229386 | 2021r |
|  27.0 | 59.2 | maize     |  9.431376 | 2021  |
| 259.0 | 58.7 | honey     | 12.997422 | 2021  |
|  27.3 | 59.1 | maize     |  8.548882 | 2021  |
|  26.1 | 58.4 | soybean   | 11.276921 | 2021  |
|  26.5 | 59.0 | maize     | 10.640715 | 2021  |
|   0.0 |  0.0 | soybean   |  9.010452 | 2021  |
|  25.7 |   NA | maize     | 13.169897 | 2021  |

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
with growing bits, various naming-rules are important to keep in mind

1.  if your mapping function returns a boolean value, the bit flags will
    be `FALSE == 0` and `TRUE == 1`.
2.  if your mapping function returns cases, they will be assigned a
    sequence of numbers that are encoded by their respective binary
    representation, i.e. if there are 3 cases (which takes up 2 bits),
    the bit flags will be `case 1 = 01`, `case 2 == 10` and
    `case 3 == 11`, and so on. Any observation that is not part of any
    case, will be `case 0 == 00`

A flag is declared by calling a suitable function, some of which are
provided here, but some of which are already available elsewhere (more
below). For example `bf_na(x = input, test = "x")` will test whether the
column `x` in the table `input` has `NA`-values. These functions are
provided to `bf_grow()`, where the bitfield is characterised.

``` r
newRegistry <- newRegistry %>%
  # tests for coordinates ...
  bf_grow(flags = bf_na(x = input, test = "x"),            # determine flags
          pos = 1,                                         # specify at which position to store the flag
          registry = .) %>%                                # provide the registry to update
  bf_grow(flags =  bf_range(x = input, test = "x", min = -180, max = 180),
          pos = 2, registry = .) %>%

  # ... or override NA test
  bf_grow(flags = bf_range(x = input, test = "y", min = -90, max = 90),
          pos = 3, na_val = FALSE, registry = .)  %>%

  # test for matches with an external vector
  bf_grow(flags = bf_match(x = input, test = "commodity", set = validComm),
          pos = 4, na_val = FALSE, registry = .) %>%
  
  # define cases
  bf_grow(flags = bf_case(x = input, exclusive = FALSE, 
                          yield >= 11, yield < 11 & yield > 9, yield < 9 & commodity == "maize"),
          pos = 5:6, registry = .)
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
          pos = 7, na_val = FALSE, registry = .) %>%
  
  # ... or stringr ...
  bf_grow(flags = str_detect(input$year, "r"), 
          name = "flag_year", desc = c("year values do have a flag, NAs are FALSE"),
          pos = 8, na_val = FALSE, registry = .) %>%
  
  # ... or even base R
  bf_grow(flags = !is.na(as.integer(input$year)), 
          name = "valid_year", desc = c("year values are valid integers"),
          pos = 9, registry = .)
#> Testing equal lat/lon
#> Flagged NA records.
#> Warning in bf_grow(flags = !is.na(as.integer(input$year)), name = "valid_year",
#> : NAs durch Umwandlung erzeugt
```

The resulting strcuture is basically a record of all the things that are
grown on the bitfield.

``` r
newRegistry
```

Finally the registry needs to be combined (note: input data vectors have
been stored into the environment `bf_env`). This will result in a vector
of integers.

``` r
(intBit <- bf_combine(registry = newRegistry))
#>  [1] 334  78 198 366 324 350 334 366 302 266
```

As mentioned above, the registry is a record of things, which is
required to decode the bitfield (similar to a key). Together with the
legend, the bit flags can then be converted back to human readable text
or used in any downstream workflow.

``` r
bitfield <- bf_unpack(x = intBit, registry = newRegistry, sep = "-")
#> # A tibble: 14 × 4
#> # Rowwise: 
#>    bits  name            flag  desc                                             
#>    <chr> <chr>           <chr> <chr>                                            
#>  1 1     na_x            0     "the value in column 'x' is NA."                 
#>  2 1     na_x            1     "the value in column 'x' is not NA."             
#>  3 2     range_x         0     "the value in column 'x' ranges between [-180,18…
#>  4 2     range_x         1     "the value in column 'x' is outside the range [-…
#>  5 3     range_y         0     "the value in column 'y' ranges between [-90,90]…
#>  6 3     range_y         1     "the value in column 'y' is outside the range [-…
#>  7 4     match_commodity 0     "the value in column 'commodity' is contained in…
#>  8 4     match_commodity 1     "the value in column 'commodity' is not containe…
#>  9 5:6   cases           00    "the observation has the case [yield >= 11]."    
#> 10 5:6   cases           01    "the observation has the case [yield < 11 & yiel…
#> 11 5:6   cases           10    "the observation has the case [yield < 9 & commo…
#> 12 7     distinct_x_y    0     "x and y coordinates are not identical, NAs are …
#> 13 8     flag_year       0     "year values do have a flag, NAs are FALSE"      
#> 14 9     valid_year      0     "year values are valid integers"

# -> prints legend by default, which is also available in bf_env$legend

input %>% 
  bind_cols(bitfield) %>% 
  kable()
```

|     x |    y | commodity |     yield | year  | bf_int | bf_binary        |
|------:|-----:|:----------|----------:|:------|-------:|:-----------------|
|  25.3 | 59.5 | soybean   | 11.192915 | 2021  |    334 | 0-1-1-1-00-1-0-1 |
|  27.9 | 58.1 | maize     | 11.986793 | NA    |     78 | 0-1-1-1-00-1-0-0 |
|  27.8 | 57.8 | NA        | 13.229386 | 2021r |    198 | 0-1-1-0-00-1-1-0 |
|  27.0 | 59.2 | maize     |  9.431376 | 2021  |    366 | 0-1-1-1-01-1-0-1 |
| 259.0 | 58.7 | honey     | 12.997422 | 2021  |    324 | 0-0-1-0-00-1-0-1 |
|  27.3 | 59.1 | maize     |  8.548882 | 2021  |    350 | 0-1-1-1-10-1-0-1 |
|  26.1 | 58.4 | soybean   | 11.276921 | 2021  |    334 | 0-1-1-1-00-1-0-1 |
|  26.5 | 59.0 | maize     | 10.640715 | 2021  |    366 | 0-1-1-1-01-1-0-1 |
|   0.0 |  0.0 | soybean   |  9.010452 | 2021  |    302 | 0-1-1-1-01-0-0-1 |
|  25.7 |   NA | maize     | 13.169897 | 2021  |    266 | 0-1-0-1-00-0-0-1 |

Together with the rules mentioned above, we can read the binary
representation on step at a time. For example, considering the second
position, with the description
`the values in column 'x' range between [-180,180]`, we see that row
five has the value `0`, which means according to naming-rule 1
(`FALSE == 0`), that the x-value here should be outside of the range of
\[-180, 180\], which we can confirm.

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
intBit <- bf_combine(...)

# ... and then converting it back to a raster
QB_rast <- crds(raster) %>% 
  bind_cols(intBit) %>% 
  rast(type = "xyz", crs = crs(raster), extent = ext(raster))
```

# To Do

- write registry show method
- include MD5 sum for a bitfield and update it each time the bitfield is
  grown further
