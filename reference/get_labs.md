# Get Labels

Gets labels for a ggplot object. Not exported, to avoid confusion.
Development version of ggplot2 implements new get_labs() interface. This
function is an abstraction that supports new vs old approaches, solely
for yamlet's interests. See
https://github.com/tidyverse/ggplot2/pull/6078.

## Usage

``` r
get_labs(plot)
```

## Arguments

- plot:

  the ggplot
