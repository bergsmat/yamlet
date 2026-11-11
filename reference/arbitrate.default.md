# Arbitrate Default

Arbitrates two attributes, the first of which is non-NULL and non-list.
If y is list, x is promoted to list and re-arbitrated thus. Otherwise, x
is returned, with warning if y not identical and not NULL.

## Usage

``` r
# Default S3 method
arbitrate(x, y, tag = "", ...)
```

## Arguments

- x:

  left attribute

- y:

  right attribute

- ...:

  passed arguments

## Value

list if y is list, else x
