# Find Common Type for dvec, double

Find common type for dvec, double.

## Usage

``` r
# S3 method for class 'dvec.double'
vec_ptype2(x, y, ...)
```

## Arguments

- x:

  dvec

- y:

  double

- ...:

  ignored

## Examples

``` r
str(vctrs::vec_ptype2(as_dvec(1), 1))
#>  'dvec' num(0) 
str(vctrs::vec_ptype2(1, as_dvec(1)))
#>  'dvec' num(0) 

str(vctrs::vec_ptype2(as_dvec(1, label = 'x'), 1))
#>  'dvec' num(0) 
#>  - attr(*, "label")= chr "x"
str(vctrs::vec_ptype2(1, as_dvec(1, label= 'x')))
#>  'dvec' num(0) 
#>  - attr(*, "label")= chr "x"
str(vctrs::vec_ptype2(as_dvec(1), structure(1, label = 'x')))
#>  'dvec' num(0) 
#>  - attr(*, "label")= chr "x"
```
