# Find Common Type for dvec, dvec

Find common type for dvec, dvec.

## Usage

``` r
# S3 method for class 'dvec.dvec'
vec_ptype2(x, y, ...)
```

## Arguments

- x:

  dvec

- y:

  dvec

- ...:

  ignored

## Examples

``` r
str(vctrs::vec_ptype2(as_dvec(1L), as_dvec(1)))
#>  'dvec' num(0) 
str(vctrs::vec_ptype2(as_dvec(1), as_dvec(1L)))
#>  'dvec' num(0) 
str(vctrs::vec_ptype2(as_dvec(TRUE), as_dvec(1L)))
#>  'dvec' int(0) 
str(vctrs::vec_ptype2(as_dvec(TRUE), as_dvec(1)))
#>  'dvec' num(0) 
str(vctrs::vec_ptype2(as_dvec(1), as_dvec(1+0i)))
#>  'dvec' cplx(0) 

vctrs::vec_ptype2(
  structure(as_dvec(1), guide = 'mg'),
  structure(as_dvec(1), guide = 'kg')
)
#> Warning: mismatched guide attributes: ignoring 'kg' in favor of 'mg'
#> numeric(0)
#> attr(,"guide")
#> [1] "mg"
```
