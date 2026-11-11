# Coerce Yamlet to Data Frame

Coerces yamlet to data.frame. Columns are constructed in the order that
attributes are encountered, beginning with top-level 'item' (default).
Cell contents are calculated using
`getOption('yamlet_cell_value', yamlet::cell_value)` to which is passed
the cell-specific metadata as well as `sep` and `def`.

## Usage

``` r
# S3 method for class 'yamlet'
as.data.frame(
  x,
  row.names = "item",
  optional = FALSE,
  sep = "\n",
  def = ": ",
  ...
)
```

## Arguments

- x:

  yamlet; see
  [`decorations`](https://bergsmat.github.io/yamlet/reference/decorations.md)
  and
  [`read_yamlet`](https://bergsmat.github.io/yamlet/reference/read_yamlet.md)

- row.names:

  a name for a column to hold top-level names, or NULL to represent
  these as row.names

- optional:

  if TRUE and row.names is NULL, row.names will not be set

- sep:

  separator for multiple items within an attribute

- def:

  definition string: separator between items and their (preceding)
  names, if any

- ...:

  ignored

## Value

data.frame

## Examples

``` r

library(magrittr)

file <- system.file(package = 'yamlet', 'extdata','quinidine.yaml')
file %>% read_yamlet %>% explicit_guide %>% as.data.frame
#>          item                                   label units
#> 1     Subject                      patient identifier  <NA>
#> 2        time               time since start of study     h
#> 3        conc           quinidine serum concentration  mg/L
#> 4        dose             quinidine administered dose    mg
#> 5    interval                     inter-dose interval     h
#> 6         Age                    baseline subject age  year
#> 7      Height                 baseline subject height    in
#> 8      Weight                     subject body weight    kg
#> 9        Race                            subject race  <NA>
#> 10      Smoke                          smoking status  <NA>
#> 11    Ethanol                    ethanol abuse status  <NA>
#> 12      Heart                congestive heart failure  <NA>
#> 13 Creatinine                    creatinine clearance  <NA>
#> 14      glyco alpha-1 acid glycoprotein concentration mg/dL
#>                                  codelist
#> 1                                    <NA>
#> 2                                    <NA>
#> 3                                    <NA>
#> 4                                    <NA>
#> 5                                    <NA>
#> 6                                    <NA>
#> 7                                    <NA>
#> 8                                    <NA>
#> 9                 Caucasian\nLatin\nBlack
#> 10                                no\nyes
#> 11                  none\ncurrent\nformer
#> 12              No/Mild\nModerate\nSevere
#> 13 < 50 mg/min: < 50\n>= 50 mg/min: >= 50
#> 14                                   <NA>
file <- system.file(package = 'yamlet', 'extdata','phenobarb.csv')

# phenobarb.yaml has conditional metadata that benefits
# from interpretation in the context of the data itself.
# thus, we
# * read the whole 'decorated' object (not just yaml),
# * resolve the 'guide' ambiguity,
# extract the best-guess decorations, and
# convert to data.frame.

file %>% io_csv %>% resolve %>% decorations %>% as.data.frame
#>       item
#> 1  Subject
#> 2       Wt
#> 3    Apgar
#> 4 ApgarInd
#> 5     time
#> 6    event
#> 7    value
#>                                                                                            label
#> 1                                                                              infant identifier
#> 2                                                                                   birth weight
#> 3                                                                           5-minute Apgar score
#> 4                                                                                    Apgar class
#> 5                                                                                  time of event
#> 6                                                                                  type of event
#> 7 event == 'dose': dose of drug administered\nevent == 'conc': serum phenobarbital concentration
#>                                           units
#> 1                                          <NA>
#> 2                                            kg
#> 3                                          <NA>
#> 4                                          <NA>
#> 5                                             h
#> 6                                          <NA>
#> 7 event == 'dose': µg/kg\nevent == 'conc': µg/L
#>                                                                                                           title
#> 1                                                                                                          <NA>
#> 2                                                                                             birth weight (kg)
#> 3                                                                                                          <NA>
#> 4                                                                                                          <NA>
#> 5                                                                                             time of event (h)
#> 6                                                                                                          <NA>
#> 7 event == 'dose': dose of drug administered (µg/kg)\nevent == 'conc': serum phenobarbital concentration (µg/L)
#>                        codelist
#> 1                          <NA>
#> 2                          <NA>
#> 3 1\n2\n3\n4\n5\n6\n7\n8\n9\n10
#> 4                     < 5\n>= 5
#> 5                          <NA>
#> 6                    dose\nconc
#> 7                          <NA>
```
