# Modify Attributes of Indicated Components by Default

Modifies the attributes of each indicated element (all elements by
default). Tries to assign the value of an expression to the supplied
label, with existing attributes and the object itself (.) available as
arguments. Gives a warning if the supplied label is considered reserved.
Intends to support anything with one or more non-empty names.

## Usage

``` r
# Default S3 method
modify(
  x,
  ...,
  .reserved = getOption("yamlet_modify_reserved", c("class", "levels", "labels",
    "names"))
)
```

## Arguments

- x:

  object

- ...:

  indicated columns, or name-value pairs

- .reserved:

  reserved labels that warn on assignment

## Value

same class as x

## Details

The name of the component itself is available during assignments as
attribute 'name' (any pre-existing attribute 'name' is temporarily
masked). After all assignments are complete, the value of 'name' is
enforced at the object level. Thus, `modify` expressions can modify
component names.

As currently implemented, the expression is evaluated by
[`eval_tidy`](https://rlang.r-lib.org/reference/eval_tidy.html), with
attributes supplied as the `data` argument. Thus, names in the
expression may be disambiguated, e.g. with `.data`. See examples.

## See also

Other modify:
[`modify()`](https://bergsmat.github.io/yamlet/reference/modify.md),
[`named()`](https://bergsmat.github.io/yamlet/reference/named.md),
[`selected()`](https://bergsmat.github.io/yamlet/reference/selected.md),
[`selected.default()`](https://bergsmat.github.io/yamlet/reference/selected.default.md)

Other interface:
[`canonical.decorated()`](https://bergsmat.github.io/yamlet/reference/canonical.decorated.md),
[`classified.data.frame()`](https://bergsmat.github.io/yamlet/reference/classified.data.frame.md),
[`decorate.character()`](https://bergsmat.github.io/yamlet/reference/decorate.character.md),
[`decorate.data.frame()`](https://bergsmat.github.io/yamlet/reference/decorate.data.frame.md),
[`desolve.decorated()`](https://bergsmat.github.io/yamlet/reference/desolve.decorated.md),
[`enscript.decorated()`](https://bergsmat.github.io/yamlet/reference/enscript.decorated.md),
[`ggplot.decorated()`](https://bergsmat.github.io/yamlet/reference/ggplot.decorated.md),
[`io_csv.character()`](https://bergsmat.github.io/yamlet/reference/io_csv.character.md),
[`io_csv.data.frame()`](https://bergsmat.github.io/yamlet/reference/io_csv.data.frame.md),
[`io_res.character()`](https://bergsmat.github.io/yamlet/reference/io_res.character.md),
[`io_res.decorated()`](https://bergsmat.github.io/yamlet/reference/io_res.decorated.md),
[`io_table.character()`](https://bergsmat.github.io/yamlet/reference/io_table.character.md),
[`io_table.data.frame()`](https://bergsmat.github.io/yamlet/reference/io_table.data.frame.md),
[`io_yamlet.character()`](https://bergsmat.github.io/yamlet/reference/io_yamlet.character.md),
[`io_yamlet.data.frame()`](https://bergsmat.github.io/yamlet/reference/io_yamlet.data.frame.md),
[`is_parseable.default()`](https://bergsmat.github.io/yamlet/reference/is_parseable.default.md),
[`mimic.default()`](https://bergsmat.github.io/yamlet/reference/mimic.default.md),
[`promote.list()`](https://bergsmat.github.io/yamlet/reference/promote.list.md),
[`read_yamlet()`](https://bergsmat.github.io/yamlet/reference/read_yamlet.md),
[`resolve.decorated()`](https://bergsmat.github.io/yamlet/reference/resolve.decorated.md),
[`selected.default()`](https://bergsmat.github.io/yamlet/reference/selected.default.md),
[`write_yamlet()`](https://bergsmat.github.io/yamlet/reference/write_yamlet.md)

## Examples
