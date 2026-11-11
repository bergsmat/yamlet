# Display Global Yamlet Options

Displays global yamlet options: those options whose names begin with
'yamlet\_'.

- **yamlet_append_units_open**: see
  [`append_units.default`](https://bergsmat.github.io/yamlet/reference/append_units.default.md).
  Controls how labels are constructed for variables with 'units'
  attributes. In brief, units are wrapped in parentheses, and appended
  to the label.

- **yamlet_append_units_close**: see
  [`append_units.default`](https://bergsmat.github.io/yamlet/reference/append_units.default.md).
  Controls how labels are constructed for variables with 'units'
  attributes. In brief, units are wrapped in parentheses, and appended
  to the label.

- **yamlet_append_units_style**: see
  [`append_units.default`](https://bergsmat.github.io/yamlet/reference/append_units.default.md).
  Determines parsing as 'plotmath' or 'latex', or 'plain' for no
  parsing.

- **yamlet_append_units_target**: see
  [`append_units.default`](https://bergsmat.github.io/yamlet/reference/append_units.default.md).
  By default, append result is assigned to attribute 'label', but could
  be something else like 'title'.

- **yamlet_default_keys**: see
  [`as_yamlet.character`](https://bergsmat.github.io/yamlet/reference/as_yamlet.character.md).
  The first two yaml attributes without specified names are assumed to
  be 'label' and 'guide'.

- **yamlet_persistence**: see
  [`decorate.list`](https://bergsmat.github.io/yamlet/reference/decorate.list.md)
  and
  [`as.integer.classified`](https://bergsmat.github.io/yamlet/reference/as.integer.classified.md).
  By default, persistence of column attributes is implemented by
  creating 'dvec' objects (decorated vectors) using vctrs methodology.

- **yamlet_cell_value**: see
  [`as.data.frame.yamlet`](https://bergsmat.github.io/yamlet/reference/as.data.frame.yamlet.md).
  Controls how cells are calculated when converting yamlet (decorations)
  to a data.frame.

- **yamlet_import**: see
  [`decorate.character`](https://bergsmat.github.io/yamlet/reference/decorate.character.md).
  Controls how primary data is read from file (default: as.csv()).

- **yamlet_extension**: see
  [`decorate.character`](https://bergsmat.github.io/yamlet/reference/decorate.character.md).
  Controls what file extension is expected for yaml metadata (default:
  '.yaml')

- **yamlet_overwrite**: see
  [`decorate.list`](https://bergsmat.github.io/yamlet/reference/decorate.list.md).
  Controls whether existing decorations are overwritten.

- **yamlet_exclude_attr**: see
  [`decorations.data.frame`](https://bergsmat.github.io/yamlet/reference/decorations.data.frame.md)
  Controls what attributes are excluded from display.

- **yamlet_with_title**: see
  [`make_title.dvec`](https://bergsmat.github.io/yamlet/reference/make_title.dvec.md)
  and
  [`drop_title.dvec`](https://bergsmat.github.io/yamlet/reference/drop_title.dvec.md).
  For objects with (implied) units attributes, titles are by default
  automatically created on resolve() and destroyed on desolve().
  Interacts with yamlet_append_units\_\*.

- **yamlet_infer_guide**: see
  [`explicit_guide.yamlet`](https://bergsmat.github.io/yamlet/reference/explicit_guide.yamlet.md).
  Identifies the function that will be used to reclassify 'guide' as
  something more explicit.

- **yamlet_explicit_guide_overwrite**: see
  [`explicit_guide.data.frame`](https://bergsmat.github.io/yamlet/reference/explicit_guide.data.frame.md)
  and
  [`explicit_guide.dvec`](https://bergsmat.github.io/yamlet/reference/explicit_guide.dvec.md).
  In the latter case, controls whether existing attributes are
  overwritten.

- **yamlet_explicit_guide_simplify**:
  [`explicit_guide.data.frame`](https://bergsmat.github.io/yamlet/reference/explicit_guide.data.frame.md)
  and
  [`explicit_guide.dvec`](https://bergsmat.github.io/yamlet/reference/explicit_guide.dvec.md).
  Ordinarily, the 'guide' attribute is removed if something more useful
  can be inferred.

- **yamlet_decorated_ggplot_search**: see
  [`ggplot_build.decorated_ggplot`](https://bergsmat.github.io/yamlet/reference/ggplot_build.decorated_ggplot.md).
  The build method for decorated_ggplot populates axis labels by
  searching first for attributes named 'expression', 'title', and
  'label'. Customizable.

- **yamlet_decorated_ggplot_discrete**: see
  [`ggplot_build.decorated_ggplot`](https://bergsmat.github.io/yamlet/reference/ggplot_build.decorated_ggplot.md).
  Discrete aesthetics to map from data decorations where available.

- **yamlet_decorated_ggplot_drop**: see
  [`ggplot_build.decorated_ggplot`](https://bergsmat.github.io/yamlet/reference/ggplot_build.decorated_ggplot.md).
  Should unused factor levels be omitted from data-driven discrete
  scales?

- **yamlet_ggready_parse**: see
  [`ggready.data.frame`](https://bergsmat.github.io/yamlet/reference/ggready.data.frame.md),
  [`ggready.decorated`](https://bergsmat.github.io/yamlet/reference/ggready.decorated.md).
  Whether to parse axis labels. TRUE by default, but may be problematic
  if unintended.

- **yamlet_modify_reserved**: see
  [`modify.default`](https://bergsmat.github.io/yamlet/reference/modify.default.md).
  A list of reserved labels that warn on reassignment.

- **yamlet_promote_reserved**: see
  [`promote.list`](https://bergsmat.github.io/yamlet/reference/promote.list.md).
  Attributes to leave untouched when promoting singularities.

- **yamlet_promote**: see
  [`filter.decorated`](https://bergsmat.github.io/yamlet/reference/filter.decorated.md).
  Whether to promote when filtering 'decorated'.

- **yamlet_as_units_preserve**:
  [`as_units.dvec`](https://bergsmat.github.io/yamlet/reference/as_units.dvec.md).
  What attributes to preserve when converting dvec to units. Just
  'label' by default. Assign
  `options(yamlet_as_units_preserve = character(0))` to remove all.

- **yamlet_print_simplify**:
  [`print.yamlet`](https://bergsmat.github.io/yamlet/reference/print.yamlet.md).
  Whether to collapse interactively-displayed decorations into a single
  line for lists that have no (nested) names and have the same length
  when unlisted. True by default. Can be misleading for lists with fine
  detail, but in most cases fine detail will likely have names.

- **yamlet_format**:
  [`enscript.default`](https://bergsmat.github.io/yamlet/reference/enscript.default.md).
  Choice of 'html' or 'latex', guessed if not supplied.

- **yamlet_warn_conflicted**:
  [`c.classified`](https://bergsmat.github.io/yamlet/reference/c.classified.md).
  Whether to warn when codelists for combined classified factors have
  conflicting names (which will be dropped).

- **yamlet_reconcile_attributes**:
  [`c.classified`](https://bergsmat.github.io/yamlet/reference/c.classified.md).
  Whether to reconcile attribute lists when combining classified. Set
  FALSE for old behavior (pre 1.3.1) of adopting attributes (other than
  levels and codelists).

- **yamlet_expand_codelist**:
  [`explicit_guide.yamlet`](https://bergsmat.github.io/yamlet/reference/explicit_guide.yamlet.md).
  If TRUE (default) an empty list as a guide attribute is short-hand for
  `sort(unique(x))`.

- **yamlet_collapse_codelist**:
  [`implicit_guide.data.frame`](https://bergsmat.github.io/yamlet/reference/implicit_guide.data.frame.md).
  An integer (default: Inf) giving the maximum number of (un-named)
  codelist elements to store explicitly. Else, if `sort(unique(x))` has
  exactly the same values as codelist, `implicit_guide` will substitute
  an empty list.

## Usage

``` r
yamlet_options()
```

## Value

list

## Examples

``` r
yamlet_options()
#> named list()
```
