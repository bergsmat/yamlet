# yamlet: Versatile Curation of Table Metadata

The yamlet package supports storage and retrieval of table metadata in
yaml format. The most important function is
[`decorate.character`](https://bergsmat.github.io/yamlet/reference/decorate.character.md):
it lets you 'decorate' your data by attaching attributes retrieved from
a file in yaml format. Typically your data will be of class
'data.frame', but it could be anything that is essentially a named list.

## Storage Format

Storage format for 'yamlet' is a text file containing well-formed yaml.
Technically, it is a map of sequences. Though well formed, it need not
be complete: attributes or their names may be missing.

In the simplest case, the data specification consists of a list of
column (item) names, followed by semicolons. Perhaps you only have one
column:

`mpg:`

or maybe several:


    mpg:
    cyl:
    disp:

If you know descriptive labels for your columns, provide them (skip a
space after the colon).


    mpg: fuel economy
    cyl: number of cylinders
    disp: displacement

If you know units, create a sequence with square brackets.


    mpg: [ fuel economy, miles/gallon ]
    cyl: number of cylinders
    disp: [ displacement , in^3 ]

If you are going to give units, you probably should give a key first,
since the first anonymous element is 'label' by default, and the second
is 'guide'. (A guide can be units for numeric variables, factor
levels/labels for categorical variables, or a format string for dates,
times, and datetimes.) You could give just the units but you would have
to be specific:

`mpg: [units: miles/gallon]`

You can over-ride default keys by providing them in your data:


    mpg: [units: miles/gallon]
    _keys: [label, units]

Notice that stored yamlet can be informationally defective while
syntactically correct. If you don't know an item key at the time of data
authoring, you can omit it:

`race: [race, [white: 0, black: 1, 2, asian: 3 ]]`

Or perhaps you know the key but not the value:

`race: [race, [white: 0, black: 1, asian: 2, ? other ]]`

Notice that `race` is factor-like; the factor sequence is nested within
the attribute sequence. Equivalently:

`race: [label: race, guide: [white: 0, black: 1, asian: 2, ? other ]]`

If you have a codelist of length one, you should still enclose it in
brackets:

`sex: [Sex, [ M ]]`

To get started using yamlet, see
[`?as_yamlet.character`](https://bergsmat.github.io/yamlet/reference/as_yamlet.character.md)
and examples there. See also
[`?decorate`](https://bergsmat.github.io/yamlet/reference/decorate.md)
which adds yamlet values to corresponding items in your data. See also
`?print.decorated` which uses label attributes, if present, as axis
labels.

Note: the quinidine and phenobarb datasets in the examples are borrowed
from nlme (`?Quinidine`, `?Phenobarb`), with some reorganization.

## See also

Useful links:

- <https://bergsmat.github.io/yamlet/>

- Report bugs at <https://github.com/bergsmat/yamlet/issues>

## Author

**Maintainer**: Tim Bergsma <bergsmat@gmail.com>
