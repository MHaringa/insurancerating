# Backward-compatible access to rating-table contents

A `rating_table` is a data frame. This method keeps the historical
`x$df` accessor available while allowing ordinary `$` access to table
columns. Package metadata is returned only when the requested name is
not a column.

## Usage

``` r
# S3 method for class 'rating_table'
x$name
```

## Arguments

- x:

  A `rating_table` object.

- name:

  Column or legacy component name.

## Value

A table column, the underlying data frame for `name = "df"`, or a stored
metadata component.
