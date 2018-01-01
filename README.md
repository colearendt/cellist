
<!-- README.md is generated from README.Rmd. Please edit that file -->
cellist
=======

The goal of cellist is to turn nested list columns into tidy data\_frames.

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
```

Key Functions
-------------

-   `col_spec` and related items (`col_list`, `col_object`, `col_double`, etc.)
-   `guess_spec`
-   `spread_list`
-   `gather_list`
-   inverse operations to rebuild the list?
-   mappings from `json_schema` objects to `col_spec`?
-   helpers to move from `xml2` and `jsonlite` objects to nested lists

API
---

Should be able to do something like `col_list()`, `col_list_spread()`, `col_list_gather()`... Should also be able to nest specs in `col_list`... something like the following...

``` r
col_spec(
  list(
    d = col_double()
    , int = col_integer()
    , obj_raw = col_list()
    , obj_spread = col_list_spread(
      a = col_double
      , name = col_character()
    )
    , obj_gather = col_list_gather(
      b = col_integer()
      , name = col_character()
    )
    , arr_raw = col_list()
    , arr_spread = col_list_spread(
      1 = col_integer()
      , 2 = col_integer()
    )
    , arr_gather = col_list_gather(
      col_integer()
    )
  )
)
```

This API seems a little unweildy, but it seems that you would be able to pull out the `collector` functionality into a separate package and make it extensible (so the code is not defined for `readr` and `tidylist`)! These collectors make use of the `name` to do look-up by reference. This is not unlike `readr`, who also has a `col_names` parameter. The difference is that in this case, I think asked-for fields should be returned, even if not present.

The real power comes in something like `guess_spec` that will generate a spec for you... you could also conceive of generating a spec from a JSON Schema / XML schema object!

This also needs to be do-able by integer reference, i.e. `list(1,"a","b")` would grab the 1st object of a list, the first "a" key, and then the first "b" key.

Open Questions
==============

-   How should we handle the existing list? Should we hold to "copy on reference" or should we modify the list using "do not repeat yourself"... pull the items out? Probaby pull the items out... some items are not reversible
-   maybe worth creating callbacks for naming the columns...? This at least for unnamed columns... named columns will be preserved? What about handling nested behavior, though... maybe a callback for that too

To Do
=====

-   Create tests to define what the internal functionality should be doing (since I cannot keep it straight otherwise...)
-   Integrate `purrr` more natively
