# Table of Contents

- [Description](#description)
- [Getting The Binary](#getting_the_binary)
- [Building From Source](#building_from_source)
- [How to Use](#how_to_use)
- [Examples](#examples)
- [Limitations](#limitations)

<a name="description"></a>
# Description

json2ecl is a command-line tool that examines JSON data and deduces the
ECL RECORD definitions necessary to parse it.  The resulting ECL definitions are returned
via standard out, suitable for piping or pasting into your favorite IDE.

See [xml2ecl](https://github.com/dancamper/xml2ecl) for an XML version of this functionality.

## ECL Record Definitions ???

[HPCC Systems](https://hpccsystems.com) is a big data system that is programmed using a
declarative language called Enterprise Control language (ECL).  It is a schema-on-read
system, meaning that you supply a schema to the function that reads data for processing.
An "ECL record definition" in this context means that schema:  json2ecl generates the
schema as text that can be pasted into an IDE and used within an ECL program.

<a name="getting_the_binary"></a>
# Getting The Binary

Head over the [releases](https://github.com/dancamper/json2ecl/releases) section of
the Github repo and choose the version that matches your operating system.  Decompress
the file and put the result somewhere on your PATH for easy reference.

<a name="building_from_source"></a>
# Building From Source

This project was written using [Steel Bank Common Lisp (SBCL)](http://www.sbcl.org) and
it has not been tested with other flavors of Lisp.  There are very few dependencies,
however, so it should work with minimal modifications with all of the Common Lisp
distributions.

The following dependencies are required:

- [ASDF](https://asdf.common-lisp.dev) (version 3.3.0 or later)
- [Quicklisp](https://www.quicklisp.org/beta/)
  - Packages installed via QuickLisp by the build script
    - [adopt](https://docs.stevelosh.com/adopt/) (parsing common-line options and arguments)
    - [com.inuoe.jzon](https://github.com/Zulu-Inuoe/jzon) (SAX-style JSON parsing)
    - [with-user-abort](https://github.com/compufox/with-user-abort) (handles ctrl-c)
- [Buildapp](https://www.xach.com/lisp/buildapp/) (used to build the binary)
  - Note that the `buildapp` binary as well as your Lisp's binary must be on your PATH.

## Build Instructions (for *nix-compatible systems)

1. Clone this repo:  `git clone https://github.com/dancamper/json2ecl.git`
1. Change directory: `cd json2ecl`
1. Run build script: `./build_binary.sh`

Built binary is `bin/json2ecl`.  You can move or copy it to a location on your path.

<a name="how_to_use"></a>
# How to Use

Usage: `json2ecl [OPTIONS] [FILE...]`

json2ecl examines JSON data and deduces the ECL RECORD definitions necessary to
parse it. The resulting ECL definitions are returned via standard out, suitable
for piping or copying and pasting into your favorite IDE.

JSON data can be supplied as one or more files or via standard input.

Multiple files, if provided, are parsed as if they should have the same record
structure. This is useful for cases where you suspect that not all JSON
key/value objects are fully defined in one file, but other files may contain the
missing data.

ECL records will be created with fields named after the keys found in JSON objects.
Every field will have an XPATH attribute added so the ECL reader can correctly
read everything, no matter what the field is named.

ECL keywords, in general, should not be used as field names in record definitions.
json2ecl will prefix those fields with "f_" when defining those field names.  Other
minor changes to the field names are also made (such as converting dashes to
underscores).

The last ECL record definition in the output will be the "root" definition; it
is the one you should pass to the ECL DATASET() function.  If you pass exactly
one file to json2ecl then that record definition will be named after the file.
If you pass multiple files, or stream JSON data in via standard input, then the
layout will be named TOPLEVEL with some added items to make it unique.

```none
Options:
  -v, --version         Display version and exit.
  -h, --help            Display help and exit.
  -s STRING-TYPE, --string-type STRING-TYPE
                        ECL datatype to use for strings; must be one of
                        UTF8|STRING|VARSTRING; defaults to UTF8
```

The `-h` and `-v` options should be obvious.

The -s option allows you to override the ECL datatype used for string values.
Because JSON data is normally in UTF-8 format, `UTF8` is the default ECL data type for
those values.  However, if you know that the data is in plain ASCII then you can override
the type with this option.  The acceptable values are:

- `UTF8`: A UTF-8 string; this is the default.
- `STRING`: An ASCII string.
- `VARSTRING`:  A C-style null-terminated ASCII string.  Don't use this unless you know why you need it.

<a name="examples"></a>
# Examples

Assuming file foo.json contains the following contents:

```json
{
  "foo": "bar",
  "start": 12,
  "end": 98.76
}
```

Simple parsing of those contents.  The `end` JSON key is an ECL keyword, so it
was modified with the `f_` prefix and an ECL XPATH markup added.

```none
$ json2ecl foo.json

FOO_LAYOUT := RECORD
    UTF8 foo {XPATH('foo')};
    UNSIGNED start {XPATH('start')};
    REAL f_end {XPATH('end')};
END;
````

You can pipe JSON content instead of reading a file.  Note that generally you cannot
pipe multiple JSON files, because the final result will not be valid JSON (there will
be no separator characters between the files' contents, for instance).

```none
$ cat foo.json | json2ecl 

TOPLEVEL_139_LAYOUT := RECORD
    UTF8 foo {XPATH('foo')};
    UNSIGNED start {XPATH('start')};
    REAL f_end {XPATH('end')};
END;
````

Simple example of overriding the default string ECL data type:

```none
$ json2ecl -s STRING foo.json

FOO_LAYOUT := RECORD
    STRING foo {XPATH('foo')};
    UNSIGNED start {XPATH('start')};
    REAL f_end {XPATH('end')};
END;
````

If you process multiple JSON files at once, json2ecl assumes that each file represents
a separate example of the same underlying structure.  This is useful, as variations in
JSON field values -- explicit null values, or outright missing fields -- could be
discovered and "filled in" by these additional data files.

Assuming a second file baz.json with the following contents:

```json
{
  "foo": "frob",
  "start": 42,
  "end": null,
  "incr": 3.5
}
```

Notice that the `end` field contains a null instead of a float, and there is an
additional field named `incr` in the object.  The two layouts from the two files
were merged:

```none
$ json2ecl foo.json baz.json 

TOPLEVEL_139_LAYOUT := RECORD
    UTF8 foo {XPATH('foo')};
    UNSIGNED start {XPATH('start')};
    STRING f_end {XPATH('end')}; // null, float
    REAL incr {XPATH('incr')};
END;
```

One of the more interesting uses for json2ecl is determining the record structure
needed to parse a REST API call, which is usually in JSON format.  Here is a call
showing an example REST reply:

```none
$ curl -s 'https://jsonplaceholder.typicode.com/todos/1'

{
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
```

You can pipe that directly to json2ecl:

```none
$ curl -s 'https://jsonplaceholder.typicode.com/todos/1' | json2ecl

TOPLEVEL_139_LAYOUT := RECORD
    UNSIGNED userid {XPATH('userId')};
    UNSIGNED id {XPATH('id')};
    UTF8 title {XPATH('title')};
    BOOLEAN completed {XPATH('completed')};
END;
```

<a name="limitations"></a>
# Limitations

If you're importing JSON data into a big data system then an underlying assumption is that
the data is composed of repeated structure:  records, in other words.  JSON itself does
not impose any structure on the data, and therein lies some potential limitations and
problems.

There are three basic JSON data types that json2ecl cares about: arrays, objects, and
scalar values.  json2ecl does consider all scalar data types "the same" though, so you can
mix numbers and strings, for example.  JSON's `null` is a wildcard type that is a valid
value for any of those three.

### Requirement: Same data type used throughout a JSON array

Within a JSON array, json2ecl expects every element to be of the same basic type (array,
object, scalar).  If it finds something else during parsing then it will abort and
emit an error.  This example will throw an error because of the second array element is
not an object:

```json
[
    { "foo": 123 },
    "Hi there",
    { "foo": 456 }
]
```

### Requirement: Same data type used between objects with the same key

Similarly, a repeated JSON object is expected to contain key values with the same type
for same-named keys.  This example will throw an error because the third object's
`foo` key has a value that is an array rather than a scalar:

```json
[
    { "foo": 123 },
    { "foo": 456 },
    { "foo": [ 789 ] }
]
```

### Limitation: JSON structure as data

Some JSON uses "structure as data" which is a bit rare but does come up in a few
use cases.  Example:

```json
{
    "manufacturer_type": "automobiles",
    "country": "United States",
    "Chevrolet": {
        "last_year_seen": 2023
    },
    "Ford": {
        "last_year_seen": 2023
    }
}
```

In that example, the automobile manufacturer's name is the JSON object key.  Running that
data through json2ecl produces:

```none
CHEVROLET_LAYOUT := RECORD
    UNSIGNED last_year_seen {XPATH('last_year_seen')};
END;

FORD_LAYOUT := RECORD
    UNSIGNED last_year_seen {XPATH('last_year_seen')};
END;

CAR_LAYOUT := RECORD
    UTF8 manufacturer_type {XPATH('manufacturer_type')};
    UTF8 country {XPATH('country')};
    DATASET(CHEVROLET_LAYOUT) chevrolet {XPATH('Chevrolet')};
    DATASET(FORD_LAYOUT) ford {XPATH('Ford')};
END;
```

json2ecl creates child record structures based on name, not contents.  That ECL structure
is valid and will work, but you can probably guess what would happen if there were
100,000 automobile manufacturers instead of just two.

### Limitation: Weird XPATH values

JSON object keys are coerced into ECL field names (and child record structures when
needed), so sometimes rewriting those keys is needed.  The XPATH notation,
however, should remain the same as the original key value so that the parser can
accurately walk the JSON structure.  Unfortunately, due to limitations within ECL's
XPATH support, that XPATH value may look a bit odd.  Given this data:

```json
{
    "this is a test": 123
}
```

It parses to this structure (note the XPATH):

```none
MULTIWORD_LAYOUT := RECORD
    UNSIGNED this_is_a_test {XPATH('this*is*a*test')};
END;
```

That XPATH works for reading, but if you turned around and tried to write a new
JSON file from the dataset using ECL, with that XPATH notation still attached to that field,
you will get an error.  The workaround is to rewrite the data, perhaps through a
PROJECT(), into a structure without XPATH or at least with more sensible XPATH values.
