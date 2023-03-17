## Description

json2ecl examines JSON data and deduces the ECL RECORD definitions necessary to parse it.
The resulting ECL definitions are returned via standard out, suitable for piping or copying
and pasting into your favorite IDE.

JSON data can be supplied as one or more files or via standard input.

Multiple files, if provided, are parsed as if they should have the same record structure.
This is useful for cases where you suspect that not all JSON key/value objects are fully
defined in one file, but other files may contain the missing data.

## ECL Record Definitions ???

[HPCC Systems](https://hpccsystems.com) is a big data system that is programmed using a
declarative language called Enterprise Control language (ECL).  It is a schema-on-read
system, meaning that you supply a schema to the function that reads data for processing.
An "ECL record definition" in this context means that schema:  json2ecl generates the
schema as text that can be pasted into an IDE and used within an ECL program.

## Requirements

This project was written using SBCL and it has not been tested with other flavors
of Lisp.  There are very few dependencies, however, so it should work out of the box
with all of the common Lisp distributions.

The following dependencies are required:

- ASDF (version 3.3.6 or later)
- Quicklisp
- Buildapp
  - Used to build the binary
  - [https://www.xach.com/lisp/buildapp/](https://www.xach.com/lisp/buildapp/)

## How to Build (for *nix-compatible systems)

1. Clone this repo:  `git clone https://github.com/dancamper/json2ecl.git`
1. Change directory: `cd json2ecl`
1. Run build script: ./build_binary.sh
1. Final binary is now bin/json2ecl.  You can move or copy that binary to a location on your path.

## How to Use

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
```

## Examples

Assuming file foo.json contains the following contents (and note that the third key in the object is an ECL reserved keyword, which affects the result):

```json
{
  "foo": "bar",
  "start": 12,
  "end": 98.76
}
```

Simple parsing of those contents:

````none
$ json2ecl foo.json
FOO_001_LAYOUT := RECORD
    UTF8 foo {XPATH('foo')};
    INTEGER start {XPATH('start')};
    REAL f_end {XPATH('end')};
END;
````

````none
$ cat foo.json | json2ecl 
TOPLEVEL_231_001_LAYOUT := RECORD
    UTF8 foo {XPATH('foo')};
    INTEGER start {XPATH('start')};
    REAL f_end {XPATH('end')};
END;
````

## License

Apache 2.0
