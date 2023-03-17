## Description

json2ecl examines JSON data and deduces the ECL RECORD definitions necessary to parse it.
The resulting ECL definitions are returned via standard out, suitable for piping or copying
and pasting into your favorite IDE.

JSON data can be supplied as one or more files or via standard input.

Multiple files, if provided, are parsed as if they should have the same record structure.
This is useful for cases where you suspect that not all JSON key/value objects are fully
defined in one file, but other files may contain the missing data.

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

```
Options:
  -v, --version         Display version and exit.
  -h, --help            Display help and exit.
```

## Examples

Assuming file foo.json contains the following contents:

```json
{
  "foo": "bar",
  "start": 12,
  "end": 98.76
}
```

Simple parsing of those contents:

````
$ json2ecl foo.json
FOO_001_LAYOUT := RECORD
    UTF8 foo {XPATH('foo')};
    INTEGER start {XPATH('start')};
    REAL f_end {XPATH('end')};
END;
````

````
$ cat foo.json | json2ecl 
TOPLEVEL_231_001_LAYOUT := RECORD
    UTF8 foo {XPATH('foo')};
    INTEGER start {XPATH('start')};
    REAL f_end {XPATH('end')};
END;
````

## License

Apache 2.0
