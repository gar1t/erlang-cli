<style>
pre { background-color: #e6e6e6; padding: 6px; }
</style>

# Goals

- Support for full featured command line interfaces (CLIs) in Erlang
- Trivially embeddable library, not a framework
- Consistent with mainstream POSIX utility conventions
- Support for commands with sub-parsers

# Design notes

The following is a collection of design notes inspired by various
publicly available sources, which are mentioned where applicable.

## Notes from The Open Group Base Specifications Issue 7

See the
[Utility Conventions](http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap12.html#tag_12)
chapter.

Note that *12.2 Utility Syntax Guidelines* in the above documentation
provides a number of guidelines. These are supported by *erlang-cli*
(i.e. they are not prohibited either technically or by convention) but
they are not strictly enforced. The one exception is guideline 9: "All
options should precede operands on the command line" which is enforced
currently (see notes below)

Options are always listed before positional arguments. This conforms
to the Utility Argument Syntax. However, it diverges from Python's
argparse, which allows options to be specified after positional
arguments. This decision is opinionated and controversial as it's
technically possible to differentiate between positional arguments and
options. For the time being however it's made for simplicity and
conformance to the POSIX guidelines.

This rule will have to be relaxed in the case of commands, which
represent sub-programs and should have their own set of options and
positional arguments.

Options may have either short forms, long forms, or both short and
long forms. Options that do not have arguments may be specified
together using their short form as a single argument.

The POSIX guidelines provides for two methods of showing command
usage. The first lists options together, as in:

    utility_name [-abcDxyz][-p arg][operand]

The second uses a catch all `[options]` placeholder, as in:

    utility_name [options][operands]

As a simplifying step *erlang-cli* will use the second form for
options, relying on the full help content below the usage
prologue. This is also controversial, especially for very simple
programs. It may be desirable to parameterize this behavior.

Option arguments and positional arguments that are optional (i.e. are
not required) are specified in usage or help content using surrounding
square brackets '[' and ']'.

Ellipses "..." are used to indicate that the preceding option or
positional argument may be repeated.

## Argument types and validation

If an option argument or positional argument can be converted to a
number, it will be. Both integers and floats will be supported. A
parser however may modify this behavior to force values to be parsed
as text.

*erlang-cli* should support a reasonably rich set of validation rules
to ensure that parsed values may be used directly without further
validation. This must include ultimately the ability to apply a
function to raw input to both validate and generate values as needed.

Usage and help text must be wrapped where possible. Wrapping rules are
yet to be defined.

## API conventions

*erlang-cli* makes use of internal types but does not expose those to
users.

Required arguments are specified as the initial arguments for a
function. Optional arguments are either specified by using functions
with higher arity (i.e. they accept more arguments) or as proplists.

Results follow the Erlang conventions of using tagged two-tuples for
both success and error results, or untagged values and exceptions.

Optional arguments lists are specified using proplists rather than maps.

Resulting lists of parsed and validated arguments use proplists rather
than maps.

## Usage and help

*erlang-cli* will provide reasonable default formatting for usage and
help text. As stated in goals, *erlang-cli* will produce output that
is consistent with POSIX usage/synopsis guidelines.

*erlang-cli* will support custom usage and help formatting and content
where reasonable.

Some of the POSIX utilities used to establish formatting guidelines include:

- ls
- cp
- sed
- grep
- mkdir
- tar

The observations below generally hold true for these utilities.

Help text starts with single line usage examples:

    Usage: <program> <options-spec> <positional-args-spec>

The word usage is always capitalized.

Some utilities list multiple usage patterns. This is arguably clearer
than providing complex syntax for argument configurations in a single
usage example. It's not clear how multiple usage examples might be
implemented in *erlang-cli*.

Immediately following the usage line is usually a single line
describing the command. The single line description is a complete
sentence - i.e. ends with a period. Additional descriptive paragraphs
may follow, though it seems by convention that the "front matter" of
utility help is kept to a minimum and detailed help is provided at the
end of the options list.

Help text appears to always be wrap at column 79.

After the command description, there's a list of options.

Options are listed with their short and/or long form, followed by
spaces up to column 30, where the option description begins. If an
option description needs to wrap, it is indented on the next line at
column 30.

Option short and long forms are formatted as follows:

    '  ' [short_form] [', '] [long_form]

where at least one of `short_form` or `long_form` appears and `', '`
appears when both `short_form` and `long_form` appear.

It's common to see two options, which appear at the end of the list of
options and only in long form:

    --help     display this help and exit
    --version  output version information and exit

None of these utilities support commands.

Commands however may be supported in a separate list using a similar
format to options.

## Formatting option help

There are a few schemes in practice for formatting option help.

Nearly all commands represent an option using the grammar listed
above. E.g. '-f/--force' would appear as:

      -f, --force

The help text formatting however differs across commands.

Some formats maintain a left margin starting at col 31. E.g.

      -f, --force                  do not prompt before overwriting

    01234567890123456789012345678901234567890123456789012345678901234567890123456789
    0         1         2         3         4         5         6         7

Long lines (past col 79) are wrapped, either at col 31 or col 33
(i.e. a two space indent on subsequent lines). E.g.

      -u, --update                 move only when the SOURCE file is newer
                                     than the destination file or when the
                                     destination file is missing

If an option name would extend into the help text, the help text is
shifted accordingly to ensure two spaces between the itself and the
name. E.g.

      -t, --target-directory=DIRECTORY  move all SOURCE arguments into DIRECTORY

Some commands use column alignment that's different from 30, though
they are less common.

Some commands, e.g. sed, display help text on a new line following the
option name.

In the interest of consistency with the majority of standard POSIX
commands, we will adopt these rules:

- Options names are displayed using the standard '-h, --help' style
- Option help text will begin on the same line as the name starting at
  column COL
- Long option help text will be wrapped to subsequent lines as needed
  with each line starting at column COL

COL may be 31 or some lower number. Ideally it would be configurable.

Subsequent lines of help text will not be indented by two spaces, as
is a common practice for commands. This added level of indentation
doesn't seem to help readability while only adding irregularity.

Here's an example from `tar`:

      -k, --keep-old-files       don't replace existing files when extracting,
                                 treat them as errors
          --keep-directory-symlink   preserve existing symlinks to directories when
                                 extracting
          --keep-newer-files     don't replace existing files that are newer than
                                 their archive copies

    01234567890123456789012345678901234567890123456789012345678901234567890123456789
    0         1         2         3         4         5         6         7

## Help for positional arguments

The help output for the most utilities does not include specific help
for positional arguments. Instead, positional arguments are
described in general help text.

This doesn't make a lot of sense I think - if options have help text I
think so too should positional arguments.

Python's argparser provides help text for each positional argument.

Python's Click framework has this to say:

> Arguments cannot be documented this way. This is to follow the
> general convention of Unix tools of using arguments for only the
> most necessary things and to document them in the introduction text
> by referring to them by name.

Their observation is correct, however it seems odd to deliberately
remove a feature that would appear to provide additional information.

I'm tempted to provide argument help text under an "Arguments" heading
(or possibly no heading at all), but let users optionally hide that
section to conform with the POSIX convention (note I haven't seen this
documented explicitly, but it's certainly the convention).

For example:

    Usage: hello [OPTION]... MSG
    Prints a message to the console.

      MSG             message to print

    Options:

      -C, --caps      print message in caps

Alternatively, to conform to the POSIX convention, the parser can be
created with a `hide_arg_help` flag, in which case help text like this
can be generated:

    Usage: hello [OPTION]... MSG
    Prints MSG to the console.

    Options:

      -C, --caps      print message in caps

This is arguably better -- it's shorter and more natural. And indeed
if the list of arguments is short, as it should be, this form of
documentation is superior.

We can decide which default behavior is preferable and name the parser
option accordingly (i.e. show_arg_help vs hide_arg_help).

## Help text width

The standard in all of the above utilities is to wrap text at
col 79. No attempt is made to wrap according to the terminal width.

Here are some reasons why this is a reasonable approach:

- Getting the correct terminal width is not always trivial - it
  introduces moving parts that can fail
- Anyone using a terminal width of less than 80 will be suffering anyway
- Text wrapping past 79 is harder to read

## Argument names

Rather than infer an argument name, `erlang-cli` will require the name
up front when defining the argument. If the argument is an option,
sensible defaults will be used for the option name. This shifts the
emphasis away from the user interface to the data structure.

This is the simplest argument definition:

    cli:arg(foo, "a positional arg")

An option may be specified using additional argument options:

    cli:arg(bar, "an option", [option])

Alternatively:

    cli:option(bar, "an option")

By default an option may be specified using its long form, which is
`"--" + NAME`, where `NAME` is the argument atom converted to a
string, substituting underscores (`_`) with hyphens (`-`).

In the example above, the value for `bar` may be specified using
`--bar VALUE` or `--bar=VALUE`.

Option names may be explicitly provided using the `name` option:

    cli:arg(bar, "an option", [option, {name, "-b, --bar"}])
    cli:option(bar, "an option", [{name, "-b, --bar"})

or alternatively with a two-tuple for `option`:

    cli:arg(bar, "an option", [{option, "-b, --bar"}])

## Optional vs required values

Positional arguments may be required or not. In one respect, an
optional positional argument could be considered an option. It may
however be more natural to use a positional. For example, a missing
FILE argument is often a signal to read from standard input.

An option may in turn have an optional value. Or, in other words, the
option will use a sensible default value if one isn't provided by the
user.

The question: how do we indicate whether or not an argument value is
required or optional?

One of the problems with 'required' and 'optional' is that the concept
collides with a similar distinction presented by 'positional argument'
and 'option'. A positional argument is generally a value that is
expected from the user, which an option is not.

What we're talking about here is the nature of the _value_ I think.

For a positional argument we should presume that the user _must_
specify a value, unless otherwise specified. For example, this
positional argument should be required:

    cli:arg(file, "file to read; use '-' for stdin")

However, if a default is provided, we can use that if the user omits
the value:

    cli:arg(file, "file to read; use '-' to for stdin (default)",
            [{default, "-"}])

In the first case, help text would be something like this:

    Usage: myprog FILE

In the second case:

    Usage: myprog [FILE]

What about options? I think the same scheme applies. Here's an option
that expects a value:

    cli:arg(file, "file to read", [option])

and one whose value may be omitted:

    cli:arg(file, "file to read", [option, {default, "-"}])

The help for the first:

          --file=VALUE         file to read

and for the second:

          --file[=VALUE]       file to read

Flags then become syntactic sugar for bool options with default of
false. So these are equivalent:

    cli:arg(caps, "print in caps", [flag])

    cli:arg(caps, "print in caps", [option, {type, bool}, {default, false}])

UPDATE: This approach isn't going to work for obvious reasons. Options
may not be specified and therefore a default value suggests the value
for the arg when the option is not provided. It only works for
positional arguments.

"Value argument" can be used however. These values apply:

- optional
- required
- none

This is similar to `narg` from Python's argparser, but applies to zero
or one arguments rather than zero or more.

## Data validation

I'm inclined to not validate or otherwise interpret values and simply
parsed the arguments and provide values as strings. If an option
doesn't have a value, it appears in the list of options without a
value (as is the standard for Erlang proplists).

## Commands and subparsers

NOTE: Support for commands will be added once the base/root
functionality is implemented.

Each parser must support optional *commands*, each of which is
associated with another parser. This allows a single command line
utility to perform multiple actions, each corresponding to a command
name.

Commands may not be nested. This is controversial, but is a
simplifying decision for now. Developers should consider using command
name spaces, separating tier with hyphens. For example, if a nested
command might be this:

    mycli foo list [OPTION]... [ARG]...
    mycli foo create [OPTION]... [ARG]...
    mycli foo delete [OPTION]... [ARG]...

Consider a flattened command structure:

    mycli foo-list [OPTION]... [ARG]...
    mycli foo-create [OPTION]... [ARG]...
    mycli foo-delete [OPTION]... [ARG]...

Common options and position arguments may be duplicated as needed
using macros, variables, functions - or any combination thereof!

# Roadmap ideas

- Commands
- Provide default argument values using environment variables
- User prompts
- Autocomplete
- Manpage integration
- Plain text documentation (i.e. storing help text in a formatted
  plain text file)
- Option sections (see `tar --help`)

# Best practices

Command and option help should not be capitalized, end with period, or
otherwise be treated as complete sentences. They are short descriptive
blurbs. Use semi-colons to add additional blurb.

# Glossary

**Argument**
: An element in the array of arguments passed to a program

**Command**
: An single positional argument that represent a command to execute
and indicates that a sub-parser should be used handle subsequent
arguments

**Option**
: An argument consisting of one or two leading hyphen characters
followed by letters or digits

**Option Argument**
: An argument immediately following an option

**Parser**
: A representation of command line parser that can be used to process
raw command line arguments

**Positional Argument**
: An argument that follows the last option or option argument

**Required Argument**
: A positional argument that must be provided

**Required Option Argument**
: An option argument that must be provided

**Short Form Option**
: An option consisting of a single leading hyphen followed by a single
character or digit

**Long Form Option**
: An option consisting of two leading hyphens followed by one or more
characters or digits

# To Do

- `--help` and `--version` options are hard coded, and this isn't
  good - provide a flag associated with the parser to disable
  either/both (maybe)

- There's no built-in indication that "COMMAND --help" is available;
  the user can provide that cue in the usage, but it's an added burden
  for something that's always available

- Using prettypr:text_par removes all spacing from help text. This is
  right in some cases but we need to support an explicit "don't
  reformat" symbol, maybe appearing at the start of a par - in which
  case we use prettpr:text, which preserves formatting.
