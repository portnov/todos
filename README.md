todos README
============
Ilya V. Portnov <portnov84@rambler.ru>

todos is a simple TODO manager. TODO records theirself are described in
plain-text file, and todos allows you to show only needed of them. So, todos
works as specialized `grep' utility.

By default, output format is the same as input; so, you can combine several
`todos' instances into unix pipes. 

If source files are not given in the commandline, file with name "TODO" in
current directory is used. If `-` is given as file name, todos will read
standart input.

todos supports two formats of input files. In first case, file contains TODOS
records and only them. In «alternative» format, source files can contain any
text, but it's ignored; only lines starting with given prefix (`TODO: ` by
default) are parsed. So, alternative format enable you to grep TODO records
from any files, for example, program source files.

Following is format of TODO record:

    [spaces]status (dates) [TAGS] title (depends) description

Here [spaces] is optional indent with spaces, «status» — is status of record
(for example, URGENT or DONE; can be any word), «dates» give info about dates
(see below), «TAGS» is a comma-separated list of tags in brackets, «title» is
title of record, «depends» — comma-separated list of depends (titles of other
records) in parenthesis, «description» is description of the record. All fields
except status and title are optional. If `-w` option is specified, status field
is not read (it's optional; when it's present, it will be part of title).
Description is separated from title with not less than 2 spaces. 

Using different indents one can create sub-records (and, so, trees of records).
Moreover, depends are interpreted in same way as sub-records. So, one can
create not only trees of records, but arbitrary graphs of records (even
cycled). When cycled graphs are used, user should control for rational level of
hierarchy (for example, one can bound height of output tree using -p option).

Dates information is described in parenthesis, as not more than 3 records,
separated with semicolon. Each records has format `date type: date`. Date type
can be one of: start, end, deadline. For dates, many formats are supported; for
example, `18 Feb`, or `02/18/2010`, `2010/02/18` are all valid dates. One can
even use relative dates specification here, such as `in 2 weeks` or `2 days
ago`, but there is no point to this: all dates are counted from program start
date.

The following commandline options are supported:

    -1           --only-first              show only first matching entry
    -c           --color                   show colored output
    -H           --highlight               instead of filtering TODOs, just highlight matching the query
    -I           --show-ids                show IDs of todos
    -A[PREFIX]   --prefix[=PREFIX]         use alternate parser: read only lines starting with PREFIX
                 --dot                     output entries in DOT (graphviz) format
    -D FORMAT    --format=FORMAT           use FORMAT to format items
    -k[STRING]   --indent-with[=STRING]    use STRING instead of two spaces for items indentation
    -w           --no-status               do not read status field from TODOs
                 --set-status=STRING       force all TODOs status to be equal to STRING
                 --set-root-status=STRING  force statuses of root TODOs to be equal to STRING
    -F           --by-file                 group TODOs by source file
    -T           --by-tag                  group TODOs by tag
    -Z           --by-status               group TODOs by status
    -p N         --prune=N                 limit tree height to N
    -m N         --min-depth=N             show first N levels of tree unconditionally
    -t TAG       --tag=TAG                 find items marked with TAG
    -g PATTERN   --grep=PATTERN            find items with PATTERN in name
    -G PATTERN   --description=PATTERN     find items with PATTERN in description
    -s STRING    --status=STRING           find items with status equal to STRING
    -i STRING    --id=STRING               find items with ID equal to STRING
    -a           --and                     logical AND
    -o           --or                      logical OR
    -n           --not                     logical NOT
                 --sort=FIELD              specify sorting
    -e[COMMAND]  --exec[=COMMAND]          run COMMAND on each matching entry
    -S DATE      --start-date=DATE         find items with start date bounded with DATE
    -E DATE      --end-date=DATE           find items with end date bounded with DATE
    -d DATE      --deadline=DATE           find items with deadline bounded with DATE
    -h           --help                    display this help

Dates in the commandline are specified in any of supported formats. There
relative dates specifications are useful: how about something like `todos
--deadline=yesterday`? ;)

When `-H` option is specified, items are not filtered at all (all items will be
printed). Instead, items matching to your query will be highlighted. This works
only with `-c` option.

When `-I` option is specified, for each TODO it's unique identifier is shown.
That ID is simply hash  from item's title, status, tags and description, so
only identical items will have identical ID. One can ask todos to show item
with specified ID using `-i` option.

With `-k` option one can specify another indentation string instead of default
two spaces. For example, `todos -k"| "` will print vertical lines between items
of same level. `-k` option without argument will force todos to not use
indentation at all.

For `--format` and `--exec` options, printf-style format strings should be
specified. Following substitution sequences are supported:

<dl>
  <dt>%n</dt>
  <dd>title of the record</dd>
  <dt>%t</dt>
  <dd>list of record's tags</dd>
  <dt>%s</dt>
  <dd>status of the record</dd>
  <dt>%p</dt>
  <dd>prefix before record in source file</dd>
  <dt>%d</dt>
  <dd>description of the record</dd>
  <dt>%f</dt>
  <dd>name of file, in which record was found</dd>
  <dt>%l</dt>
  <dd>number of line in source file</dd>
  <dt>%D</dt>
  <dd>dates of item</dd>
</dl>

Backslash-escaping is supported too. Supported sequences are: `\t \n \v \b`.

For example, `todos -tBUG -e"vi %f +%l"` command for each record with «BUG» tag
will open vi with corresponding file on corresponding line.

When `--dot` option is specified, todos will output a graph definition for DOT
(graphviz), instead of normal output. For example, you can try

    todos --dot | dot -Tpng -o todos.png

and then see the file todos.png. Items with status «GROUP» will become DOT's
clusters. More precisely, if one item have «GROUP» status, it will become a
cluster, and all it's children will be inside that cluster. There is a
limitation in DOT: it can not draw graphs where one item is in several clusters
properly, it will draw such item only in one cluster.

todos can read configuration files: `~/.config/todos/todos.conf` («global») and
`./.todos.conf` («local»). Also, if there is not `./.todos.conf`, todos looks
for file named `.todos.conf` in all ancestor directories. For example, if
current directory is `/home/user/work/projects`, these files are checked:

  * `/home/user/work/projects/.todos.conf`
  * `/home/user/work/.todos.conf`
  * `/home/user/.todos.conf`
  * `/home/.todos.conf`
  * `/.todos.conf`

If one of these files is found, todos checks if it starts with `%` (single
percent character) word. If so, options from «ancestor» config file are
prepended to options from this file. Otherwise, ancestor config is not read.

Config files contain command line options, in the same format as in the
commandline. Resulting commandline is composed as: (options in global config)
+ (options in local config) + (options in the actual command line). If there is
no one of configs, todos will ignore it.

For example, if  `/home/user/work/projects/.todos.conf` file contents is `% --dot`,
and `/home/user/work/.todos.conf` contains `-A -F`, «effective command line» would be
`-A -F --dot`.

Config files might be specified in the command line. `@path/to/file.conf`
option will force todos to read options from `path/to/file.conf`, and add them
to «effective command line» before other command line options.
`@@path/to/file.conf` is almost the same, but here todos will not read «global»
and «local» configs at all. Specifying only `@@` in the command line, one can
disable reading config files at all.

Moreover, todos could be customized in «xmonad way». You write file
`~/.config/todos/todos.hs` and run todos. todos will compile and run that file.
Simplest example of `todos.hs` possible is `todos.hs` in this directory. A bit
more complex example:

```
    import System.Console.ANSI

    import Todos

    main :: IO ()
    main = todos $ defaultConfig {
                      itemConsoleColor = myItemColor
                   }

    myItemColor item =
      if itemStatus item == ":"
         then Just (Dull, Blue)
         else Nothing
```

So, items with `:` status will be printed in blue color.

