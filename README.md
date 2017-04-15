# html2code

A simple tool to convert HTML to EDSL source code.

## Installation

1. Get `stack` from https://haskellstack.org/
2. Clone the html2code repo: `git clone https://github.com/tdammers/html2code`
3. In the html2code directory, say `stack setup` to install a suitable compiler
   and build toolchain (in case you don't have it yet).
4. Say `stack install` to build and install the thing.
5. Make sure you have `~/.local/bin/` on your `$PATH` so that your shell can
   find the binary.

If you want to install html2code system-wide, something like this should do the
trick:

    sudo install ~/.local/bin/html2code /usr/local/bin/

## Usage

    # generate PureScript code for Halogen:
    html2code --halogen <input.html >output.purs

    # generate JavaScript code for HyperScript:
    html2code --hyperscript <input.html >output.js

CLI argument support is very limited right now; html2code always reads HTML
from stdin, outputs code on stdout, and issues warnings on stderr. One of
`--halogen` or `--hyperscript` must be provided to select the output language.

## License

This is free software, released under the terms of the 3-Clause BSD License.
See the enclosed `LICENSE` file for details.

Copyright (c) 2017 Tobias Dammers
