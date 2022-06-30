# KDA Command Line Tool

A command line tool for automating all-things Kadena.

## Requirements

### Shell Composability

This tool's subcommands should be designed in a way that they are easily
composable using standard shell facilities such as pipes, redirecting to
files, etc.

### No sensitive data on the command line.

This includes private keys, seed phrases, etc.  These pieces of sensitive
information should never be typed on the command line to prevent accidental
exposure via shell history logs.

Early on in this tool's evolution it probably won't do anything involving
signing.  There are already other tools for this such as the `pact` command
line tool, [keychain](https://github.com/kadena-community/keychain), etc.
However, this tool could easily evolve to include signing capabilities and in
that case data protections will be important.
