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

## Formats

### Key Formats

There are two key formats: plain and hd. 

#### Plain Key Format

The first is a plain ED25519 key format compatible with Pact which is a YAML
file with two fields: `public` and `secret`.

```
public: 40c1e2e86cc3974cc29b8953e127c1f31905665fcc98846ebf6c00cb8610a213
secret: badbadbadbadbadbadbadbadbadbadbadbadbadbadbadbadbadbadbadbadbade
```

#### HD Key Format

The second format is a 12-word recovery phrase compatible with Chainweaver HD
key generation. This format consists of all 12 words on a single line, each word
separated by a single space.

```
acid baby cage dad earn face gain hair ice jar keen lab
```

An arbitrary number of key pairs can be generated from a single 12-word recovery
phrase, each pair identified by an index which is a natural number.

### Transaction Formats

Need to find the right names for the transaction formats.

#### Template

#### Finalized

`CommandSigData` from the signing API

#### Fully Signed

Pact's `Command Text`
