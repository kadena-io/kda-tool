# KDA Command Line Tool

A command line tool for automating all-things Kadena.

![Example Animation](https://i.imgur.com/By4KVir.gif)

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

`SigData` ... more recently evolved to `CommandSigData` from the signing API

#### Fully Signed

Pact's `Command Text`

```
{
  "hash": "H6XjdPHzMai2HLa3_yVkXfkFYMgA0bGfsB0kOsHAMuI",
  "sigs": [
    {
      "sig": "8d452109cc0439234c093b5e204a7428bc0a54f22704402492e027aaa9375a34c910d8a468a12746d0d29e9353f4a3fbebe920d63bcc7963853995db015d060f"
    }
  ],
  "cmd": "{\"payload\":{\"exec\":{\"data\":null,\"code\":\"(+ 1 2)\"}},\"signers\":[{\"pubKey\":\"368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca\"}],\"meta\":{\"gasLimit\":1000,\"chainId\":\"0\",\"gasPrice\":1.0e-2,\"sender\":\"368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca\"},\"nonce\":\"nonce-value\"}"
}
```
