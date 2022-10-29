# KDA Command Line Tool

A command line tool for automating all-things Kadena.

![Example Animation](https://s1.gifyu.com/images/kda-tool-demo3.gif)

## Features

* Generate keys
* Construct transactions across multiple chains using transaction templates
* Conveniently create transactions from personalized templates stored in a configurable public GitHub repo
* Sign transactions with both plain ED25519 key pairs or with Chainweaver-compatible HD keys
* Sign transactions using the Kadena wallet signing API
* Sign transactions with your local chainweaver keys directly by entering your password
* Easily test, send, and poll results on the blockchain for multiple transactions
* Other basic operations for interacting with nodes

# Using Kda-tool

## Getting Help

Kda-tool has bult-in help facilities that give information about available
commands and options. The command `kda -h` or `kda --help` will give you a
top-level summary of available commands. Adding `-h` or `--help` to any partial
command will give you help for that command.

## Transaction Construction & Templating

Kda-tool uses mustache templates to let you quickly and easily construct
transactions for multiple chains and many other access patterns. It provides the
`gen` command for doing this.

This command takes a transaction template file (usually denoted with a `.ktpl`
extension) and fills the template with user-supplied values. To illustrate,
let's look at a concrete example template for a simple transfer. Consider the
following file we'll call `transfer.ktpl`:

### Generating a single transaction

```
code: |-
  (coin.transfer "{{{from-acct}}}" "{{{to-acct}}}" {{amount}})
data:
meta:
  chainId: "{{chain}}"
  sender: "{{{from-acct}}}"
  gasLimit: 2300
  gasPrice: 0.000001
  ttl: 600
networkId: testnet04
signers:
  - public: badbadbadbadbadbadbadbadbadbadbadbadbadbadbadbadbadbadbadbadbadb
    caps:
      - name: "coin.TRANSFER"
        args: ["{{{from-acct}}}", "{{{to-acct}}}", {{amount}}]
      - name: "coin.GAS"
        args: []
type: exec
```

You can get a list of all the values this template needs with the `--holes`
option which prints the following:

```
$ kda gen -t transfer.ktpl --holes
amount: null
chain: null
from-acct: null
to-acct: null
```

We can save this to a file with `kda gen -t transfer.ktpl --holes > data.yaml`.
Then we can edit this file with the following values:

```
amount: 1.5
chain: 0
from-acct: alice
to-acct: bob
```

Then we can generate a transaction from this template as follows:

```
$ kda gen -t transfer.ktpl -d data.yaml
Wrote commands to: ["tx.yaml"]
```

The file `tx.yaml` contains a finalized transaction that has no signatures and
will need to be signed before it can be submitted to the blockchain. See the
next section "Transaction Signing" to see how you can use kda-tool to sign this
transaction.

If you do not supply `-d`, kda-tool will prompt you to enter all the necessary
information.

### Generating multiple transactions

We can use the same `transfer.ktpl` template to generate transactions on
multiple chains by making a simple change to the `data.yaml` file:

```
amount: 1.5
chain: [0,1,2]
from-acct: alice
to-acct: bob
```

All the data is the same as before except we changed the `chain` field to a
list. When kda-tool sees a list it generates a separate transaction using each
value in the list:

```
$ kda gen -t transfer.ktpl -d data.yaml
Wrote commands to: ["tx-0.yaml","tx-1.yaml","tx-2.yaml"]
```

These three files contain transactions for transferring 1.5 KDA from alice to
bob on chains 0, 1, and 2.

You can use lists for any field. For example, we could use the following to
transfer 0.1 KDA from alice to each of bob, carol, and dave:

```
amount: 0.1
chain: 0
from-acct: alice
to-acct: [bob, carol, dave]
```

Generating using this data gives something slightly different:

```
$ kda gen -t transfer.ktpl -d data.yaml
Wrote commands to: ["tx-bob.yaml","tx-carol.yaml","tx-dave.yaml"]
```

The filename is `tx-` followed by the value of the field you're looping over.
You can set your own filename scheme using the `-o` option and a
mustache-templated filename as follows:

```
$ kda gen -t transfer.ktpl -d data.yaml -o foo_{{to-acct}}_bar.yaml
Wrote commands to: ["foo_bob_bar.yaml","foo_carol_bar.yaml","foo_dave_bar.yaml"]
```

If you want to transfer a different amount on each chain, you can also put a
list in the amount field as follows:

```
amount: ["1.0","2.0","3.0"]
chain: [0,1,2]
from-acct: alice
to-acct: bob
```

The two lists must be the same length otherwise kda-tool will throw an error.
Note that the amounts are strings because YAML does not distinguish between
integer and decimal but Pact does. This will generate three separate
transactions transferring 1 KDA on chain 0, 2 KDA on chain 1, and 3 KDA on chain
2.

### Generating from predefined GitHub templates

The `gen` command's `-t` option reads te template from a file on your local
machine, but kda-tool has built-in support for predefined templates stored on
GitHub with the `-g` option. For example, `kda gen -g transfer` will generate a
transaction using a template called `transfer.ktpl` on GitHub. By default,
kda-tool looks in the repository `kadena-io/txlib` to find the transaction
templates, but you can specify your own transaction repository using the `-r`
option.

## Config File

By default the `kda gen` command looks for transaction templates in the
`kadena-io/txlib` GitHub repo. You can configure kda-tool to use your own
transaction repos by creating a config file. The default location for the config
file is `$HOME/.config/kda/config.json`. You can also pass the `-c` option to
use your own config file stored somewhere else. Here is an example config file:

```
{
  "tx-repos": [
    "blockchaindev/my-marmalade-templates",
    "blockchaindev/txlib",
    "my-favorite-dex/txlib",
    "kadena-io/txlib"
  ]
}
```

Each repo is tried in the order they appear in the config file, stopping after
the first one that works. This allows projects building on Kadena to publish
libraries of transation templates for working with their smart contracts and for
users to use any combination of template sets that they desire.

## Transaction Signing

### Signing with key files

The `kda gen` command generates transactions and outputs them in a YAML format
suitable for adding signatures. Once you have generated transactions to be
signed you can use the `kda sign` command to add signatures to these files.

In the above example of multiple transactions generated for chains 0, 1, and 2,
we can sign all those transactions with a single command:

```
kda sign -k my-key.kda tx-*.yaml
```

In this example, `my-key.kda` is a file with a plain ED25519 key pair in the
format generated by `pact -g` or `kda keygen plain`. You can also use a file
containing a Chainweaver-compatible HD recovery phrase with the same option:

```
kda keygen hd > my-hd-key.phrase
kda sign -k my-hd-key.phrase tx-*.yaml
```

HD recovery phrases generate an arbitrary number of keys and each one is
identified by an index. Chainweaver will automatically try the first 100 keys
for you and signatures will be generated for any of the matching keys. If you
want to limit it to signing with only one key or use a key with an index > 100,
then you can use the `-i` option to specify an HD key index.

### Signing with Chainweaver keys

If you already have desktop Chainweaver installed and want to sign transactions
with keys stored in Chainweaver, you can use `kda sign --chainweaver`. You will
be prompted to enter your Chainweaver password to decrypt the keys before
signing.

### Signing with other wallets and the signing API

The `kda wallet-sign` comamnd signs transactions using any wallet that supports
the Kadena signing API (such as Chainweaver or Zelcore). By default, kda-tool
will use the quicksign signing method that supports signing multiple
transactions in a single signing request. If your wallet does not support
quicksign, you can use the `--old` option to use the old single-transaction
signing method. You can still sign multiple transactions with this method, but
each transaction will require a separate approval.

### Multi-sig signing

Kadena has built-in support for multi-sig. Kda-tool's signing functions handle
this as follows. Signing happens with YAML transaction format designed for
easily accumulating signatures. You supply YAML files to any of the kda-tool
signing commands. If the transaction still needs more signatures after you sign,
your signatures are added to the input YAML files and you can pass them on to
other parties for more signatures. If the transaction is fully signed, kda-tool
will save the resulting transaction in a `.json` file in a format that can be
submitted directly to the blockchain.

### Combining signatures

There are two different patterns of signing multi-sig transactions: sequential
and parallel. With the sequential method, after each signature added the new
file with the additional signature is passed to the next party for signing. As
mentioned above, when the transaction is fully signing, kda-tool will output a
`.json` file that can be submitted to the blockchain.

  kda gen
+----------+   alice sign    +------+   bob sign    +------+
| unsigned | --------------> | yaml | ------------> | json |
+----------+                 +------+               +------+

However, the parallel method looks like this:

+----------+   alice sign    +------+
| unsigned | --------------> | yaml |
+----------+                 +------+

+----------+    bob sign     +------+
| unsigned | --------------> | yaml |
+----------+                 +------+

If you use the parallel method, you'll end up with multiple partially-signed
YAML files. The `kda combine-sigs` command lets you combine multiple partially
signed files. It is very smart. All you have to do is give it a list of
transaction files--any transaction files for any number of different
transactions across different networks--and it will group them all by network
and transaction, combine any signatures that were found, and write JSON or YAML
files depending on whether the transaction is fully or partially signed. Just
throw in whatever partially signed transactions you have and combine-sigs will
do the right thing.

# Kda-tool Data Formats

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

#### Chainweaver on-disk format

Kda-tool also understands the on-disk key storage format used by desktop
Chainweaver. This is an encrypted format and kda-tool will prompt you to enter
your Chainweaver password to decrypt the keys.

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
