# Bigchaindb-hs

[BigchainDB](https://github.com/bigchaindb/bigchaindb) client tools for creating and signing transactions.

Achtung! Work in progress! Much of the below may already be wrong!

## Building

Install [Stack](https://docs.haskellstack.org/en/stable/README/). This will take care of installing GHC for you. Then run: `stack build`.

To build shared object:

`make so`


## Testing

Run `stack test`.

### Autocompletion for CLI

The CLI supports autocomplete, `bigchaindb-hs` binary must be on the $PATH.

To try it, run:

```source <(bigchaindb-hs --bash-completion-script `which bigchaindb-hs`)````
