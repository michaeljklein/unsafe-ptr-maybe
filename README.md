# unsafe-ptr-maybe

`Maybe` implemented using `reallyUnsafePtrEquality`.

### In theory:

It allows using a boxed value's box as `Maybe` and thus saves
both time and memory usage.


### In practice:

There are bugs, go figure.


## Tests

Tests currently pass, but simple examples fail in `GHCi`.

Run tests with:

```bash
stack build
stack exec -- unsafe-ptr-maybe-exe
```


# Docs

Haddock generated documentation may be found [here](https://michaeljklein.github.io/unsafe-ptr-maybe/)

