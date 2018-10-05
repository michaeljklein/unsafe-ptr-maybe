# unsafe-ptr-maybe

`Maybe` implemented using `reallyUnsafePtrEquality`.

## In theory:

It allows using a boxed value's box as `Maybe` and thus saves
both time and memory usage.


## In practice:

There are bugs, go figure.


