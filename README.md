### SYNOPSIS

Building a Scheme JIT from scratch and experimenting with runtime
feedback based optimizations.

### BUILD

Make sure `Rust >= 1.5.0` is installed, and you are on a `x86_64 Linux`
or `x86_64 OSX` machine. Then:

    cd src/scheme && cargo build --release

### RUN

    cd src/scheme && ./target/release/scheme scheme-src/fibo-unsafe.ss

### NOMENCLATURE

No, I'm not referring to [Sanya](https://en.wikipedia.org/wiki/Sanya).

The name came from [サーニャ・V・リトヴャク](http://strikewitches.wikia.com/wiki/Sanya_V._Litvyak)
from [Strike Witches (youtube video, with sound)](https://www.youtube.com/watch?v=WUMCA_GqGwY).
