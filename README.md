# minipat

Music pattern language + live coding environment

This project is a love letter to [Tidal](https://github.com/tidalcycles/Tidal/).
The pattern language is mostly the same, as are many of the combinators. It can
even play music using [SuperDirt](https://github.com/musikinformatik/SuperDirt)!

What does `minipat` add?

* Swappable backends - see how small `minipat-dirt` is!
* Patterns can be pretty-printed back into textual form.
* The pattern language has a plain old syntax tree (`Pat`) with standard
  functions for traversal and recursion (even through location annotations).
* Lots of tiny changes...

If you have `stack` installed, and you have `supercollider` running for use
with `tidal`, you should be able to run `bin/dirt-ghci` to enter `ghci` with
everything set up:

    [Info] Initializing
    [Info] Handshaking ...
    [Info] ... handshake succeeded
    Loaded GHCi configuration from BootDirt.hs

    -- Play kick-snare in orbit 1
    > d1 $ s "bd sd"

## TODO

* Finish midi notes/chords/arps
* Implement polymeters
* Add documentation
* Additional combinators like `arp, off, jux, |+, every, squiz, range`
* Backends for... Plain old MIDI? Renoise?
* Backend with push/pull of textual patterns
* More meaningful `Pretty` subclasses for pattern rep or plain old logging
