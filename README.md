# minipat

Music pattern language + live coding environment

This project is a love letter to [Tidal](https://github.com/tidalcycles/Tidal/).
The pattern language is mostly the same, as are many of the combinators. It can
even play music using [SuperDirt](https://github.com/musikinformatik/SuperDirt)!

What does `minipat` add?

* Swappable backends - see how small `minipat-dirt` is!
* A live MIDI backend (`minipat-midi`)
* Patterns can be pretty-printed back into textual form.
* The pattern language has a plain old syntax tree (`Pat`) with standard
  functions for traversal and recursion (even through location annotations).
* Lots of tiny changes...

If you have `stack` installed, and you have `supercollider` running for use
with `tidal`, you should be able to run `bin/minipat dirt` to enter `ghci` with
everything set up:

    [Info] Initializing
    [Info] Handshaking ...
    [Info] ... handshake succeeded

    -- Play kick-snare in orbit 1
    > d1 $ s "bd sd"

The MIDI backend requies basically no setup (`bin/minipat midi`):

    -- Send some MIDI note on/off events
    > d1 $ n "c5 d6"

However, if you have different port settings for SuperDirt or want to use a non-default
MIDI output, you will probably have to edit the `Repl.ghci` files in either backend or
run `reallocate myBackendOptions myCoreOptions >>= initialize` in the REPL.

Please be aware that this is young software in an "it works for me" state!

## Editor integration

There is a Neovim plugin - see the [README](minipat-nvim/README.md) for installation
instructions.

## Contributions

Your help is very welcome. Some TODOs follow.

### TODO

* Implement swing
* Implement polymeters
* Implement chords/arps
* Add more MIDI channel voice events
* Support rendering to MIDI file (with `dahdit-midi`)
* Additional combinators like `arp, off, jux, |+, every, squiz, range`
* Backends for... Bitwig? Renoise?
* Backend with push/pull of textual patterns from DAW
* More meaningful `Pretty` subclasses for pattern rep or plain old logging
* More and better documentation
* Ensure that common exceptions have a useful `displayException`
* Ensure that live errors do/don't interrupt playback based on debug state
* More thoughtful handling of "continuous" streams/signals (including sampling rate)

## License

This project is BSD-licensed. It is a bottom-up semi-compatible rewrite of Tidal that uses certain
APIs, algorithms, names, types, and constants according to "fair use."
