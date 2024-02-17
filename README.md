# minipat

A mini pattern language for music, like Tidal

## TODO

* rotL, seqP, etc
* Finish Midi notes/chords/arps
* Strongly typed `Attrs`
* Implement polymeters
* Handle errors gracefully in `minipat-dirt` loops
* Additional combinators
  * `arp`
  * `off`
  * `jux`
  * `|+` etc
  * `every`
  * `squiz`
  * `range`


Even more combinators to implement:

```
-- seqP :: Seq (CycleTime, CycleTime, Stream a) -> Stream a
-- seqPLoop :: Seq (CycleTime, CycleTime, Stream a) -> Stream a
-- rotL, rotR
-- rev
-- swingBy
-- swing
-- cat
-- fast
-- fastBy
-- slowBy
-- slow
-- echo
-- off
-- cat
-- fastCat
-- timeCat
-- randCat
-- wrandCat
-- append
-- fastAppend
-- slowAppend
-- wedge
-- fromList
-- fastFromList
-- slowFromList
```
