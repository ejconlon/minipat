# Scratch

## Interactive debugging

```
import Control.Exception (throwIO)
let run n s = prettyPrintAll "\n" (tapeToList (streamRun (s :: Stream Ident) (Arc 0 (fromInteger n))))
let eval t = either throwIO pure (evalPat identP t) :: IO (Stream Ident)
```

```
ghci> eval "[x y]/2" >>= run 2
(0/1, 1/1) (0/1, 1/1) x
(3/2, 2/1) (1/1, 2/1) y
```
