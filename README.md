# hw-mquery
[![0.0-branch](https://circleci.com/gh/haskell-works/hw-mquery/tree/0.0-branch.svg?style=svg)](https://circleci.com/gh/haskell-works/hw-mquery/tree/0.0-branch)

```
import HaskellWorks.Data.LoadJson
import HaskellWorks.Data.Micro
import HaskellWorks.Data.MQuery
import Text.PrettyPrint.ANSI.Leijen
import qualified Data.DList as DL
import Data.Function
```

```
!json <- loadJsonPartial "data/78mb.json"
let q = MQuery (DL.singleton json)
putPretty $ q >>= expandArray & limit 10
```

```
!json <- loadJsonPartial "data/78mb.json"
let q = MQuery (DL.singleton json)
putPretty $ q >>= expandArray & page 10 1
```

```
!json <- loadJsonPartial "data/78mb.json"
let q = MQuery (DL.singleton json)
putPretty $ q >>= expandArray >>= expandObject
```

```
!json <- loadJsonPartial "data/78mb.json"
let q = MQuery (DL.singleton json)
:{
putPretty $ do
    j <- q
    e <- expandArray j
    (k, v) <- expandObject e
    return k
:}
```

```
!json <- loadJsonPartial "data/78mb.json"
let q = MQuery (DL.singleton json)
:{
putPretty $ do
    j <- q
    e <- expandArray j
    (k, v) <- expandObject e
    guard (k == "name")
    return v
:}
```

```
!json <- loadJsonWithIndex "data/78mb.json"
let q = MQuery (DL.singleton json)
putPretty $ q >>= expandArray >>= expandObject
```
