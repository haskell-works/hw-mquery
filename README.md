# hw-mquery
[![0.0-branch](https://circleci.com/gh/haskell-works/hw-mquery/tree/0.0-branch.svg?style=svg)](https://circleci.com/gh/haskell-works/hw-mquery/tree/0.0-branch)

```
import HaskellWorks.Data.MQuery
import qualified Data.DList as DL
!json <- loadJsonPartial "data/78mb.json"
let q = MQuery (DL.singleton json)
q >>= expandArray >>= expandObject
```

```
import HaskellWorks.Data.MQuery
import qualified Data.DList as DL
!json <- loadJsonPartial "data/78mb.json"
let q = MQuery (DL.singleton json)
:{
do
    j <- q
    e <- expandArray j
    (k, v) <- expandObject e
    return k
:}
```

```
import HaskellWorks.Data.MQuery
import qualified Data.DList as DL
!json <- loadJsonPartial "data/78mb.json"
let q = MQuery (DL.singleton json)
:{
do
    j <- q
    e <- expandArray j
    (k, v) <- expandObject e
    guard (k == "name")
    return v
:}
```
