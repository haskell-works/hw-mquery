# hw-mquery
[![0.0-branch](https://circleci.com/gh/haskell-works/hw-mquery/tree/0.0-branch.svg?style=svg)](https://circleci.com/gh/haskell-works/hw-mquery/tree/0.0-branch)

```
import           Control.Monad
import qualified Data.DList as DL
import           Data.Function
import           Data.List
import           HaskellWorks.Data.LoadJson
import           HaskellWorks.Data.Micro
import           HaskellWorks.Data.MQuery
import           HaskellWorks.Data.Json.PartialValue
import           HaskellWorks.Data.Row
import           Text.PrettyPrint.ANSI.Leijen
```

```
!json <- loadJsonPartial "data/78mb.json"
!json <- loadJsonWithIndex "data/78mb.json"
!json <- loadJsonWithPoppy512Index "data/78mb.json"
let q = MQuery (DL.singleton json)
```

```
putPretty $ q >>= item & limit 10
putPretty $ q >>= item & page 10 1
putPretty $ q >>= item >>= hasKV "founded_year" (JsonPartialNumber 2005) & limit 10
putPretty $ q >>= item >>= entry
putPretty $ q >>= item >>= entry >>= named "name" & limit 10
putPretty $ q >>= (item >=> entry >=> key) & limit 10
putPretty $ q >>= item >>= entry >>= key & limit 100 & onList (uniq . sort)
putPretty $ (q >>= item >>= entry & limit 1) >>= field "name" & limit 10
putPretty $ do {j <- q; e <- item j; (k, v) <- entry e; return k}
putPretty $ do {j <- q; e <- item j; (k, v) <- entry e; guard (k == "name"); return v}
```
