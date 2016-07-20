# hw-mquery
[![0.0-branch](https://circleci.com/gh/haskell-works/hw-mquery/tree/0.0-branch.svg?style=svg)](https://circleci.com/gh/haskell-works/hw-mquery/tree/0.0-branch)

```
λ> import HaskellWorks.Data.MQuery
λ> import qualified Data.DList as DL
λ> !json <- loadJsonPartial "data/78mb.json"
λ> let q = MQuery (DL.singleton json)
λ> q >>= expandArray >>= expandObject
```
