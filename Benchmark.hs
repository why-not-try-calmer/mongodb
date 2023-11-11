import Control.Monad (forM_, void)
import Criterion.Main
import Data.Bson
  ( Document,
    Field (..),
    Javascript,
    Label,
    Val,
    Value (Bool, Doc, String),
    at,
    genObjectId,
    look,
    lookup,
    valueAt,
    (=:),
    (=?),
  )
import qualified Data.Text as T
import qualified Database.MongoDB as M
import Database.MongoDB.Query

main =
  defaultMain
    [ bgroup "insert" [bench "1000" $ nfIO doInserts]
    ]

doInserts = do
  let docs = (flip map) [0 .. 1000] $ \i ->
        ["name" M.=: (T.pack $ "name " ++ (show i))]

  pipe <- M.connect (M.host "127.0.0.1")

  forM_ docs $ \doc -> do
    void $ M.access pipe M.master "mongodb-haskell-test" $ M.insert "bigCollection" doc

  M.close pipe
