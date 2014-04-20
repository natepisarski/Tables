{- |
   Module      :   Tables.Tables1
   Copyright   :   (c) 2014 by Nate Pisarski
   License     :   BSD3
   Maintainer  :   nathanpisarski@gmail.com
   Stability   :   Stable
   Portability :   Portable (Standalone - ghc)
   Database client for the Quill1 language. 
-}

module Tables.Tables1 {-# DEPREACTED "Use Tables2 instead. Due for removal June 1st, 2014." #-} where
import Cookbook.Essential.IO
import Cookbook.Project.Quill.Quill
import System.IO
import System.Environment
import Cookbook.Ingredients.Lists.Access
import Cookbook.Ingredients.Lists.Modify
import Cookbook.Essential.Continuous


both :: String -> (String,String)
both x = let (a:b:_) = splitOn x ':' in (a,b)

allThree :: String -> (String,String,String)
allThree x = let (a:b:c:_) = splitOn x ':' in (a,b,c)

tbOutp :: (String,String) -> String
tbOutp (a,b) = a ++ " : " ++ b

main = do
  (file:command:table:_) <- getArgs
  lines <- filelines file
  let allTables = tables lines

  case command of
    "create" -> createT file table
    "read" -> readT file table
    "delete" -> deleteT file table
    "list" -> mapM_ putStrLn $ map fst (tables lines)
    _ -> mapM_ putStrLn $ map show (tables lines)

--create :: String -> String -> IO ()
createT file tbl
  | count tbl ':' == 2 = updateTable file (before tbl ':') (both (after tbl ':')) -- create table1:item:value
  | otherwise = makeTable file tbl -- create table

--readT :: String -> String -> IO ()
readT file tbl
  | ':' `elem` tbl = readItem file (both tbl) -- read table1:item
  | otherwise = readTable file tbl --read table

--delete :: String -> String -> IO ()
deleteT file tbl
  | ':' `elem` tbl = deleteItem file (both tbl) -- delete table1:item
  | otherwise = deleteTable file tbl -- delete table1

-- Create command stack
--updateTable :: String -> String -> (String,String) -> IO ()
updateTable file table (lval,rval) =
  do --updateTable
    lines <- filelines file
    let allTables = tables (lines)
    putStrLn (show allTables)
    putStrLn (show (lval,rval))
    writeFile file $ unlines $ map tableToString $ addItem allTables table (lval,rval)

--makeTable :: String -> String -> IO ()
makeTable file table =
  do
    lines <- filelines file
    let allTables = tables lines
    writeFile file $ (unlines (map tableToString $ createTable allTables table))

-- Read command stack
--readItem :: String -> (String,String) -> IO ()
readItem file (table,item) =
  do
    lines <- filelines file
    let allTables = tables lines
    mapM_ putStrLn $ lookUp allTables (table,item)

--readTable :: String -> String -> IO ()
readTable file table =
  do
    lines <- filelines file
    let allTables = tables lines
    mapM_ putStrLn (map tbOutp $ getTable allTables table) --lol line 69

-- Delete command stack
--deleteItem :: String -> (String,String) -> IO ()
deleteItem file (tb,it) =
  do
    lines <- filelines file
    let allTables = tables lines
    writeFile file $ (unlines (map tableToString $ removeItem allTables (tb,it)))

--deleteTable :: String -> String -> IO ()
deleteTable file tb =
  do
    lines <- filelines file
    let allTables = tables lines
    writeFile file $ (unlines (map tableToString $ removeTable allTables tb)) 
