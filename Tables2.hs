{- |
   Module      :   Tables.Tables2
   Copyright   :   (c) 2014 by Nate Pisarski
   License     :   BSD3
   Maintainer  :   nathanpisarski@gmail.com
   Stability   :   Stable
   Portability :   Portable (Standalone - ghc)
   Database client for the Quill2 database language. Use this, as Tables1 will be deprecated on June 1st, 2014.
-}

module Tables.Tables2 where

import qualified Cookbook.Project.Quill.Quill2.Meta as Qm
import qualified Cookbook.Essential.Meta as Cm

import System.IO
import System.Environment

allOptions = ["add: add [table|list] name [name|(name,item)]",
              "remove: remove [name|(name,item)]",
              "change: change"]
{-
 Script:
 tables .gdb change x in list1 to y
 tables .gdb change x in table1 to y
 tables .gdb add x to table1 as y
 tables .gdb add y to list1
 tables .gdb remove x from table1
 tables .gdb remove x from list1
 tables .gdb combine list1 and list2 as listname
 tables .gdb map list1 to list2 as tablename
 tables .gdb combine list1 and list2 as listname and map listname to list1 as newList
 tables .gdb change x in list1 to y if the length of list is less than the length of list2
-}

main = do
  arguments <- getArgs
  database  <- Qm.fromFile (head arguments)
  dispatch database (head arguments) arguments
  putStrLn "Done"
{-
handleMQ :: FilePath -> [Qm.Quill] -> Qm.QuillStatus Qm.Quill -> IO Qm.Quill
handleMQ fName x c = case c of
  (Qm.QuillSuccess a) -> return a
  (Qm.QuillMissing a) -> do
    pr <- Cm.prompt ("Quill " ++ a ++ " missing. Create? y/n")
    if pr == "y" then do
      pr2 <- Cm.prompt "Table or List? t/l"
      nm  <- Cm.prompt "Name? "
      Qm.toFile fName ((nm,if (pr2 == "t") then (Qm.Table "") else (Qm.List ""))++[x])
      Qm.fromFile fName
    else
     error "Table not created"
  (Qm.QuillMultiple a) -> do
    pr <- Cm.prompt ("Multiple quills " ++ a ++ " detected. Sanitize all?")
    if pr == "y" then do 
      Qm.toFile (Qm.removeQuill x a) 
      return Qm.fromFile fName 
    else error "Multiple quills remain"
 -}
qError :: Qm.QuillStatus a -> a
qError x = case x of
  (Qm.QuillSuccess a)  -> a -- FIXME make this handle the cases, not complain about them.
  (Qm.QuillMultiple a) -> error   ("multiple " ++ a)
  (Qm.QuillMissing a)  -> error ("missing "  ++ a)

dispatch :: [Qm.Quill] -> String -> [String] -> IO ()
dispatch database fName ("change":x:"in":y:"to":z:w) = do
  let qu = qError (Qm.getQuill database y)
  case (snd qu) of
    (Qm.Table c) -> Qm.toFile fName (qError (Qm.changeItem database (Qm.ATable (x,y,z))))
    (Qm.List c)  -> Qm.toFile fName (qError (Qm.changeItem database (Qm.AList (x,z))))
  dispatch database fName w

dispatch database fName ("add":x:"to":y:"as":z:w) = do
  Qm.toFile fName (qError (Qm.addItem database (Qm.ATable (y,z,x))))
  dispatch database fName w

dispatch database fName ("add":x:"to":y:z) = do
  Qm.toFile fName (qError (Qm.addItem database (Qm.AList (y,x))))
  dispatch database fName z

dispatch database fName ("remove":x:"from":y:z) = do
  Qm.toFile fName $ qError (Qm.removeItem database (y,x))
  dispatch database fName z


dispatch database _ ("change":_) = error "Malformed syntax in change expression"
