{- |
   Module      :   Tables.Tables2
   Copyright   :   (c) 2014 by Nate Pisarski
   License     :   BSD3
   Maintainer  :   nathanpisarski@gmail.com
   Stability   :   Stable
   Portability :   Portable (Standalone - ghc)
   Database client for the Quill2 database language. Use this, as Tables1 will be deprecated on June 1st, 2014.
-}
module Main(main,dispatch) where

import qualified Cookbook.Project.Quill.Quill2.Meta as Qm
import qualified Cookbook.Essential.Meta as Cm

import qualified Cookbook.Ingredients.Lists.Modify as Md
import System.IO
import System.Environment
import System.Exit

-- | Displayed from the "help" request in dispatch.
allArgs = [("Add a Quill","add x to y as z | add x to y | add x as table | add x as list"),
           ("Remove a Quill","remove x from y | remove x"),
           ("Change a Quill","change x in y to z"),
           ("Get a quill", "get x from y | get x | list"),
           ("Combine Quills", "map x to y as z | combine x with y as z")]
          
-- | Dispatches the command line arguments.
main = do
  arguments <- getArgs
  database  <- Qm.fromFile (head arguments)
  dispatch database (head arguments) arguments
  putStrLn "Done"

-- | Wraps a Quill error in the database by exiting, preventing database corruption.
qError :: Qm.QuillStatus a -> IO a
qError x = case x of
  (Qm.QuillSuccess a)  -> return a -- FIXME make this handle the cases, not complain about them.
  (Qm.QuillMultiple a) -> do
    putStrLn ("Multiple " ++ a ++ " exist in file. Exiting. Fix manually")
    exitFailure
  (Qm.QuillMissing a)  -> do
    putStrLn (a ++ " Missing from file. Exiting. Fix manually.")
    exitFailure

-- | Handles the command-line arguments with destructuring.
dispatch :: [Qm.Quill] -> String -> [String] -> IO ()
dispatch database fName ("change":x:"in":y:"to":z:w) = do
  qu <- qError (Qm.getQuill database y)
  case (snd qu) of
    (Qm.Table c) -> do
      myDB <- (qError (Qm.changeItem database (Qm.ATable (y,x,z))))
      Qm.toFile fName myDB
    (Qm.List c)  -> do
      mod <- qError $ Qm.removeItem database (y,x) -- FIXME make QuillAddition work for lists.
      myDB <- (qError (Qm.addItem mod (Qm.AList (y,z))))
      Qm.toFile fName myDB
  dispatch database fName w

dispatch database fName ("add":x:"to":y:"as":z:w) = do
  myDB <- (qError (Qm.addItem database (Qm.ATable (y,z,x))))
  Qm.toFile fName myDB
  dispatch database fName w

dispatch database fName ("add":x:"to":y:z) = do
  myDB <- (qError (Qm.addItem database (Qm.AList (y,x))))
  Qm.toFile fName myDB
  dispatch database fName z

dispatch database fName ("add":x:"as":y:w) = do
  fl <- Cm.filelines fName
  writeFile fName (unlines ((y ++ "(" ++ x ++ "){}"):fl))
  dispatch database fName w
  
dispatch database fName ("remove":x:"from":y:z) = do
  myDB <- qError (Qm.removeItem database (y,x))
  Qm.toFile fName $ myDB
  dispatch database fName z

dispatch database fName ("remove":x:y) = do
  Qm.toFile fName (Qm.removeQuill database x)
  dispatch database fName y

dispatch database fName ("get":x:"from":y:z) = do
  (qError $ Qm.lookUp database (y,x)) >>= putStrLn 
  dispatch database fName z

dispatch database fName ("get":x:y) = do
  myDB <- (qError (Qm.getQuill database x))
  case (snd myDB) of
    (Qm.List a) -> mapM_ putStrLn (map ppTable [(show fi,se) | fi <- [0..length a], se <- a])
    (Qm.Table a) -> mapM_ putStrLn (map ppTable a)
  dispatch database fName y

dispatch database fName ("list":y) = do
  mapM_ listOff database

-- Composite functions
dispatch database fName ("map":x:"to":y:"as":w:z) = do
  l1 <- qError (Qm.getQuill database x)
  l2 <- qError (Qm.getQuill database y)
  let l1list = case (snd l1) of
        (Qm.List a)  -> a
        (Qm.Table _) -> error "Error! Attempting to map table"
  let l2list = case (snd l2) of
        (Qm.List a)  -> a
        (Qm.Table _) -> error "Error! Attempting to map table"
  Qm.toFile fName ((w,Qm.Table (zip l1list l2list)):database)
  dispatch database fName z

dispatch database fName ("combine":x:"with":y:"as":w:z) = do
  l1 <- qError (Qm.getQuill database x)
  l2 <- qError (Qm.getQuill database y)
  case snd l1 of
    (Qm.List a)  -> case (snd l2) of
      (Qm.List b)  -> Qm.toFile fName ((w,Qm.List (b ++ a)):database)
      (Qm.Table b) -> error "Error! Attempted to combine a table and list"
    (Qm.Table a) -> case (snd l2) of
      (Qm.Table b) -> Qm.toFile fName ((w,Qm.Table (b ++ a)):database)
      (Qm.List b)  -> error "Error! Attempted to combine a table and list"
  dispatch database fName z

dispatch database fname ("file":y:z) = do
  fl <- Cm.filelines y
  mapM_ (dispatch database fname) (map (`Md.splitOn` ' ') fl)
  dispatch database fname z

dispatch database fname ("repl":_) = do
  repl database fname
  
dispatch database fName ("help":y) = do
  mapM_ putStrLn (map ppTable allArgs)
  dispatch database fName y
  
dispatch _ _ [] = return ()
dispatch fName db ("and":xs) = dispatch fName db xs
dispatch fName db (x:xs) = do
  putStrLn ("Did not recognize command: "++x)
  dispatch fName db xs

-- | Pretty print a table.
ppTable :: (String, String) -> String
ppTable (a,b) = Cm.flt [a," : ",b]

-- | List the contents of a Quill.
listOff :: Qm.Quill -> IO ()
listOff x = case (snd x) of
  (Qm.Table _) -> putStrLn $ "Table " ++ (fst x)
  (Qm.List _)  -> putStrLn $ "List " ++ (fst x)

-- | Evaluate Quill commands in an interactive Read-eval-print loop.
repl fname dbase = do
  inp <- Cm.prompt "$ "
  dispatch fname dbase(Md.splitOn inp ' ')
  repl fname dbase
