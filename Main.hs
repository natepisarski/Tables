{- |
   Module      :   Tables.Tables2
   Copyright   :   (c) 2014 by Nate Pisarski
   License     :   BSD3
   Maintainer  :   nathanpisarski@gmail.com
   Stability   :   Stable
   Portability :   Portable (Standalone - ghc)
   Database client for the Quill2 database language. Use this, as Tables1 will be deprecated on June 1st, 2014.
-}
module Main(main) where

import qualified Cookbook.Project.Quill.Quill2.Meta as Qm
import qualified Cookbook.Essential.Meta as Cm

import System.IO
import System.Environment

main = do
  arguments <- getArgs
  database  <- Qm.fromFile (head arguments)
  dispatch database (head arguments) arguments
  putStrLn "Done"

qError :: Qm.QuillStatus a -> a
qError x = case x of
  (Qm.QuillSuccess a)  -> a -- FIXME make this handle the cases, not complain about them.
  (Qm.QuillMultiple a) -> error   ("multiple " ++ a)
  (Qm.QuillMissing a)  -> error ("missing "  ++ a)

dispatch :: [Qm.Quill] -> String -> [String] -> IO ()
dispatch database fName ("change":x:"in":y:"to":z:w) = do
  let qu = qError (Qm.getQuill database y)
  case (snd qu) of
    (Qm.Table c) -> Qm.toFile fName (qError (Qm.changeItem database (Qm.ATable (y,x,z))))
    (Qm.List c)  -> do
      let mod = qError $ Qm.removeItem database (y,x) -- FIXME make QuillAddition work for lists.
      Qm.toFile fName (qError (Qm.addItem mod (Qm.AList (y,z))))
  dispatch database fName w

dispatch database fName ("add":x:"to":y:"as":z:w) = do
  Qm.toFile fName (qError (Qm.addItem database (Qm.ATable (y,z,x))))
  dispatch database fName w

dispatch database fName ("add":x:"to":y:z) = do
  Qm.toFile fName (qError (Qm.addItem database (Qm.AList (y,x))))
  dispatch database fName z

dispatch database fName ("add":x:"as":y:w) = do
  fl <- Cm.filelines fName
  writeFile fName (unlines ((y ++ "(" ++ x ++ "){}"):fl))
  dispatch database fName w
  
dispatch database fName ("remove":x:"from":y:z) = do
  Qm.toFile fName $ qError (Qm.removeItem database (y,x))
  dispatch database fName z

dispatch database fName ("remove":x:y) = do
  Qm.toFile fName (Qm.removeQuill database x)
  dispatch database fName y

dispatch database fName ("get":x:"from":y:z) = do
  putStrLn $ qError $ Qm.lookUp database (y,x)
  dispatch database fName z

dispatch database fName ("get":x:y) = do
  case (snd (qError (Qm.getQuill database x))) of
    (Qm.List a) -> mapM_ putStrLn (map ppTable [(show fi,se) | fi <- [0..length a], se <- a])
    (Qm.Table a) -> mapM_ putStrLn (map ppTable a)
  dispatch database fName y

dispatch database fName ("list":y) = do
  mapM_ listOff database

-- Composite functions
dispatch database fName ("map":x:"to":y:"as":w:z) = do
  let l1 = qError (Qm.getQuill database x)
  let l2 = qError (Qm.getQuill database y)
  let l1list = case (snd l1) of
        (Qm.List a)  -> a
        (Qm.Table _) -> error "Error! Attempting to map table"
  let l2list = case (snd l2) of
        (Qm.List a)  -> a
        (Qm.Table _) -> error "Error! Attempting to map table"
  Qm.toFile fName ((w,Qm.Table (zip l1list l2list)):database)
  dispatch database fName z

dispatch database fName ("combine":x:"with":y:"as":w:z) = do
  let l1 = qError (Qm.getQuill database x)
  let l2 = qError (Qm.getQuill database y)
  let l1list = case (snd l1) of
        (Qm.List a)  -> a
        (Qm.Table _) -> error "Error! Attempting to map table"
  let l2list = case (snd l2) of
        (Qm.List a)  -> a
        (Qm.Table _) -> error "Error! Attempting to map table"
  Qm.toFile fName ((w,Qm.List (l1list++l2list)):database)
  dispatch database fName z
  
dispatch _ _ [] = return ()
dispatch fName db ("and":xs) = dispatch fName db xs
dispatch fName db (x:xs) = do
  putStrLn ("Did not recognize command: "++x)
  dispatch fName db xs

ppTable :: (String, String) -> String
ppTable (a,b) = Cm.flt [a," : ",b]

listOff :: Qm.Quill -> IO ()
listOff x = case (snd x) of
  (Qm.Table _) -> putStrLn $ "Table " ++ (fst x)
  (Qm.List _)  -> putStrLn $ "List " ++ (fst x)
