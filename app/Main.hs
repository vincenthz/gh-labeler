{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import qualified GitHub.Auth   as Github
import qualified GitHub.Endpoints.Issues as Github
import qualified GitHub.Endpoints.Issues.Labels as Github
-- import qualified GitHub.Data.Name as Github

import qualified Data.Vector as V
import qualified Data.ByteArray as BA

import qualified Data.Text as T

import           System.Directory (getHomeDirectory)
import           Data.Word (Word8)
import           Data.Proxy
import           Data.List (intercalate)
import           Control.Monad

import           System.Environment
import           System.Exit

isHex :: Char -> Bool
isHex = flip elem ("0123456789abcdefABCDEF" :: String)

toToken :: String -> Github.Auth
toToken s
    | all isHex s = Github.OAuth $ BA.pack $ map toW8 s
    | otherwise   = err
  where
    toW8 :: Char -> Word8
    toW8 = fromIntegral . fromEnum
    err = error $ "not a valid token. only expecting an hexadecimal token, but got: " ++ show s

data Cmd = List | Create String String | Delete [String] | Sync String
    deriving (Show,Eq)

newtype Color = Color String
    deriving (Eq)

instance Show Color where
    show (Color s) = s

newtype LabelName = LabelName String
    deriving (Eq)
instance Show LabelName where
    show (LabelName s) = s

data Label = Label { getLabelName :: LabelName, getLabelColor :: Color }
    deriving (Show,Eq)

data LabelCommand =
      LabelDelete LabelName
    | LabelCreate LabelName Color
    | LabelUpdate LabelName LabelName Color
    deriving (Show,Eq)

lblNameToGithub :: LabelName -> Github.Name entity
lblNameToGithub (LabelName name) = Github.mkName Proxy $ T.pack name

labelCommand :: LabelCommand -> Github.Auth -> Github.Name Github.Owner -> Github.Name Github.Repo -> IO (Either Github.Error ())
labelCommand (LabelDelete lbl)               =
    \tok ghHandle repo -> Github.deleteLabel tok ghHandle repo (lblNameToGithub lbl)
labelCommand (LabelCreate lbl (Color color)) =
    \tok ghHandle repo -> fmap (const ()) <$> Github.createLabel tok ghHandle repo (lblNameToGithub lbl) color
labelCommand (LabelUpdate old new (Color color)) =
    \tok ghHandle repo -> fmap (const ()) <$> Github.updateLabel tok ghHandle repo (lblNameToGithub old) (lblNameToGithub new) color

readToken :: IO Github.Auth
readToken = do
    home <- getHomeDirectory
    maybe (error "file doesn't contain a token") toToken . headM . lines <$> readFile (home ++ "/" ++ ".gh-labeler")
  where
    headM []    = Nothing
    headM (x:_) = Just x


parseColor :: String -> Either String Color
parseColor l = case l of
    ('#':lHash) | isColorFormat lHash -> Right $ Color lHash
                | otherwise           -> invalidColor
    _ | isColorFormat l -> Right $ Color l
      | otherwise       -> invalidColor
  where
    invalidColor = Left ("invalid: color " ++ l)
    isColorFormat s = length s == 6 && all isHex s

readLabels :: String -> IO [Label]
readLabels file = do
    (sequence . map toLabel . map breakOne . lines <$> readFile file) >>= either failExit pure
  where
    breakOne s = let (s1,s2) = break (== ' ') s
                  in (s1, tail s2)
    toLabel (colorS, name) =
        case parseColor colorS of
            Left err    -> Left (err ++ " for " ++ name)
            Right color -> Right $ Label (LabelName name) color

failExit :: String -> IO a
failExit s = putStrLn ("error: " ++ s) >> exitFailure

processError :: LabelCommand -> Either Github.Error () -> IO ()
processError cmd (Left err) = failExit (show err ++ " when trying to " ++ show cmd)
processError _   _          = pure ()

getRepoLabels :: Github.Auth -> Github.Name Github.Owner -> Github.Name Github.Repo -> IO [Label]
getRepoLabels tok ghHandle repo = do
    result <- Github.labelsOnRepo' (Just tok) ghHandle repo
    case result of
        Left e  -> failExit ("failed getting labels on repository " ++ show ghHandle ++ "/" ++ show repo ++ ":\n" ++ show e)
        Right r ->
            case sequence $ map toLabel $ V.toList r of
                Left err -> failExit err
                Right l  -> pure l
  where
    toLabel lbl =
        case parseColor (T.unpack $ Github.labelColor lbl) of
            Left err -> Left (err ++ " for label " ++ lblName)
            Right c  -> Right $ Label (LabelName lblName) c
      where lblName = T.unpack (Github.untagName $ Github.labelName lbl)

findLabel :: LabelName -> [Label] -> Maybe Label
findLabel name = go
  where go []                = Nothing
        go (x@(Label n _):xs)
            | n == name      = Just x
            | otherwise      = go xs

command :: Github.Name Github.Owner -> Github.Name Github.Repo -> Cmd -> IO () 
command ghHandle repo List = do
    tok    <- readToken
    labels <- getRepoLabels tok ghHandle repo
    mapM_ (putStrLn . showLabel) labels
  where
    showLabel :: Label -> String
    showLabel (Label name color) =
        show name ++ " " ++ show color

command ghHandle repo (Sync file) = do
    tok    <- readToken
    labels <- readLabels file

    existingLabels <- getRepoLabels tok ghHandle repo

    forM_ labels $ \(Label name color) -> do
        case findLabel name existingLabels of
            Nothing -> do
                putStrLn ("creating label " ++ show name ++ " with color " ++ show color)
                let cmd = LabelCreate name color
                processError cmd =<< labelCommand cmd tok ghHandle repo
            Just (Label _ currentCol)
                | currentCol == color -> do
                    putStrLn ("skipping label " ++ show name)
                | otherwise -> do
                    putStrLn ("updating label " ++ show name ++ " color from " ++ show currentCol ++ " to " ++ show color)
                    let cmd = LabelUpdate name name color
                    processError cmd =<< labelCommand cmd tok ghHandle repo

    let others = filter (\(Label n _) -> not $ elem n labelNames) existingLabels
                   where labelNames = map getLabelName labels

    let quote s = "\"" ++ s ++ "\""
    let x = map (quote . show . getLabelName) others
    putStrLn $ "other tags: " ++ intercalate " " x
    pure ()

command ghHandle repo (Create name color) = do
    tok    <- readToken
    case parseColor color of
        Left err -> putStrLn err >> exitFailure
        Right c  -> do
            let cmd = LabelCreate (LabelName name) c
            processError cmd =<< labelCommand cmd tok ghHandle repo
    
command ghHandle repo (Delete names) = do
    tok            <- readToken
    --existingLabels <- getRepoLabels tok ghHandle repo
    forM_ names $ \n -> do
        let cmd = LabelDelete (LabelName n)
        putStrLn ("Deleting " ++ n)
        processError cmd =<< labelCommand cmd tok ghHandle repo

main :: IO ()
main = do
    args <- getArgs
    case args of
        ghHandle:repo:"list":[] ->
            command (Github.mkName Proxy $ T.pack ghHandle) (Github.mkName Proxy $ T.pack repo) List
        ghHandle:repo:"create":name:color:[] ->
            command (Github.mkName Proxy $ T.pack ghHandle) (Github.mkName Proxy $ T.pack repo) (Create name color)
        ghHandle:repo:"sync":file:[] ->
            command (Github.mkName Proxy $ T.pack ghHandle) (Github.mkName Proxy $ T.pack repo) (Sync file)
        ghHandle:repo:"delete":l ->
            command (Github.mkName Proxy $ T.pack ghHandle) (Github.mkName Proxy $ T.pack repo) (Delete l)
        _ -> do
            putStrLn "usage: gh-labeler <github-handle> <repository> <cmd>"
            putStrLn ""
            putStrLn "  list"
            putStrLn "  sync <label-file>"
            putStrLn "  create <label> <color>"
            putStrLn "  delete <label1> [label2...]"
            putStrLn ""
            putStrLn "label file format:"
            putStrLn "  <hexa color> <name>"

