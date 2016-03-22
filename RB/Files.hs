module RB.Files (
	load
) where

import RB.Parser
import System.Random (newStdGen,randomRs)
import System.IO
import Network.HTTP.Conduit (simpleHttp)
import System.Directory (doesFileExist)

load name 
	| isWebResource name = do
			g <- newStdGen
			let webname = getResource name
			content <- simpleHttp webname
			let result = parse web content
			if isGrab name	
				then writeFile (newFile (20,g) (getProtoExt,webname)) result 
				else putStrLn result
	| isFileResource name = do
			g <- newStdGen
			let filename = getResource name
			exists <- doesFileExist filename
			if exists
				then do
					content <- readFile filename
					let result = parse file content
					if isGrab name	
						then writeFile (newFile (20,g) (getFileExt,filename)) result 
						else putStrLn result
				else do
					(putStrLn . noExistMsg) filename
	| otherwise = do
		putStrLn generalFailureMsg
			
generalFailureMsg = "Incorrect Syntax. "
			++ "Requires three arguments in the form "
			++ "<web|file> <_url_|_resource_> <grab|pre>"
			
noExistMsg f = "File or Resource Not Found: " ++ f

corLen n 
	| len n == 3 	= True
	| otherwise 	= False
	where len n = length (split " " n) 
	


fstSplit s n = unpack $ (split s n) !! 0
sndSplit s n = unpack $ (split s n) !! 1
thdSplit s n = unpack $ (split s n) !! 2

getResource n = sndSplit " " n
getResType n = fstSplit " " n
getResAction n = thdSplit " " n

isWebResource n
	| getResType n == "web" 	= True
	| getResType n == "WEB"		= True
	| getResType n == "W"		= True
	| getResType n == "w"		= True
	| otherwise					= False
		
isFileResource n
	| getResType n == "file" 	= True
	| getResType n == "FILE"	= True
	| getResType n == "f"		= True
	| getResType n == "F"		= True
	| otherwise					= False
	
isGrab n
	| getResAction n == "grab" 	= True
	| getResAction n == "GRAB"	= True
	| getResAction n == "g"		= True
	| getResAction n == "G"		= True
	| otherwise					= False
	
	
fileSep = "."
protoSep = ":"
getFileExt f = reverse $ fstSplit fileSep (reverse f)
getProtoExt f = fstSplit protoSep f

randomString (s,g) = (take s (randomRs ('a', 'z') g))
newFile p t = (randomString p) ++ fileSep ++ newFileName t
newFileName (f,e) = f e
	

