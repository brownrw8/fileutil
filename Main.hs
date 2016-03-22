import RB.Files

main = do
	repl
	
repl = do
	name <- getLine
	if isExit name
		then return ()
		else do
			load name
			repl
	
isExit f 
	| f == "q" 		= True
	| f == "Q"		= True
	| f == "quit" 	= True
	| f == "QUIT"	= True
	| otherwise 	= False	
	
