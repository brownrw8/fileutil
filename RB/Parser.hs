module RB.Parser (
	parse,
	web,
	file,
	pack, 
	unpack, 
	split
) where

import Data.Text (pack, unpack, splitOn)

split s n = (splitOn (pack s) (pack n))

data Content = WebContent | FileContent
web = WebContent
file = FileContent

parse t content = case t of
	WebContent  -> (process . show) content
	FileContent -> (process . show) content

process content = [ chp c | c <- content, c /= ' ', c /= '\\']


chp c = case c of
	'\n' -> '\n'
	otherwise -> c
	

	


