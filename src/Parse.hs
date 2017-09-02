module Parse where

import Parse.TParser
import Parse.Tokens

-- TODO output the AST
parser :: TParser ()
parser = tok_array 
