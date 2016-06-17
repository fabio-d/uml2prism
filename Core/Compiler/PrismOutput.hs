module PrismOutput where
import SemanticTree

data PrismExpr =
	  PrismExprBoolLiteral Bool				-- bool literal
	| PrismExprIntLiteral Int				-- int literal
	| PrismExprVariable String				-- variable to read
	| PrismExprBinOp String PrismExpr PrismExpr		-- binary operator
	| PrismExprNotOp PrismExpr				-- not operator
	| PrismExprUnProp Char Char PrismExpr			-- unary property
	| PrismExprBinProp Char Char PrismExpr PrismExpr	-- binary property

formatPrismExpr :: PrismExpr -> String
formatPrismExpr (PrismExprBoolLiteral True) = "true"
formatPrismExpr (PrismExprBoolLiteral False) = "false"
formatPrismExpr (PrismExprIntLiteral val) = show val
formatPrismExpr (PrismExprVariable var) = var
formatPrismExpr (PrismExprBinOp op e1 e2) = "(" ++ (formatPrismExpr e1) ++ op ++ (formatPrismExpr e2) ++ ")"
formatPrismExpr (PrismExprNotOp e) = "!" ++ (formatPrismExpr e)
formatPrismExpr (PrismExprUnProp q o a) = q:" [ " ++ o:' ':(formatPrismExpr a) ++ " ]"
formatPrismExpr (PrismExprBinProp q o a b) = q:" [ " ++ (formatPrismExpr a) ++ ' ':o:' ':(formatPrismExpr b) ++ " ]"

data PrismGlobalVarDecl =
	  PrismGlobalVarDeclBool String PrismExpr	-- boolean global variable with a given initial value
	| PrismGlobalVarDeclInt String Int Int PrismExpr -- bounded int global variable with a given initial value

formatPrismGlobalVarDecl :: PrismGlobalVarDecl -> String
formatPrismGlobalVarDecl (PrismGlobalVarDeclBool varName expr) =
	"global " ++ varName ++ " : bool init " ++ (formatPrismExpr expr)
formatPrismGlobalVarDecl (PrismGlobalVarDeclInt varName from to expr) =
	"global " ++ varName ++ " : [" ++ (show from) ++ ".." ++ (show to) ++ "] init " ++ (formatPrismExpr expr)

formatPrismAssignment :: String -> PrismExpr -> String
formatPrismAssignment varName expr = "(" ++ varName ++ "'=" ++ (formatPrismExpr expr) ++ ")"
