{-# LANGUAGE DeriveDataTypeable #-}
module Compiler where
import Data.Generics.Uniplate.Data
import Data.List
import PrismOutput
import SemanticTree

-- Given a type, list all its possible values
allValues :: Type -> [Expr]
allValues TypeBool = [ExprBoolLiteral v | v <- [False, True]]
allValues (TypeEnumeration vs) = [(ExprEnumLiteral (TypeEnumeration vs) n) | n <- [0..length vs]]
allValues (TypeClass []) = [ExprTuple (TypeClass []) []]
allValues (TypeClass ((n,t):other_fields)) = [
	ExprTuple (TypeClass ((n,t):other_fields)) (v:vs) |
	v <- allValues t, ExprTuple _ vs <- allValues (TypeClass other_fields)]
allValues (TypeSet t) = [ExprTuple (TypeSet t) elems | elems <- subsets (allValues t)] where
	subsets []  = [[]]
	subsets (x:xs) = [ mx ++ mxs | mx <- [[], [x]], mxs <- subsets xs ]

-- Given a type, list all its possible values along with a value ID
allValuesWithKey :: Type -> [(String, Expr)]
allValuesWithKey t = zip ['k':(show n) | n <- [1..]] (allValues t)

-- Given a type, list all its possible value IDs
allKeys :: Type -> [String]
allKeys t = [ k | (k,_) <- allValuesWithKey t ]

-- Given an identifier, return its type
typeOfIdnt :: Idnt -> Type
typeOfIdnt (IdntGlobal _ t) = t
typeOfIdnt (IdntMember _ _ t) = t

-- Given an expression, return its type
typeOfExpr :: Expr -> Type
typeOfExpr (ExprBoolLiteral _) = TypeBool
typeOfExpr (ExprEnumLiteral t _) = t
typeOfExpr (ExprClassNilLiteral t) = t
typeOfExpr (ExprStateCheck _) = TypeBool
typeOfExpr (ExprVariable idnt) = typeOfIdnt idnt
typeOfExpr (ExprEqOp _ _) = TypeBool
typeOfExpr (ExprNeqOp _ _) = TypeBool
typeOfExpr (ExprAndOp _ _) = TypeBool
typeOfExpr (ExprOrOp _ _) = TypeBool
typeOfExpr (ExprImpliesOp _ _) = TypeBool
typeOfExpr (ExprIffOp _ _) = TypeBool
typeOfExpr (ExprNotOp _) = TypeBool
typeOfExpr (ExprUnProp _ _ _) = TypeBool
typeOfExpr (ExprBinProp _ _ _ _) = TypeBool
typeOfExpr (ExprTuple t _) = t
typeOfExpr (ExprSetContains _ _) = TypeBool

-- Given a type, return its default value
nilValue :: Type -> Expr
nilValue TypeBool = ExprBoolLiteral False
nilValue (TypeEnumeration values) = ExprEnumLiteral (TypeEnumeration values) 0
nilValue (TypeClass members) = ExprClassNilLiteral (TypeClass members)
nilValue (TypeSet innerType) = ExprTuple (TypeSet innerType) []

-- Given an expression and a set of expression, return an expression that is
-- true if the former is contained in the latter
anyMatch :: Expr -> [Expr] -> Expr
anyMatch _ [] = ExprBoolLiteral False
anyMatch valueExpr candidateExprs = foldr1 ExprOrOp [ ExprEqOp valueExpr c | c <- candidateExprs ]

-- Replace StmtSetInsert statements with equivalent statement sequences
expandSetInsertStatements :: Stmt -> Stmt
expandSetInsertStatements =
	let
		guardedInsert setIdnt testExpr [(key, _)] =
			StmtAssignment (IdntMember setIdnt key TypeBool) (ExprBoolLiteral True)
		guardedInsert setIdnt testExpr ((key,refVal):others) = StmtIfElse
			(ExprEqOp testExpr refVal)
			(StmtAssignment (IdntMember setIdnt key TypeBool) (ExprBoolLiteral True))
			(guardedInsert setIdnt testExpr others)
	in let
		f (StmtSetInsert idnt expr) =
			guardedInsert idnt expr (allValuesWithKey elemtype)
			where TypeSet elemtype = typeOfIdnt idnt
		f x = x
	in
		transform f

-- Expand assignments to classes and sets
expandAssignmentStatements :: Stmt -> Stmt
expandAssignmentStatements =
	let

		assignVariableToVariable dest src fields = StmtCompound [
			StmtAssignment (IdntMember dest n t) (ExprVariable ((IdntMember src n t))) | (n,t) <- fields ]
		expandClassAssignment dest (ExprClassNilLiteral (TypeClass fields)) = StmtCompound [
			StmtAssignment (IdntMember dest n t) (nilValue t) | (n,t) <- fields ]
		expandClassAssignment dest (ExprVariable src) = assignVariableToVariable dest src fields
			where TypeClass fields = typeOfIdnt dest
		expandClassAssignment dest (ExprTuple (TypeClass fields) values) = StmtCompound [
			StmtAssignment (IdntMember dest n t) v | ((n,t),v) <- zip fields values ]
		expandSetAssignment dest (ExprVariable src) = assignVariableToVariable dest src [
			(n,TypeBool) | n <- allKeys innerType ] where TypeSet innerType = typeOfIdnt dest
		expandSetAssignment dest (ExprTuple (TypeSet innerType) items) = StmtCompound [
			StmtAssignment (IdntMember dest k TypeBool) (anyMatch v items) | (k,v) <- allValuesWithKey innerType]
	in let
		f (StmtAssignment idnt expr) | TypeClass _ <- typeOfIdnt idnt = Just (expandClassAssignment idnt expr)
		f (StmtAssignment idnt expr) | TypeSet _ <- typeOfIdnt idnt = Just (expandSetAssignment idnt expr)
		f _ = Nothing
	in
		rewrite f

-- Replace ExprSetContains expressions with equivalent expressions
expandSetContainsExpressions :: Expr -> Expr
expandSetContainsExpressions =
	let
		expandSetContains setIdnt candidateExpr [(k,v)] = ExprAndOp
			(ExprEqOp candidateExpr v)
			(ExprVariable (IdntMember setIdnt k TypeBool))
		expandSetContains setIdnt candidateExpr ((k,v):others) = ExprOrOp
			(expandSetContains setIdnt candidateExpr [(k,v)])
			(expandSetContains setIdnt candidateExpr others)
	in let
		f (ExprSetContains idnt expr) = expandSetContains idnt expr (allValuesWithKey innerType)
			where TypeSet innerType = typeOfIdnt idnt
		f x = x
	in
		transform f

-- Replace ExprEqOp and ExprNeqOp expressions operating on classes and sets with
-- equivalent expressions
expandCompareExpressions :: Expr -> Expr
expandCompareExpressions =
	let
		expandVariableToVariable idnt1 idnt2 fields cmpOp combOp = foldr1 combOp [
			cmpOp (ExprVariable ((IdntMember idnt1 n t))) (ExprVariable ((IdntMember idnt2 n t))) | (n,t) <- fields ]

		expandSetEqVariableToTuple idnt [] innerType = foldr1 ExprAndOp [
			ExprEqOp (ExprVariable (IdntMember idnt k TypeBool)) (ExprBoolLiteral False) | k <- allKeys innerType ]
		expandSetEqVariableToTuple idnt values innerType = foldr1 ExprAndOp (
			[ExprEqOp (ExprVariable (IdntMember idnt k TypeBool)) (anyMatch v values) | (k,v) <- allValuesWithKey innerType ]
			++
			[expandSetContainsExpressions (ExprSetContains idnt expr) | expr <- values])

		expandSetEqTupleToTuple values1 values2 innerType = foldr1 ExprAndOp (
			[anyMatch expr1 values2 | expr1 <- values1]
			++
			[anyMatch expr2 values1 | expr2 <- values2])

		expandClassOp cmpOp combOp notOp trueExpr fields = g where
			g (ExprClassNilLiteral _) (ExprClassNilLiteral _) = trueExpr
			g (ExprClassNilLiteral _) (ExprVariable idnt) = foldr1 combOp [ cmpOp (ExprVariable (IdntMember idnt n t)) (nilValue t) | (n,t) <- fields ]
			g (ExprClassNilLiteral _) (ExprTuple _ values) = foldr1 combOp [ cmpOp v (nilValue t) | ((_,t),v) <- zip fields values ]
			g (ExprVariable idnt1) (ExprVariable idnt2) = expandVariableToVariable idnt1 idnt2 fields cmpOp combOp
			g (ExprVariable idnt) (ExprTuple _ values) = foldr1 combOp [ cmpOp (ExprVariable (IdntMember idnt n t)) v | ((n,t),v) <- zip fields values ]
			g (ExprTuple _ v1s) (ExprTuple _ v2s) = foldr1 combOp [ cmpOp v1 v2 | (v1,v2) <- zip v1s v2s ]
			g a b = g b a

		expandSetOp cmpOp combOp notOp trueExpr innerType = h where
			h (ExprVariable idnt1) (ExprVariable idnt2) = expandVariableToVariable idnt1 idnt2 [(n,TypeBool) | n <- allKeys innerType] cmpOp combOp
			h (ExprVariable idnt) (ExprTuple _ values) = notOp (expandSetEqVariableToTuple idnt values innerType)
			h (ExprTuple _ values1) (ExprTuple _ values2) = notOp (expandSetEqTupleToTuple values1 values2 innerType)
			h a b = h b a
	in let
		f (ExprEqOp e1 e2)
			| TypeClass fields <- typeOfExpr e1 = Just (expandClassOp ExprEqOp ExprAndOp id (ExprBoolLiteral True) fields e1 e2)
			| TypeSet innerType <- typeOfExpr e1 = Just (expandSetOp ExprEqOp ExprAndOp id (ExprBoolLiteral True) innerType e1 e2)
		f (ExprNeqOp e1 e2)
			| TypeClass fields <- typeOfExpr e1 = Just (expandClassOp ExprNeqOp ExprOrOp ExprNotOp (ExprBoolLiteral False) fields e1 e2)
			| TypeSet innerType <- typeOfExpr e1 = Just (expandSetOp ExprNeqOp ExprOrOp ExprNotOp (ExprBoolLiteral False) innerType e1 e2)
		f _ = Nothing
	in
		rewrite f

-- Apply expandAssignmentStatements and expandSetInsertStatements to a Stmt
expandStatement :: Stmt -> Stmt
expandStatement = expandAssignmentStatements . expandSetInsertStatements

-- Apply expandCompareExpressions and expandSetContainsExpressions to a Stmt
expandExpression :: Expr -> Expr
expandExpression = expandCompareExpressions . expandSetContainsExpressions

data UnrollElem =
	  UnrollAssgn Idnt Expr
	| UnrollGuard Expr
	| UnrollBranch String
	deriving (Show)

-- Unroll all possible code paths
unrollSeq :: [UnrollElem] -> Stmt -> [[UnrollElem]]
unrollSeq prevseq (StmtCompound []) = [prevseq]
unrollSeq prevseq (StmtCompound (stmt:stmts)) = concat [
	unrollSeq stmtseq (StmtCompound stmts) |
	stmtseq <- unrollSeq prevseq stmt]
unrollSeq prevseq (StmtAssignment ident expr) =
	let
		expandedExpr = expandExpression expr
	in
		[prevseq ++ [(UnrollAssgn ident expandedExpr)]]
unrollSeq prevseq (StmtIfElse cond tstmt fstmt) =
	let
		expandedCond = expandExpression cond
	in
		(unrollSeq (prevseq ++ [UnrollGuard expandedCond]) tstmt) ++
		(unrollSeq (prevseq ++ [UnrollGuard (ExprNotOp expandedCond)]) fstmt)
unrollSeq prevseq (StmtChoiceOr alt1 alt2) = (unrollSeq prevseq alt1) ++ (unrollSeq prevseq alt2)
unrollSeq prevseq (StmtBranch str) = [prevseq ++ [UnrollBranch str]]

-- Truncate a sequence of UnrollElem to the first UnrollBranch. Returns Nothing
-- if no UnrollBranch is found
truncToBranch :: [UnrollElem] -> Maybe [UnrollElem]
truncToBranch [] = Nothing
truncToBranch ((UnrollBranch str):_) = Just [UnrollBranch str]
truncToBranch (e:es) = truncToBranch es >>= (\r -> Just (e:r))

-- Given a sequence of UnrollElem containing UnrollAssgn elements, this function
-- replaces later occurrences of assigned variables to create the illusion of
-- sequential execution
forwardSubst :: [UnrollElem] -> [UnrollElem]
forwardSubst =
	let
		-- Add a '$' somewhere in each variable's name
		markFunc (ExprVariable (IdntGlobal name t)) | (head name /= '$') = ExprVariable (IdntGlobal ('$':name) t)
		markFunc (ExprVariable (IdntMember parent name t)) | (head name /= '$') = ExprVariable (IdntMember parent ('$':name) t)
		markFunc x = x
		markVariables = transform markFunc
		-- Remove the '$'
		unmarkFunc (ExprVariable (IdntGlobal ('$':name) t)) = ExprVariable (IdntGlobal name t)
		unmarkFunc (ExprVariable (IdntMember parent ('$':name) t)) = ExprVariable (IdntMember parent name t)
		unmarkFunc x = x
		unmarkVariables = transform unmarkFunc
		-- Replace every occurrence of a given identifier with an
		-- expression in a sequence of UnrollElem
		subst elems idnt newexpr =
			let
				substExpr (ExprVariable idntCandidate) | (idntCandidate == idnt) = newexpr
				substExpr x = x
				substFunc (UnrollAssgn i e) = UnrollAssgn i (transform substExpr e)
				substFunc (UnrollGuard e) = UnrollGuard (transform substExpr e)
				substFunc x = x
			in
				map substFunc elems
		-- Traverse the list of elems.
		--  - when UnrollAssgn is found, all further occurrences of the
		--    assigned identifier are replaced with the actual
		--    expression, marking all its variables so that they do not
		--    get replaced again later in the process
		--  - when an element is emitted in its final form, markers are
		--    removed
		seqEater ((UnrollAssgn idnt expr):xs) = (UnrollAssgn idnt (unmarkVariables expr)):(seqEater (subst xs idnt (markVariables expr)))
		seqEater ((UnrollGuard expr):xs) = (UnrollGuard (unmarkVariables expr)):(seqEater xs)
		seqEater (x:xs) = x:(seqEater xs)
		seqEater [] = []
	in
		seqEater

-- Given a user identifier name, double all underscores, because the compiler
-- uses single underscores as a separator and we want avoid conflicts. Dots are
-- replaced with a single underscore
escapeString :: String -> String
escapeString ('_':cs) = '_':'_':(escapeString cs)
escapeString ('.':cs) = '_':(escapeString cs)
escapeString (c:cs) = c:(escapeString cs)
escapeString "" = ""

-- Convert a Idnt to a string (each segment being separated by a dot)
flattenIdnt :: Idnt -> String
flattenIdnt (IdntGlobal name _) = name
flattenIdnt (IdntMember parent name _) = (flattenIdnt parent) ++ "." ++ name

-- Convert a Idnt to an escaped string
escapeIdnt :: Idnt -> String
escapeIdnt = escapeString . flattenIdnt

-- Convert an expanded Expr (see expandExpression) to a prism Expr
-- Note: some Expr cases are not handled because they never occur in expanded
-- expressions
convertExpandedExprToPrismExpr :: Expr -> PrismExpr
convertExpandedExprToPrismExpr (ExprBoolLiteral v) = PrismExprBoolLiteral v
convertExpandedExprToPrismExpr (ExprEnumLiteral _ idx) = PrismExprIntLiteral idx
convertExpandedExprToPrismExpr (ExprStateCheck stateName) = PrismExprBinOp ">" (PrismExprVariable (escapeString stateName)) (PrismExprIntLiteral 0)
convertExpandedExprToPrismExpr (ExprVariable idnt) = PrismExprVariable (escapeIdnt idnt)
convertExpandedExprToPrismExpr (ExprEqOp a b) = PrismExprBinOp "=" (convertExpandedExprToPrismExpr a) (convertExpandedExprToPrismExpr b)
convertExpandedExprToPrismExpr (ExprNeqOp a b) = PrismExprBinOp "!=" (convertExpandedExprToPrismExpr a) (convertExpandedExprToPrismExpr b)
convertExpandedExprToPrismExpr (ExprAndOp a b) = PrismExprBinOp "&" (convertExpandedExprToPrismExpr a) (convertExpandedExprToPrismExpr b)
convertExpandedExprToPrismExpr (ExprOrOp a b) = PrismExprBinOp "|" (convertExpandedExprToPrismExpr a) (convertExpandedExprToPrismExpr b)
convertExpandedExprToPrismExpr (ExprImpliesOp a b) = PrismExprBinOp "=>" (convertExpandedExprToPrismExpr a) (convertExpandedExprToPrismExpr b)
convertExpandedExprToPrismExpr (ExprIffOp a b) = PrismExprBinOp "<=>" (convertExpandedExprToPrismExpr a) (convertExpandedExprToPrismExpr b)
convertExpandedExprToPrismExpr (ExprNotOp a) = PrismExprNotOp (convertExpandedExprToPrismExpr a)
convertExpandedExprToPrismExpr (ExprUnProp q o a) = PrismExprUnProp q o (convertExpandedExprToPrismExpr a)
convertExpandedExprToPrismExpr (ExprBinProp q o a b) = PrismExprBinProp q o (convertExpandedExprToPrismExpr a) (convertExpandedExprToPrismExpr b)

compileVariableDeclaration :: String -> Expr -> String
compileVariableDeclaration varName initVal =
	let
		varType = typeOfExpr initVal
	in let
		setValList (TypeSet innerType) = concat [
			"//  " ++ show k ++ " -> " ++ show v ++ "\n" | (k,v) <- allValuesWithKey innerType ]
		setValList _ = ""
		convToVarDecls [[]] = ""
		convToVarDecls [((UnrollAssgn idnt expr):vs)]
			| TypeBool <- typeOfIdnt idnt = formatPrismGlobalVarDecl (PrismGlobalVarDeclBool (escapeIdnt idnt) (convertExpandedExprToPrismExpr expr)) ++ ";\n" ++ convToVarDecls [vs]
			| TypeEnumeration enumvals <- typeOfIdnt idnt = formatPrismGlobalVarDecl (PrismGlobalVarDeclInt (escapeIdnt idnt) 0 (length enumvals) (convertExpandedExprToPrismExpr expr)) ++ ";\n" ++ convToVarDecls [vs]
		initValAssignment = StmtAssignment (IdntGlobal varName varType) initVal
	in
		setValList varType ++ (convToVarDecls (unrollSeq [] (expandStatement initValAssignment)))

compileSignalDeclaration :: String -> Type -> String
compileSignalDeclaration s t = compileVariableDeclaration s (nilValue t)

compileSimpleAssignment :: String -> Expr -> String
compileSimpleAssignment varName initVal =
	let
		varType = typeOfExpr initVal
	in let
		convToAssigments [[]] = []
		convToAssigments [((UnrollAssgn idnt expr):vs)] = (formatPrismAssignment (escapeIdnt idnt) (convertExpandedExprToPrismExpr expr)) : convToAssigments [vs]
		initValAssignment = StmtAssignment (IdntGlobal varName varType) initVal
	in
		intercalate " & " (convToAssigments (unrollSeq [] (expandStatement initValAssignment)))

compileNilAssignment :: String -> Type -> String
compileNilAssignment varName t = compileSimpleAssignment varName (nilValue t)

compileScriptedAction :: String -> Stmt -> Bool -> String
compileScriptedAction name script branchEnabled =
	let
		unrolledSequences = unrollSeq [] (expandStatement script)
		truncatedSequences
			| (branchEnabled == True) = map truncToBranch unrolledSequences
			| (branchEnabled == False) = map Just unrolledSequences
		containsNothing [] = False
		containsNothing (Nothing:_) = True
		containsNothing (_:xs) = containsNothing xs
		filterAssgn [] = []
		filterAssgn ((UnrollAssgn idnt expr):xs) = (UnrollAssgn idnt expr):(filterAssgn xs)
		filterAssgn (_:xs) = filterAssgn xs
		filterGuard [] = []
		filterGuard ((UnrollGuard expr):xs) = (UnrollGuard expr):(filterGuard xs)
		filterGuard (_:xs) = filterGuard xs
		findBranch [UnrollBranch str] = str -- UnrollBranch is always the last element
		findBranch (_:xs) = findBranch xs
		-- the "$signalWaitConditions$" placeholder is replaced by C++ code
		nodeActiveGuard = "(" ++ (escapeString name) ++ ">0)$signalWaitConditions$"
		-- the "$max{TARGET_NODE_NAME}$" placeholder is replaced by C++ code
		branchGuard targetNodeName = "(" ++ (escapeString targetNodeName) ++ "<$max" ++ targetNodeName ++ "$)"
		branchAssignments str = [formatPrismAssignment (escapeString name) (PrismExprIntLiteral 0), formatPrismAssignment (escapeString str) (PrismExprBinOp "+" (PrismExprVariable str) (PrismExprIntLiteral 1))]
		compileSequence s =
			let
				guards = (if branchEnabled then [branchGuard (findBranch s)] else []) ++ [ formatPrismExpr (convertExpandedExprToPrismExpr e) | UnrollGuard e <- filterGuard s]
				assignments = (if branchEnabled then branchAssignments (findBranch s) else []) ++ [ formatPrismAssignment (escapeIdnt idnt) (convertExpandedExprToPrismExpr expr) | UnrollAssgn idnt expr <- filterAssgn s ]
			in
				if length assignments == 0
					then "// No action when " ++ (intercalate " & " (nodeActiveGuard:guards))
					else "[] " ++ (intercalate " & " (nodeActiveGuard:guards)) ++ " -> 1.0 : " ++ (intercalate " & " assignments) ++ ";"
	in
		if containsNothing truncatedSequences
			then "E/Branch statement is missing in one or more code paths"
			else concat ("G/":[ compileSequence (forwardSubst s) ++ "\n" | Just s <- truncatedSequences ])

compileNotNilCheck :: String -> Type -> String
compileNotNilCheck varName t =
	let
		expr = ExprNeqOp (ExprVariable (IdntGlobal varName t)) (nilValue t)
	in
		formatPrismExpr (convertExpandedExprToPrismExpr (expandExpression expr))

compileProperty :: Expr -> String
compileProperty = formatPrismExpr . convertExpandedExprToPrismExpr . expandExpression

compileLabel :: String -> Expr -> String
compileLabel name expr = "label \"" ++ (escapeString name) ++ "\" = " ++ (compileProperty expr) ++ ";\n"
