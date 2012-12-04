module Pattern where
import Utilities

-- converts Maybe [a] to [a], returns empty list if Maybe [a] == Nothing
removeMaybe :: Maybe [a] -> [a]
removeMaybe Nothing = []
removeMaybe (Just a) = a


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute wildcard (t:ts) s
	| ts == [] && t == wildcard = s
	| ts == [] && t /= wildcard = [t]
	| t == wildcard = s ++ substitute wildcard ts s
	| otherwise = (t:substitute wildcard ts s)

-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match wc [] [] = Just []
match wc [] mList = Nothing
match wc pList [] = Nothing
match wc (p:ps) (m:ms)
	| (p /= wc) && (p == m) = match wc ps ms
	| p /= wc = Nothing
	| p == wc = (singleWildcardMatch (p:ps) (m:ms)) `orElse` (longerWildcardMatch (p:ps) (m:ms))


-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs) = mmap (x:) (mmap (take 0) (match wc ps xs))
longerWildcardMatch (wc:ps) (x:xs) = mmap (x:) (match wc (wc:ps) xs)



-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions



-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply x f mList (pList, sList) = mmap ((substitute x sList) . f) (match x pList mList)

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply x f patterns mList = foldl1 (orElse) [ transformationApply x f mList p | p <- patterns ]