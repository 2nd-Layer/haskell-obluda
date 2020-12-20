-- obluda 
-- written in January 2007 by Anicka Bernathova <anicka@anicka.net>

module Main where 

import IO
import Random
import System.Environment

-- type for representation of AVL tree
-- Nil or (left subtree, key, value, balance, right subtree)
data BTree a b = Nil | Tree (BTree a b) a b Integer  (BTree a b)

instance (Ord a, Show a, Ord b, Show b) => Show (BTree a b) where
    show x = quork [] x

quork s Nil = s ++ "Nil\n"
quork s (Tree l k v d p) = quork ("    "++s) p ++ s ++"("++show k++","++show v++","++show d++ ")\n" ++ quork ("    "++s) l

--returns balance of the AVL tree
getD :: Ord a => BTree a b -> Integer
getD Nil = 0
getD (Tree _ _ _ d _) = d

plus :: Integer -> Integer
plus n | n>0 = n
       | True = 0

--left rotation of AVL tree
rl :: BTree a b -> BTree a b 
rl (Tree l v val d p) = Tree (Tree l v val r pl) pv pval s pp 
                      where (Tree pl pv pval pd pp) = p
	       		    r = d - 1 - plus pd
                            s = - 1 + pd + r - plus r
--right rotation
rr :: BTree a b -> BTree a b 
rr (Tree l v val d p) = Tree ll lv lval s (Tree lp v val r p)	
                  where (Tree ll lv lval ld lp) = l
			r = d - ld + 1 + plus ld 
			s = 1 + ld + plus r	 
--double rotations 
rlrr :: BTree a b -> BTree a b 
rlrr (Tree l v val d p) = rr (Tree (rl l) v val d p)

rrrl :: BTree a b -> BTree a b 
rrrl (Tree l v val d p) = rl (Tree l v val d (rr p))

insert :: Ord a => a -> BTree a Integer -> (BTree a Integer, Integer)
insert n Nil = ((Tree Nil n 1 0 Nil),1)
insert n (Tree l v val d p)     | n==v = ((Tree l v (val+1) d p),0) 
                                | n<v = if s==0 then ((Tree t v val d p),0)
				         else case d of
				              1 -> ((Tree t v val 0 p),0)
				              0 -> ((Tree t v val (-1) p),1)
				              -1 -> (t2,0)
			        | n>v = if j==0 then ((Tree l v val d i),0) 
			        	    else case d of
				        	 1 -> (i2,0)
					         0 -> ((Tree l v val 1 i),1)
					         -1 -> ((Tree l v val 0 i),0)
			        where (t, s)   = insert n l
				      td     = getD t
				      t2     = if td == -1 then rr (Tree t v val (d-1) p)
                                             else rlrr(Tree t v val (d-1) p)
                                      (i, j)  = insert n p
				      id      = getD i
				      i2      = if id == 1 then rl (Tree l v val (d+1) i)
						else rrrl (Tree l v val (d+1) i) 

ins :: Ord a => a -> BTree a Integer -> BTree a Integer 
ins n t = tt
        where (tt,_)= insert n t

--parses the input string and feeds our tree
savestring :: String -> (Char,Char) -> BTree String Integer -> BTree String Integer
savestring (z:xs) (x,y) t | (z /= ' ') = savestring xs (y,z) $! (ins [x,y,z] t)
			  | True = savestring xs (' ',' ') $! (ins [x,y,z] t)
savestring [] (x,y) t = ins [x,y,' '] t

straight :: BTree a b -> [(a, b)]
straight Nil = []
straight (Tree l v val _ p) = (straight l) ++ [(v,val)] ++ (straight p)

probm2 :: [(String, Integer)] -> [(String, Char, Float)]
probm2 l = zip3 ctxl charl probs
           where (sl,nl) = unzip l
                 ctxl = map (\[x,y,z] -> [x,y]) sl
                 charl = map (\[x,y,z] -> z) sl
                 sum = foldl1 (+) nl
                 probs = map (\i -> fromInteger i / fromInteger sum) (scanl1 (+) nl)

probm :: [(String, Integer)] -> [(String, Char, Float)]
probm l@(([x,y,_],_):_) = probm2 l1 ++ probm l2
                          where (l1,l2) = span same l
                                same = \([x',y',_],_) -> (x,y) == (x',y')
probm [] = []

mktree :: [(String,Char,Float)] -> BTree (String,Float) Char
mktree [] = Nil
mktree x  = (Tree (mktree l) (a,c) b 0 (mktree p))
          where (a,b,c) = x!!((length x) `div` 2)
                l = [(l,m,n) | (l,m,n)<-x, (l,n)<(a,c)]
                p = [(i,j,k) | (i,j,k)<-x, (i,k)>(a,c)]

getnext :: BTree (String,Float) Char -> String -> Float -> Char -> Char
getnext Nil _ _ temp = temp
getnext t s prob temp | s<str = getnext l s prob temp
                      | s>str = getnext p s prob temp
                      | s==str = (if prob<fl then getnext l s prob char else getnext p s prob temp)
                      where Tree l (str,fl) char _ p = t  

--returns a random word (length can be very long) 
getword :: BTree (String,Float) Char -> String -> String -> IO (String)
getword t prefix ctxt = do
            prob <- randomRIO (0::Float,1)
            let c = getnext t ctxt prob ' '
              in if c==' ' then return prefix
                           else getword t (prefix ++ [c]) ((drop 1 ctxt) ++ [c])

--prints given number of words, length can be tweaked inside
genwords :: Integer -> BTree (String,Float) Char -> IO ()
genwords number tree = do
                       c <- getword tree [] "  "
                       let d = length c 
                         in if (d < 100) && (d>5) then do 
                                                       putStrLn c
                                                       if number>1 then genwords (number-1) tree
                                                                   else putStr ""
                                                  else genwords number tree 
dump :: String -> IO () 
dump path = bracket
          (openFile path ReadMode)
          hClose
          (\h -> do
           cont <- (hGetContents h)
           putStr (show (probm (straight (savestring cont (' ',' ') Nil))))
            )

rfdump :: String -> IO (BTree (String,Float) Char)
rfdump string  = do 
                 let c=(read string)
                   in return $! (mktree c)

rf :: String -> IO (BTree (String,Float) Char)
rf cont = return $! (mktree (probm (straight (savestring cont (' ',' ') Nil)))) 

action :: String -> Integer ->Integer -> IO ()
action path mode number = do
                            h <- (openFile path ReadMode)
                            cont <- (hGetContents h)
			    tree <- (if mode==0 then rf else rfdump) cont 
                            hClose h
                            genwords number tree

usage :: String
usage = "Usage: obluda -c corpus_file number_of_lines\n" 
       ++"              -r dump_file number_of_lines\n" 
       ++"              -d corpus_file"

main :: IO ()
main = getArgs >>= \argv ->
       case argv of 
        ["-c",filename,number] -> action filename 0 (read number)
        ["-r",filename,number] -> action filename 1 (read number)
        ["-d",filename] -> dump filename
        _ -> putStrLn usage 

