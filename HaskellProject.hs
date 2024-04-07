type Location = (Char, Int)
data Player = White | Black deriving (Show, Eq)
data Piece = P Location | N Location | K Location | Q Location | R Location
						| B Location deriving (Show, Eq)
type Board = (Player, [Piece], [Piece])




visualizeBoard (p,w,b) = unlines(helper3 b "B" (helper3 w "W" ["    a    b    c    d    e    f    g    h ","8 |    |    |    |    |    |    |    |    |    ","7 |    |    |    |    |    |    |    |    |    ","6 |    |    |    |    |    |    |    |    |    ","5 |    |    |    |    |    |    |    |    |    ","4 |    |    |    |    |    |    |    |    |    ","3 |    |    |    |    |    |    |    |    |    ","2 |    |    |    |    |    |    |    |    |    ","1 |    |    |    |    |    |    |    |    |    ","","Turn : " ++ show p]))

helper3 :: [Piece] -> String -> [String] -> [String]

helper3 [] r h = h
helper3 (P (c,i):t) r [ha,h8,h7,h6,h5,h4,h3,h2,h1,hs,hp] | i == 1 = helper3 t r [ha,h8,h7,h6,h5,h4,h3,h2,helper2 c ("P" ++ r) h1,hs,hp] 
														| i == 2 = helper3 t r [ha,h8,h7,h6,h5,h4,h3,helper2 c ("P" ++ r) h2,h1,hs,hp] 
														| i == 3 = helper3 t r [ha,h8,h7,h6,h5,h4,helper2 c ("P" ++ r) h3,h2,h1,hs,hp] 
														| i == 4 = helper3 t r [ha,h8,h7,h6,h5,helper2 c ("P" ++ r) h4,h3,h2,h1,hs,hp] 
														| i == 5 = helper3 t r [ha,h8,h7,h6,helper2 c ("P" ++ r) h5,h4,h3,h2,h1,hs,hp] 
														| i == 6 = helper3 t r [ha,h8,h7,helper2 c ("P" ++ r) h6,h5,h4,h3,h2,h1,hs,hp] 
														| i == 7 = helper3 t r [ha,h8,helper2 c ("P" ++ r) h7,h6,h5,h4,h3,h2,h1,hs,hp] 
														| i == 8 = helper3 t r [ha,helper2 c ("P" ++ r) h8,h7,h6,h5,h4,h3,h2,h1,hs,hp]
														
helper3 (Q (c,i):t) r [ha,h8,h7,h6,h5,h4,h3,h2,h1,hs,hp] | i == 1 = helper3 t r [ha,h8,h7,h6,h5,h4,h3,h2,helper2 c ("Q" ++ r) h1,hs,hp] 
														| i == 2 = helper3 t r [ha,h8,h7,h6,h5,h4,h3,helper2 c ("Q" ++ r) h2,h1,hs,hp] 
														| i == 3 = helper3 t r [ha,h8,h7,h6,h5,h4,helper2 c ("Q" ++ r) h3,h2,h1,hs,hp] 
														| i == 4 = helper3 t r [ha,h8,h7,h6,h5,helper2 c ("Q" ++ r) h4,h3,h2,h1,hs,hp] 
														| i == 5 = helper3 t r [ha,h8,h7,h6,helper2 c ("Q" ++ r) h5,h4,h3,h2,h1,hs,hp] 
														| i == 6 = helper3 t r [ha,h8,h7,helper2 c ("Q" ++ r) h6,h5,h4,h3,h2,h1,hs,hp] 
														| i == 7 = helper3 t r [ha,h8,helper2 c ("Q" ++ r) h7,h6,h5,h4,h3,h2,h1,hs,hp] 
														| i == 8 = helper3 t r [ha,helper2 c ("Q" ++ r) h8,h7,h6,h5,h4,h3,h2,h1,hs,hp] 


helper3 (R (c,i):t) r [ha,h8,h7,h6,h5,h4,h3,h2,h1,hs,hp] | i == 1 = helper3 t r [ha,h8,h7,h6,h5,h4,h3,h2,helper2 c ("R" ++ r) h1,hs,hp] 
														| i == 2 = helper3 t r [ha,h8,h7,h6,h5,h4,h3,helper2 c ("R" ++ r) h2,h1,hs,hp] 
														| i == 3 = helper3 t r [ha,h8,h7,h6,h5,h4,helper2 c ("R" ++ r) h3,h2,h1,hs,hp] 
														| i == 4 = helper3 t r [ha,h8,h7,h6,h5,helper2 c ("R" ++ r) h4,h3,h2,h1,hs,hp] 
														| i == 5 = helper3 t r [ha,h8,h7,h6,helper2 c ("R" ++ r) h5,h4,h3,h2,h1,hs,hp] 
														| i == 6 = helper3 t r [ha,h8,h7,helper2 c ("R" ++ r) h6,h5,h4,h3,h2,h1,hs,hp] 
														| i == 7 = helper3 t r [ha,h8,helper2 c ("R" ++ r) h7,h6,h5,h4,h3,h2,h1,hs,hp] 
														| i == 8 = helper3 t r [ha,helper2 c ("R" ++ r) h8,h7,h6,h5,h4,h3,h2,h1,hs,hp] 
														
														
helper3 (B (c,i):t) r [ha,h8,h7,h6,h5,h4,h3,h2,h1,hs,hp] | i == 1 = helper3 t r [ha,h8,h7,h6,h5,h4,h3,h2,helper2 c ("B" ++ r) h1,hs,hp] 
														| i == 2 = helper3 t r [ha,h8,h7,h6,h5,h4,h3,helper2 c ("B" ++ r) h2,h1,hs,hp] 
														| i == 3 = helper3 t r [ha,h8,h7,h6,h5,h4,helper2 c ("B" ++ r) h3,h2,h1,hs,hp] 
														| i == 4 = helper3 t r [ha,h8,h7,h6,h5,helper2 c ("B" ++ r) h4,h3,h2,h1,hs,hp] 
														| i == 5 = helper3 t r [ha,h8,h7,h6,helper2 c ("B" ++ r) h5,h4,h3,h2,h1,hs,hp] 
														| i == 6 = helper3 t r [ha,h8,h7,helper2 c ("B" ++ r) h6,h5,h4,h3,h2,h1,hs,hp] 
														| i == 7 = helper3 t r [ha,h8,helper2 c ("B" ++ r) h7,h6,h5,h4,h3,h2,h1,hs,hp] 
														| i == 8 = helper3 t r [ha,helper2 c ("B" ++ r) h8,h7,h6,h5,h4,h3,h2,h1,hs,hp] 


helper3 (K (c,i):t) r [ha,h8,h7,h6,h5,h4,h3,h2,h1,hs,hp] | i == 1 = helper3 t r [ha,h8,h7,h6,h5,h4,h3,h2,helper2 c ("K" ++ r) h1,hs,hp] 
														| i == 2 = helper3 t r [ha,h8,h7,h6,h5,h4,h3,helper2 c ("K" ++ r) h2,h1,hs,hp] 
														| i == 3 = helper3 t r [ha,h8,h7,h6,h5,h4,helper2 c ("K" ++ r) h3,h2,h1,hs,hp] 
														| i == 4 = helper3 t r [ha,h8,h7,h6,h5,helper2 c ("K" ++ r) h4,h3,h2,h1,hs,hp] 
														| i == 5 = helper3 t r [ha,h8,h7,h6,helper2 c ("K" ++ r) h5,h4,h3,h2,h1,hs,hp] 
														| i == 6 = helper3 t r [ha,h8,h7,helper2 c ("K" ++ r) h6,h5,h4,h3,h2,h1,hs,hp] 
														| i == 7 = helper3 t r [ha,h8,helper2 c ("K" ++ r) h7,h6,h5,h4,h3,h2,h1,hs,hp] 
														| i == 8 = helper3 t r [ha,helper2 c ("K" ++ r) h8,h7,h6,h5,h4,h3,h2,h1,hs,hp] 	

helper3 (N (c,i):t) r [ha,h8,h7,h6,h5,h4,h3,h2,h1,hs,hp] | i == 1 = helper3 t r [ha,h8,h7,h6,h5,h4,h3,h2,helper2 c ("N" ++ r) h1,hs,hp] 
														| i == 2 = helper3 t r [ha,h8,h7,h6,h5,h4,h3,helper2 c ("N" ++ r) h2,h1,hs,hp] 
														| i == 3 = helper3 t r [ha,h8,h7,h6,h5,h4,helper2 c ("N" ++ r) h3,h2,h1,hs,hp] 
														| i == 4 = helper3 t r [ha,h8,h7,h6,h5,helper2 c ("N" ++ r) h4,h3,h2,h1,hs,hp] 
														| i == 5 = helper3 t r [ha,h8,h7,h6,helper2 c ("N" ++ r) h5,h4,h3,h2,h1,hs,hp] 
														| i == 6 = helper3 t r [ha,h8,h7,helper2 c ("N" ++ r) h6,h5,h4,h3,h2,h1,hs,hp] 
														| i == 7 = helper3 t r [ha,h8,helper2 c ("N" ++ r) h7,h6,h5,h4,h3,h2,h1,hs,hp] 
														| i == 8 = helper3 t r [ha,helper2 c ("N" ++ r) h8,h7,h6,h5,h4,h3,h2,h1,hs,hp] 														
helper2 :: Char -> String -> String -> String
helper2 c s h | c=='a' = take 4 h ++ s ++ drop 6 h
			  |c=='b' = take 9 h ++ s ++ drop 11 h
			  |c=='c' = take 14 h ++ s ++ drop 16 h
			  |c=='d' = take 19 h ++ s ++ drop 21 h
			  |c=='e' = take 24 h ++ s ++ drop 26 h
			  |c=='f' = take 29 h ++ s ++ drop 31 h 
			  |c=='g' = take 34 h ++ s ++ drop 36 h
			  |c=='h' = take 39 h ++ s ++ drop 41 h







setBoard:: Board
setBoard=  (White, [R ('h',1),N ('g',1),B ('f',1),K ('e',1),Q ('d',1),B ('c',1),N ('b',1),R ('a',1),P ('h',2),P ('g',2),P ('f',2),P ('e',2),P ('d',2),P ('c',2),P ('b',2),P ('a',2)] ,[R ('h',8),N ('g',8),B ('f',8),K ('e',8),Q ('d',8),B ('c',8),N ('b',8),R ('a',8),P ('h',7),P ('g',7),P ('f',7),P ('e',7),P ('d',7),P ('c',7),P ('b',7),P ('a',7)])



isLegal (P (c,i)) (p,w,b) (ct,it)= if elem (P (c,i)) w then (checkBlockw (P (c,i)) (p,w,b) (ct,it))  else (checkBlockb (P (c,i)) (p,w,b) (ct,it))
isLegal (N (c,i)) b (ct,it) =if isLegalMove (N (c,i)) (ct,it) then  checkLocation (N (c,i)) (ct,it) b else False
isLegal (B (c,i)) b (ct,it)= if isLegalMove (B (c,i)) (ct,it) then  (checkLocation (B (c,i)) (ct,it) b) && (checkBlock (B (c,i)) b (ct,it)) else False
isLegal (K (c,i)) b (ct,it)= if isLegalMove (K (c,i)) (ct,it) then  checkLocation (K (c,i)) (ct,it) b else False
isLegal (Q (c,i)) b (ct,it) =if isLegalMove (Q (c,i)) (ct,it) then  checkLocation (Q (c,i)) (ct,it) b && (checkBlock (Q (c,i)) b (ct,it))   else False
isLegal (R (c,i)) b (ct,it) =if isLegalMove (R (c,i)) (ct,it) then  checkLocation (R (c,i)) (ct,it) b && (checkBlock (R (c,i)) b (ct,it))  else False






 
isLegalColumn :: Piece -> Char -> Bool
isLegalColumn (P (c,i)) ct = c == ct

isLegalRow:: Piece -> Int -> Bool
isLegalRow (P (c,i)) it = (i== 7 && i-it <=2 && it <i) || (i==2 && it-i <=2 && i<it) || (it >i && it-i ==1 )||(i>it && i-it==1)

isLegalMove :: Piece -> Location -> Bool
isLegalMove (N (c,i)) (ct,it)   |(getIndex c == getIndex ct + 2)   = (if (i == it + 1) || (i == it -1 ) then True else False)
								|(getIndex c == getIndex ct - 2) = (if (i == it + 1) || (i == it -1 ) then True else False)
								|((i == it + 2)) = (if ((getIndex c == getIndex ct + 1) ) then True else if (getIndex c == getIndex ct - 1) then True else False)
								|(i == it - 2) = if ((getIndex c == getIndex ct + 1) || (getIndex c == getIndex ct - 1)) then True else False
								|otherwise = False

isLegalMove (B (c,i)) (ct,it) | getIndex c + i == getIndex ct + it = True
								| getIndex c - i == getIndex ct - it = True
	|otherwise = False


isLegalMove (K (c,i)) (ct,it) | c == ct = if (i == it + 1) || (i == it -1 ) then True else False
	| i == it = if (getIndex c == getIndex ct + 1) || (getIndex c == getIndex ct - 1) then True else False
	| getIndex c + i == getIndex ct + it  && abs (it-i) == 1= True
	|  getIndex c - i == getIndex ct - it && abs (it-i) == 1 = True
	|otherwise = False

isLegalMove (Q (c,i)) (ct,it) | c == ct = True
	| i == it = True
	| getIndex c + i == getIndex ct + it = True
	| getIndex c - i == getIndex ct - it = True
	| otherwise = False

isLegalMove (R (c,i)) (ct,it) | c == ct = True
	| i == it = True
	|otherwise = False

checkLocation :: Piece -> Location -> Board -> Bool
checkLocation (N (c,i)) (ct,it) (_,w,b) =  not (checkSameColor (c,i) (ct,it) (getLocationList w)) && not (checkSameColor (c,i) (ct,it) (getLocationList b))
checkLocation (P (c,i)) (ct,it) (_,w,b) =  not (checkSameColor (c,i) (ct,it) (getLocationList w)) && not (checkSameColor (c,i) (ct,it) (getLocationList b))
checkLocation (B (c,i)) (ct,it) (_,w,b) =  not (checkSameColor (c,i) (ct,it) (getLocationList w)) && not (checkSameColor (c,i) (ct,it) (getLocationList b))
checkLocation (R (c,i)) (ct,it) (_,w,b) =  not (checkSameColor (c,i) (ct,it) (getLocationList w)) && not (checkSameColor (c,i) (ct,it) (getLocationList b))
checkLocation (Q (c,i)) (ct,it) (_,w,b) =  not (checkSameColor (c,i) (ct,it) (getLocationList w)) && not (checkSameColor (c,i) (ct,it) (getLocationList b))
checkLocation (K (c,i)) (ct,it) (_,w,b) =  not (checkSameColor (c,i) (ct,it) (getLocationList w)) && not (checkSameColor (c,i) (ct,it) (getLocationList b))



checkBlock (B (c,i)) b (ct,it) | getIndex c + i == getIndex ct + it && it > i= helper ([(x,y) | x <- ['a','b','c','d','e','f','g','h'] , y <- [1,2,3,4,5,6,7,8], (getIndex x < getIndex c && getIndex x > getIndex ct && y > i && y < it) && getIndex x + y == getIndex c + i]) b
								| getIndex c + i == getIndex ct + it && it < i= helper ([(x,y) | x <- ['a','b','c','d','e','f','g','h'] , y <- [1,2,3,4,5,6,7,8],  (getIndex x > getIndex c && getIndex x < getIndex ct && y < i && y > it) && getIndex x + y == getIndex c + i] ) b

								| getIndex c - i == getIndex ct - it && it > i = helper ([(x,y) | x <- ['a','b','c','d','e','f','g','h'] , y <- [1,2,3,4,5,6,7,8], getIndex x > getIndex c && getIndex x < getIndex ct && y > i && y < it && getIndex x - y == getIndex c - i] ) b
								| getIndex c - i == getIndex ct - it && it < i = helper ([(x,y) | x <- ['a','b','c','d','e','f','g','h'] , y <- [1,2,3,4,5,6,7,8], getIndex x < getIndex c && getIndex x > getIndex ct && y < i && y > it && getIndex x - y == getIndex c - i] ) b

checkBlock (R (c,i)) b (ct,it) |  getIndex c == getIndex ct && i < it = helper ([(x,y) | x <- ['a','b','c','d','e','f','g','h'] , y <- [1,2,3,4,5,6,7,8],  (getIndex x == getIndex c && getIndex x == getIndex ct && y > i && y < it) ] ) b
								|  getIndex c == getIndex ct && i > it = helper ([(x,y) | x <- ['a','b','c','d','e','f','g','h'] , y <- [1,2,3,4,5,6,7,8],  (getIndex x == getIndex c && getIndex x == getIndex ct && y < i && y > it) ] ) b

								|  getIndex c > getIndex ct && i == it = helper ([(x,y) | x <- ['a','b','c','d','e','f','g','h'] , y <- [1,2,3,4,5,6,7,8],  (getIndex x < getIndex c && getIndex x > getIndex ct && y == i && y == it) ] ) b
								|  getIndex c < getIndex ct && i == it = helper ([(x,y) | x <- ['a','b','c','d','e','f','g','h'] , y <- [1,2,3,4,5,6,7,8],  (getIndex x > getIndex c && getIndex x < getIndex ct && y == i && y == it) ] ) b


checkBlock (Q (c,i)) b (ct,it) | getIndex c + i == getIndex ct + it && it > i= helper ([(x,y) | x <- ['a','b','c','d','e','f','g','h'] , y <- [1,2,3,4,5,6,7,8], (getIndex x < getIndex c && getIndex x > getIndex ct && y > i && y < it) && getIndex x + y == getIndex c + i]) b
								| getIndex c + i == getIndex ct + it && it < i= helper ([(x,y) | x <- ['a','b','c','d','e','f','g','h'] , y <- [1,2,3,4,5,6,7,8],  (getIndex x > getIndex c && getIndex x < getIndex ct && y < i && y > it) && getIndex x + y == getIndex c + i] ) b

								| getIndex c - i == getIndex ct - it && it > i = helper ([(x,y) | x <- ['a','b','c','d','e','f','g','h'] , y <- [1,2,3,4,5,6,7,8], getIndex x > getIndex c && getIndex x < getIndex ct && y > i && y < it && getIndex x - y == getIndex c - i] ) b
								| getIndex c - i == getIndex ct - it && it < i = helper ([(x,y) | x <- ['a','b','c','d','e','f','g','h'] , y <- [1,2,3,4,5,6,7,8], getIndex x < getIndex c && getIndex x > getIndex ct && y < i && y > it && getIndex x - y == getIndex c - i] ) b
								|  getIndex c == getIndex ct && i < it = helper ([(x,y) | x <- ['a','b','c','d','e','f','g','h'] , y <- [1,2,3,4,5,6,7,8],  (getIndex x == getIndex c && getIndex x == getIndex ct && y > i && y < it) ] ) b
								|  getIndex c == getIndex ct && i > it = helper ([(x,y) | x <- ['a','b','c','d','e','f','g','h'] , y <- [1,2,3,4,5,6,7,8],  (getIndex x == getIndex c && getIndex x == getIndex ct && y < i && y > it) ] ) b

								|  getIndex c > getIndex ct && i == it = helper ([(x,y) | x <- ['a','b','c','d','e','f','g','h'] , y <- [1,2,3,4,5,6,7,8],  (getIndex x < getIndex c && getIndex x > getIndex ct && y == i && y == it) ] ) b
								|  getIndex c < getIndex ct && i == it = helper ([(x,y) | x <- ['a','b','c','d','e','f','g','h'] , y <- [1,2,3,4,5,6,7,8],  (getIndex x > getIndex c && getIndex x < getIndex ct && y == i && y == it) ] ) b


checkBlockw (P (c,i)) (p,w,b) (ct,it) | i==2  && (it-i==2 || it-i==1) && getIndex c == getIndex ct = helper ([(x,y) | x <- ['a','b','c','d','e','f','g','h'] , y <- [1,2,3,4,5,6,7,8],  (getIndex x == getIndex c && getIndex x == getIndex ct && y > i && y <= it) ] ) (p,w,b)
										| i>2 && (it-i==1) && getIndex c == getIndex ct =  helper ([(x,y) | x <- ['a','b','c','d','e','f','g','h'] , y <- [1,2,3,4,5,6,7,8],  (getIndex x == getIndex c && getIndex x == getIndex ct && y > i && y <= it) ] ) (p,w,b)
										|  (getIndex c  == getIndex ct + 1 || getIndex c + 1  == getIndex ct ) && it-i==1 && elem (ct,it) (getLocationList b) =  True 
										| otherwise = False


checkBlockb (P (c,i)) (p,w,b) (ct,it) 	| i==7  && (i-it==2 || i-it==1) && getIndex c == getIndex ct = helper ([(x,y) | x <- ['a','b','c','d','e','f','g','h'] , y <- [1,2,3,4,5,6,7,8],  (getIndex x == getIndex c && getIndex x == getIndex ct && y <= i && y > it) ] ) (p,w,b)
										| i<7 && (i-it==1) && getIndex c == getIndex ct =  helper ([(x,y) | x <- ['a','b','c','d','e','f','g','h'] , y <- [1,2,3,4,5,6,7,8],  (getIndex x == getIndex c && getIndex x == getIndex ct && y <= i && y > it) ] ) (p,w,b)
										|  (getIndex c  == getIndex ct + 1 || getIndex c + 1  == getIndex ct ) && i-it==1 && elem (ct,it) (getLocationList w) =  True 
										| otherwise = False



helper [] (p,w,b) = True						
helper  (h:t) (p,w,b) | elem h ((getLocationList w) ++ (getLocationList b))  = False
						|otherwise = helper t (p,w,b)


getLocationList :: [Piece] -> [Location] 
getLocationList [] = []
getLocationList (P (c,i) : t) = (c,i) : getLocationList t
getLocationList (N (c,i) : t) = (c,i) : getLocationList t
getLocationList (Q (c,i) : t) = (c,i) : getLocationList t
getLocationList (K (c,i) : t) = (c,i) : getLocationList t
getLocationList (B (c,i) : t) = (c,i) : getLocationList t
getLocationList (R (c,i) : t) = (c,i) : getLocationList t

checkSameColor :: Location -> Location -> [Location] -> Bool
checkSameColor (c,i) (ct,it) l = (elem (c,i) l) && (elem (ct,it) l)




suggestMove (P (c,i)) (p,w,b) = [(x,y) | x <- ['a','b','c','d','e','f','g','h'] , y <- [1,2,3,4,5,6,7,8], isLegal (P (c,i)) (p,w,b) (x,y) ] 
suggestMove (Q (c,i)) (p,w,b) =[(x,y) | x <- ['a','b','c','d','e','f','g','h'] , y <- [1,2,3,4,5,6,7,8], isLegal (Q (c,i)) (p,w,b) (x,y) ]
suggestMove (R (c,i)) (p,w,b)= [(x,y) | x <- ['a','b','c','d','e','f','g','h'] , y <- [1,2,3,4,5,6,7,8], isLegal (R (c,i)) (p,w,b) (x,y) ]
suggestMove (N (c,i)) (p,w,b)= [(x,y) | x <- ['a','b','c','d','e','f','g','h'] , y <- [1,2,3,4,5,6,7,8], isLegal (N (c,i)) (p,w,b) (x,y) ]
suggestMove (K (c,i)) (p,w,b)=[(x,y) | x <- ['a','b','c','d','e','f','g','h'] , y <- [1,2,3,4,5,6,7,8], isLegal (K (c,i)) (p,w,b) (x,y) ]
suggestMove (B (c,i)) (p,w,b)= [(x,y) | x <- ['a','b','c','d','e','f','g','h'] , y <- [1,2,3,4,5,6,7,8], isLegal (B (c,i)) (p,w,b) (x,y) ]







getIndex c | c == 'a' = 8
	| c == 'b' = 7
	| c == 'c' = 6
	| c == 'd' = 5
	| c == 'e' = 4
	| c == 'f' = 3
	| c == 'g' = 2
	| c == 'h' = 1








move:: Piece -> Location -> Board -> Board
move  pi lo (player,w,b)|elem pi w  = if (isLegal pi (player,w,b) lo) then 
																		if player == White then (Black, changeLocation w pi lo, removeLocation b  lo) 
																							else error ("This is Black player's turn, White can't move.")  
																		else error ("Illegal move for piece " ++ show pi)
						| elem pi b  = if (isLegal pi (player,w,b) lo) then if player == Black then (White, removeLocation w lo, changeLocation b pi lo) else error ("This is White player's turn, Black can't move.")  else error ("Illegal move for piece " ++ show pi)
																							
						

changeLocation [] piece (ct,it) = []
changeLocation (P (c,i) : t) piece (ct,it) | piece == (P (c,i)) = (P (ct,it) : t)
											| otherwise = (P (c,i) : changeLocation t piece (ct,it))
changeLocation (R (c,i) : t) piece (ct,it) | piece == (R (c,i)) = (R (ct,it) : t)
											| otherwise = (R (c,i) : changeLocation t piece (ct,it))
changeLocation (Q (c,i) : t) piece (ct,it) | piece == (Q (c,i)) = (Q (ct,it) : t)
											| otherwise = (Q (c,i) : changeLocation t piece (ct,it))
changeLocation (K (c,i) : t) piece (ct,it) | piece == (K (c,i)) = (K (ct,it) : t)
											| otherwise = (K (c,i) : changeLocation t piece (ct,it))
changeLocation (B (c,i) : t) piece (ct,it) | piece == (B (c,i)) = (B (ct,it) : t)
											| otherwise = (B (c,i) : changeLocation t piece (ct,it))
changeLocation (N (c,i) : t) piece (ct,it) | piece == (N (c,i)) = (N (ct,it) : t)
											| otherwise = (N (c,i) : changeLocation t piece (ct,it))


removeLocation [] lo = []											
removeLocation (P l :t) lo | lo == l = t
							|otherwise = (P l) : removeLocation t lo
removeLocation (R l :t) lo | lo == l = t
							|otherwise = (R l) : removeLocation t lo
removeLocation (Q l :t) lo | lo == l = t
							|otherwise = (Q l) : removeLocation t lo
removeLocation (K l :t) lo | lo == l = t
							|otherwise = (K l) : removeLocation t lo
removeLocation (B l :t) lo | lo == l = t
							|otherwise = (B l) : removeLocation t lo
removeLocation (N l :t) lo | lo == l = t
							|otherwise = (N l) : removeLocation t lo