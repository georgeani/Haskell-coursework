---- Part 1 ----

--printLine prints the symbols in a straight string
--it takes a integer as a value and prints that many symbols
--the name has to do with what the function does in this case printing a line

--stage11 prints the initial rectangle that will be used in the steps
--it takes two integer values and produces a string which is the rectangle
--it is called stage11 as it is task 1 stage 1, as it builds the first part of what the function
--needs to do

--stage12 incraments and builts the steps as asked by the user it has 5 Integers as arguments
--three for the steps and how big the initial rectangle must be and the other two are to keep track how many

-- steps have taken place
--it is called stage11 as it is task 1 stage 2, as it builds the second part of what the function
--steps simplify the process for the end user


printLine:: Int -> String
printLine x
		|x==0 = ""
		|otherwise = "*" ++ printLine(x-1)


stage11:: Int -> Int -> String
stage11 x y
		|x<0 = "Error"
		|x==0 = ""
		|otherwise = printLine(y) ++ "\n" ++ stage11(x-1) y


stage12:: Int -> Int -> Int -> Int -> Int -> String
stage12 x y z e m
	|x<0 = "Error"
	|e `mod` x == 0 && e>0 && e/=z*x = stage11 x (y+m) ++ stage12 x (y+m) z (e+x) m
	|e== 0 = stage11 x y ++ stage12 x y z (e+x) m
	|e==z*x = ""

	
steps:: Int -> Int -> Int -> String
steps x y z
	|x<=0 || y<=0 || z<=0 = ""
	|otherwise = stage12 x y z 0 y



---- Part 2 ----

--printThatLine prints the symbols in a straight string
--it takes a integer as a value and prints that many symbols
--the name has to do with what the function does in this case printing a line

--emptied prints an empty string
--it takes a integer as a value and prints that whitespaces
--the name has to do with what the function does in this case printing a an empty string

--fstageodd produces the upper part of the design for odd number input
--it takes 4 Integer inputs, the first one is size, the 2 in the middle are to keep track which line it is printing
--the last one is to showcase which line is the middle and is also the exit condition
--the name means first stage odd, which is what it does

--sstageodd produces the pattern for odd number input
--it takes one integer as an input which is how big the pattern must be
--the name means second stage odd

--fstageeven produces the upper part of the design for odd number input
--it takes 4 Integer inputs, the first one is size, the 2 in the middle are to keep track which line it is printing
--the last one is to showcase which line is the middle and is also the exit condition
--the name means first stage even, which is what it does, the difference is that in the middle it has 2 stars

--sstageeven produces the pattern for even number input
--it takes one integer as an input which is how big the pattern must be
--the name means second stage even

--flagpattern: it simplifies the process for the end user
--we use the min function when looking for the compatibility
--as it allows us to check if the characters have been crossed out more than once

printThatLine:: Int -> String
printThatLine x
		|x==0 = ""
		|otherwise = "*" ++ printThatLine(x-1)


emptied::Int -> String
emptied x
		|x<0 = "Error"
		|x==0 = ""
		|otherwise = " " ++ emptied (x-1)


fstageodd:: Int -> Int -> Int -> Int -> String
fstageodd x y z m
  |x<5 = "Error"
  |y==0 = printThatLine x ++ "\n" ++ fstageodd x (y+1) z m
  |y==m = printThatLine 1 ++ emptied ((x-3)`div`2) ++ printThatLine 1 ++ emptied ((x-3)`div`2) ++ printThatLine 1
  |otherwise = printThatLine 1 ++ emptied z ++ printThatLine 1 ++ emptied ((x-4)-(2*z)) ++ printThatLine 1 ++ emptied z ++ printThatLine 1 ++ "\n" ++ fstageodd x (y+1) (z+1) m
 
 
sstageodd:: Int -> String
sstageodd x
    |x<5 ="Error"
	|otherwise = fstageodd x 0 0 (x`div`2) ++ reverse (take (((x)`div`2)*(x+1)) (fstageodd x 0 0 (x`div`2)))

  
fstageeven:: Int -> Int -> Int -> Int -> String
fstageeven x y z m
  |x<5 = "Error"
  |y==0 = printThatLine x ++ "\n" ++ fstageeven x (y+1) z m
  |y==m = printThatLine 1 ++ emptied ((x-3)`div`2) ++ printThatLine 2 ++ emptied ((x-3)`div`2) ++ printThatLine 1
  |otherwise = printThatLine 1 ++ emptied z ++ printThatLine 1 ++ emptied ((x-4)-(2*z)) ++ printThatLine 1 ++ emptied z ++ printThatLine 1 ++ "\n" ++ fstageeven x (y+1) (z+1) m

 
sstageeven:: Int -> String
sstageeven x
    |x<5 ="Error"
	|otherwise = fstageeven x 0 0 ((x`div`2)-1) ++ "\n" ++ reverse ((fstageeven x 0 0 ((x`div`2)-1)))


flagpattern:: Int -> String
flagpattern x
    |x<5 = "Error"
	|x`mod`2==0 = sstageeven x
	|otherwise = sstageodd x


---- Part 3 ----


--searcher: this function is used to search a list of Strings for the work
--we are looking for and it will
--return the index we are looking for
--it takes as an input two Strings the one that we look into and the value
--that we are looking for
--the name of the function has to do with the fact that it searches 

--stage31: this function is used in order to replace the previous word with the new one
--we provide the index found by the searcher function 
--the name signifies function 3 stage 1, which is what it does as it is does the first part by modifying the string

--modifierrr: this function is used to change the second string that we wil change the second one with
--it modifies it so it looks similar to the 1st one
--we provide the string that we are going to modify, as well as the one we want to change it with. The function will modify the reolacement string with the proper suffix.

--modifier: it is the function that simplifies the use of stage31 function and 
--changes the word with the one that we provide it with, its name comes from the fact that it modifies the string

--origswap: the origswap function is used as it brings all the previous functions together and in order to make the change faster
--it can only swap only one word instance at the time
--it is called origswap as it is the most primitive type of swap in this task
--it takes as an input three strings, the string we are looking for, the one that we want to change it with and the String that we want to modify

--advancedswap: it makes sure that all instances of a word are changed
--it primarily uses the origswap function
--it goes through the produced string each time to check if all instances of the word has been changed
--if no other instance is found it outputs the complete string and finishes the string 
--it takes 4 values as an input, 3 strings and 1 integer, the one we are looking for, the one that we want to change it with and the String that we want to modify
--it uses an integer as an initiation value, in order to modify the string once
--its name is associated with the fact that it is an advanced form of the origswap

--swapwords: it is the function that simplifies the entire process of swapping strings for the end user
--it takes as an input three strings, the string we are looking for, the one that we want to change it with and the String that we want to modify

searcher:: [String] -> String -> Int -> Int
searcher xs x i
	|null xs = -1
	|head xs == x= i
	|(take (length x) (head xs)) == x = i
	|otherwise = searcher (tail xs) x (i+1)

modifierrr:: [String] -> String -> String -> String
modifierrr xs x y
	|null xs = ""
	|head xs == x= y
	|(take (length x) (head xs)) == x = y ++ (drop (length x) (head xs))
	|otherwise = modifierrr (tail xs) x y

	
stage31::Int -> Int -> Int -> [a] -> [a] -> [a]
stage31 n x l xs m
    |x==0 = take (n-1) xs ++ m ++ (drop (length m) (drop (n-1) (take n xs))) ++ stage31 n (x+n) l (drop n xs) m
    |otherwise = xs


modifier:: [a] -> [a] -> Int -> [a]
modifier xs m n
    | n<0 = xs
    |otherwise = stage31 n 0 (length xs) xs m


advancedswap::String -> String -> String -> Int -> String
advancedswap x y xs i
   |x == y= xs
   |i==0 = advancedswap x y (origswap x y xs) (i+1)
   |searcher (words xs) x 1 == -1 = xs
   |otherwise = advancedswap x y (origswap x y xs) (i+1)
   
	
origswap::String -> String -> String -> String
origswap x y xs
    |x == y= xs
    |otherwise = unwords (modifier (words xs) (words (modifierrr (words xs) x y)) (searcher (words xs) x 1))
	

swapwords::String -> String -> String -> String
swapwords x y xs
   |x==y = xs
   |null x = xs
   |null y = xs
   |null xs = xs
   |otherwise = advancedswap x y xs 0



---- Part 4 ----




--supersearchInt: this function searches a given string for a specific character
--it takes three values x, the string, y the character and i which is and integer and an empty string
--it looks for one instance of a character and which character is in the list 

--advancedsearch: it a function that 
--looks for similar characters in the strings that we want to find
--it takes 2 values the two strings that we will search for similar characters
--and it will use the function supersearchInt to find where the instances of two characters are and it will replace them with a *, it will output the string with the crossed characters
--the name is appropriate as it a more advanced for of the search function

--stage43: it is in charge in finding if it is l or p or h or i
--it does that by determining how many times l, p, h and i have iterated
--it has an String input which is given by the advancedsearch function
--the name signifies 4th task 3rd stage because this is the 3rd part of the solution

--stage44: it is in charge of finding what is the compatibility
--it does that by using the mod function or the equality function
--the name signifies 4th task 4th stage because this is the 4th part of the solution

--removerofspace function is in charge of removing the spaces from the strings in order to ensure that the result is accurate no matter the difference in the lengths of the
--strings it uses the remover which removes the spaces and the "addition" which adds a space at the end of a string in case it does not in order to ensure that the we do not lose any
--characters comprising the list

--compatibility: it is in charge of simplifying the compatibility process for the end user, we use mean to ensure that if a search shows that there are less similarities that is the truth


supersearchInt::String -> Char -> Int -> String -> Int
supersearchInt x y i ret= 
  if null x then
    i
  else if head x == y then
    supersearchInt (drop (length x) x) y (i+1) (ret ++ "*")  
  else supersearchInt (tail x) y (i+1) (ret ++ [head x])


corrector::String -> String
corrector x =
 if null x then
  x
 else (x ++ [last x])  

advancedsearch:: String -> String -> String
advancedsearch x y =
  if null y then
    x
  else if supersearchInt x (head y) 0 "" >0 then 
    advancedsearch ((take ((supersearchInt x (head y) 0 "")- 1 )x) ++ "*" ++ (drop (supersearchInt x (head y) 0 "")x)) (tail y)
  else advancedsearch x (tail y)
  

stage43::String -> Int -> Int
stage43 x y =
  if null x then
    y
  else if head x /= '*' then
    stage43 (tail x) (y + 1)
  else stage43 (tail x) y
  
  
stage44::Int -> String
stage44 x=
  if x== -1 then
    "Empty list"
  else if x==5 || x==1 || x`mod`5==4 then
    " loves "
  else if x`mod`4==0 || x== 0 then
    " is indifferent to "
  else if (x`mod`3==1 || x`mod`3==0 || x`mod`3==2) && x/=0 then
    " hates "
  else if x`mod`2==0 then
    " is physical to "
  else if x==9 then
    " loves "
  else " loves "


compatibility::String -> String -> String
compatibility x y = 
  if null x || null y then
    ""
  else if x==y then
    (x) ++ stage44(stage43 (advancedsearch ( (removerofspace x ' ')) (removerofspace y ' ')) 0) ++ y ++ " and " ++ y ++ stage44(stage43 (advancedsearch ( (removerofspace y ' ')) (removerofspace x ' ')) 0) ++ x
  else x ++ stage44(stage43 (advancedsearch (corrector(removerofspace x ' ')) (removerofspace y ' ')) 0) ++ y ++ " and " ++ y ++ stage44(stage43 (advancedsearch (corrector(removerofspace y ' ')) (removerofspace x ' ')) 0) ++ x


remover:: Eq a=> [a] -> a -> [a] -> [a] -> [a]
remover orig s t f =
   if null orig then
     f
   else if head orig == s then
    remover (tail orig) s (drop (length t) t) (f ++ t)
   else remover (tail orig) s ( t ++ (take 1 orig)) f


addition:: [a] -> [a] -> [a]
addition xs x=
  if null xs then
    xs
  else xs ++ x
  
removerofspace:: Eq a=> [a] -> a -> [a]
removerofspace xs x = 
 if null xs then
  []
 else if (last xs) == x then
  remover xs x [] []
 else remover (addition xs [x]) x [] []
  
  
 ---- Part 5 ----

 
  --split1: it is the first stage of the split function, it it in charge of splitting the inputted list by a character provided
--it has 4 internal variables, one to hold the list, one for the character, one for an intermediary list and one for the final nesterd
--list that it ouitputs, its name showcases the fact that it is the first stage of split

--adder: this function allows for a correct splitting of a list by putting the character that is used to indicate where to split the list at the end
--this is done to accomodate the way that the split1 function works
--its name comes from the fact that it adds to a list

--converted:: this function converts a character to a list in order to be used by the adder function

--split:: it simplifies the splitting process for the end user

split1:: Eq a=> [a] -> a -> [a] -> [[a]] -> [[a]]
split1 orig s t f =
   if null orig then
     f
   else if head orig == s then
    split1 (tail orig) s (drop (length t) t) (f ++ [t])
   else split1 (tail orig) s ( t ++ (take 1 orig)) f


adder:: [a] -> [a] -> [a]
adder xs x=
  if null xs then
    xs
  else xs ++ x

converted:: a -> [a]
converted x = [x]

split:: Eq a=> [a] -> a -> [[a]]
split xs x = 
 if null xs then
  []
 else if (last xs) ==x then
  garbagecoll (split1 xs x [] []) []
 else garbagecoll (split1 (adder xs (converted x)) x [] []) []


garbagecoll::Eq a=> [[a]] -> [[a]]-> [[a]]
garbagecoll xs temp =
  if null xs then
    temp
  else if head xs == [] then
    garbagecoll (tail xs) temp
  else garbagecoll (tail xs) (temp ++ [head xs])
