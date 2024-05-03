adjust_date :: (Int, Int, Int) -> Int -> (Int, Int, Int)
adjust_date (a, b, c) d =
 if checker (a, b, c) d == False
   then error "invalid input"
   else calc (a, b, c) d

--This functions does the calculations checking if a year is a leaperyear and also getting the values of the months from monthcheck
calc :: (Int, Int, Int) -> Int -> (Int, Int, Int)
calc (a, b, c) d 
 | ((isleapyear c == False) || ((isleapyear c == True) && b /= 2)) =
   if d > 0
     then if (a + d) > monthcheck b
       then if (b + 1) > 12
         then (((a+d)-monthcheck 1), 1, (c+1))
         else (((a+d)- monthcheck b), (b+1), c)
       else ((a+d), b, c)
     else if d < 0
       then if (a + d) <= 0
         then if (b - 1 == 2) && (isleapyear c == True)
           then (((a+d) + 29), 2, c)
           else if b - 1 == 0
             then (((a+d) + monthcheck 12), 12, (c - 1))
             else (((a+d) + monthcheck (b - 1)), (b-1), c)
       else ((a+d), b, c)
     else (0,0,0)
 | otherwise =
   if (a + d) > 29
     then (((a+d) - 29), (b+1), (c))
     else if (a + d) <= 0
       then (((a+d) + monthcheck 1), 1, (c))
       else ((a+d), b, (c))


--This function checks if a given year is a leap year using the conditions required to check
isleapyear :: Int -> Bool
isleapyear a =
  if a `mod` 4 /= 0
    then False
    else if (a `mod` 100 == 0) && (a `mod` 400 == 0)
      then True
      else if a `mod` 100 /= 0
        then True
        else False


--This function checks if a given date is valid by checking if it fits within the given conditions 
validdate :: (Int, Int, Int) -> Bool
validdate (a, b, c) 
 | (((a >= 1) && (b >= 1) && (c >=1600)) && ((a <= monthcheck b) && (b <= 12) && (c <=3000)) && (isleapyear c == False)) = True
 | (((a >= 1) && (c >=1600)) && ((a <= 29) && (b == 2) && (c <=3000)) && (isleapyear c == True)) = True
 | (((a >= 1) && (b >= 1) && (c >=1600)) && ((a <= monthcheck b) && (b /= 2) && (c <=3000)) && (isleapyear c == True)) = True
 | otherwise = False


--This function checks if the offset value is valid 
validvalue :: Int -> Bool
validvalue a = ((a >= -25) && ( a<= 25))


--This function puts the validdate and validvalue tofether to giive error messages if a invalid value is detected
checker :: (Int, Int, Int) -> Int -> Bool
checker (x, y, z) a =
 if validdate (x, y, z) == False
   then False
   else if validvalue a == False
    then False
    else True

--This function checks what month it is and returns its respective amount of days
monthcheck :: Int -> Int
monthcheck a =
 if ((a >= 4) && (a <8))
   then if a `mod` 2 == 0
     then 30
     else 31
   else if a > 8
     then if a `mod` 2 == 1
       then 30
       else 31
   else if a < 4
     then if a == 2
       then 28
       else 31
   else 31