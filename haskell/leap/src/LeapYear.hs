module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year | rem year 4 /= 0 = False
isLeapYear year | rem year 100 /= 0 = True
isLeapYear year | rem year 400 == 0 = True
isLeapYear _year = False
