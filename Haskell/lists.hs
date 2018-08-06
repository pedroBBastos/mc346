myLast l = last l

lastButOne l = last (init l)

elementAt l p = l !! p-1

myLength l = length l

isPalindrome l = if l == reverse l then True else False
