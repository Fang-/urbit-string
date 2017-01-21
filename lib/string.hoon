::  String processing library.
::
::  When we refer to a "string", we mean a tape unless otherwise is specified.
::
!:
::
|%
::
::::
::  Deducing properties of the string.
::
::  String length.
::  (lent str)
::
::  Sum of the values of the string's characters.
++  sum
  |=  str/tape
  ^-  @ud
  ::TODO  Would it be better to just use roll here?
  =|  sm/@ud
  |-
  ?~  str  sm
  $(sm (add sm i.str), str t.str)
::
::::
::  Getting parts of the string.
::
::  First a characters.
::  (scag a str)
::
::  Last a characters.
++  tail
  |=  {str/tape a/@ud}
  ^-  tape
  ?:  (gte a (lent str))  str
  (slag (sub (lent str) a) str)
::
::  Get b characters starting from index a.
::  (swag [a b] str)
::
::  Characters from index a up to but excluding index b.
++  get
  |=  {str/tape a/@ud b/@ud}
  ^-  tape
  ?:  (gth a b)  ~
  (swag [a (sub b a)] str)
::
::  The character at index a.
::  (snag a str)
::
::::
::  Finding string(s) in the string.
::
::  Find the first occurence of nedl in the string.
::  (find nedl str)
::
::  Find all occurences of nedl in the string.
::  (fand nedl str)
::
::  Starts with nedl?
++  starts-with
  |=  {str/tape nedl/tape}
  ^-  ?
  ::TODO  find vs scag & compare?
  =((find nedl str) [~ 0])
::
::  Ends with nedl?
++  ends-with
  |=  {str/tape nedl/tape}
  ^-  ?
  ?:  (gth (lent nedl) (lent str))  |
  ::TODO  find vs slag & compare?
  =((find nedl str) [~ (sub (lent str) (lent nedl))])
::
::  Contains nedl?
++  contains
  |=  {str/tape nedl/tape}
  ^-  ?
  ?~  (find nedl str)  |  &
::
::  Contains any nedl?
++  contains-any
  |=  {str/tape nedls/(list tape)}
  ^-  ?
  =+  i=0
  =+  m=(lent nedls)
  |-
  ?:  =(i m)  |
  ?:  (contains str (snag i nedls))  &
  $(i +(i))
::
::  Contains all nedls?
++  contains-all
  |=  {str/tape nedls/(list tape)}
  ^-  ?
  =+  i=0
  =+  m=(lent nedls)
  |-
  ?:  =(i m)  &
  ?.  (contains str (snag i nedls))  |
  $(i +(i))
::
::  Find the starting index of the first occurence of nedl in the string.
::  (find nedl str)
::
::  Find the starting index of the last occurence of nedl in the string.
++  find-last
  |=  {str/tape nedl/tape}
  ^-  (unit @ud)
  ::TODO  Be less lazy, maybe.
  =+  res=(find (flop nedl) (flop str))
  ?~  res  ~
  [~ (sub (lent str) (add (lent nedl) u.res))]
::
::  Find the starting index of the nth occurence of nedl in the string.
++  find-nth
  |=  {str/tape nedl/tape n/@ud}
  ^-  (unit @ud)
  ::TODO  Be less lazy, maybe.
  =+  res=(fand nedl str)
  ?:  (gth n (lent res))  ~
  [~ (snag (sub n 1) res)]
::
::  Find all occurences of nedl in the string.
::  (fand nedl str)
::
::  Finds the first occurence of any of the nedls, returning the index and the
::   nedl found at that index.
++  find-any
  |=  {str/tape nedls/(list tape)}
  =|  best/(unit {i/@ud nedl/tape})
  ^+  best
  =+  ni=0
  |-  ^+  best
  ?:  =(ni (lent nedls))  best
  =+  res=(find (snag ni nedls) str)
  %=  $
    ni    (add ni 1)
    best  ?~  res  best
          [~ u.res (snag ni nedls)]
    str   ?~  res  str
          (scag u.res str)
  ==
::
::  Finds the last occurence of any of the nedls, returning the index and the
::   nedl found at that index.
++  find-last-any
  |=  {str/tape nedls/(list tape)}
  ^-  (unit {i/@ud nedl/tape})
  =/  best
    %+  find-any
    (flop str)
    (turn nedls flop)
  ?~  best  best
  :+  ~
  (sub (lent str) (add (lent nedl.u.best) i.u.best))
  (flop nedl.u.best)
::
::::
::  Modifying the string into a new string.
::
::  Concatenate string a onto the string.
::  (weld str a)
::
::  Reverse the string.
::  (flop str)
::
::  Trim whitespace off string left-side.
++  trim-left
  |=  str/tape
  ^-  tape
  (scan str ;~(pfix spac:poja (star next)))
::
::  Trim whitespace off string right-side.
++  trim-right
  |=  str/tape
  ^-  tape
  ::TODO  Be less lazy, maybe.
  %-  flop
  (trim-left (flop str))
::
::  Trim whitespace off string ends.
++  trim
  |=  str/tape
  ^-  tape
  (trim-left (trim-right str))
::
::  Delete b characters from the string, starting at index a.
::  (oust [a b] str)
::
::  Delete characters from index a up to but excluding index b.
++  delete
  |=  {str/tape a/@ud b/@ud}
  ^-  tape
  ?:  (gth a b)  ~
  (oust [a=a b=(sub b a)] str)  ::TODO  Remove faces when bug is fixed.
::
::  Deletes the first occurence of nedl in the string.
++  delete-first
  |=  {str/tape nedl/tape}
  ^-  tape
  ::TODO  Be less lazy, maybe.
  =+  res=(find nedl str)
  ?~  res  str
  (oust [a=u.res b=(lent nedl)] str)  ::TODO  Remove faces when bug is fixed.
::
::  Deletes the last occurence of nedl in the string.
++  delete-last
  |=  {str/tape nedl/tape}
  ^-  tape
  =+  res=(find-last str nedl)
  ?~  res  str
  (oust [a=u.res b=(lent nedl)] str)  ::TODO  Remove faces when bug is fixed.
::
::  Delete the nth occurence of nedl in the string.
++  delete-nth
  |=  {str/tape nedl/tape n/@ud}
  ^-  tape
  =+  res=(find-nth str nedl n)
  ?~  res  str
  (oust [a=u.res b=(lent nedl)] str)
::
::  Delete all occurences of nedl in the string.
++  delete-all
  |=  {str/tape nedl/tape}
  ^-  tape
  =+  res=(fand nedl str)
  ?:  =((lent res) 0)  str
  =+  s=str
  =+  l=(lent nedl)
  =+  i=(lent res)
  |-
  ?:  =(i 0)  s
  $(i (sub i 1), s (oust [a=(snag (sub i 1) res) b=l] s))
::
::  Replace characters at index a up to but excluding index b with string s.
++  replace
  |=  {str/tape {a/@ b/@} s/tape}
  ^-  tape
  :(welp (scag a str) s (slag b str))
::
::  Replace the first occurence of nedl in the string with s.
++  replace-first
  |=  {str/tape nedl/tape s/tape}
  ^-  tape
  =+  res=(find nedl str)
  ?~  res  str
  (replace str [u.res (sub (add u.res (lent s)) 2)] s)
::
::  Replace the last occurence of nedl in the string with s.
++  replace-last
  |=  {str/tape nedl/tape s/tape}
  ^-  tape
  =+  res=(find-last str nedl)
  ?~  res  str
  (replace str [u.res (sub (add u.res (lent s)) 2)] s)
::
::  Replace the nth occurence of nedl in the string with s.
++  replace-nth
  |=  {str/tape nedl/tape s/tape n/@ud}
  ^-  tape
  =+  res=(find-nth str nedl n)
  ?~  res  str
  (replace str [u.res (sub (add u.res (lent s)) 2)] s)
::
::  Replace all occurences of nedl in the string with s.
++  replace-all
  |=  {str/tape nedl/tape s/tape}
  ^-  tape
  %+  roll
  (scan str (star ;~(pose (cold (crip s) (jest (crip nedl))) next)))
  ::TODO  Can't we do this within the parser?
  |=  {p/cord c/tape}
  ^-  tape
  (weld c (trip p))
::
::::
::  Modifying the string into a non-string.
::
::  Split string by string delimiter.
++  split
  |=  {str/tape delim/tape}
  ^-  (list tape)
  (split-rule str (jest (crip delim)))
::
::  Split string by parsing rule delimiter.
++  split-rule
  |*  {str/tape delim/rule}
  ^-  (list tape)
  %+  fall
    (rust str (more delim (star ;~(less delim next))))
  [str ~]
::
::::
::  Create a string from a non-string.
::
::  Glue a list of strings into a single string using a delimiter.
++  glue
  |=  {pieces/(list tape) delim/tape}
  ^-  tape
  %+  roll  pieces
  |=  {piece/tape str/tape}
  ?~  str  piece
  :(welp str delim piece)
::
--
