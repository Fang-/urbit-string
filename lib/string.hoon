::  String processing library.
::
::  When we refer to a "string", we mean a tape unless otherwise is specified.
::
!:
::
|_  str/tape
::
::::
::  Deducing properties of the string.
::
::  String length.
::  (lent str)
::
::  Sum of the values of the string's characters.
++  sum
  |.
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
  |=  a/@ud
  ^-  tape
  ?:  (gte a (lent str))  str
  (slag (sub (lent str) a) str)
::
::  Get b characters starting from index a.
::  (swag [a b] str)
::
::  Characters from index a up to but excluding index b.
++  get
  |=  {a/@ud b/@ud}
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
++  swith
  |=  nedl/tape
  ^-  ?
  ::TODO  find vs scag & compare?
  =((find nedl str) [~ 0])
::
::  Ends with nedl?
++  ewith
  |=  nedl/tape
  ^-  ?
  ?:  (gth (lent nedl) (lent str))  |
  ::TODO  find vs slag & compare?
  =((find nedl str) [~ (sub (lent str) (lent nedl))])
::
::  Contains nedl?
++  cont
  |=  nedl/tape
  ^-  ?
  ?~  (find nedl str)  |  &
::
::  Contains any nedl?
++  cany
  |=  nedls/(list tape)
  ^-  ?
  =+  i=0
  =+  m=(lent nedls)
  |-
  ?:  =(i m)  |
  ?:  (cont (snag i nedls))  &
  $(i +(i))
::
::  Contains all nedls?
++  call
  |=  nedls/(list tape)
  ^-  ?
  =+  i=0
  =+  m=(lent nedls)
  |-
  ?:  =(i m)  &
  ?.  (cont (snag i nedls))  |
  $(i +(i))
::
::  Find the starting index of the first occurence of nedl in the string.
::  (find nedl str)
::
::  Find the starting index of the last occurence of nedl in the string.
++  finl
  |=  nedl/tape
  ^-  (unit @ud)
  ::TODO  Be less lazy, maybe.
  =+  res=(find (flop nedl) (flop str))
  ?~  res  ~
  [~ (sub (lent str) (add (lent nedl) u.res))]
::
::  Find the starting index of the nth occurence of nedl in the string.
++  finn
  |=  {nedl/tape n/@ud}
  ^-  (unit @ud)
  ::TODO  Be less lazy, maybe.
  =+  res=(fand nedl str)
  ?:  (gth n (lent res))  ~
  [~ (snag (sub n 1) res)]
::
::  Find all occurences of nedl in the string.
::  (fand nedl str)
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
++  triml
  |.
  ^-  tape
  (scan str ;~(pfix spac:poja (star next)))
::
::  Trim whitespace off string right-side.
++  trimr
  |.
  ^-  tape
  ::TODO  Be less lazy, maybe.
  %-  flop
  (triml(str (flop str)))
::
::  Trim whitespace off string ends.
++  trim
  |.
  ^-  tape
  (triml(str $:trimr))
::
::  Delete b characters from the string, starting at index a.
::  (oust [a b] str)
::
::  Delete characters from index a up to but excluding index b.
++  del
  |=  {a/@ud b/@ud}
  ^-  tape
  ?:  (gth a b)  ~
  (oust [a=a b=(sub b a)] str)  ::TODO  Remove faces when bug is fixed.
::
::  Deletes the first occurence of nedl in the string.
++  delf
  |=  nedl/tape
  ^-  tape
  ::TODO  Be less lazy, maybe.
  =+  res=(find nedl str)
  ?~  res  str
  (oust [a=u.res b=(lent nedl)] str)  ::TODO  Remove faces when bug is fixed.
::
::  Deletes the last occurence of nedl in the string.
++  dell
  |=  nedl/tape
  ^-  tape
  =+  res=(finl nedl)
  ?~  res  str
  ~&  res
  (oust [a=u.res b=(lent nedl)] str)  ::TODO  Remove faces when bug is fixed.
::
::  Delete the nth occurence of nedl in the string.
++  deln
  |=  {nedl/tape n/@ud}
  ^-  tape
  =+  res=(finn nedl n)
  ?~  res  str
  (oust [a=u.res b=(lent nedl)] str)
::
::  Delete all occurences of nedl in the string.
++  dela
  |=  nedl/tape
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
++  rep
  |=  {{a/@ b/@} s/tape}
  ^-  tape
  :(welp (scag a str) s (slag b str))
::
::  Replace the first occurence of nedl in the string with s.
++  repf
  |=  {nedl/tape s/tape}
  ^-  tape
  =+  res=(find nedl str)
  ?~  res  str
  (rep [u.res (add u.res (lent s))] s)
::
::  Replace the last occurence of nedl in the string with s.
++  repl
  |=  {nedl/tape s/tape}
  ^-  tape
  =+  res=(finl nedl)
  ?~  res  str
  (rep [u.res (add u.res (lent s))] s)
::
::  Replace the nth occurence of nedl in the string with s.
++  repn
  |=  {nedl/tape s/tape n/@ud}
  ^-  tape
  =+  res=(finn nedl n)
  ?~  res  str
  (rep [u.res (add u.res (lent s))] s)
::
::  Replace all occurences of nedl in the string with s.
++  repa
  |=  {nedl/tape s/tape}
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
  |=  delim/tape
  ^-  (list tape)
  (splitr (jest (crip delim)))
::
::  Split string by parsing rule delimiter.
++  splitr
  |*  delim/rule
  ^-  (list tape)
  %+  fall
    (rust str (more delim (star ;~(less delim next))))
  [str ~]
::
--
