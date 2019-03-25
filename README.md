# Urbit String

String processing library for Urbit.

Work in progress, feedback and pull requests welcome!

### Installing / Using

1. Copy lib/string.hoon into {pier}/{desk}/lib
1. Copy app/string-tester.hoon into {pier}/{desk}/app
1. From the dojo:
  * |start %string-tester
  * :string-tester %test

You should see:
```
>=
%sum
%tail
%get
%starts-with
%ends-with
%contains
%contains-any
%contains-all
%find-last
%find-nth
%find-first-any
%find-last-any
%trim-left
%trim-right
%trim
%delete
%delete-first
%delete-last
%delete-nth
%delete-all
%replace
%replace-first
%replace-last
%replace-nth
%replace-all
%split
%glue
[%all-passed "(:"]
```
