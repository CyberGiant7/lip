# Reverse a list

Write a function to reverse the the elements of a list, with the following type:
```ocaml
val rev : 'a list -> 'a list = <fun>
```

For example:
```ocaml
rev [1;2;3;4;5] ;;
- : int list = [5; 4; 3; 2; 1]
```

*Hint*: recall the operator ``@`` that concatenates two lists:
```ocaml
[1;2;3] @ [4;5] 
- : int list = [1; 2; 3; 4; 5]
```
