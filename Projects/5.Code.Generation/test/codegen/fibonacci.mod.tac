../test/codegen/fibonacci.mod:
[[ module: fibonacci
  [[ type manager
    base types:
      <NULL>
      <int>
      <char>
      <bool>
      <ptr(4) to <NULL>>
    pointer types:
      <ptr(4) to <NULL>>
      <ptr(4) to <array of <char>>>
      <ptr(4) to <array 18 of <char>>>
      <ptr(4) to <array 29 of <char>>>
      <ptr(4) to <array 11 of <char>>>
      <ptr(4) to <array 5 of <char>>>
    array types:
      <array of <char>>
      <array 18 of <char>>
      <array 29 of <char>>
      <array 11 of <char>>
      <array 5 of <char>>
  ]]
  [[
    [ *DIM(<ptr(4) to <NULL>>,<int>) --> <int>     ]
    [ *DOFS(<ptr(4) to <NULL>>) --> <int>     ]
    [ *ReadInt() --> <int>     ]
    [ *ReadNumber(<ptr(4) to <array of <char>>>) --> <int>     ]
    [ *WriteChar(<char>) --> <NULL>     ]
    [ *WriteInt(<int>) --> <NULL>     ]
    [ *WriteLn() --> <NULL>     ]
    [ *WriteStr(<ptr(4) to <array of <char>>>) --> <NULL>     ]
    [ @_str_1   <array 18 of <char>>     ]
      [ data: 'Fibonacci numbers' ]
    [ @_str_2   <array 29 of <char>>     ]
      [ data: 'Enter a number (0 to exit): ' ]
    [ @_str_3   <array 11 of <char>>     ]
      [ data: 'fibonacci(' ]
    [ @_str_4   <array 5 of <char>>     ]
      [ data: ') = ' ]
    [ @_str_5   <array 29 of <char>>     ]
      [ data: 'Enter a number (0 to exit): ' ]
    [ *fib(<int>) --> <int>     ]
    [ @i        <int>     ]
    [ $t0       <ptr(4) to <array 18 of <char>>>     ]
    [ $t1       <ptr(4) to <array 29 of <char>>>     ]
    [ $t2       <int>     ]
    [ $t3       <ptr(4) to <array 11 of <char>>>     ]
    [ $t4       <ptr(4) to <array 5 of <char>>>     ]
    [ $t5       <int>     ]
    [ $t6       <ptr(4) to <array 29 of <char>>>     ]
    [ $t7       <int>     ]
  ]]
  [[ fibonacci
      0:     &()    t0 <- _str_1
      1:     param  0 <- t0
      2:     call   WriteStr
      3:     call   WriteLn
      4:     call   WriteLn
      5:     &()    t1 <- _str_2
      6:     param  0 <- t1
      7:     call   t2 <- ReadNumber
      8:     assign i <- t2
      9: 5_while_cond:
     10:     if     i > 0 goto 6_while_body
     11:     goto   4
     12: 6_while_body:
     13:     &()    t3 <- _str_3
     14:     param  0 <- t3
     15:     call   WriteStr
     16:     param  0 <- i
     17:     call   WriteInt
     18:     &()    t4 <- _str_4
     19:     param  0 <- t4
     20:     call   WriteStr
     21:     param  0 <- i
     22:     call   t5 <- fib
     23:     param  0 <- t5
     24:     call   WriteInt
     25:     call   WriteLn
     26:     &()    t6 <- _str_5
     27:     param  0 <- t6
     28:     call   t7 <- ReadNumber
     29:     assign i <- t7
     30:     goto   5_while_cond
     31: 4:
  ]]

  [[ procedure: fib
    [[
      [ %n        <int>       ]
      [ $t0       <int>       ]
      [ $t1       <int>       ]
      [ $t2       <int>       ]
      [ $t3       <int>       ]
      [ $t4       <int>       ]
    ]]
    [[ fib
        0:     if     n <= 1 goto 1_if_true
        1:     goto   2_if_false
        2: 1_if_true:
        3:     return n
        4:     goto   0
        5: 2_if_false:
        6:     sub    t0 <- n, 1
        7:     param  0 <- t0
        8:     call   t1 <- fib
        9:     sub    t2 <- n, 2
       10:     param  0 <- t2
       11:     call   t3 <- fib
       12:     add    t4 <- t1, t3
       13:     return t4
       14: 0:
    ]]
  ]]

  [[ procedure: ReadNumber
    [[
      [ $i        <int>       ]
      [ %str      <ptr(4) to <array of <char>>>       ]
      [ $t0       <int>       ]
    ]]
    [[ ReadNumber
        0:     param  0 <- str
        1:     call   WriteStr
        2:     call   t0 <- ReadInt
        3:     assign i <- t0
        4:     return i
    ]]
  ]]
]]
