//
// test6
//
// IR generation
//

module test12;

var i: integer;
    A: boolean[10];

procedure foo(M: boolean[]);
begin
  M[i] := M[i]
end foo;

begin
  i := 0;
  foo(A)
end test12.
