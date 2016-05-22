//
// test6
//
// IR generation
//

module test12;

var i: integer;
    A: integer[10];

procedure foo(M: integer[]);
begin
  M[i] := i
end foo;

begin
  i := 0;
  foo(A)
end test12.
