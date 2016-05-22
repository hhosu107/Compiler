//
// test6
//
// IR generation
//

module test9;

var i: integer;
    A: integer[10][5][4];

procedure foo(M: integer[][][]);
begin
  M[1][M[4][5][6]][3] := i
end foo;

begin
  i := 0;
  foo(A)
end test9.
