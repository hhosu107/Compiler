//
// test6
//
// IR generation
//

module test11;

var i: integer;
    A: integer[10];

procedure foo(M: integer);
begin
  M := i
end foo;

begin
  i := 0;
  foo(A[3])
end test11.
