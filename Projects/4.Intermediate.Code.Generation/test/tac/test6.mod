//
// test6
//
// IR generation
//

module test6;

var i: integer;
    A: integer[10][5][4];
    b: boolean;
    b1: boolean[5];
    b2: boolean[5][7];

function foo(M: integer[][]; B: boolean): boolean;
begin
  M[1][2] := i;
  return B
end foo;

procedure bar(C: boolean);
begin
end bar;

begin
  i := 0;
  b := foo(A[i], b);
  bar(b2[i][i]);
  if(foo(A[i], b)) then
    i := 1
  else
    if(b1[3]) then
      i := 2
    end
  end
end test6.
