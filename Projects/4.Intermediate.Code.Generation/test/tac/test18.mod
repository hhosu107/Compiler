module test18;
var a: integer[5][5][5];
    b: integer[5][5];
    c: integer[5];
    d: integer;
function f1(A: integer[][][]) : integer;
begin
  A[1][2][3] := 1;
  return 1
end f1;

function f2(A: integer[][]) : integer;
begin
  A[1][2] := 2;
  return 2
end f2;

function f3(A: integer[]) : integer;
begin
  A[1] := 3;
  return 3
end f3;
function f4(A: integer) : integer;
begin
  A := 4;
  return 4
end f4;
begin
  f1(a);
  f2(a[1]);
  f3(a[1][2]);
  f4(a[1][2][3])
end test18.
