module test17;

var a : integer[3][5][5][6];

function f(a : integer[][5][][]) : integer;
begin
    if (a[0][0][0][0] > a[1][1][1][1]) then
    else
// a[0][0][0][0] := a[1][1][1][1]
    end;
    return -2147483647
end f;

begin
    WriteInt(f(a))
end test17.
