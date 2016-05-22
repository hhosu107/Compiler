//
// test3
//
// IR generation
//

module test3;

var i: integer;

begin
  if (i > 3) then
    if (i > 5) then
      i := 0;
      i := 1
    else
      i := 1
    end;

    i := 0
  else
    i := 1
  end
end test3.
