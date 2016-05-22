//
// test8
//
// IR generation
//

module test10;

var i: integer;
    b: boolean;

begin
  i := -i+i;
  b := !b;
  if (!b) then
    b := !b
  else
    b := b
  end
end test10.
