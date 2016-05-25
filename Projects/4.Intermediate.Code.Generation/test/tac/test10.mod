//
// test8
//
// IR generation
//

module test10;

var i: integer;
    b: boolean;
    c: char;

begin
  i := -i;
  b := !true;
  if (false || true || (c # 't')) then
    b := false
  else
    b := true
  end
end test10.
