var x >= 0;
var y >= 0;

maximize z: 2*x + 3*y;

subject to r1: x + 2*y <= 10;
subject to r2: 3*x + 2*y <= 15;
subject to r3: y <= 4;
end;
