var x{1..4} >= 0;

minimize z: x[1] + x[2] - 2*x[3] + x[4];

subject to
r1: x[1] - x[2] - x[3] - 2*x[4] >= 2;
r2: x[1] + x[2] - x[4] <= 8;
r3: sum{i in 1..4} x[i] = 4;

end;
