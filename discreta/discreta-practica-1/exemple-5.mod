param b{1..6};
param c{1..4};
param a {1..6, 1..4};

var x{1..4} >= 0;

maximize z: sum{i in 1..4} c[i]*x[i];

subject to
r{i in 1..6}: sum{j in 1..4} a[i,j]*x[j] <= 4;

data;
param a:
    1   2   3   4 :=
1   20  20  10  20
2   20  30  30  50
3   40  30  10  20
4   20  30  10  20
5   20  20  0   40
6   20  20  20  40;

param b := 1 6 2 10 3 11 4 7 5 8 6 12;
param c := 1 4 2 1 3 3 4 2;
end;
