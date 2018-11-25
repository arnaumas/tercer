set aliatges;
param produccio;
param costAliatge{aliatges};
param propCu{aliatges};
param propAl{aliatges};

var x{aliatges} >= 0;

minimize cost: sum{i in aliatges} costAliatge[i]*x[i];

subject to
totalprod: sum{i in aliatges} x[i] = produccio;
alumini: sum{i in aliatges} propAl[i]*x[i] >= 0.23*produccio;
coure: sum{i in aliatges} propCu[i]*x[i] >= 0.7*produccio;

data;

set aliatges := A B;
param produccio := 1;
param costAliatge := A 8000 B 6000;
param propCu := A 0.8 B 0.68;
param propAl := A 0.2 B 0.32;

end;
