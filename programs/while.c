define x;
define y;
define z;

assign x := 10;
assign y := 1;

while (x < 12) {
    assign x := x + 1;
    assign y := y * x;
};
return y;

