define x;
define y;
define z;

assign x := 10;
assign y := 1;

while (0 < x) {
    assign x := x + 1;
    assign y := y * x;
};

assign z := 42;
