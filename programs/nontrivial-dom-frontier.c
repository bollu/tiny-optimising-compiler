define x;
define y;
assign x := 1;
assign y := 2;

if x < 1 {
    assign y := y + 2;
} else {
    assign y := y + 3;
};

if y < 42 {
    assign y :=  y * 4;
} else {
    assign y := y * 5;
};

assign x := 2 * y;
return x;
