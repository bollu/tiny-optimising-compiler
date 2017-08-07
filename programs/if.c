define x;
define y;
assign x := 1;
if x < 1 {
    assign y := 2;
} else {
    assign y := 3;
};

assign x := 2 * y;
return x;
