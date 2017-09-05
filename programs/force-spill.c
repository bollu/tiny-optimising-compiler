define x1;
define x2;
define x3;
define x4;
define x5;
define x6;
define x7;
define x8;
define x9;
define loop;

assign x1 := 1;
assign x2 := 1;
assign x3 := 1;
assign x4 := 1;
assign x5 := 1;
assign x6 := 1;
assign x7 := 1;
assign x8 := 1;
assign x9 := 1;
assign loop := 1;

while loop < 12 {
    assign x1 := loop + 1;
    assign x2 := loop + 2;
    assign x3 := loop + 3;
    assign x4 := loop + 4;
    assign x5 := loop + 5;
    assign x6 := loop + 6;
    assign x7 := loop + 7;
    assign x8 := loop + 8;
    assign x9 := loop + 9;
    assign loop := loop + 1;
};

return x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9;
