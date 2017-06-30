define x;
define y;
define z;

assign x := 10;
assign y = 1;

while (0 < x) {
    x = x - 1;
    y = y * x;
};

assign z = 42;
