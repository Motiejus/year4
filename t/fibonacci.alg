begin
    t1 := 1;
    t2 := 1;
    i := 2;
    read d;
    write t1;
    write t2;
    while i < d do
        begin
            t3 := t1 + t2;
            t1 := t2;
            t2 := t3;
            i := i + 1;
            write t3
        end
end
