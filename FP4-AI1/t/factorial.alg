begin
    i := 1;
    ans := 1;
    read d;
    while i < d + 1 do
        begin
            ans := ans * i;
            i := i + 1
        end;
    write ans
end
