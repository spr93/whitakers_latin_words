with CONFIG;

package body PREFACE is

    procedure PUT (S : in String) is
    begin
        if not CONFIG.SUPPRESS_PREFACE then
            Text_IO.Put (Text_IO.Current_Output, S);
        end if;
    end PUT;

    procedure SET_COL (PC : in Text_IO.Positive_Count) is
    begin
        if not CONFIG.SUPPRESS_PREFACE then
            Text_IO.Set_Col (Text_IO.Current_Output, PC);
        end if;
    end SET_COL;

    procedure PUT_LINE (S : in String) is
    begin
        if not CONFIG.SUPPRESS_PREFACE then
            Text_IO.Put_Line (Text_IO.Current_Output, S);
        end if;
    end PUT_LINE;

    procedure NEW_LINE (SPACING : Text_IO.Positive_Count := 1) is
    begin
        if not CONFIG.SUPPRESS_PREFACE then
            Text_IO.New_Line (Text_IO.Current_Output, SPACING);
        end if;
    end NEW_LINE;

    procedure PUT (N : in Integer; WIDTH : Text_IO.Field := Integer'Width) is
        package INTEGER_IO is new Text_IO.Integer_IO (Integer);
    begin
        if not CONFIG.SUPPRESS_PREFACE then
            INTEGER_IO.Put (Text_IO.Current_Output, N, WIDTH);
        end if;
    end PUT;

    procedure Format (Format : in STRINGS_PACKAGE.Format_Command) is
    begin
        if not CONFIG.SUPPRESS_PREFACE then
            STRINGS_PACKAGE.Format (Text_IO.Current_Output, Format);
        end if;
    end Format;

end PREFACE;
