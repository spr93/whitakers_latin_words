package Unicode_Features is

Unicode_Function_Available : constant Boolean := False;

function Unicode_To_Basic_Text (W_Line : in Wide_String) return String;

procedure Get_Unicode (LINE : in out String; L : in out Integer);

procedure handle_Unicode_Exception;

end Unicode_Features;
