package Unicode_Features is

Unicode_Function_Available : constant Boolean := False;

function Unicode_To_Basic_Text (W_Line : in Wide_String) return String;
  pragma Inline_Always(Unicode_To_Basic_Text);
  pragma Wide_Character_Encoding(UTF8);

procedure Get_Unicode (LINE : in out String; L : in out Integer);

end Unicode_Features;
