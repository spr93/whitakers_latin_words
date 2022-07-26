package Unicode_Features is

   Unicode_Function_Available : constant Boolean := True;

   Unicode_Exception : exception;
   procedure Handle_Unicode_Exception;
   
   pragma Wide_Character_Encoding (UTF8);
   procedure Get_Unicode (LINE : in out String; L : in out Integer);
   pragma Inline_Always (Get_Unicode);

   function Unicode_To_Basic_Text (W_Line : in Wide_String) return String;
   pragma Inline_Always (Unicode_To_Basic_Text);

end Unicode_Features;
