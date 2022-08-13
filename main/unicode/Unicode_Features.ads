package Unicode_Features is

   -- Converts Unicode accented forms to basic ASCII Useful for input that
   -- includes macrons. E.g., this causes 'ē' to be processed as 'e'

   -- THIS PACKAGE RELIES ON ADA 2022 FEATURES (Unicode decompositions and
   -- To_Basic for Wide_Characters). (AI12-0260-1; 2022 ARM (draft) A.3.5 61.1/5)
   -- FSF GNAT implemented them in or around July 2020. To my knowledge, other compilers
   -- have not yet implemented these features.

   pragma Compile_Time_Warning (Standard.True,
          "UNICODE-PROCESSING VERSION (for macron input); if build fails, use the files in the nonunicode directory);

   Unicode_Function_Available : constant Boolean := True;

   Unicode_Exception : exception;
   procedure Handle_Unicode_Exception;

   procedure Get_Unicode (LINE : in out String; L : in out Integer);
   pragma Inline_Always (Get_Unicode);

   function Unicode_To_Basic_Text (W_Line : in Wide_String) return String;
   pragma Inline_Always (Unicode_To_Basic_Text);

end Unicode_Features;
