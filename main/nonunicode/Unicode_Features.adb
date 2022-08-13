with Text_IO;
with Ada.Characters.Conversions;
with Ada.Characters.Handling;
with CONFIG;          use CONFIG;
with PREFACE;         use PREFACE;
with WORD_PARAMETERS; use WORD_PARAMETERS;

-- NON-UNICODE VERSION: The routines here just Wide_String to String; otherwise
-- they're empty
--                       placeholders to prevent compiler errors re missing routines.

package body Unicode_Features is

   -- NON-UNICODE VERSION: The routines here just Wide_String to String;
   -- otherwise they're empty placeholders to prevent compiler errors re
   -- missing routines.

   function Unicode_To_Basic_Text (W_Line : in Wide_String) return String is
   begin
      return Ada.Characters.Conversions.To_String (W_Line);
   end Unicode_To_Basic_Text;

   procedure Get_Unicode (LINE : in out String; L : in out Integer) is
   begin
      Text_IO.Get_Line (LINE, L);
      LINE (LINE'First .. L) :=
        Ada.Characters.Handling.To_Basic (LINE (LINE'First .. L));
   end Get_Unicode;

   procedure Handle_Unicode_Exception is
   begin
      WORDS_MODE (DO_UNICODE_INPUT) := False;
   end Handle_Unicode_Exception;

end Unicode_Features;
