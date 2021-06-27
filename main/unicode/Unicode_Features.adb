 with Text_IO;
 with Ada.Characters.Conversions;
 with Ada.Characters.Handling;
 with Config;  use Config;
 with Preface; use Preface;

 package body Unicode_Features is

   ---BEGIN ADA202X-DEPENDENT CODE---
      -- THIS FUNCTION RELIES ON ADA 202X FEATURES (Unicode decompositions and
      -- To_Basic for Wide_Characters).
      -- FSF GNAT implemented them in or around July 2020. Non-GNAT compilers have not implemented
      -- these features (yet) to my knowledge.

   function Unicode_To_Basic_Text (W_Line : in Wide_String) return String is
            -- Converts Unicode accented forms to basic ASCII
            -- Useful for input that includes macrons.
            -- E.g., this causes 'ē' to be processed as 'e'

   begin
     return Ada.Characters.Conversions.To_String(W_Line);
   end Unicode_To_Basic_Text;

  ----END ADA202X-DEPENDENT---

  procedure Get_Unicode (LINE : in out String; L : in out Integer) is
  begin
    Text_IO.Get_Line (Line, L);
    Line (Line'First .. L) := Ada.Characters.Handling.To_Basic(
                              Line (Line'First .. L));
  end Get_Unicode;

 end Unicode_Features;
