with Ada.Wide_Text_IO; use Ada.Wide_Text_IO;
with Ada.Characters.Conversions;
with Ada.Wide_Characters.Handling;
with STRINGS_PACKAGE;  use STRINGS_PACKAGE;
with WORD_PARAMETERS;  use WORD_PARAMETERS;
with PREFACE;
with Config; use Config;

package body Unicode_Features is

   ---BEGIN ADA202X-DEPENDENT CODE---
   -- THIS FUNCTION RELIES ON ADA 202X FEATURES (Unicode decompositions and
   -- To_Basic for Wide_Characters). FSF GNAT implemented them in or around
   -- July 2020. Non-GNAT compilers have not implemented these features (yet)
   -- to my knowledge.

   function Unicode_To_Basic_Text (W_Line : in Wide_String) return String is
   -- Converts Unicode accented forms to basic ASCII Useful for input that
   -- includes macrons. E.g., this causes 'ē' to be processed as 'e'

   begin
      return
        Ada.Characters.Conversions.To_String(
           Ada.Wide_Characters.Handling.To_Basic (W_Line));
   exception

      when Ada.Wide_Text_IO.Data_Error | Ada.Wide_Text_IO.Layout_Error =>
         if WORDS_MODE (DO_ANSI_FORMATTING) then
            PREFACE.PUT (ASCII.ESC & "[0m" & ASCII.ESC & "[7m");
         end if;
         PREFACE.PUT_LINE
           ("ERROR processing input as Unicode. Falling back to non-Unicode mode.");
         PREFACE.PUT
           ("If this resolves the problem, disable DO_UNICODE in the preferences menu.  Enter " &
            CHANGE_PARAMETERS_CHARACTER);
         if METHOD /= INTERACTIVE then
            PREFACE.PUT (" in interactive mode");
         end if;
         PREFACE.NEW_LINE;
         WORDS_MODE (DO_UNICODE_INPUT) := False;
         if WORDS_MODE (DO_ANSI_FORMATTING) then
            PREFACE.PUT (ASCII.ESC & "[0m");
         end if;
         PREFACE.NEW_LINE;
      return ("");
   end Unicode_To_Basic_Text;
   ----END ADA202X-DEPENDENT---

   procedure Get_Unicode (LINE : in out String; L : in out Integer) is

      pragma Wide_Character_Encoding (UTF8);
      pragma Inline (Get_Unicode);

      W_Line : constant Wide_String := Ada.Wide_Text_IO.Get_Line;

      T_Line : constant String := Unicode_To_Basic_Text (W_Line);

   begin

      if T_Line'Last <= INPUT_LINE_LENGTH then
         LINE (T_Line'RANGE) := T_Line;
         L                   := T_Line'Length;
      else
         LINE := T_Line (T_Line'First .. LINE'Last);
         L    := LINE'Last;
      end if;

   end Get_Unicode;

end Unicode_Features;
