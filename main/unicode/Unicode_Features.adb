with Ada.Wide_Text_IO; use Ada.Wide_Text_IO;
with Ada.Characters.Conversions;
with Ada.Wide_Characters.Handling;
with Text_IO;
with STRINGS_PACKAGE;  use STRINGS_PACKAGE;
with WORD_PARAMETERS;  use WORD_PARAMETERS;
with PREFACE;
with CONFIG;           use CONFIG;

package body Unicode_Features is

   ---BEGIN ADA2022-DEPENDENT CODE---

   -- THIS FUNCTION RELIES ON ADA 202X FEATURES (Unicode decompositions and
   -- To_Basic for Wide_Characters). FSF GNAT implemented them in or around
   -- July 2020. Non-GNAT compilers have not implemented these features
   -- to my knowledge.

   function Unicode_To_Basic_Text (W_Line : in Wide_String) return String is
   -- Converts Unicode accented forms to basic ASCII Useful for input that
   -- includes macrons. E.g., this causes 'ē' to be processed as 'e'

   begin
      return
        Ada.Characters.Conversions.To_String
          (Ada.Wide_Characters.Handling.To_Basic (W_Line));

   exception
      when others =>
         Handle_Unicode_Exception;
         return "";

   end Unicode_To_Basic_Text;

   ----END ADA2022-DEPENDENT CODE---

   procedure Get_Unicode (LINE : in out String; L : in out Integer) is

      pragma Wide_Character_Encoding (UTF8);
      pragma Inline (Get_Unicode);

      W_Line : constant Wide_String := Ada.Wide_Text_IO.Get_Line;

      T_Line : constant String := Unicode_To_Basic_Text (W_Line);

   begin

      if T_Line'Last <= INPUT_LINE_LENGTH then
         LINE (T_Line'Range) := T_Line;
         L                   := T_Line'Length;
      else
         LINE := T_Line (T_Line'First .. LINE'Last);
         L    := LINE'Last;
      end if;

   exception
      when Ada.Wide_Text_IO.Data_Error | Ada.Wide_Text_IO.Layout_Error =>
         Handle_Unicode_Exception;

   end Get_Unicode;

   procedure Handle_Unicode_Exception is
      Error_Notice : constant String := "ERROR processing input as Unicode.";
   begin

      case METHOD is
         when INTERACTIVE =>
            PREFACE.Format (INVERSE);
            PREFACE.PUT_LINE (Error_Notice);
            PREFACE.PUT
              ("Try again. If works, disable DO_UNICODE in the preferences menu. Enter " &
               CHANGE_PARAMETERS_CHARACTER);
            PREFACE.Format (RESET);
            PREFACE.NEW_LINE;
            WORDS_MODE (DO_UNICODE_INPUT) := False;
         when others =>
            Set_Output (Standard_Error);
            Text_IO.Put_Line (Error_Notice);
            Text_IO.Put_Line
              ("Try disabling DO_UNICODE and saving preferences in interactive mode.");
            raise Use_Error; -- Need to stop processing this file, otherwise we could keep looping this error
      end case; --METHOD

   end Handle_Unicode_Exception;

end Unicode_Features;
