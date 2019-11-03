with Ada.Text_IO; use ADA.Text_IO;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1;

with CONFIG; use CONFIG;
with STRINGS_PACKAGE; use STRINGS_PACKAGE;
with LATIN_FILE_NAMES; use LATIN_FILE_NAMES;
with WORD_PARAMETERS; use WORD_PARAMETERS;
with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
with ADDONS_PACKAGE; use ADDONS_PACKAGE;
with UNIQUES_PACKAGE; use UNIQUES_PACKAGE;
with WORD_SUPPORT_PACKAGE; use WORD_SUPPORT_PACKAGE;
with DEVELOPER_PARAMETERS; use DEVELOPER_PARAMETERS;
with WORD_PACKAGE; use WORD_PACKAGE;
with DICTIONARY_FORM;
with PUT_EXAMPLE_LINE;
with LIST_SWEEP;
with PUT_STAT;

-- Forms and age codes guided by sources incl.
-- Wolfram Mathworld at http://mathworld.wolfram.com/RomanNumerals.html
-- and Christomalis, S., "Trends and Traditions in the History of Written Numerals" in The Shape of Script: How and Why Writing Systems Change (2012)
-- and Hunt, L. N.H., et al., "The Historical Roots of Elementary Mathematics" (1988)
-- and _especially_ the clear discussions in Menninger, K., "Number Words and Number Symbols: A Cultural History of Numbers" (Eng. tr. 1992).

package Arabic2Roman is

subtype Numeral_String is Unbounded_String;
 Roman_Nums_CLASSICAL : constant array (1..11) of Numeral_String    := ( 1 => (To_Unbounded_String("I")),
                                                       2         => (To_Unbounded_String("V")),
                                                       3         => (To_Unbounded_String("X")),
                                                       4         => (To_Unbounded_String("L")),
                                                       5         => (To_Unbounded_String("C")),
                                                       6         => (To_Unbounded_String("|)")),
                                                       7         => (To_Unbounded_String("(|)")),
                                                       8         => (To_Unbounded_String("|))")),
                                                       9         => (To_Unbounded_String("((|))")),
                                                      10         => (To_Unbounded_String("|)))")),
                                                      11         => (To_Unbounded_String("(((|)))"))); -- 100_000

                                                      -- Stop at 100_000 for classical period.  See also Pliny.


   Arabic_Num : Natural range 0..999_999_999;

   -- EOL : constant Character := Ada.Characters.Latin_1.LF;

   User_Input :  Integer range -999_999_999..999_999_999;


   procedure Arabic2Roman (OUTPUT : File_Type; INPUT_WORD : in String; Arabic_Process_All : in Boolean);
   function Integer_Test (Arabic_String: in String) return Boolean;

   function Generate_Additive (Arabic_Num : in Integer) return Unbounded_String;
   function Generate_Subtractive (Arabic_Num : in Integer) return Unbounded_String;


  end  Arabic2Roman;
