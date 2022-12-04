with Text_IO;     use Text_IO;
with Ada.Characters.Handling;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed;

package STRINGS_PACKAGE is

   NULL_STRING : constant String (2 .. 1) := (others => ' ');

   function MAX (A, B : in Integer) return Integer;
   function MIN (A, B : in Integer) return Integer;

-- BEGIN ADA 95 RENAMES
   -- In this package, Whitaker implemented, in Ada 83, text-handling procedures that later became part
   -- of the Ada 95 standard.  Now they're renames that (1) reduce the number of packages that
   -- need to be WITH'd in other units and (2) let us avoid unnecessary refactors.

   function Upper_Case (Item : in Character) return Character       renames
     Ada.Characters.Handling.To_Upper;
   function Upper_Case (Item : in String)    return String          renames
     Ada.Characters.Handling.To_Upper;

   function Lower_Case (Item : in Character) return Character       renames
     Ada.Characters.Handling.To_Lower;
   function Lower_Case (Item : in String)    return String          renames
     Ada.Characters.Handling.To_Lower;

   function TRIM
     (SOURCE : in String; SIDE : in Trim_End := Both) return String renames
     Ada.Strings.Fixed.Trim;

   function HEAD
     (SOURCE : in String; COUNT : in Natural; PAD : in Character := ' ')
                                              return String         renames
     Ada.Strings.Fixed.Head;

-- END ADA 95 RENAMES --

   procedure GET_NON_COMMENT_LINE
     (F : in Text_IO.File_Type; S : out String; LAST : out Integer);

   INPUT_LINE_LENGTH : Integer := 1_023;

   type Pearse_Code_Type is new Integer range 0..7;
   Pearse_Code_Array : constant array(Pearse_Code_Type) of String (1..3) := ("00 ","01 ","02 ","03 ","04 ","05 ","06 ","07 ");
   procedure Put_Pearse_Code (OUTPUT : in Text_IO.File_Type; Code : in Pearse_Code_Type);

   ---------------------------
   -- ANSI Formatting Codes --
   ---------------------------
   -- for DO_ANSI_FORMATTING WORDS_PARAMETER
   -- UNDER WINDOWS THESE CODES MUST BE ENABLED AT STARTUP USING WINDOWS_VT100.ADB
   Format_Underline : constant String :=
     (ASCII.ESC & "[4m");  -- For dictionary line only (corresp. Pearse 02)
   Format_Bold : constant String :=
     (ASCII.ESC & "[1m");  -- For definition  (corresp. Pearse 03)
   Format_Inverse : constant String :=
     (ASCII.ESC & "[7m");  -- Use sparingly for important notes, warnings, and errors
   Format_Faint : constant String :=
     (ASCII.ESC & "[2m");  -- For examples; they get distracting in long output
   Format_Reset : constant String := (ASCII.ESC & "[0m");

   type Format_Command is (UNDERLINE, BOLD, INVERSE, FAINT, RESET);
   procedure Format (OUTPUT : in File_Type; Format : in Format_Command);
   pragma Inline_Always (Format);

end STRINGS_PACKAGE;

