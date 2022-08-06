with Text_IO;     use Text_IO;
with Ada.Characters.Handling;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed;

-- This package originally implemented string-handling procedures that later became
-- part of the Ada 95 standard.  
-- Today it contains output-formatting routines and a series of renames that reduce
-- the number of standard packages that need to be with'd in other units.


package STRINGS_PACKAGE is

   NULL_STRING : constant String (2 .. 1) := (others => ' ');

   function MAX (A, B : in Integer) return Integer;
   function MIN (A, B : in Integer) return Integer;

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

   procedure GET_NON_COMMENT_LINE
     (F : in Text_IO.File_Type; S : out String; LAST : out Integer);

   INPUT_LINE_LENGTH : Integer := 2_500;

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
     (ASCII.ESC & "[7m");  -- Use sparingly for important notes
   Format_Faint : constant String :=
     (ASCII.ESC & "[2m");  -- For examples; they get distracting in long output
   Format_Reset : constant String := (ASCII.ESC & "[0m");  

   type Format_Command is (UNDERLINE, BOLD, INVERSE, FAINT, RESET);
   procedure Format (OUTPUT : in File_Type; Format : in Format_Command);
   pragma Inline_Always (Format);

end STRINGS_PACKAGE;

