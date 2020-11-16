with Text_IO; use Text_IO;
with Ada.Characters.Handling; 
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;

package STRINGS_PACKAGE is

   
     NULL_STRING                :   constant STRING(2..1)     
                                := (others => ' ');
   
     function MAX(A, B          : in INTEGER)    return INTEGER;
     function MIN(A, B          : in INTEGER)    return INTEGER;

     function Upper_Case(Item   : in Character)  return Character    renames Ada.Characters.Handling.To_Upper;
     function Upper_Case(Item   : in String)     return String       renames Ada.Characters.Handling.To_Upper;
  
     function Lower_Case(Item   : in Character)  return Character    renames Ada.Characters.Handling.To_Lower;
     function Lower_Case(Item   : in String)     return String       renames Ada.Characters.Handling.To_Lower;
   
     function TRIM    (SOURCE   : in STRING;
                       SIDE     : in TRIM_END  := BOTH)          
                                                  return STRING      renames Ada.Strings.Fixed.Trim;

     function HEAD(SOURCE       : in STRING; 
                    COUNT       : in NATURAL; 
                      PAD       : in CHARACTER := ' ') return STRING renames Ada.Strings.Fixed.Head;  
   
     procedure GET_NON_COMMENT_LINE (F         :  in TEXT_IO.FILE_TYPE; 
                                     S         :  out STRING; 
                                     LAST      :  out INTEGER);
   
     INPUT_LINE_LENGTH          : Integer      := 2_500;

     procedure GET_UNICODE(LINE : in out String; 
                           L    : in out Integer);
     pragma Wide_Character_Encoding(UTF8);
     pragma Inline(GET_UNICODE);
   
   ---------------------------
   -- ANSI Formatting Codes --
   ---------------------------
   --  for DO_ANSI_FORMATTING WORDS_PARAMETER

   Format_Underline : constant String := (ASCII.ESC & "[4m");  -- For dictionary line only (corresp. Pearse 02)
   Format_Bold      : constant String := (ASCII.ESC & "[1m");  -- For definition  (corresp. Pearse 03)
   Format_Inverse   : constant String := (ASCII.ESC & "[7m");  -- Use sparingly for important notes
   Format_Faint     : constant String := (ASCII.ESC & "[2m");  -- For examples; they get distracting in long output
   Format_Reset     : constant String := (ASCII.ESC & "[0m");
   -- FOR WINDOWS THESE CODES MUST BE ENABLED AT STARTUP WITH THE FUNCTION IN WINDOWS_VT100.ADB
   
   type Format_Command is (UNDERLINE, BOLD, INVERSE, FAINT, RESET);
   procedure Format (OUTPUT : in File_Type; Format : In Format_Command);
   pragma Inline_Always(Format);

   
end STRINGS_PACKAGE;  
