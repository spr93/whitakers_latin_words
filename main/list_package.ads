with TEXT_IO;
with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
with Ada.Characters.Latin_1; 
package LIST_PACKAGE is

         
--  SCROLL_LINE_NUMBER : INTEGER := 0;
--  OUTPUT_SCROLL_COUNT : INTEGER := 0;

  
  procedure LIST_STEMS(OUTPUT   : TEXT_IO.FILE_TYPE;
                       RAW_WORD : STRING;
                       INPUT_LINE : STRING;
                       PA       : in out PARSE_ARRAY; 
                       PA_LAST  : in out INTEGER);
                       
                       
  procedure LIST_ENTRY(OUTPUT   : TEXT_IO.FILE_TYPE;
                       D_K      : DICTIONARY_KIND;
                       MN       : DICT_IO.COUNT);

                         
  procedure UNKNOWN_SEARCH(UNKNOWN       :  in STRING;
                           UNKNOWN_COUNT : out DICT_IO.COUNT);
                                  
  procedure LIST_NEIGHBORHOOD(OUTPUT : TEXT_IO.FILE_TYPE; INPUT_WORD : STRING);

   
   ---------------------------
   -- ANSI Formatting Codes --
   ---------------------------
   --  for DO_ANSI_FORMATTING WORDS_PARAMETER
   use Ada.Characters.Latin_1;
   Format_Underline : constant String := (ESC & "[4m");  -- For dictionary line only (corresp. Pearse 02)
   Format_Bold      : constant String := (ESC & "[1m");  -- For definition  (corresp. Pearse 03)
   Format_Inverse   : constant String := (ESC & "[7m");  -- Use sparingly for important notes
   Format_Faint     : constant String := (ESC & "[2m");  -- For examples; they get distracting in long output
   Format_Reset     : constant String := (ESC & "[0m");
   
   
end LIST_PACKAGE;
