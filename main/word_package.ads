with TEXT_IO;
with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
with ADDONS_PACKAGE; use ADDONS_PACKAGE;
with WORD_SUPPORT_PACKAGE; use WORD_SUPPORT_PACKAGE;
with Ada.Directories;
with Ada.Environment_Variables;


package WORD_PACKAGE is
    
  LINE_NUMBER, WORD_NUMBER : INTEGER := 0;

  type STEM_ARRAY_TYPE is array (INTEGER range <>) of STEM_TYPE;
  subtype STEM_ARRAY is STEM_ARRAY_TYPE(0..MAX_STEM_SIZE);

  NOT_A_STEM : constant STEM_TYPE := (others => 'x');
  NOT_A_STEM_ARRAY : STEM_ARRAY  := (others => NOT_A_STEM);

  SA, SSA : STEM_ARRAY := NOT_A_STEM_ARRAY;
  SSA_MAX : INTEGER := 0;

  type PRUNED_DICTIONARY_ITEM is
    record
      DS   : DICTIONARY_STEM;
      D_K  : DICTIONARY_KIND := DEFAULT_DICTIONARY_KIND;
    end record;
  NULL_PRUNED_DICTIONARY_ITEM : PRUNED_DICTIONARY_ITEM;
  type PRUNED_DICTIONARY_LIST is array (1..100) of PRUNED_DICTIONARY_ITEM;
  --  Aug 96   QU_PRON max 42, PACK max 54
  --  Jan 97   QU_PRON max 42, PACK max 74  --  Might reduce
  --  July 2020 more headroom for situations where there are multiple addons + dictionary entries (e.g., praedivine, praedivinere, etc.)
 
  PDL : PRUNED_DICTIONARY_LIST := (others => NULL_PRUNED_DICTIONARY_ITEM);
  PDL_INDEX : INTEGER := 0;

  SAL_LENGTH : Integer := 250;
  subtype SAL is PARSE_ARRAY(1..SAL_LENGTH);

  type DICT_RESTRICTION is (X, REGULAR, QU_PRON_ONLY, PACK_ONLY);
   
  type Special_Meaning_Array Is array(INTEGER range  1..Sal_Length) of Meaning_Type;
  Null_Special_Meaning_Array : Special_Meaning_Array := (others => Null_Meaning_Type);
  XXX_MEANING : Special_Meaning_Array := Null_Special_Meaning_Array;  --  For TRICKS
  YYY_MEANING : Special_Meaning_Array := Null_Special_Meaning_Array;  --  For SYNCOPE
  NNN_MEANING : Special_Meaning_Array := Null_Special_Meaning_Array;  --  For Names
  RRR_MEANING : Special_Meaning_Array := Null_Special_Meaning_Array;  --  For Roman Numerals
  PPP_MEANING : Special_Meaning_Array := Null_Special_Meaning_Array;  --  For COMPOUNDS
  
  XXX_MEANING_COUNTER, YYY_MEANING_COUNTER, NNN_MEANING_COUNTER, 
  RRR_MEANING_COUNTER, PPP_MEANING_COUNTER : INTEGER := 1;
   
  SCROLL_LINE_NUMBER : INTEGER := 0;
  OUTPUT_SCROLL_COUNT : INTEGER := 0;

  procedure PAUSE(OUTPUT : TEXT_IO.FILE_TYPE);
       
  function MIN(A, B : INTEGER) return INTEGER;  
      
  function LTU(C, D : CHARACTER) return BOOLEAN;  
      
  function EQU(C, D : CHARACTER) return BOOLEAN;   
      
  function GTU(C, D : CHARACTER) return BOOLEAN;  
      
  function LTU(S, T : STRING) return BOOLEAN;  
      
  function GTU(S, T : STRING) return BOOLEAN;  
      
  function EQU(S, T : STRING) return BOOLEAN;  
              
  procedure RUN_INFLECTIONS(S : in STRING; SL : in out SAL;
                            RESTRICTION : DICT_RESTRICTION := REGULAR);

  procedure SEARCH_DICTIONARIES(SSA : in STEM_ARRAY_TYPE;
                                PREFIX : PREFIX_ITEM; SUFFIX : SUFFIX_ITEM;
                                RESTRICTION : DICT_RESTRICTION := REGULAR);

  procedure WORD(RAW_WORD : in STRING;
                 PA : in out PARSE_ARRAY; PA_LAST : in out INTEGER);

  procedure CHANGE_LANGUAGE(C : CHARACTER);  
    
  procedure INITIALIZE_WORD_PACKAGE;

  procedure CHECK_PATH_VARIABLE; -- search for WORD data files in the PATH directories
   
end WORD_PACKAGE;
