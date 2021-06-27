with Text_IO;
with INFLECTIONS_PACKAGE;  use INFLECTIONS_PACKAGE;
with DICTIONARY_PACKAGE;   use DICTIONARY_PACKAGE;
with ADDONS_PACKAGE;       use ADDONS_PACKAGE;
with WORD_SUPPORT_PACKAGE; use WORD_SUPPORT_PACKAGE;

package WORD_PACKAGE is

   type STEM_ARRAY_TYPE is array (Integer range <>) of STEM_TYPE;
   subtype STEM_ARRAY is STEM_ARRAY_TYPE (0 .. MAX_STEM_SIZE);

   NOT_A_STEM       : constant STEM_TYPE := (others => 'x');
   NOT_A_STEM_ARRAY : STEM_ARRAY         := (others => NOT_A_STEM);

   SA, SSA : STEM_ARRAY := NOT_A_STEM_ARRAY;
   SSA_MAX : Integer    := 0;

   type PRUNED_DICTIONARY_ITEM is record
      DS  : DICTIONARY_STEM;
      D_K : DICTIONARY_KIND := DEFAULT_DICTIONARY_KIND;
   end record;
   NULL_PRUNED_DICTIONARY_ITEM : PRUNED_DICTIONARY_ITEM;
   type PRUNED_DICTIONARY_LIST is array (1 .. 100) of PRUNED_DICTIONARY_ITEM;
   --  Aug 96 QU_PRON max 42, PACK max 54 Jan 97 QU_PRON max 42, PACK max 74
   --  July 2020 more headroom for situations where there are
   --  multiple addons + dictionary entries (e.g., praedivine, praedivinere)

   PDL : PRUNED_DICTIONARY_LIST := (others => NULL_PRUNED_DICTIONARY_ITEM);
   PDL_INDEX : Integer                := 0;

   SAL_LENGTH : Integer := 250;
   subtype SAL is PARSE_ARRAY (1 .. SAL_LENGTH);

   type DICT_RESTRICTION is (X, REGULAR, QU_PRON_ONLY, PACK_ONLY);

   type Special_Meaning_Array is
     array (Integer range 1 .. SAL_LENGTH) of MEANING_TYPE;
   Null_Special_Meaning_Array : Special_Meaning_Array :=
     (others => NULL_MEANING_TYPE);
   XXX_MEANING : Special_Meaning_Array :=
     Null_Special_Meaning_Array;  --  For TRICKS
   YYY_MEANING : Special_Meaning_Array :=
     Null_Special_Meaning_Array;  --  For SYNCOPE
   NNN_MEANING : Special_Meaning_Array :=
     Null_Special_Meaning_Array;  --  For Names
   RRR_MEANING : Special_Meaning_Array :=
     Null_Special_Meaning_Array;  --  For Roman Numerals
   PPP_MEANING : Special_Meaning_Array :=
     Null_Special_Meaning_Array;  --  For COMPOUNDS

   XXX_MEANING_COUNTER, YYY_MEANING_COUNTER, NNN_MEANING_COUNTER,
   RRR_MEANING_COUNTER, PPP_MEANING_COUNTER : Integer := 1;

   SCROLL_LINE_NUMBER  : Integer := 0;
   OUTPUT_SCROLL_COUNT : Integer := 0;

   procedure PAUSE (OUTPUT : Text_IO.File_Type);

   function MIN (A, B : Integer) return Integer;

   function LTU (C, D : Character) return Boolean;

   function EQU (C, D : Character) return Boolean;

   function GTU (C, D : Character) return Boolean;

   function LTU (S, T : String) return Boolean;

   function GTU (S, T : String) return Boolean;

   function EQU (S, T : String) return Boolean;

   procedure RUN_INFLECTIONS
     (S           : in String; SL : in out SAL;
      RESTRICTION :    DICT_RESTRICTION := REGULAR);

   procedure SEARCH_DICTIONARIES
     (SSA : in STEM_ARRAY_TYPE; PREFIX : PREFIX_ITEM; SUFFIX : SUFFIX_ITEM;
      RESTRICTION :    DICT_RESTRICTION := REGULAR);

   procedure WORD
     (RAW_WORD : in String; PA : in out PARSE_ARRAY; PA_LAST : in out Integer);

   procedure CHANGE_LANGUAGE (C : Character);

   procedure FIND_DICTIONARY_FILES;

   procedure INITIALIZE_WORD_PACKAGE;

end WORD_PACKAGE;
