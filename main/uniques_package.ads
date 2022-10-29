with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
with DICTIONARY_PACKAGE;  use DICTIONARY_PACKAGE;
package UNIQUES_PACKAGE is

   type UNIQUE_ITEM;
   type UNIQUE_LIST is access UNIQUE_ITEM;

   type UNIQUE_ITEM is record
      STEM : STEM_TYPE      := NULL_STEM_TYPE;
      QUAL : QUALITY_RECORD := NULL_QUALITY_RECORD;
      KIND : KIND_ENTRY     := NULL_KIND_ENTRY;
      MNPC : DICT_IO.Count  := NULL_MNPC;
      SUCC : UNIQUE_LIST;
   end record;

   type LATIN_UNIQUES is array (Character range 'a' .. 'z') of UNIQUE_LIST;
   NULL_LATIN_UNIQUES : LATIN_UNIQUES := (others => null);

   UNQ : LATIN_UNIQUES := NULL_LATIN_UNIQUES;

   type UNIQUES_DE_ARRAY is
     array (DICT_IO.Positive_Count range <>) of DICTIONARY_ENTRY;
   UNIQUES_DE : UNIQUES_DE_ARRAY (1 .. 100) :=
     (others => NULL_DICTIONARY_ENTRY);

end UNIQUES_PACKAGE;
