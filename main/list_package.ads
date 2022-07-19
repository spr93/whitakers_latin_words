with TEXT_IO;             use Text_IO; 
with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
with DICTIONARY_PACKAGE;  use DICTIONARY_PACKAGE;


package LIST_PACKAGE is


  function TRIM_BAR (S : String) return String;  -- Make it visible to search_english
   
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

  INFLECTION_FREQUENCY : array (FREQUENCY_TYPE) of String (1 .. 8) :=
     ("        ",  --  X

      "mostfreq",  --  A

      "sometime",  --  B

      "uncommon",  --  C

      "infreq  ",  --  D

      "rare    ",  --  E

      "veryrare",  --  F

      "inscript",  --  I

      "        ",  --  Not used

      "        ");
   INFLECTION_AGE : array (AGE_TYPE) of String (1 .. 8) :=
     ("Always  ",   --  X

      "Archaic ",   --  A

      "Early   ",   --  B

      "Classic ",   --  C

      "Late    ",   --  D

      "Later   ",   --  E

      "Medieval",   --  F

      "Scholar ",   --  G

      "Modern  ");  --  H

   DICTIONARY_FREQUENCY : array (FREQUENCY_TYPE) of String (1 .. 8) :=
     ("        ",  --  X

      "veryfreq",  --  A

      "frequent",  --  B

      "common  ",  --  C

      "lesser  ",  --  D

      "uncommon",  --  E

      "veryrare",  --  F

      "inscript",  --  I

      "graffiti",  --  J

      "Pliny   "); --  N

   DICTIONARY_AGE : array (AGE_TYPE) of String (1 .. 8) :=
     ("        ",   --  X

      "Archaic ",   --  A

      "Early   ",   --  B

      "Classic ",   --  C

      "Late    ",   --  D

      "Later   ",   --  E

      "Medieval",   --  F

      "NeoLatin",   --  G

      "Modern  "); --  H
   
end LIST_PACKAGE;
