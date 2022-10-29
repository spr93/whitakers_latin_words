with Text_IO;
with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
with Ada.Direct_IO;

package DICTIONARY_PACKAGE is
   pragma Elaborate_Body;
   use Text_IO;

   ZZZ_STEM : constant STEM_TYPE := "zzz" & (4 .. MAX_STEM_SIZE => ' ');
   type STEMS_TYPE is array (STEM_KEY_TYPE range 1 .. 4) of STEM_TYPE;
   NULL_STEMS_TYPE : constant STEMS_TYPE := (others => NULL_STEM_TYPE);

   type DICTIONARY_KIND is
     (X,            --  null
      ADDONS,       --  For FIXES
      XXX,          --  TRICKS
      YYY,          --  Syncope
      NNN,          --  Unknown Name
      RRR,          --  Roman Numerals
      PPP,          --  Compounds
      GENERAL, SPECIAL, LOCAL, UNIQUE);

   package DICTIONARY_KIND_IO is new Text_IO.Enumeration_IO (DICTIONARY_KIND);

   EXT : array (DICTIONARY_KIND) of String (1 .. 3) :=
     ("X  ", "ADD", "XXX", "YYY", "NNN", "RRR", "PPP", "GEN", "SPE", "LOC",
      "UNI");

   DEFAULT_DICTIONARY_KIND : DICTIONARY_KIND := X;

   DICTIONARY_AVAILABLE : array (DICTIONARY_KIND) of Boolean :=
     (others => False);
   --  Start out as FALSE and set to TRUE when the DICT is loaded

   type AREA_TYPE is
     (X,      --  All or none
      A,      --  Agriculture, Flora, Fauna, Land, Equipment, Rural
      B,      --  Biological, Medical, Body Parts
      D,      --  Drama, Music, Theater, Art, Painting, Sculpture
      E,      --  Ecclesiastic, Biblical, Religious
      G,      --  Grammar, Retoric, Logic, Literature, Schools
      L,      --  Legal, Government, Tax, Financial, Political, Titles
      P,      --  Poetic
      S,      --  Science, Philosophy, Mathematics, Units/Measures
      T,      --  Technical, Architecture, Topography, Surveying
      W,      --  War, Military, Naval, Ships, Armor
      Y       --  Mythology
   );

   package AREA_TYPE_IO is new Text_IO.Enumeration_IO (AREA_TYPE);

   type GEO_TYPE is
     (X,      --  All or none
      A,      --  Africa
      B,      --  Britian
      C,      --  China
      D,      --  Scandinavia
      E,      --  Egypt
      F,      --  France, Gaul
      G,      --  Germany
      H,      --  Greece
      I,      --  Italy, Rome
      J,      --  India
      K,      --  Balkans
      N,      --  Netherlands
      P,      --  Persia
      Q,      --  Near East
      R,      --  Russia
      S,      --  Spain, Iberia
      U       --  Eastern Europe
   );

   package GEO_TYPE_IO is new Text_IO.Enumeration_IO (GEO_TYPE);

   type SOURCE_TYPE is
     (X,      --  General or unknown or too common to say
      A,
      B,      --  C.H.Beeson, A Primer of Medieval Latin, 1925 (Bee)
      C,      --  Charles Beard, Cassell's Latin Dictionary 1892 (Cas)
      D,      --  J.N.Adams, Latin Sexual Vocabulary, 1982 (Sex)
      E,      --  L.F.Stelten, Dictionary of Eccles. Latin, 1995 (Ecc)
      F,      --  Roy J. Deferrari, Dictionary of St. Thomas Aquinas, 1960 (DeF)
      G,      --  Gildersleeve + Lodge, Latin Grammar 1895 (G+L)
      H,      --  Collatinus Dictionary by Yves Ouvrard
      I,      --  Leverett, F.P., Lexicon of the Latin Language, Boston 1845
      J,      --  Bracton: De Legibus Et Consuetudinibus Anglicae
      K,      --  Calepinus Novus, modern Latin, by Guy Licoppe (Cal)
      L,      --  Lewis, C.S., Elementary Latin Dictionary 1891
      M,      --  Latham, Revised Medieval Word List, 1980 (Latham)
      N,      --  Lynn Nelson, Wordlist (Nel)
      O,      --  Oxford Latin Dictionary (OLD), 1982 (most entries), 2012 (a few entries)
      P,      --  Souter, A Glossary of Later Latin to 600 A.D., Oxford 1949 (Souter)
      Q,      --  Other, cited or unspecified dictionaries
      R,      --  Plater + White, A Grammar of the Vulgate, Oxford 1926 (Plater)
      S,      --  Lewis and Short, A Latin Dictionary, 1879 (L+S)
      T,      --  Found in a translation  --  no dictionary reference
      U,      --
      V,      --  Vademecum in opus Saxonis - Franz Blatt (Saxo)
      W,      --  My personal guess, mostly obvious extrapolation (Whitaker or W)
      Y,      --  Temp special code
      Z       --  Sent by user --  no dictionary reference
   --  mostly John White of Blitz Latin

      --  Consulted but used only indirectly Liddell + Scott Greek-English
      --  Lexicon (Lid) Various translations of Ovid and Vergil

      --  Consulted but used only occasionally, seperately referenced D.A.
      --  Kidd, Collins Latin Gem Dictionary, 1957 (Col) Allen + Greenough, New
      --  Latin Grammar, 1888 (A+G) Harrington/Pucci/Elliott, Medieval Latin
      --  2nd Ed 1997 (Harr) C.C./C.L. Scanlon Latin Grammar/Second Latin, TAN
      --  1976 (SCANLON) W. M. Lindsay, Short Historical Latin Grammar, 1895
      --  (Lindsay) Du Cange Oxford English Dictionary (OED)

--  Note that the WORDS dictionary is not a copy of source info, but the
--  indicated SOURCE is a main reference/check point used to derive the entry

);

   package SOURCE_TYPE_IO is new Text_IO.Enumeration_IO (SOURCE_TYPE);

   type KIND_ENTRY (POFS : PART_OF_SPEECH_TYPE := X) is record
      case POFS is
         when N =>
            N_KIND : NOUN_KIND_TYPE := X;
         when PRON =>
            PRON_KIND : PRONOUN_KIND_TYPE := X;
         when PACK =>
            PACK_KIND : PRONOUN_KIND_TYPE := X;
         when ADJ =>
            null;
         when NUM =>
            NUM_VALUE : NUMERAL_VALUE_TYPE := 0;
         when V =>
            V_KIND : VERB_KIND_TYPE := X;
         when VPAR =>
            VPAR_KIND : VERB_KIND_TYPE := X;
         when SUPINE =>
            SUPINE_KIND : VERB_KIND_TYPE := X;
         when others =>
            null;
      end case;
   end record;

   package KIND_ENTRY_IO is
      DEFAULT_WIDTH : Natural;
      procedure GET
        (F : in File_Type; PS : in PART_OF_SPEECH_TYPE; P : out KIND_ENTRY);
      procedure GET (PS : in PART_OF_SPEECH_TYPE; P : out KIND_ENTRY);
      procedure PUT
        (F : in File_Type; PS : in PART_OF_SPEECH_TYPE; P : in KIND_ENTRY);
      procedure PUT (PS : in PART_OF_SPEECH_TYPE; P : in KIND_ENTRY);
      procedure GET
        (S    : in     String; PS : in PART_OF_SPEECH_TYPE; P : out KIND_ENTRY;
         LAST :    out Integer);
      procedure PUT
        (S : out String; PS : in PART_OF_SPEECH_TYPE; P : in KIND_ENTRY);
   end KIND_ENTRY_IO;

   NULL_KIND_ENTRY : KIND_ENTRY;

   type TRANSLATION_RECORD is record
      AGE    : AGE_TYPE       := X;
      AREA   : AREA_TYPE      := X;
      GEO    : GEO_TYPE       := X;
      FREQ   : FREQUENCY_TYPE := X;
      SOURCE : SOURCE_TYPE    := X;
   end record;

   NULL_TRANSLATION_RECORD : TRANSLATION_RECORD;

   package TRANSLATION_RECORD_IO is
      DEFAULT_WIDTH : Text_IO.Field;
      procedure GET (F : in Text_IO.File_Type; TR : out TRANSLATION_RECORD);
      procedure GET (TR : out TRANSLATION_RECORD);
      procedure PUT (F : in Text_IO.File_Type; TR : in TRANSLATION_RECORD);
      procedure PUT (TR : in TRANSLATION_RECORD);
      procedure GET
        (S : in String; TR : out TRANSLATION_RECORD; LAST : out Integer);
      procedure PUT (S : out String; TR : in TRANSLATION_RECORD);
   end TRANSLATION_RECORD_IO;

   type NOUN_ENTRY is record
      DECL   : DECN_RECORD    := (0, 0);
      GENDER : GENDER_TYPE    := X;
      KIND   : NOUN_KIND_TYPE := X;
   end record;

   package NOUN_ENTRY_IO is
      DEFAULT_WIDTH : Natural;
      procedure GET (F : in File_Type; N : out NOUN_ENTRY);
      procedure GET (N : out NOUN_ENTRY);
      procedure PUT (F : in File_Type; N : in NOUN_ENTRY);
      procedure PUT (N : in NOUN_ENTRY);
      procedure GET (S : in String; N : out NOUN_ENTRY; LAST : out Integer);
      procedure PUT (S : out String; N : in NOUN_ENTRY);
   end NOUN_ENTRY_IO;

   type PRONOUN_ENTRY is record
      DECL : DECN_RECORD       := (0, 0);
      KIND : PRONOUN_KIND_TYPE := X;
   end record;

   package PRONOUN_ENTRY_IO is
      DEFAULT_WIDTH : Natural;
      procedure GET (F : in File_Type; P : out PRONOUN_ENTRY);
      procedure GET (P : out PRONOUN_ENTRY);
      procedure PUT (F : in File_Type; P : in PRONOUN_ENTRY);
      procedure PUT (P : in PRONOUN_ENTRY);
      procedure GET (S : in String; P : out PRONOUN_ENTRY; LAST : out Integer);
      procedure PUT (S : out String; P : in PRONOUN_ENTRY);
   end PRONOUN_ENTRY_IO;

   type PROPACK_ENTRY is record
      DECL : DECN_RECORD       := (0, 0);
      KIND : PRONOUN_KIND_TYPE := X;
   end record;

   package PROPACK_ENTRY_IO is
      DEFAULT_WIDTH : Natural;
      procedure GET (F : in File_Type; P : out PROPACK_ENTRY);
      procedure GET (P : out PROPACK_ENTRY);
      procedure PUT (F : in File_Type; P : in PROPACK_ENTRY);
      procedure PUT (P : in PROPACK_ENTRY);
      procedure GET (S : in String; P : out PROPACK_ENTRY; LAST : out Integer);
      procedure PUT (S : out String; P : in PROPACK_ENTRY);
   end PROPACK_ENTRY_IO;

   type ADJECTIVE_ENTRY is record
      DECL : DECN_RECORD     := (0, 0);
      CO   : COMPARISON_TYPE := X;
   end record;

   package ADJECTIVE_ENTRY_IO is
      DEFAULT_WIDTH : Natural;
      procedure GET (F : in File_Type; A : out ADJECTIVE_ENTRY);
      procedure GET (A : out ADJECTIVE_ENTRY);
      procedure PUT (F : in File_Type; A : in ADJECTIVE_ENTRY);
      procedure PUT (A : in ADJECTIVE_ENTRY);
      procedure GET
        (S : in String; A : out ADJECTIVE_ENTRY; LAST : out Integer);
      procedure PUT (S : out String; A : in ADJECTIVE_ENTRY);
   end ADJECTIVE_ENTRY_IO;

   type NUMERAL_ENTRY is record
      DECL  : DECN_RECORD        := (0, 0);
      SORT  : NUMERAL_SORT_TYPE  := X;
      VALUE : NUMERAL_VALUE_TYPE := 0;
   end record;

   package NUMERAL_ENTRY_IO is
      DEFAULT_WIDTH : Natural;
      procedure GET (F : in File_Type; NUM : out NUMERAL_ENTRY);
      procedure GET (NUM : out NUMERAL_ENTRY);
      procedure PUT (F : in File_Type; NUM : in NUMERAL_ENTRY);
      procedure PUT (NUM : in NUMERAL_ENTRY);
      procedure GET
        (S : in String; NUM : out NUMERAL_ENTRY; LAST : out Integer);
      procedure PUT (S : out String; NUM : in NUMERAL_ENTRY);
   end NUMERAL_ENTRY_IO;

   type ADVERB_ENTRY is record
      CO : COMPARISON_TYPE := X;
   end record;

   package ADVERB_ENTRY_IO is
      DEFAULT_WIDTH : Natural;
      procedure GET (F : in File_Type; A : out ADVERB_ENTRY);
      procedure GET (A : out ADVERB_ENTRY);
      procedure PUT (F : in File_Type; A : in ADVERB_ENTRY);
      procedure PUT (A : in ADVERB_ENTRY);
      procedure GET (S : in String; A : out ADVERB_ENTRY; LAST : out Integer);
      procedure PUT (S : out String; A : in ADVERB_ENTRY);
   end ADVERB_ENTRY_IO;

   type VERB_ENTRY is record
      CON  : DECN_RECORD    := (0, 0);
      KIND : VERB_KIND_TYPE := X;
   end record;

   package VERB_ENTRY_IO is
      DEFAULT_WIDTH : Natural;
      procedure GET (F : in File_Type; V : out VERB_ENTRY);
      procedure GET (V : out VERB_ENTRY);
      procedure PUT (F : in File_Type; V : in VERB_ENTRY);
      procedure PUT (V : in VERB_ENTRY);
      procedure GET (S : in String; V : out VERB_ENTRY; LAST : out Integer);
      procedure PUT (S : out String; V : in VERB_ENTRY);
   end VERB_ENTRY_IO;

   type PREPOSITION_ENTRY is record
      OBJ : CASE_TYPE := X;
   end record;

   package PREPOSITION_ENTRY_IO is
      DEFAULT_WIDTH : Natural;
      procedure GET (F : in File_Type; P : out PREPOSITION_ENTRY);
      procedure GET (P : out PREPOSITION_ENTRY);
      procedure PUT (F : in File_Type; P : in PREPOSITION_ENTRY);
      procedure PUT (P : in PREPOSITION_ENTRY);
      procedure GET
        (S : in String; P : out PREPOSITION_ENTRY; LAST : out Integer);
      procedure PUT (S : out String; P : in PREPOSITION_ENTRY);
   end PREPOSITION_ENTRY_IO;

   type CONJUNCTION_ENTRY is record
      null;
   end record;

   package CONJUNCTION_ENTRY_IO is
      DEFAULT_WIDTH : Natural;
      procedure GET (F : in File_Type; C : out CONJUNCTION_ENTRY);
      procedure GET (C : out CONJUNCTION_ENTRY);
      procedure PUT (F : in File_Type; C : in CONJUNCTION_ENTRY);
      procedure PUT (C : in CONJUNCTION_ENTRY);
      procedure GET
        (S : in String; C : out CONJUNCTION_ENTRY; LAST : out Integer);
      procedure PUT (S : out String; C : in CONJUNCTION_ENTRY);
   end CONJUNCTION_ENTRY_IO;

   type INTERJECTION_ENTRY is record
      null;
   end record;

   package INTERJECTION_ENTRY_IO is
      DEFAULT_WIDTH : Natural;
      procedure GET (F : in File_Type; I : out INTERJECTION_ENTRY);
      procedure GET (I : out INTERJECTION_ENTRY);
      procedure PUT (F : in File_Type; I : in INTERJECTION_ENTRY);
      procedure PUT (I : in INTERJECTION_ENTRY);
      procedure GET
        (S : in String; I : out INTERJECTION_ENTRY; LAST : out Integer);
      procedure PUT (S : out String; I : in INTERJECTION_ENTRY);
   end INTERJECTION_ENTRY_IO;

   type PART_ENTRY (POFS : PART_OF_SPEECH_TYPE := X) is record
      case POFS is
         when N =>
            N : NOUN_ENTRY;
         when PRON =>
            PRON : PRONOUN_ENTRY;
         when PACK =>
            PACK : PROPACK_ENTRY;
         when ADJ =>
            ADJ : ADJECTIVE_ENTRY;
         when NUM =>
            NUM : NUMERAL_ENTRY;
         when ADV =>
            ADV : ADVERB_ENTRY;
         when V =>
            V : VERB_ENTRY;
         when VPAR =>
            null;  --  There will be no VPAR dictionary entries
         when SUPINE =>
            null;  --  There will be no SUPINE dictionary entries
         when PREP =>
            PREP : PREPOSITION_ENTRY;
         when CONJ =>
            CONJ : CONJUNCTION_ENTRY;
         when INTERJ =>
            INTERJ : INTERJECTION_ENTRY;
         when others =>
            null;
      end case;
   end record;

   package PART_ENTRY_IO is
      DEFAULT_WIDTH : Natural;
      procedure GET (F : in File_Type; P : out PART_ENTRY);
      procedure GET (P : out PART_ENTRY);
      procedure PUT (F : in File_Type; P : in PART_ENTRY);
      procedure PUT (P : in PART_ENTRY);
      procedure GET (S : in String; P : out PART_ENTRY; LAST : out Integer);
      procedure PUT (S : out String; P : in PART_ENTRY);
   end PART_ENTRY_IO;

   NULL_PART_ENTRY : PART_ENTRY;

   function "<" (LEFT, RIGHT : PART_ENTRY) return Boolean;

   type DICTIONARY_ENTRY is record
      STEMS : STEMS_TYPE := NULL_STEMS_TYPE;
      PART  : PART_ENTRY := NULL_PART_ENTRY;
--    KIND  : KIND_ENTRY         := NULL_KIND_ENTRY;
      TRAN : TRANSLATION_RECORD := NULL_TRANSLATION_RECORD;
      MEAN : MEANING_TYPE       := NULL_MEANING_TYPE;
   end record;

   package DICTIONARY_ENTRY_IO is
      DEFAULT_WIDTH : Field;
      procedure GET (F : in File_Type; D : out DICTIONARY_ENTRY);
      procedure GET (D : out DICTIONARY_ENTRY);
      procedure PUT (F : in File_Type; D : in DICTIONARY_ENTRY);
      procedure PUT (D : in DICTIONARY_ENTRY);
      procedure GET
        (S : in String; D : out DICTIONARY_ENTRY; LAST : out Integer);
      procedure PUT (S : out String; D : in DICTIONARY_ENTRY);
   end DICTIONARY_ENTRY_IO;

   NULL_DICTIONARY_ENTRY : DICTIONARY_ENTRY;

   package DICT_IO is new Ada.Direct_IO (DICTIONARY_ENTRY);
   DICT_FILE : array (DICTIONARY_KIND) of DICT_IO.File_Type;

   package MNPC_IO is new Text_IO.Integer_IO (DICT_IO.Count);
   subtype MNPC_TYPE is DICT_IO.Count;
   NULL_MNPC : DICT_IO.Count := DICT_IO.Count'First;
   LAST_MNPC : DICT_IO.Count := NULL_MNPC;

   type PARSE_RECORD is record
      STEM : STEM_TYPE         := NULL_STEM_TYPE;
      IR   : INFLECTION_RECORD := NULL_INFLECTION_RECORD;
      D_K  : DICTIONARY_KIND   := DEFAULT_DICTIONARY_KIND;
      MNPC : DICT_IO.Count     := NULL_MNPC;
   end record;

   NULL_PARSE_RECORD : PARSE_RECORD;

   package PARSE_RECORD_IO is
      DEFAULT_WIDTH : Text_IO.Field;
      procedure GET (F : in Text_IO.File_Type; PR : out PARSE_RECORD);
      procedure GET (PR : out PARSE_RECORD);
      procedure PUT (F : in Text_IO.File_Type; PR : in PARSE_RECORD);
      procedure PUT (PR : in PARSE_RECORD);
      procedure GET (S : in String; PR : out PARSE_RECORD; LAST : out Integer);
      procedure PUT (S : out String; PR : in PARSE_RECORD);
   end PARSE_RECORD_IO;

   type PARSE_ARRAY is array (Integer range <>) of PARSE_RECORD;

   function NUMBER_OF_STEMS (P : PART_OF_SPEECH_TYPE) return STEM_KEY_TYPE;

   function "<=" (LEFT, RIGHT : AREA_TYPE) return Boolean;

   Sum_Array : constant array
     (MOOD_TYPE range IND .. SUB, TENSE_TYPE range PRES .. FUTP,
      NUMBER_TYPE range S .. P, PERSON_TYPE range 1 .. 3) of String (1 .. 9) :=
     (
      (         --  IND

       (("sum      ", "es       ", "est      "),
        ("sumus    ", "estis    ", "sunt     ")),
       (("eram     ", "eras     ", "erat     "),
        ("eramus   ", "eratis   ", "erant    ")),
       (("ero      ", "eris     ", "erit     "),
        ("erimus   ", "eritis   ", "erunt    ")),
       (("fui      ", "fuisti   ", "fuit     "),
        ("fuimus   ", "fuistis  ", "fuerunt  ")),
       (("fueram   ", "fueras   ", "fuerat   "),
        ("fueramus ", "fueratis ", "fuerant  ")),
       (("fuero    ", "fueris   ", "fuerit   "),
        ("fuerimus ", "fueritis ", "fuerunt  "))),
      (         --  SUB

       (("sim      ", "sis      ", "sit      "),
        ("simus    ", "sitis    ", "sint     ")),
       (("essem    ", "esses    ", "esset    "),
        ("essemus  ", "essetis  ", "essent   ")),
       (("zzz      ", "zzz      ", "zzz      "),
        ("zzz      ", "zzz      ", "zzz      ")),
       (("fuerim   ", "fueris   ", "fuerit   "),
        ("fuerimus ", "fueritis ", "fuerint  ")),
       (("fuissem  ", "fuisses  ", "fuisset  "),
        ("fuissemus", "fuissetis", "fuissent ")),
       (("zzz      ", "zzz      ", "zzz      "),
        ("zzz      ", "zzz      ", "zzz      "))));

end DICTIONARY_PACKAGE;
