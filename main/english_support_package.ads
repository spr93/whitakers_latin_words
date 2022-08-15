with Text_IO;
with Direct_IO;
with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
with DICTIONARY_PACKAGE;  use DICTIONARY_PACKAGE;


package ENGLISH_SUPPORT_PACKAGE is

   EWORD_SIZE        : constant := 24;
   AUX_WORD_SIZE     : constant := 12;
   LINE_NUMBER_WIDTH : constant := 10;
   PRIORITY_WIDTH    : constant := 3;

   subtype EWORD is String (1 .. EWORD_SIZE);
   NULL_EWORD : EWORD := (others => ' ');
   subtype AUXWORD is String (1 .. AUX_WORD_SIZE);
   NULL_AUXWORD : AUXWORD := (others => ' ');
   subtype PRIORITY_TYPE is Integer range 0 .. 99;

   NUMBER_OF_EWORDS : Integer := 0;

   type EWDS_RECORD is record
      W    : EWORD               := NULL_EWORD;
      AUX  : AUXWORD             := NULL_AUXWORD;
      N    : Integer             := 0;
      POFS : PART_OF_SPEECH_TYPE := X;
      FREQ : FREQUENCY_TYPE      := X;
      SEMI : Integer             := 0;
      KIND : Integer             := 0;
      RANK : Integer             := 0;
   end record;

   NULL_EWDS_RECORD : EWDS_RECORD :=
     ((others => ' '), (others => ' '), 0, X, X, 0, 0, 0);

   type EWDS_ARRAY is array (Positive range <>) of EWDS_RECORD;

   package EWDS_DIRECT_IO is new Direct_IO (EWDS_RECORD);

   package EWDS_RECORD_IO is
      DEFAULT_WIDTH : Natural;
      procedure GET (F : in Text_IO.File_Type; P : out EWDS_RECORD);
      procedure GET (P : out EWDS_RECORD);
      procedure PUT (F : in Text_IO.File_Type; P : in EWDS_RECORD);
      procedure PUT (P : in EWDS_RECORD);
      procedure GET (S : in String; P : out EWDS_RECORD; LAST : out Integer);
      procedure PUT (S : out String; P : in EWDS_RECORD);
   end EWDS_RECORD_IO;

   ENGLISH_DICTIONARY_AVAILABLE : array (DICTIONARY_KIND) of Boolean :=
     (False,
      False,
      False,
      False,
      False,
      False,
      False,  --  don't SEARCH
      False, False, False,
      False);

   EWDS_FILE : EWDS_DIRECT_IO.File_Type;

end ENGLISH_SUPPORT_PACKAGE;
