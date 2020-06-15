with CONFIG;               use CONFIG;
with STRINGS_PACKAGE;      use STRINGS_PACKAGE;
with LATIN_FILE_NAMES;     use LATIN_FILE_NAMES;
with WORD_PARAMETERS;      use WORD_PARAMETERS;
with INFLECTIONS_PACKAGE;  use INFLECTIONS_PACKAGE;
with DICTIONARY_PACKAGE;   use DICTIONARY_PACKAGE;
with ADDONS_PACKAGE;       use ADDONS_PACKAGE;
with UNIQUES_PACKAGE;      use UNIQUES_PACKAGE;
with WORD_SUPPORT_PACKAGE; use WORD_SUPPORT_PACKAGE;
with DEVELOPER_PARAMETERS; use DEVELOPER_PARAMETERS;
with WORD_PACKAGE;         use WORD_PACKAGE;
with DICTIONARY_FORM;
with PUT_EXAMPLE_LINE;
with LIST_SWEEP;
with PUT_STAT;
package body LIST_PACKAGE is

   package BOOLEAN_IO is new Text_IO.Enumeration_IO (Boolean);

   subtype XONS is PART_OF_SPEECH_TYPE range TACKON .. SUFFIX;

   type DICTIONARY_MNPC_RECORD is record
      D_K  : DICTIONARY_KIND  := DEFAULT_DICTIONARY_KIND;
      MNPC : MNPC_TYPE        := NULL_MNPC;
      DE   : DICTIONARY_ENTRY := NULL_DICTIONARY_ENTRY;
   end record;
   NULL_DICTIONARY_MNPC_RECORD : DICTIONARY_MNPC_RECORD :=
     (X, NULL_MNPC, NULL_DICTIONARY_ENTRY);

   MAX_MEANING_PRINT_SIZE : constant := 79;
   MM                     : Integer  := MAX_MEANING_SIZE;
   I, J, K                : Integer  := 0;

   Last_Meaning_Same, Next_Form_Same, Last_Form_Same, Put_Form_Anyway,
   Put_Meaning_Anyway : Boolean := False;



   function CAP_STEM (S : String) return String is
   begin
      if ALL_CAPS then
         return UPPER_CASE (S);
      elsif CAPITALIZED then
         return UPPER_CASE (S (S'FIRST)) & S (S'FIRST + 1 .. S'LAST);
      else
         return S;
      end if;
   end CAP_STEM;

   function CAP_ENDING (S : String) return String is
   begin
      if ALL_CAPS then
         return UPPER_CASE (S);
      else
         return S;
      end if;
   end CAP_ENDING;


      function TRIM_BAR (S : String) return String is
      --  Takes vertical bars from beginning of MEAN and TRIMs
      begin

         if S'LENGTH > 3 and then S (S'FIRST .. S'FIRST + 3) = "||||" then
            return TRIM (S (S'FIRST + 4 .. S'LAST));
         elsif S'LENGTH > 2 and then S (S'FIRST .. S'FIRST + 2) = "|||" then
            return TRIM (S (S'FIRST + 3 .. S'LAST));
         elsif S'LENGTH > 1 and then S (S'FIRST .. S'FIRST + 1) = "||" then
            return TRIM (S (S'FIRST + 2 .. S'LAST));
         elsif S (S'FIRST) = '|' then
            return TRIM (S (S'FIRST + 1 .. S'LAST));
         else
            return TRIM (S);
         end if;
   end TRIM_BAR;

   procedure PUT_DICTIONARY_FLAGS
     (OUTPUT : Text_IO.File_Type; DE : DICTIONARY_ENTRY; HIT : out Boolean)
   is
   begin

      if WORDS_MODE (SHOW_AGE) or
        (TRIM (DICTIONARY_AGE (DE.TRAN.AGE))'LENGTH /= 0)
      then  --  Not X
         Text_IO.Put (OUTPUT, "  " & TRIM (DICTIONARY_AGE (DE.TRAN.AGE)));
         HIT := True;
      end if;
      if (WORDS_MODE (SHOW_FREQUENCY) or (DE.TRAN.FREQ >= D)) and
        (TRIM (DICTIONARY_FREQUENCY (DE.TRAN.FREQ))'LENGTH /= 0)
      then
         Text_IO.Put
           (OUTPUT, "  " & TRIM (DICTIONARY_FREQUENCY (DE.TRAN.FREQ)));
         HIT := True;
      end if;
   end PUT_DICTIONARY_FLAGS;


   procedure PUT_DICTIONARY_FORM
     (OUTPUT : Text_IO.File_Type; D_K : DICTIONARY_KIND; MNPC : DICT_IO.Count;
      DE     : DICTIONARY_ENTRY)
   is
      CHIT, DHIT, EHIT, FHIT, LHIT : Boolean :=
        False;   --  Things on this line?
      DICTIONARY_LINE_NUMBER : Integer := Integer (MNPC);
      --DE : DICTIONARY_ENTRY := DM.DE;

   begin                               --  PUT_DICTIONARY_FORM
      if WORDS_MODE (DO_DICTIONARY_FORMS) then

         if WORDS_MODE (DO_ANSI_FORMATTING) then
            Text_IO.Put (OUTPUT, Format_Reset);
            Text_IO.Put (OUTPUT, Format_Underline);
         end if;

         if WORDS_MDEV (DO_PEARSE_CODES) then
            Text_IO.Put (OUTPUT, "02 ");
            DHIT := True;
         end if;

         if DICTIONARY_FORM (DE)'LENGTH /= 0 then
            Text_IO.Put (OUTPUT, DICTIONARY_FORM (DE) & "  ");
            DHIT := True;

            -- qui and aliqui (PRON) are frequent enough that they should have dictionary forms
            -- due to the program's structure we'll use another kludge to get there
         elsif DE.PART.POFS = PRON then
            if TRIM (DE.STEMS (1)) = "qu" then
               Text_IO.Put (OUTPUT, "qui, quae, quod  PRON  ");
               --   Dhit := True;
            elsif TRIM (DE.STEMS (1)) = "aliqu" then
               Text_IO.Put (OUTPUT, "aliqui, aliquae, aliquod  PRON  ");
               -- Dhit := True;
            end if;

         end if;

         if D_K = UNIQUE and then WORDS_MDEV (SHOW_DICTIONARY) = False then
            Text_IO.Put (OUTPUT, " [unique] ");
         end if;

      end if;

      if WORDS_MDEV (SHOW_DICTIONARY_CODES) and then DE.PART.POFS not in XONS
      then
         Text_IO.Put (OUTPUT, " [");
         AGE_TYPE_IO.Put (OUTPUT, DE.TRAN.AGE);
         AREA_TYPE_IO.Put (OUTPUT, DE.TRAN.AREA);
         GEO_TYPE_IO.Put (OUTPUT, DE.TRAN.GEO);
         FREQUENCY_TYPE_IO.Put (OUTPUT, DE.TRAN.FREQ);
         SOURCE_TYPE_IO.Put (OUTPUT, DE.TRAN.SOURCE);
         Text_IO.Put (OUTPUT, "]  ");
         CHIT := True;
      end if;

      if WORDS_MDEV (SHOW_DICTIONARY) then
         Text_IO.Put (OUTPUT, EXT (D_K) & ">");
         EHIT := True;
      end if;

      if WORDS_MDEV (SHOW_DICTIONARY_LINE) then
         if DICTIONARY_LINE_NUMBER > 0 then
            Text_IO.Put
              (OUTPUT,
               "(" & TRIM (Integer'IMAGE (DICTIONARY_LINE_NUMBER)) & ")");
            LHIT := True;
         end if;
      end if;

      PUT_DICTIONARY_FLAGS (OUTPUT, DE, FHIT);

       if WORDS_MODE (DO_ANSI_FORMATTING) then
         Text_IO.Put (Format_Reset);
       end if;

      if (CHIT or DHIT or EHIT or FHIT or LHIT) then
         Text_IO.New_Line (OUTPUT);
      end if;

   end PUT_DICTIONARY_FORM;

   procedure LIST_STEMS
     (OUTPUT :    Text_IO.File_Type; RAW_WORD : String; INPUT_LINE : String;
      PA     : in out PARSE_ARRAY; PA_LAST : in out Integer)
   is
      use Text_IO;
      use DICT_IO;

      --  The main WORD processing has been to produce an array of PARSE_RECORD
      --      type PARSE_RECORD is
      --        record
      --          STEM  : STEM_TYPE := NULL_STEM_TYPE;
      --          IR    : INFLECTION_RECORD := NULL_INFLECTION_RECORD;
      --          D_K   : DICTIONARY_KIND := DEFAULT_DICTIONARY_KIND;
      --          MNPC  : DICT_IO.COUNT := NULL_MNPC;
      --        end record;
      --  This has involved STEMFILE and INFLECTS, no DICTFILE

   --  PARSE_RECORD is put through the LIST_SWEEP procedure that does TRIMing
   --  Then, for processing for output, the data is converted to arrays of
      --      type STEM_INFLECTION_RECORD is
      --        record
      --          STEM : STEM_TYPE          := NULL_STEM_TYPE;
      --          IR   : INFLECTION_RECORD  := NULL_INFLECTION_RECORD;
      --        end record;
      --  and
      --      type DICTIONARY_MNPC_RECORD is
      --        record
      --          D_K  : DICTIONARY_KIND;
      --          MNPC : MNPC_TYPE;
      --          DE   : DICTIONARY_ENTRY;
      --        end record;
      --  containing the same data plus the DICTFILE data DICTIONARY_ENTRY
      --  but breaking it into two arrays allows different manipulation
      --  These are only within this routine, used to clean up the output

      type STEM_INFLECTION_RECORD is record
         STEM : STEM_TYPE         := NULL_STEM_TYPE;
         IR   : INFLECTION_RECORD := NULL_INFLECTION_RECORD;
      end record;
      NULL_STEM_INFLECTION_RECORD : STEM_INFLECTION_RECORD;

      STEM_INFLECTION_ARRAY_SIZE       : constant := 12;
      STEM_INFLECTION_ARRAY_ARRAY_SIZE : constant := 40;
      -- SPR:  Increased inflection array size to prevent raising exception when a single rececord generates too many QUALs (e.g., 'ludica' generates 12 QUALs
      --       However, this exposed an issue where QUALs could be duplicated (ludica does this)
      --       -> corrected this by deleting duplicate quals in list_sweep

      type STEM_INFLECTION_ARRAY is
        array (Integer range <>) of STEM_INFLECTION_RECORD;
      type STEM_INFLECTION_ARRAY_ARRAY is
        array
          (Integer range <>) of STEM_INFLECTION_ARRAY
          (1 .. STEM_INFLECTION_ARRAY_SIZE);

      SRA, OSRA,
      NULL_SRA : STEM_INFLECTION_ARRAY (1 .. STEM_INFLECTION_ARRAY_SIZE) :=
        (others => (NULL_STEM_TYPE, NULL_INFLECTION_RECORD));
      SRAA,
      NULL_SRAA : STEM_INFLECTION_ARRAY_ARRAY
        (1 .. STEM_INFLECTION_ARRAY_ARRAY_SIZE) :=
        (others => NULL_SRA);

--      type DICTIONARY_MNPC_RECORD is record
--        D_K  : DICTIONARY_KIND := DEFAULT_DICTIONARY_KIND;
--        MNPC : MNPC_TYPE := NULL_MNPC;
--        DE   : DICTIONARY_ENTRY := NULL_DICTIONARY_ENTRY;
--      end record;
--      NULL_DICTIONARY_MNPC_RECORD : DICTIONARY_MNPC_RECORD
--                                  := (X, NULL_MNPC, NULL_DICTIONARY_ENTRY);
      DM, ODM : DICTIONARY_MNPC_RECORD := NULL_DICTIONARY_MNPC_RECORD;

      DICTIONARY_MNPC_ARRAY_SIZE : constant := 40;

      type DICTIONARY_MNPC_ARRAY is
        array (1 .. DICTIONARY_MNPC_ARRAY_SIZE) of DICTIONARY_MNPC_RECORD;
      DMA, ODMA, NULL_DMA : DICTIONARY_MNPC_ARRAY;

      --MEANING_ARRAY_SIZE : constant := 5;
      --MEANING_ARRAY : array (1..MEANING_ARRAY_SIZE) of MEANING_TYPE;

      DEA : DICTIONARY_ENTRY := NULL_DICTIONARY_ENTRY;

      W                  : constant String := RAW_WORD;
      J, J1, J2, K       : Integer         := 0;
      THERE_IS_AN_ADVERB : Boolean         := False;




      procedure PUT_INFLECTION
        (SR : STEM_INFLECTION_RECORD; DM : DICTIONARY_MNPC_RECORD)
      is
         --  Handles putting ONLY_MEAN, PEARSE_CODES, CAPS, QUAL, V_KIND, FLAGS
         procedure PUT_INFLECTION_FLAGS is
         begin

            if
              (WORDS_MODE (SHOW_AGE) or
               (SR.IR.AGE /= X)) and     --  Warn even if not to show AGE
              TRIM (INFLECTION_AGE (SR.IR.AGE))'LENGTH /= 0
            then
               Text_IO.Put (OUTPUT, "  " & INFLECTION_AGE (SR.IR.AGE));
            end if;
            if
              (WORDS_MODE (SHOW_FREQUENCY) or
               (SR.IR.FREQ >= C)) and    --  Warn regardless
              TRIM (INFLECTION_FREQUENCY (SR.IR.FREQ))'LENGTH /= 0
            then
               Text_IO.Put (OUTPUT, "  " & INFLECTION_FREQUENCY (SR.IR.FREQ));
            end if;
         end PUT_INFLECTION_FLAGS;

      begin
-- TEXT_IO.PUT_LINE("PUT_INFLECTION ");
         if
           (not WORDS_MODE (DO_ONLY_MEANINGS) and
            not (CONFIGURATION = ONLY_MEANINGS))
         then
            Text_IO.Set_Col (OUTPUT, 1);
            if WORDS_MDEV (DO_PEARSE_CODES) then
               if DM.D_K = ADDONS then
                  Text_IO.Put (OUTPUT, "05 ");
               elsif DM.D_K in XXX .. YYY then
                  Text_IO.Put (OUTPUT, "06 ");
               else
                  Text_IO.Put (OUTPUT, "01 ");
               end if;
            end if;

--TEXT_IO.PUT(OUTPUT, CAP_STEM(TRIM(SR.STEM)));
            Text_IO.Put (OUTPUT, (TRIM (SR.STEM)));
            if SR.IR.ENDING.SIZE > 0 then
               Text_IO.Put (OUTPUT, ".");
--TEXT_IO.PUT(OUTPUT, TRIM(CAP_ENDING(SR.IR.ENDING.SUF)));
               Text_IO.Put (OUTPUT, TRIM ((SR.IR.ENDING.SUF)));
            end if;

            if WORDS_MDEV (DO_PEARSE_CODES) then
               Text_IO.Set_Col (OUTPUT, 25);
            else
               Text_IO.Set_Col (OUTPUT, 22);
            end if;

            if SR.IR /= NULL_INFLECTION_RECORD then

--PRINT_MODIFIED_QUAL:   --  Really pedantic
--declare
--  OUT_STRING : STRING(1..QUALITY_RECORD_IO.DEFAULT_WIDTH);
--WHICH_START : constant INTEGER
--            := PART_OF_SPEECH_TYPE_IO.DEFAULT_WIDTH + 1 + 1; --  8
--VARIANT_START : constant INTEGER
--            := WHICH_START + WHICH_TYPE_IO_DEFAULT_WIDTH + 1;
--VARIANT_FINISH : constant INTEGER
--            := VARIANT_START + VARIANT_TYPE_IO_DEFAULT_WIDTH;
--WHICH_BLANK : constant STRING(WHICH_START..VARIANT_START) := (others => ' ');
--VARIANT_BLANK : constant STRING(VARIANT_START..VARIANT_FINISH) := (others => ' ');
--begin
--  QUALITY_RECORD_IO.PUT(OUT_STRING, SR.IR.QUAL);
--  case SR.IR.QUAL.POFS is
--    when N | NUM | V | VPAR | SUPINE  =>         --  ADJ?
--      OUT_STRING(VARIANT_START..VARIANT_FINISH) := VARIANT_BLANK;
--    when PRON | PACK   =>
--      OUT_STRING(WHICH_START..VARIANT_FINISH) := WHICH_BLANK & VARIANT_BLANK;
--    when ADJ  =>
--      if SR.IR.QUAL.ADJ.DECL.WHICH = 1  then
--        OUT_STRING(VARIANT_START..VARIANT_FINISH) := VARIANT_BLANK;
--      end if;
--    when others  =>
--      null;
--  end case;
--  TEXT_IO.PUT(OUTPUT, OUT_STRING);
--end PRINT_MODIFIED_QUAL;

--QUALITY_RECORD_IO.PUT(OUTPUT, SR.IR.QUAL);
--NEW_LINE(OUTPUT);
--DICTIONARY_ENTRY_IO.PUT(OUTPUT, DM.DE);
--NEW_LINE(OUTPUT);

               PRINT_MODIFIED_QUAL :
               declare
                  OUT_STRING : String (1 .. QUALITY_RECORD_IO.DEFAULT_WIDTH);

                  PASSIVE_START : Integer :=
                    PART_OF_SPEECH_TYPE_IO.Default_Width + 1 +
                    DECN_RECORD_IO.DEFAULT_WIDTH + 1 +
                    TENSE_TYPE_IO.Default_Width + 1;
                  PASSIVE_FINISH : Integer :=
                    PASSIVE_START + VOICE_TYPE_IO.Default_Width;
                  PPL_START : Integer :=
                    PART_OF_SPEECH_TYPE_IO.Default_Width + 1 +
                    DECN_RECORD_IO.DEFAULT_WIDTH + 1 +
                    CASE_TYPE_IO.Default_Width + 1 +
                    NUMBER_TYPE_IO.Default_Width + 1 +
                    GENDER_TYPE_IO.Default_Width + 1 +
                    TENSE_TYPE_IO.Default_Width + 1;
                  PPL_FINISH : Integer :=
                    PPL_START + VOICE_TYPE_IO.Default_Width;
                  PASSIVE_BLANK : constant String
                    (1 .. VOICE_TYPE_IO.Default_Width) :=
                    (others => ' ');
               begin

--TEXT_IO.PUT_LINE("PASSIVE_START   = " & INTEGER'IMAGE(PASSIVE_START));
--TEXT_IO.PUT_LINE("PASSIVE_FINISH  = " & INTEGER'IMAGE(PASSIVE_FINISH));
--TEXT_IO.PUT_LINE("PPL_START   =  " & INTEGER'IMAGE(PPL_START));
--TEXT_IO.PUT_LINE("PPL_FINISH   =  " & INTEGER'IMAGE(PPL_FINISH));
--

--TEXT_IO.PUT_LINE("START PRINT MODIFIED QUAL " );
--  QUALITY_RECORD_IO.PUT(OUT_STRING, SR.IR.QUAL);
--  if (DM.D_K in GENERAL..LOCAL)  and then  --  UNIQUES has no DE
--     (SR.IR.QUAL.POFS = V)    and then
--    ((SR.IR.QUAL.V.TENSE_VOICE_MOOD.MOOD in IND..INF)   and
--     (DM.DE.PART.V.KIND = DEP))       then
----TEXT_IO.PUT_LINE("PRINT MODIFIED QUAL 1a" );
--    OUT_STRING(PASSIVE_START+1..PASSIVE_FINISH) := PASSIVE_BLANK;
--  ----TEXT_IO.PUT_LINE("PRINT MODIFIED QUAL 2a" );
--  elsif (DM.D_K in GENERAL..LOCAL)  and then  --  UNIQUES has no DE
--     (SR.IR.QUAL.POFS = VPAR)  and then
--    ((SR.IR.QUAL.V.TENSE_VOICE_MOOD.MOOD = PPL)   and
--     (DM.DE.PART.V.KIND = DEP))       then
--TEXT_IO.PUT_LINE("PRINT MODIFIED QUAL 1b" );
--    OUT_STRING(PPL_START+1..PPL_FINISH) := PASSIVE_BLANK;
--TEXT_IO.PUT_LINE("PRINT MODIFIED QUAL 2b" );
--
--  end if;
----TEXT_IO.PUT_LINE("PRINT MODIFIED QUAL 3" );

--TEXT_IO.PUT_LINE("START PRINT MODIFIED QUAL " );      --  Prints the line(s) that explain each inflected form
                  QUALITY_RECORD_IO.PUT
                    (OUT_STRING,
                     SR.IR
                       .QUAL);        --  E.g., "V      1 1 PRES ACTIVE  IMP 2 S   "

                  if (DM.D_K in GENERAL .. LOCAL) then  --  UNIQUES has no DE

                     if (SR.IR.QUAL.POFS = V)
                       and then (DM.DE.PART.V.KIND = DEP)
                       and then
                       (SR.IR.QUAL.V.TENSE_VOICE_MOOD.MOOD in IND .. INF)
                     then
--TEXT_IO.PUT_LINE("START PRINT MODIFIED QUAL   V" );
                        OUT_STRING (PASSIVE_START + 1 .. PASSIVE_FINISH) :=
                          PASSIVE_BLANK;
                     elsif (SR.IR.QUAL.POFS = VPAR)
                       and then (DM.DE.PART.V.KIND = DEP)
                       and then (SR.IR.QUAL.VPAR.TENSE_VOICE_MOOD.MOOD = PPL)
                     then
--TEXT_IO.PUT_LINE("START PRINT MODIFIED QUAL   VPAR" );
                        OUT_STRING (PPL_START + 1 .. PPL_FINISH) :=
                          PASSIVE_BLANK;
                     end if;

                  end if;
                  Text_IO.Put (OUTPUT, OUT_STRING);

--                  TEXT_IO.PUT_LINE("PRINT MODIFIED QUAL 4" );

               end PRINT_MODIFIED_QUAL;

               PUT_INFLECTION_FLAGS;

               -- PUT EXAMPLE LINE
               if WORDS_MODE (DO_ANSI_FORMATTING)
                 and then WORDS_MODE (DIM_EXAMPLES_TEXT)
                 and then WORDS_MODE (DO_EXAMPLES)
                 and then WORDS_MODE (WRITE_OUTPUT_TO_FILE) = False
               then
                  Text_IO.Put (Format_Reset);
                  Text_IO.Put (Format_Faint);
               end if;

               if DM.DE.PART /= NULL_DICTIONARY_ENTRY.PART
               then  -- SPR:  Added because some UNIQUES won't generate a DE,
               --       raising an exception unless when it finds a null entry
                  PUT_EXAMPLE_LINE (OUTPUT, SR.IR, DM.DE);

               end if;                                       --

               if WORDS_MODE (DO_ANSI_FORMATTING)
                 and then WORDS_MODE (DIM_EXAMPLES_TEXT)
                 and then WORDS_MODE (DO_EXAMPLES)
                 and then WORDS_MODE (WRITE_OUTPUT_TO_FILE) = False
               then
                  Text_IO.Put (Format_Reset);
               end if;
               --  END PUT EXAMPLE LINE

            end if;
         end if;

         Text_IO.New_Line (OUTPUT);

      end PUT_INFLECTION;

--         procedure PUT_DICTIONARY_FORM(OUTPUT : TEXT_IO.FILE_TYPE;
--                                       DM     : DICTIONARY_MNPC_RECORD) is
--             HIT : BOOLEAN := FALSE;   --  Is anything on this line
--             DICTIONARY_LINE_NUMBER : INTEGER := INTEGER(DM.MNPC);
--             DE : DICTIONARY_ENTRY := DM.DE;
--
--
--         begin                               --  PUT_DICTIONARY_FORM
--               if WORDS_MODE(DO_DICTIONARY_FORMS)  then
--                 if WORDS_MDEV(DO_PEARSE_CODES) then
--                   TEXT_IO.PUT(OUTPUT, "02 ");
--                   HIT := TRUE;
--                end if;
--                if DICTIONARY_FORM(DE)'LENGTH /= 0  then
--                  TEXT_IO.PUT(OUTPUT, DICTIONARY_FORM(DE) & "  ");
--                  HIT := TRUE;
--                end if;
--               end if;
--
--
--
--               if WORDS_MDEV(SHOW_DICTIONARY_CODES) and then
--                  DE.PART.POFS not in XONS              then
--                  TEXT_IO.PUT(OUTPUT, " [");
--                 AGE_TYPE_IO.PUT(OUTPUT, DE.TRAN.AGE);
--                 AREA_TYPE_IO.PUT(OUTPUT, DE.TRAN.AREA);
--                 GEO_TYPE_IO.PUT(OUTPUT, DE.TRAN.GEO);
--                 FREQUENCY_TYPE_IO.PUT(OUTPUT, DE.TRAN.FREQ);
--                 SOURCE_TYPE_IO.PUT(OUTPUT, DE.TRAN.SOURCE);
--                 TEXT_IO.PUT(OUTPUT, "]  ");
--                 HIT := TRUE;
--               end if;
--
--
--               if WORDS_MDEV(SHOW_DICTIONARY) then
--                 TEXT_IO.PUT(OUTPUT, EXT(DM.D_K) & ">");
--                 HIT := TRUE;
--              end if;
--
--
--               if WORDS_MDEV(SHOW_DICTIONARY_LINE)  then
--                 if DICTIONARY_LINE_NUMBER > 0  then
--                   TEXT_IO.PUT(OUTPUT, "("
--                         & TRIM(INTEGER'IMAGE(DICTIONARY_LINE_NUMBER)) & ")");
--                   HIT := TRUE;
--                  end if;
--               end if;
--
--
--
--               PUT_DICTIONARY_FLAGS(DM, HIT);
--
--
--               if HIT   then
--                  TEXT_IO.NEW_LINE(OUTPUT);
--               end if;
--
--            --end if;
--
--         end PUT_DICTIONARY_FORM;
--
--
      procedure PUT_FORM
        (SR : STEM_INFLECTION_RECORD; DM : DICTIONARY_MNPC_RECORD)
      is
      --  Handles PEARSE_CODES and DICTIONARY_FORM (which has FLAGS) and D_K
      --  The Pearse 02 is handled in PUT_DICTIONARY_FORM
      begin
         if (SR.IR.QUAL.POFS not in XONS) and (DM.D_K in GENERAL .. UNIQUE)
         then
            --   Text_IO.New_Line(OUTPUT);
            PUT_DICTIONARY_FORM
              (OUTPUT, DM.D_K, DM.MNPC,
               DM.DE);  -- prints dictionary form line; e.g.,
         end if;                                                  --  amo, amare, amavi, amatus  V (1st)   [XXXAO]      veryfreq
      end PUT_FORM;

      procedure PUT_MEANING (OUTPUT : Text_IO.File_Type; RAW_MEANING : String)
      is
      --  Handles the MM screen line limit and TRIM_BAR, then TRIMs

      begin

         Text_IO.Put (OUTPUT, TRIM (HEAD (TRIM_BAR (RAW_MEANING), MM)));
      end PUT_MEANING;

      function CONSTRUCTED_MEANING
        (SR : STEM_INFLECTION_RECORD; DM : DICTIONARY_MNPC_RECORD)
         return String
      is
         --  Constructs the meaning for NUM from NUM.SORT and NUM_VALUE
         S : String (1 .. MAX_MEANING_SIZE) := NULL_MEANING_TYPE;
         N : Integer                        := 0;
      begin
         if DM.DE.PART.POFS = NUM then
            N := DM.DE.PART.NUM.VALUE;
            if SR.IR.QUAL.POFS = NUM then    --  Normal parse
               case SR.IR.QUAL.NUM.SORT is
                  when CARD =>
                     S :=
                       HEAD
                         (Integer'IMAGE (N) & " - (CARD answers 'how many');",
                          MAX_MEANING_SIZE);
                  when ORD =>
                     S :=
                       HEAD
                         (Integer'IMAGE (N) &
                          "th - (ORD, 'in series'); (a/the)" &
                          Integer'IMAGE (N) & "th (part) (fract. w/ pars?);",
                          MAX_MEANING_SIZE);
                  when DIST =>
                     S :=
                       HEAD
                         (Integer'IMAGE (N) &
                          " each/apiece/times/fold/together/at a time - 'how many each'; by " &
                          Integer'IMAGE (N) & "s; ",
                          MAX_MEANING_SIZE);
                  when ADVERB =>
                     S :=
                       HEAD
                         (Integer'IMAGE (N) & " times, on" &
                          Integer'IMAGE (N) &
                          " occasions - (ADVERB answers 'how often');",
                          MAX_MEANING_SIZE);
                  when others =>
                     null;
               end case;
            else  -- there is fix so POFS is not NUM
               S := HEAD ("Number " & Integer'IMAGE (N), MAX_MEANING_SIZE);
            end if;
         end if;
         return S;

      end CONSTRUCTED_MEANING;

      procedure PUT_MEANING_LINE
        (SR : STEM_INFLECTION_RECORD; DM : DICTIONARY_MNPC_RECORD)
      is
      begin
         if DM.D_K not in ADDONS .. PPP then
            if WORDS_MODE (DO_ANSI_FORMATTING) then
               Text_IO.Put (Format_Reset);
               Text_IO.Put (Format_Bold);
            end if;

            if WORDS_MDEV (DO_PEARSE_CODES) then
               Text_IO.Put (OUTPUT, "03 ");
            end if;
            if DM.DE.PART.POFS = NUM and then DM.DE.PART.NUM.VALUE > 0 then
               Text_IO.Put_Line
                 (OUTPUT,
                  CONSTRUCTED_MEANING (SR, DM));    --  Constructed MEANING
            elsif DM.D_K = UNIQUE then
               PUT_MEANING (OUTPUT, UNIQUES_DE (DM.MNPC).MEAN);
               Text_IO.New_Line (OUTPUT);
            else
               PUT_MEANING (OUTPUT, TRIM_BAR (DM.DE.MEAN));
               Text_IO.New_Line (OUTPUT);
            end if;
         else
            if DM.D_K = RRR then
               if RRR_MEANING /= NULL_MEANING_TYPE then
                  --PUT_DICTIONARY_FLAGS;

                  if WORDS_MODE (DO_ANSI_FORMATTING) then
                     Text_IO.Put (Format_Reset);
                     Text_IO.Put (Format_Bold);
                  end if;
                  if WORDS_MDEV (DO_PEARSE_CODES) then
                     Text_IO.Put (OUTPUT, "03 ");
                  end if;
                  PUT_MEANING (OUTPUT, RRR_MEANING);      --  Roman Numeral
                  RRR_MEANING := NULL_MEANING_TYPE;
                  Text_IO.New_Line (OUTPUT);
                  -- Text_IO.Put_Line("-----------");

               end if;

            elsif DM.D_K = NNN then
               if NNN_MEANING /= NULL_MEANING_TYPE then
                  --PUT_DICTIONARY_FLAGS;
                  if WORDS_MODE (DO_ANSI_FORMATTING) then
                     Text_IO.Put (Format_Reset);
                     Text_IO.Put (Format_Bold);
                  end if;
                  if WORDS_MDEV (DO_PEARSE_CODES) then
                     Text_IO.Put (OUTPUT, "03 ");
                  end if;
                  PUT_MEANING (OUTPUT, NNN_MEANING);  --  Unknown Name
                  NNN_MEANING := NULL_MEANING_TYPE;
                  Text_IO.New_Line (OUTPUT);
               end if;

            elsif DM.D_K = XXX then
               if XXX_MEANING /= NULL_MEANING_TYPE then
                  if WORDS_MDEV (DO_PEARSE_CODES) then
                     Text_IO.Put (OUTPUT, "06 ");
                  end if;
                  if WORDS_MODE (DO_ANSI_FORMATTING) then
                     Text_IO.Put (Format_Reset);
                     Text_IO.Put (Format_Inverse);
                  end if;
                  PUT_MEANING (OUTPUT, XXX_MEANING);  --  TRICKS
                  XXX_MEANING := NULL_MEANING_TYPE;
                  if WORDS_MODE (DO_ANSI_FORMATTING) then
                     Text_IO.Put (Format_Reset);
                  end if;

                  Text_IO.New_Line (OUTPUT);
               end if;

            elsif DM.D_K = YYY then
               if YYY_MEANING /= NULL_MEANING_TYPE then
                  if WORDS_MDEV (DO_PEARSE_CODES) then
                     Text_IO.Put (OUTPUT, "06 ");
                  end if;
                  if WORDS_MODE (DO_ANSI_FORMATTING) then
                     Text_IO.Put (Format_Reset);
                     Text_IO.Put (Format_Inverse);
                  end if;
                  PUT_MEANING (OUTPUT, YYY_MEANING);  --  Syncope
                  if WORDS_MODE (DO_ANSI_FORMATTING) then
                     Text_IO.Put (Format_Reset);
                  end if;
                  YYY_MEANING := NULL_MEANING_TYPE;
                  Text_IO.New_Line (OUTPUT);
               end if;

            elsif DM.D_K = PPP then
               if PPP_MEANING /= NULL_MEANING_TYPE then
                  if WORDS_MDEV (DO_PEARSE_CODES) then
                     Text_IO.Put (OUTPUT, "06 ");
                  end if;
                  if WORDS_MODE (DO_ANSI_FORMATTING) then
                     Text_IO.Put (Format_Reset);
                     Text_IO.Put (Format_Inverse);
                  end if;
                  PUT_MEANING (OUTPUT, PPP_MEANING); --  Compounds
                  if WORDS_MODE (DO_ANSI_FORMATTING) then
                     Text_IO.Put (Format_Reset);
                  end if;
                  PPP_MEANING := NULL_MEANING_TYPE;
                  Text_IO.New_Line (OUTPUT);
               end if;

            elsif DM.D_K = ADDONS then
               if WORDS_MDEV (DO_PEARSE_CODES) then
                  Text_IO.Put (OUTPUT, "06 ");
               end if;
               if WORDS_MODE (DO_ANSI_FORMATTING) then
                  Text_IO.Put (Format_Reset);
                  Text_IO.Put (Format_Inverse);
               end if;
               PUT_MEANING (OUTPUT, MEANS (Integer (DM.MNPC)));
               if WORDS_MODE (DO_ANSI_FORMATTING) then
                  Text_IO.Put (Format_Reset);
               end if;
               Text_IO.New_Line (OUTPUT);

            end if;

         end if;
         if WORDS_MODE (DO_ANSI_FORMATTING) then
            Text_IO.Put (Format_Reset);
         end if;
      end PUT_MEANING_LINE;

   begin
      TRIMMED := False;

      --  Since this procedure weeds out possible parses, if it weeds out all
      --  (or all of a class) it must fix up the rest of the parse array,
      --  e.g., it must clean out dangling prefixes and suffixes

--    --  Just to find the words with long/complicated output at the processing level
--    --  This is done with the final PA_LAST, entering LIST_STEM, before SWEEP
--       if PA_LAST > PA_LAST_MAX   then
--         PUT_STAT("$PA_LAST_MAX    for RAW_WORD " & HEAD(RAW_WORD, 24) & "   = " & INTEGER'IMAGE(PA_LAST));
--         PA_LAST_MAX := PA_LAST;
--       end if;

--TEXT_IO.PUT_LINE("PA on entering LIST_STEMS    PA_LAST = " & INTEGER'IMAGE(PA_LAST));
--for I in 1..PA_LAST  loop
--PARSE_RECORD_IO.PUT(PA(I)); TEXT_IO.NEW_LINE;
--end loop;

      if (Text_IO.Name (OUTPUT) = Text_IO.Name (Text_IO.Standard_Output)) then
         MM :=
           MAX_MEANING_PRINT_SIZE;   --  to keep from overflowing screen line
         --  or even adding blank line
      else
         MM := MAX_MEANING_SIZE;

      end if;

      -------  The gimmick of adding an ADV if there is only ADJ VOC  ----
--TEXT_IO.PUT_LINE("About to do the ADJ -> ADV kludge");
      for I in PA'FIRST .. PA_LAST loop
         if PA (I).IR.QUAL.POFS = ADV then
            THERE_IS_AN_ADVERB := True;
            exit;
         end if;
      end loop;

--TEXT_IO.PUT_LINE("In the ADJ -> ADV kludge  Checked to see if there is an ADV");

      if ((not THERE_IS_AN_ADVERB) and (WORDS_MODE (DO_FIXES))) then
--TEXT_IO.PUT_LINE("In the ADJ -> ADV kludge  There is no ADV");
         for I in reverse PA'FIRST .. PA_LAST loop

            if PA (I).IR.QUAL.POFS = ADJ
              and then
              (PA (I).IR.QUAL.ADJ = ((1, 1), VOC, S, M, POS) or
               ((PA (I).IR.QUAL.ADJ.CS = VOC) and
                (PA (I).IR.QUAL.ADJ.NUMBER = S) and
                (PA (I).IR.QUAL.ADJ.GENDER = M) and
                (PA (I).IR.QUAL.ADJ.CO = SUPER)))
            then

               J := I;

               while J >= PA'FIRST loop  --Back through other ADJ cases
                  if PA (J).IR.QUAL.POFS /= ADJ then
                     J2 :=
                       J;                          --  J2 is first (reverse) that is not ADJ
                     exit;
                  end if;
                  J := J - 1;
               end loop;
               while J >= PA'FIRST loop  --  Sweep up associated fixes
                  if PA (J).IR.QUAL.POFS not in XONS then
                     J1 :=
                       J;                      --  J1 is first (reverse) that is not XONS
                     exit;
                  end if;
                  J := J - 1;
               end loop;

               for J in J1 + 1 .. J2 loop
                  PA (PA_LAST + J - J1 + 1) := PA (J);
               end loop;
--TEXT_IO.PUT_LINE("In the ADJ -> ADV kludge  Ready to add PA for ADV");
               PA_LAST      := PA_LAST + J2 - J1 + 1;
               PA (PA_LAST) := PA (J2 + 1);
--TEXT_IO.PUT_LINE("In the ADJ -> ADV kludge  Adding SUFFIX E ADV");
               PA (PA_LAST) :=
                 ("e                 ",
                  ((SUFFIX, NULL_SUFFIX_RECORD), 0, NULL_ENDING_RECORD, X, B),
                  PPP, NULL_MNPC);
--PARSE_RECORD_IO.PUT(PA(PA_LAST)); TEXT_IO.NEW_LINE;
               PA_LAST := PA_LAST + 1;

               if PA (J2 + 1).IR.QUAL.ADJ.CO = POS then

--TEXT_IO.PUT_LINE("In the ADJ -> ADV kludge  Adding POS for ADV");
                  PA (PA_LAST) :=
                    (PA (J2 + 1).STEM,
                     ((POFS => ADV,
                       ADV  =>
                         (CO        => PA (J2 + 1).IR.QUAL.ADJ.CO,
                          GENERATED => ADJADV)),
                      KEY => 0, ENDING => (1, "e      "), AGE => X, FREQ => B),
                     PA (J2 + 1).D_K, PA (J2 + 1).MNPC);
                  --PARSE_RECORD_IO.PUT(PA(PA_LAST)); TEXT_IO.NEW_LINE;
                  PPP_MEANING :=
                    HEAD
                      ("Suffix may indicate ADV formed from ADJ (caution: the ADV form may not exist)",
                       MAX_MEANING_SIZE);

               elsif PA (J2 + 1).IR.QUAL.ADJ.CO = SUPER then
                  PA (PA_LAST) :=
                    (PA (J2 + 1).STEM,
                     ((POFS => ADV,
                       ADV  =>
                         (CO        => PA (J2 + 1).IR.QUAL.ADJ.CO,
                          GENERATED => ADJADV)),
                      KEY => 0, ENDING => (2, "me     "), AGE => X, FREQ => B),
                     PA (J2 + 1).D_K, PA (J2 + 1).MNPC);
                  PPP_MEANING :=
                    HEAD
                      ("Suffix may indicate ADV formed from ADJ (caution: the ADV form may not exist)",
                       MAX_MEANING_SIZE);

               end if;
      --TEXT_IO.PUT_LINE("In the ADJ -> ADV kludge  Done adding PA for ADV");
            end if;           --  PA(I).IR.QUAL.POFS = ADJ

         end loop;

      end if;           --  not THERE_IS_AN_ADVERB
-- TEXT_IO.PUT_LINE("In the ADJ -> ADV kludge  FINISHED");

      LIST_SWEEP (PA (1 .. PA_LAST), PA_LAST);

--  TEXT_IO.PUT_LINE("PA after leaving LIST_SWEEP    PA_LAST = "  & INTEGER'IMAGE(PA_LAST));
--  for I in 1..PA_LAST  loop
--  PARSE_RECORD_IO.PUT(PA(I)); TEXT_IO.NEW_LINE;
--  end loop;

--               --  Does STATS
--
--TEXT_IO.PUT_LINE("Before STATING FIXES");
      if WORDS_MDEV (WRITE_STATISTICS_FILE) then      --  Omit rest of output

         for I in 1 .. PA_LAST loop                       --  Just to PUT_STAT
            if (PA (I).D_K = ADDONS) then
               if PA (I).IR.QUAL.POFS = PREFIX then
                  PUT_STAT
                    ("ADDON PREFIX at " &
                     HEAD (Integer'IMAGE (LINE_NUMBER), 8) &
                     HEAD (Integer'IMAGE (WORD_NUMBER), 4) & "   " &
                     HEAD (W, 20) & "   " & PA (I).STEM & "  " &
                     Integer'IMAGE (Integer (PA (I).MNPC)));
               elsif PA (I).IR.QUAL.POFS = SUFFIX then
                  PUT_STAT
                    ("ADDON SUFFIX at " &
                     HEAD (Integer'IMAGE (LINE_NUMBER), 8) &
                     HEAD (Integer'IMAGE (WORD_NUMBER), 4) & "   " &
                     HEAD (W, 20) & "   " & PA (I).STEM & "  " &
                     Integer'IMAGE (Integer (PA (I).MNPC)));
               elsif PA (I).IR.QUAL.POFS = TACKON then
                  PUT_STAT
                    ("ADDON TACKON at " &
                     HEAD (Integer'IMAGE (LINE_NUMBER), 8) &
                     HEAD (Integer'IMAGE (WORD_NUMBER), 4) & "   " &
                     HEAD (W, 20) & "   " & PA (I).STEM & "  " &
                     Integer'IMAGE (Integer (PA (I).MNPC)));
               end if;
            end if;
         end loop;

----    --  Just to find the words with long/complicated output at the LIST level
----    --  This is done with the final PA_LAST, after SWEEP
--       if PA_LAST > FINAL_PA_LAST_MAX   then
--         PUT_STAT("$FINAL_PA_LAST_MAX    for RAW_WORD " & HEAD(RAW_WORD, 24) & "   = " & INTEGER'IMAGE(PA_LAST));
--         FINAL_PA_LAST_MAX := PA_LAST;
--       end if;

      end if;

--TEXT_IO.PUT_LINE("After STATING FIXES");

--  Convert from PARSE_RECORDs to DICTIONARY_MNPC_RECORD and STEM_INFLECTION_RECORD
--TEXT_IO.PUT_LINE("Doing arrays in LIST_STEMS    PA_LAST = "  &
--                 INTEGER'IMAGE(PA_LAST));
      I    := 1;           --  I cycles on PA
      J := 0;           --  J indexes the number of DMA arrays  --  Initialize
      SRAA := NULL_SRAA;
      DMA  := NULL_DMA;
      CYCLE_OVER_PA :
      while I <= PA_LAST loop       --  I cycles over full PA array
--TEXT_IO.PUT_LINE("Starting loop for I    I = " & INTEGER'IMAGE(I));
         ODM := NULL_DICTIONARY_MNPC_RECORD;

         if PA (I).D_K = UNIQUE then
            J            := J + 1;
            SRAA (J) (1) := (PA (I).STEM, PA (I).IR);
--TEXT_IO.PUT_LINE("UNIQUE   I = " & INTEGER'IMAGE(I) & "  J = " & INTEGER'IMAGE(J));
            DM      := NULL_DICTIONARY_MNPC_RECORD;
            DM.D_K  := UNIQUE;
            DM.MNPC := PA (I).MNPC;
            DM.DE   := UNIQUES_DE (PA (I).MNPC);
            DMA (J) := DM;
            I       := I + 1;
         else

            case PA (I).IR.QUAL.POFS is

               when N =>
                  OSRA := NULL_SRA;
                  ODMA := NULL_DMA;
                  --ODM := NULL_DICTIONARY_MNPC_RECORD;
                  --DM := NULL_DICTIONARY_MNPC_RECORD;
                  while PA (I).IR.QUAL.POFS = N and I <= PA_LAST loop
--TEXT_IO.PUT_LINE("Starting loop for N    I = " & INTEGER'IMAGE(I) & "   K = " & INTEGER'IMAGE(K));
                     if PA (I).MNPC /= ODM.MNPC
                     then   --  Encountering new MNPC
                        OSRA := SRA;
                        K    :=
                          1;                  --  K indexes within the MNPCA array --  Initialize
--TEXT_IO.PUT_LINE("Starting IRA for N    I = " & INTEGER'IMAGE(I) & "   K = " & INTEGER'IMAGE(K));
                        J :=
                          J +
                          1;             --  J indexes the number of MNPCA arrays - Next MNPCA
--TEXT_IO.PUT_LINE("Shifting J for N  I = " & INTEGER'IMAGE(I) & "   J = " & INTEGER'IMAGE(J));
                        SRAA (J) (K) := (PA (I).STEM, PA (I).IR);
                        DICT_IO.Set_Index
                          (DICT_FILE (PA (I).D_K), PA (I).MNPC);
                        DICT_IO.Read (DICT_FILE (PA (I).D_K), DEA);
                        DM      := (PA (I).D_K, PA (I).MNPC, DEA);
                        DMA (J) := DM;
                        ODM     := DM;
                     else
                        K :=
                          K +
                          1;              --  K indexes within the MNPCA array  - Next MNPC
--TEXT_IO.PUT_LINE("Continuing IRA for N  I = " & INTEGER'IMAGE(I) & "   K = " & INTEGER'IMAGE(K)
--                                                                 & "   J = " & INTEGER'IMAGE(J));
                        SRAA (J) (K) := (PA (I).STEM, PA (I).IR);
                     end if;

                     I := I + 1;              --  I cycles over full PA array
                  end loop;

               when PRON =>
                  OSRA := NULL_SRA;
                  ODMA := NULL_DMA;
                  --ODM := NULL_DICTIONARY_MNPC_RECORD;
                  --DM := NULL_DICTIONARY_MNPC_RECORD;
                  while PA (I).IR.QUAL.POFS = PRON and I <= PA_LAST loop
                     if PA (I).MNPC /= ODM.MNPC
                     then   --  Encountering new MNPC
                        OSRA := SRA;
                        K    :=
                          1;                  --  K indexes within the MNPCA array --  Initialize
                        J :=
                          J +
                          1;             --  J indexes the number of MNPCA arrays - Next MNPCA
                        SRAA (J) (K) := (PA (I).STEM, PA (I).IR);
                        DICT_IO.Set_Index
                          (DICT_FILE (PA (I).D_K), PA (I).MNPC);
                        DICT_IO.Read (DICT_FILE (PA (I).D_K), DEA);
                        DM      := (PA (I).D_K, PA (I).MNPC, DEA);
                        DMA (J) := DM;
                        ODM     := DM;
                     else
                        K :=
                          K +
                          1;              --  K indexes within the MNPCA array  - Next MNPC
                        SRAA (J) (K) := (PA (I).STEM, PA (I).IR);
                     end if;

                     I := I + 1;              --  I cycles over full PA array
                  end loop;

               when PACK =>
                  OSRA := NULL_SRA;
                  ODMA := NULL_DMA;
                  --ODM := NULL_DICTIONARY_MNPC_RECORD;
                  --DM := NULL_DICTIONARY_MNPC_RECORD;
                  while PA (I).IR.QUAL.POFS = PACK and I <= PA_LAST loop
                     if PA (I).MNPC /= ODM.MNPC
                     then   --  Encountering new MNPC
                        OSRA := SRA;
                        K    :=
                          1;                  --  K indexes within the MNPCA array --  Initialize
                        J :=
                          J +
                          1;             --  J indexes the number of MNPCA arrays - Next MNPCA
                        SRAA (J) (K) := (PA (I).STEM, PA (I).IR);
                        DICT_IO.Set_Index
                          (DICT_FILE (PA (I).D_K), PA (I).MNPC);
                        DICT_IO.Read (DICT_FILE (PA (I).D_K), DEA);
                        DM      := (PA (I).D_K, PA (I).MNPC, DEA);
                        DMA (J) := DM;
                        ODM     := DM;
                     else
                        K :=
                          K +
                          1;              --  K indexes within the MNPCA array  - Next MNPC
                        SRAA (J) (K) := (PA (I).STEM, PA (I).IR);
                     end if;

                     I := I + 1;              --  I cycles over full PA array
                  end loop;

               when ADJ =>
                  OSRA := NULL_SRA;
                  ODMA := NULL_DMA;
                  --ODM := NULL_DICTIONARY_MNPC_RECORD;
                  --DM := NULL_DICTIONARY_MNPC_RECORD;
                  while PA (I).IR.QUAL.POFS = ADJ and I <= PA_LAST loop
--TEXT_IO.PUT_LINE("SRAA - ADJ");
                     if PA (I).MNPC /= ODM.MNPC
                     then   --  Encountering new MNPC
                        OSRA := SRA;
                        K    :=
                          1;                  --  K indexes within the MNPCA array --  Initialize
                        J :=
                          J +
                          1;             --  J indexes the number of MNPCA arrays - Next MNPCA
                        SRAA (J) (K) := (PA (I).STEM, PA (I).IR);
                        DICT_IO.Set_Index
                          (DICT_FILE (PA (I).D_K), PA (I).MNPC);
                        DICT_IO.Read (DICT_FILE (PA (I).D_K), DEA);
                        DM      := (PA (I).D_K, PA (I).MNPC, DEA);
                        DMA (J) := DM;
                        ODM     := DM;
                     else
                        K :=
                          K +
                          1;              --  K indexes within the MNPCA array  - Next MNPC
                        SRAA (J) (K) := (PA (I).STEM, PA (I).IR);
                     end if;
--TEXT_IO.PUT_LINE("SRAA  + ADJ");
                     I := I + 1;              --  I cycles over full PA array
                  end loop;

               when NUM =>
                  OSRA := NULL_SRA;
                  ODMA := NULL_DMA;
                  --ODM := NULL_DICTIONARY_MNPC_RECORD;
                  --DM := NULL_DICTIONARY_MNPC_RECORD;
                  while PA (I).IR.QUAL.POFS = NUM and I <= PA_LAST loop
                     if (PA (I).D_K = RRR) then        --  Roman numeral
                        OSRA := SRA;
                        K    :=
                          1;                  --  K indexes within the MNPCA array --  Initialize
                        J :=
                          J +
                          1;             --  J indexes the number of MNPCA arrays - Next MNPCA
                        SRAA (J) (K) := (PA (I).STEM, PA (I).IR);
                        --DICT_IO.SET_INDEX(DICT_FILE(PA(I).D_K), PA(I).MNPC);
                        --DICT_IO.READ(DICT_FILE(PA(I).D_K), DEA);

                        DEA     := NULL_DICTIONARY_ENTRY;
                        DM      := (PA (I).D_K, PA (I).MNPC, DEA);
                        DMA (J) := DM;
                        ODM     := DM;
                     elsif (PA (I).MNPC /= ODM.MNPC)
                     then    --  Encountering new MNPC
                        OSRA := SRA;
                        K    :=
                          1;                  --  K indexes within the MNPCA array --  Initialize
                        J :=
                          J +
                          1;             --  J indexes the number of MNPCA arrays - Next MNPCA
                        SRAA (J) (K) := (PA (I).STEM, PA (I).IR);
                        DICT_IO.Set_Index
                          (DICT_FILE (PA (I).D_K), PA (I).MNPC);
                        DICT_IO.Read (DICT_FILE (PA (I).D_K), DEA);
                        DM      := (PA (I).D_K, PA (I).MNPC, DEA);
                        DMA (J) := DM;
                        ODM     := DM;
                     else
                        K :=
                          K +
                          1;              --  K indexes within the MNPCA array  - Next MNPC
                        SRAA (J) (K) := (PA (I).STEM, PA (I).IR);
                     end if;

                     I := I + 1;              --  I cycles over full PA array
                  end loop;

               when V | VPAR | SUPINE =>
                  OSRA := NULL_SRA;
                  ODMA := NULL_DMA;
                  --ODM := NULL_DICTIONARY_MNPC_RECORD;
                  --DM := NULL_DICTIONARY_MNPC_RECORD;
                  while
                    (PA (I).IR.QUAL.POFS = V or PA (I).IR.QUAL.POFS = VPAR or
                     PA (I).IR.QUAL.POFS = SUPINE) and
                    I <= PA_LAST
                  loop
--TEXT_IO.PUT_LINE("Starting loop for VPAR I = " & INTEGER'IMAGE(I) & "   K = " & INTEGER'IMAGE(K));
                     if (PA (I).MNPC /= ODM.MNPC) and (PA (I).D_K /= PPP)
                     then   --  Encountering new MNPC
                        OSRA :=
                          SRA;                                               --  But not for compound
                        K :=
                          1;                  --  K indexes within the MNPCA array --  Initialize
--TEXT_IO.PUT_LINE("Starting IRA for VPAR I = " & INTEGER'IMAGE(I) & "   K = " & INTEGER'IMAGE(K));
                        J :=
                          J +
                          1;             --  J indexes the number of MNPCA arrays - Next MNPCA
--TEXT_IO.PUT_LINE("Shifting J for VPAR I = " & INTEGER'IMAGE(I) & "   J = " & INTEGER'IMAGE(J));
                        SRAA (J) (K) := (PA (I).STEM, PA (I).IR);
                        if PA (I).D_K /= PPP then
                           DICT_IO.Set_Index
                             (DICT_FILE (PA (I).D_K), PA (I).MNPC);
                           DICT_IO.Read (DICT_FILE (PA (I).D_K), DEA);
                        end if;     --  use previous DEA
                        DM      := (PA (I).D_K, PA (I).MNPC, DEA);
                        DMA (J) := DM;
                        ODM     := DM;
                     else
                        K :=
                          K +
                          1;              --  K indexes within the MNPCA array  - Next MNPC
                        --TEXT_IO.PUT_LINE("Continuing IRA for VPAR  I = " & INTEGER'IMAGE(I) & "   K = " & INTEGER'IMAGE(K)
                        --                                                                      & "   J = " & INTEGER'IMAGE(J));
                        SRAA (J) (K) := (PA (I).STEM, PA (I).IR);
                     end if;

                     I := I + 1;              --  I cycles over full PA array
                  end loop;

               when others =>
--TEXT_IO.PUT_LINE("Others");
                  OSRA := NULL_SRA;
                  ODMA := NULL_DMA;
                  --ODM := NULL_DICTIONARY_MNPC_RECORD;
                  --DM := NULL_DICTIONARY_MNPC_RECORD;
                  while I <= PA_LAST loop
                     --TEXT_IO.PUT_LINE("Starting loop for OTHER I = " & INTEGER'IMAGE(I) & "   K = " & INTEGER'IMAGE(K));
                     if (ODM.D_K /= PA (I).D_K) or (ODM.MNPC /= PA (I).MNPC)
                     then   --  Encountering new single (K only 1)
                        OSRA := SRA;
                        K    :=
                          1;                  --  K indexes within the MNPCA array --  Initialize
                        --TEXT_IO.PUT_LINE("Starting IRA for OTHER I = " & INTEGER'IMAGE(I) & "   K = " & INTEGER'IMAGE(K));
                        J :=
                          J +
                          1;             --  J indexes the number of MNPCA arrays - Next MNPCA
                        --TEXT_IO.PUT_LINE("Shifting J for OTHER I = " & INTEGER'IMAGE(I) & "   J = " & INTEGER'IMAGE(J));
                        SRAA (J) (K) := (PA (I).STEM, PA (I).IR);
                        if PA (I).MNPC /= NULL_MNPC then
                           if PA (I).D_K = ADDONS then
                              DEA :=
                                NULL_DICTIONARY_ENTRY;   --  Fix for ADDONS in MEANS, not DICT_IO
                           else
                              DICT_IO.Set_Index
                                (DICT_FILE (PA (I).D_K), PA (I).MNPC);
                              DICT_IO.Read (DICT_FILE (PA (I).D_K), DEA);
                           end if;
                        else                       --  Has no dictionary to read
                           DEA := NULL_DICTIONARY_ENTRY;
                        end if;
                        DM      := (PA (I).D_K, PA (I).MNPC, DEA);
                        DMA (J) := DM;
                        ODM     := DM;
                        --else
--  K := K + 1;              --  K indexes within the MNPCA array  - Next MNPC
--  SRAA(J)(K) := (PA(I).STEM, PA(I).IR);
                     end if;

                     I := I + 1;              --  I cycles over full PA array
                     exit;                    --  Since Other is only one, don't loop
                  end loop;

            end case;

         end if;
--  --  This just for developer test, will be commented out
--  if K > SRA_MAX  then
--    SRA_MAX := K;
--PUT_STAT("*SRA_MAX for RAW_WORD " & HEAD(RAW_WORD, 26) & "   = " & INTEGER'IMAGE(SRA_MAX));
--  end if;
--  if J > DMA_MAX  then
--    DMA_MAX := J;
--PUT_STAT("*DMA_MAX for RAW_WORD " & HEAD(RAW_WORD, 26) & "   = " & INTEGER'IMAGE(DMA_MAX));
--  end if;

      end loop CYCLE_OVER_PA;

--TEXT_IO.PUT_LINE("Made QA");

      --  Sets + if capitalized
   --  Strangely enough, it may enter LIST_STEMS with PA_LAST /= 0
   --  but be weeded and end up with no parse after LIST_SWEEP  -  PA_LAST = 0
      if PA_LAST = 0 then  --  WORD failed
   --????      (DMA(1).D_K in ADDONS..YYY  and then TRIM(DMA(1).DE.STEMS(1)) /= "que")  then  --  or used FIXES/TRICKS
         if WORDS_MODE (IGNORE_UNKNOWN_NAMES) and CAPITALIZED then
            NNN_MEANING :=
              HEAD
                ("Assume this is capitalized proper name/abbr, under MODE IGNORE_UNKNOWN_NAME ",
                 MAX_MEANING_SIZE);
            PA (1) :=
              (HEAD (RAW_WORD, MAX_STEM_SIZE),
               ((N, ((0, 0), X, X, X)), 0, NULL_ENDING_RECORD, X, X), NNN,
               NULL_MNPC);
            PA_LAST      := 1;    --  So LIST_NEIGHBORHOOD will not be called
            SRAA         := NULL_SRAA;
            DMA          := NULL_DMA;
            SRAA (1) (1) := (PA (1).STEM, PA (1).IR);
            DMA (1)      := (NNN, 0, NULL_DICTIONARY_ENTRY);
         elsif WORDS_MODE (IGNORE_UNKNOWN_CAPS) and ALL_CAPS then
            NNN_MEANING :=
              HEAD
                ("Assume this is capitalized proper name/abbr, under MODE IGNORE_UNKNOWN_CAPS ",
                 MAX_MEANING_SIZE);
            PA (1) :=
              (HEAD (RAW_WORD, MAX_STEM_SIZE),
               ((N, ((0, 0), X, X, X)), 0, NULL_ENDING_RECORD, X, X), NNN,
               NULL_MNPC);
            PA_LAST      := 1;
            SRAA         := NULL_SRAA;
            DMA          := NULL_DMA;
            SRAA (1) (1) := (PA (1).STEM, PA (1).IR);
            DMA (1)      := (NNN, 0, NULL_DICTIONARY_ENTRY);
         end if;

      end if;

--                --  Does STATS
--
----TEXT_IO.PUT_LINE("Before STATING FIXES");
--     if  WORDS_MDEV(WRITE_STATISTICS_FILE)    then      --  Omit rest of output
----
----       for I in 1..PA_LAST  loop                       --  Just to PUT_STAT
----         if (PA(I).D_K = ADDONS)  then
----           if PA(I).IR.QUAL.POFS = PREFIX  then
----             PUT_STAT("ADDON PREFIX at "
----                     & HEAD(INTEGER'IMAGE(LINE_NUMBER), 8) & HEAD(INTEGER'IMAGE(WORD_NUMBER), 4)
----                     & "   " & HEAD(W, 20) & "   "  & PA(I).STEM);
----           elsif PA(I).IR.QUAL.POFS = SUFFIX  then
----             PUT_STAT("ADDON SUFFIX at "
----                     & HEAD(INTEGER'IMAGE(LINE_NUMBER), 8) & HEAD(INTEGER'IMAGE(WORD_NUMBER), 4)
----                     & "   " & HEAD(W, 20) & "   "  & PA(I).STEM);
----           elsif PA(I).IR.QUAL.POFS = TACKON  then
----             PUT_STAT("ADDON TACKON at "
----                     & HEAD(INTEGER'IMAGE(LINE_NUMBER), 8) & HEAD(INTEGER'IMAGE(WORD_NUMBER), 4)
----                     & "   " & HEAD(W, 20) & "   "  & PA(I).STEM);
----           end if;
----         end if;
----       end loop;
--
--
----    --  Just to find the words with long/complicated output at the LIST level
----    --  This is done with the final PA_LAST, after SWEEP
--       if PA_LAST > FINAL_PA_LAST_MAX   then
--         PUT_STAT("$FINAL_PA_LAST_MAX    for RAW_WORD " & HEAD(RAW_WORD, 24) & "   = " & INTEGER'IMAGE(PA_LAST));
--         FINAL_PA_LAST_MAX := PA_LAST;
--       end if;
--
--     end if;

      if PA_LAST = 0 then

         if WORDS_MODE (WRITE_OUTPUT_TO_FILE) then
            if WORDS_MDEV (DO_PEARSE_CODES) then
               Text_IO.Put (OUTPUT, "04 ");
            end if;
            Text_IO.Put (OUTPUT, RAW_WORD);
            Text_IO.Set_Col (OUTPUT, 30);
            INFLECTIONS_PACKAGE.INTEGER_IO.Put (OUTPUT, LINE_NUMBER, 7);
            INFLECTIONS_PACKAGE.INTEGER_IO.Put (OUTPUT, WORD_NUMBER, 7);
            Text_IO.Put_Line (OUTPUT, "    ========   UNKNOWN    ");
            --TEXT_IO.NEW_LINE(OUTPUT);
         else              --  Just screen output
           -- TEXT_IO.NEW_LINE(OUTPUT);
            if WORDS_MDEV (DO_PEARSE_CODES) then
               Text_IO.Put (OUTPUT, "04 ");
            end if;
            Text_IO.Put (RAW_WORD);
            Text_IO.Set_Col (OUTPUT, 30);
            Text_IO.Put_Line (OUTPUT, "    ========   UNKNOWN    ");
            --TEXT_IO.NEW_LINE;
         end if;

         if WORDS_MODE (WRITE_UNKNOWNS_TO_FILE) then
            if WORDS_MDEV (INCLUDE_UNKNOWN_CONTEXT) or
              WORDS_MDEV (DO_ONLY_INITIAL_WORD)
            then
               Text_IO.Put_Line (INPUT_LINE);
               Text_IO.Put_Line (UNKNOWNS, INPUT_LINE);
            end if;
            if WORDS_MDEV (DO_PEARSE_CODES) then
               Text_IO.Put (UNKNOWNS, "04 ");
            end if;
            Text_IO.Put (UNKNOWNS, RAW_WORD);
            Text_IO.Set_Col (UNKNOWNS, 30);
            INFLECTIONS_PACKAGE.INTEGER_IO.Put (UNKNOWNS, LINE_NUMBER, 7);
            INFLECTIONS_PACKAGE.INTEGER_IO.Put (UNKNOWNS, WORD_NUMBER, 7);
            Text_IO.Put_Line (UNKNOWNS, "    ========   UNKNOWN    ");
         end if;
      end if;

      if PA_LAST = 0 then

         if WORDS_MODE (DO_STEMS_FOR_UNKNOWN) then
            if WORDS_MODE (WRITE_OUTPUT_TO_FILE)
              and then not WORDS_MODE (WRITE_UNKNOWNS_TO_FILE)
            then
               LIST_NEIGHBORHOOD (OUTPUT, RAW_WORD);
            elsif WORDS_MODE (WRITE_OUTPUT_TO_FILE)
              and then WORDS_MODE (WRITE_UNKNOWNS_TO_FILE)
            then
               LIST_NEIGHBORHOOD (OUTPUT, RAW_WORD);
               LIST_NEIGHBORHOOD (UNKNOWNS, RAW_WORD);
            elsif (Name (Current_Input) = Name (Standard_Input)) then

               LIST_NEIGHBORHOOD (OUTPUT, RAW_WORD);
            end if;
         end if;
      end if;
--
      if PA_LAST = 0 then
         if WORDS_MDEV
             (UPDATE_LOCAL_DICTIONARY) and  -- Don't if reading from file
           (Name (Current_Input) = Name (Standard_Input))
         then
            UPDATE_LOCAL_DICTIONARY_FILE;
            WORD
              (RAW_WORD, PA,
               PA_LAST);       --  Circular if you dont update!!!!!
         end if;
      end if;

      --  Exit if UNKNOWNS ONLY (but had to do STATS above)
      if WORDS_MODE (DO_UNKNOWNS_ONLY) then      --  Omit rest of output
         return;
      end if;

--TEXT_IO.PUT_LINE("PUTting INFLECTIONS");
      J    := 1;
      OSRA := NULL_SRA;
      OUTPUT_LOOP :
      while DMA (J) /= NULL_DICTIONARY_MNPC_RECORD loop
----!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
--            if (J > 1)  and then ((DMA(J-1).D_K = PPP)  or                               --!!!!!!!!!!!!!!!!!!!!!!!!
--               (DICTIONARY_FORM(DMA(J).DE) = DICTIONARY_FORM(DMA(J-1).DE)))  then        --!!!!!!!!!!!!!!!!!!!!!!!!
--             null;                                                                       --!!!!!!ND mod!!!!!!!!!!!!
--            else                                                                         --!!!!!!!!!!!!!!!!!!!!!!!!
--              NEW_LINE(OUTPUT);                                                          --!!!!!!!!!!!!!!!!!!!!!!!!
--            end if;                                                                      --!!!!!!!!!!!!!!!!!!!!!!!!
--                                                                                         --!!!!!!!!!!!!!!!!!!!!!!!!
-- --!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         if SRAA (J) /= OSRA then --  Skips one identical SRA
            --  no matter what comes next

            PUT_INFLECTION_ARRAY_J :
            for K in SRAA (J)'RANGE loop
               exit when SRAA (J) (K) = NULL_STEM_INFLECTION_RECORD;
               PUT_INFLECTION (SRAA (J) (K), DMA (J));
               if SRAA (J) (K).STEM (1 .. 3) = "PPL" then
                  Text_IO.Put_Line (OUTPUT, HEAD (PPP_MEANING, MM));
               end if;
            end loop PUT_INFLECTION_ARRAY_J;
            OSRA := SRAA (J);
         end if;

         --Text_Io.Put_Line("Setting next/last MEAN and F booleans");

         Last_Meaning_Same := False;
         Last_Form_Same    := False;
         Next_Form_Same    := False;

         if Last_Form_Same and then Next_Form_Same
           and then DMA (J).DE.STEMS /= DMA (J + 1).DE.STEMS
         then
            Put_Form_Anyway := True;
         end if;

         if J > 1 then
            if DMA (J).DE.MEAN = DMA (J - 1).DE.MEAN then
               Last_Meaning_Same := True;
            end if;

            if DMA (J).DE.PART.POFS = DMA (J - 1).DE.PART.POFS
              and then
               --  For same reason checking .TRAN will always return false
               --  but checking individual elements works
               DMA (J).DE.TRAN.AGE = DMA (J - 1).DE.TRAN.AGE
              and then DMA (J).DE.TRAN.AREA = DMA (J - 1).DE.TRAN.AREA
              and then DMA (J).DE.TRAN.GEO = DMA (J - 1).DE.TRAN.GEO
              and then DMA (J).DE.TRAN.FREQ = DMA (J - 1).DE.TRAN.FREQ
              and then DMA (J).DE.TRAN.SOURCE = DMA (J - 1).DE.TRAN.SOURCE
            then
               Last_Form_Same := True;
            end if;

         end if;

         if DMA (J).DE.PART.POFS = DMA (J + 1).DE.PART.POFS
           and then DMA (J).DE.TRAN.AGE = DMA (J + 1).DE.TRAN.AGE
           and then DMA (J).DE.TRAN.AREA = DMA (J + 1).DE.TRAN.AREA
           and then DMA (J).DE.TRAN.GEO = DMA (J + 1).DE.TRAN.GEO
           and then DMA (J).DE.TRAN.FREQ = DMA (J + 1).DE.TRAN.FREQ
           and then DMA (J).DE.TRAN.SOURCE = DMA (J + 1).DE.TRAN.SOURCE
         then
            Next_Form_Same := True;
         end if;

         Put_Meaning_Anyway := False;

-- TEXT_IO.PUT_LINE("PUTting FORM");
         PUTTING_FORM :
         begin

--DEBUG
--       Text_IO.Put_Line("                               LAST FORM SAME: " & Last_Form_Same'Image);
--       Text_IO.Put_Line("                               NEXT FORM SAME: " & Next_Form_Same'Image);
--       Text_IO.Put_Line("                            LAST MEANING SAME: " & Last_MEANING_Same'Image);
--       Text_IO.Put_Line("                              PUT FORM ANYWAY: " & Put_Form_Anyway'Image);
--       Text_IO.Put_Line("                           PUT MEANING ANYWAY: " & Put_Meaning_Anyway'Image);
--DEBUG

            if Put_Form_Anyway then
               PUT_FORM (SRAA (J) (1), DMA (J));
               Put_Meaning_Anyway := True;

            elsif Last_Form_Same and then DMA (J).D_K = GENERAL
              and then SRAA (J - 1) /= SRAA (J)
            then
               PUT_FORM (SRAA (J) (1), DMA (J));
               Put_Meaning_Anyway :=
                 True;            -- Make sure dictionary line always prints after inflections
               -- rare, occurs with "QUAE"

            elsif DMA (J).D_K = UNIQUE
            then            -- Always print uniques,otherwise we get orphaned inflections
               PUT_FORM
                 (SRAA (J) (1),
                  DMA
                    (J));          -- when there's more than one unique entry for the same form
               Put_Meaning_Anyway := True;            -- e.g., quippiam, bobus

            elsif Last_Form_Same then
               null;

            else
               PUT_FORM (SRAA (J) (1), DMA (J));
            end if;

         end PUTTING_FORM;

         -- TEXT_IO.PUT_LINE("PUTting MEANING");
         PUTTING_MEANING :
         begin

            if Put_Meaning_Anyway then
               PUT_MEANING_LINE (SRAA (J) (1), DMA (J));
               Put_Meaning_Anyway := False;
            elsif Last_Meaning_Same then
               null;
            else
               PUT_MEANING_LINE (SRAA (J) (1), DMA (J));
            end if;

         end PUTTING_MEANING;

         DO_PAUSE :
         begin
            if I = PA_LAST then
               Text_IO.New_Line (OUTPUT);
            elsif
              (Integer (Text_IO.Line (OUTPUT)) >
               SCROLL_LINE_NUMBER + OUTPUT_SCREEN_SIZE)
            then
               PAUSE (OUTPUT);
               SCROLL_LINE_NUMBER := Integer (Text_IO.Line (OUTPUT));
            end if;
         end DO_PAUSE;
         --TEXT_IO.PUT_LINE("End of OUTPUT_LOOP with J = " & INTEGER'IMAGE(J));

         J := J + 1;
      end loop OUTPUT_LOOP;
      --TEXT_IO.PUT_LINE("Finished OUTPUT_LOOP");

               if TRIMMED  then
                        if WORDS_MODE (DO_ANSI_FORMATTING) then
               Text_IO.Put (OUTPUT, Format_Reset);
               Text_IO.Put (OUTPUT, Format_Inverse);

            end if;

         Text_Io.New_Line(Output);

            Text_Io.PUT_LINE(OUTPUT, "OUTPUT TRIMMED:  Turn off TRIM_OUTPUT to see more.");

             if WORDS_MODE (DO_ANSI_FORMATTING) then
               Text_IO.Put (OUTPUT, Format_Reset);
            end if;

       end if;

      Text_IO.New_Line (OUTPUT);

   exception
      when others =>
         Text_IO.Put_Line
           ("Unexpected exception in LIST_STEMS processing " & RAW_WORD);
         PUT_STAT
           ("EXCEPTION LS at " & HEAD (Integer'IMAGE (LINE_NUMBER), 8) &
            HEAD (Integer'IMAGE (WORD_NUMBER), 4) & "   " & HEAD (W, 20) &
            "   " & PA (I).STEM);
   end LIST_STEMS;

   procedure LIST_ENTRY
     (OUTPUT : Text_IO.File_Type; D_K : DICTIONARY_KIND; MN : DICT_IO.Count)
   is
      DE : DICTIONARY_ENTRY;
   begin
      DICT_IO.Read (DICT_FILE (D_K), DE, MN);
      Text_IO.Put (OUTPUT, "=>  ");
      --TEXT_IO.PUT_LINE(OUTPUT, DICTIONARY_FORM(DE));
      PUT_DICTIONARY_FORM (OUTPUT, D_K, MN, DE);
      if WORDS_MODE (DO_ANSI_FORMATTING) then
         Text_IO.Put (Format_Bold);
      end if;
      Text_IO.Put_Line
        (OUTPUT, TRIM_BAR(TRIM (HEAD (DE.MEAN, MM))));  --  so it wont line wrap/put CR
      if WORDS_MODE (DO_ANSI_FORMATTING) then
         Text_IO.Put (Format_Reset);
      end if;
   end LIST_ENTRY;

   procedure UNKNOWN_SEARCH
     (UNKNOWN : in String; UNKNOWN_COUNT : out DICT_IO.Count)
   is

      use STEM_IO;

      D_K           : constant DICTIONARY_KIND := GENERAL;
      J, J1, J2, JJ : STEM_IO.Count            := 0;

      INDEX_ON : constant String :=
        LOWER_CASE
          (UNKNOWN); -- SPR:  Fixes bug that prevented unknown_search from working if ignore_unknown's were set to N, do_unknowns_only set to Y, and input had a capital letter
      INDEX_FIRST, INDEX_LAST : STEM_IO.Count := 0;
      DS                      : DICTIONARY_STEM;
      FIRST_TRY, SECOND_TRY   : Boolean       := True;

      function FIRST_TWO (W : String) return String is
      --  'v' could be represented by 'u', like the new Oxford Latin Dictionary
      --  Fixes the first two letters of a word/stem which can be done right
         S  : constant String  := LOWER_CASE (W);
         SS : String (W'RANGE) := W;

         function UI (C : Character) return Character is
         begin
            if (C = 'v') then
               return 'u';
            elsif (C = 'V') then
               return 'U';
            elsif (C = 'j') then
               return 'i';
            elsif (C = 'J') then
               return 'I';
            else
               return C;
            end if;
         end UI;

      begin

         if S'LENGTH = 1 then
            SS (S'FIRST) := UI (W (S'FIRST));
         else
            SS (S'FIRST)     := UI (W (S'FIRST));
            SS (S'FIRST + 1) := UI (W (S'FIRST + 1));
         end if;

         return SS;
      end FIRST_TWO;

   begin

      if DICTIONARY_AVAILABLE (D_K) then
         if not Is_Open (STEM_FILE (D_K)) then
            Open
              (STEM_FILE (D_K), STEM_IO.In_File,
               ADD_FILE_NAME_EXTENSION
                 (STEM_FILE_NAME, DICTIONARY_KIND'IMAGE (D_K)));
         end if;

         INDEX_FIRST := FIRST_INDEX (FIRST_TWO (INDEX_ON), D_K);
         INDEX_LAST  := LAST_INDEX (FIRST_TWO (INDEX_ON), D_K);

         if INDEX_FIRST > 0 and then INDEX_FIRST <= INDEX_LAST then

            J1 := STEM_IO.Count (INDEX_FIRST);    --######################
            J2 := STEM_IO.Count (INDEX_LAST);

            FIRST_TRY := True;

            SECOND_TRY := True;

            J := (J1 + J2) / 2;

            BINARY_SEARCH :
            loop

               if (J1 = J2 - 1) or (J1 = J2) then
                  if FIRST_TRY then
                     J         := J1;
                     FIRST_TRY := False;
                  elsif SECOND_TRY then
                     J          := J2;
                     SECOND_TRY := False;
                  else
                     JJ := J;
                     exit BINARY_SEARCH;
                  end if;
               end if;

               Set_Index (STEM_FILE (D_K), STEM_IO.Count (J));
               Read (STEM_FILE (D_K), DS);

               if LTU (LOWER_CASE (DS.STEM), UNKNOWN) then
                  J1 := J;
                  J  := (J1 + J2) / 2;
               elsif GTU (LOWER_CASE (DS.STEM), UNKNOWN) then
                  J2 := J;
                  J  := (J1 + J2) / 2;
               else
                  for I in reverse J1 .. J loop
                     Set_Index (STEM_FILE (D_K), STEM_IO.Count (I));
                     Read (STEM_FILE (D_K), DS);

                     if EQU (LOWER_CASE (DS.STEM), UNKNOWN) then
                        JJ := I;

                     else
                        exit;
                     end if;
                  end loop;

                  for I in J + 1 .. J2 loop
                     Set_Index (STEM_FILE (D_K), STEM_IO.Count (I));
                     Read (STEM_FILE (D_K), DS);

                     if EQU (LOWER_CASE (DS.STEM), UNKNOWN) then
                        JJ := I;

                     else
                        exit BINARY_SEARCH;
                     end if;
                  end loop;
                  exit BINARY_SEARCH;

               end if;
            end loop BINARY_SEARCH;
            J1 := JJ;
            J2 := STEM_IO.Count (INDEX_LAST);

         end if;
         UNKNOWN_COUNT := DS.MNPC;

         Close (STEM_FILE (D_K));  --??????
      end if;
      -- TEXT_IO.PUT_LINE("Leaving LIST_NEIGHBORHOOD    UNKNOWN_SEARCH");
   end UNKNOWN_SEARCH;

   procedure LIST_NEIGHBORHOOD
     (OUTPUT : Text_IO.File_Type; INPUT_WORD : String)
   is

      D_K      : constant DICTIONARY_KIND := GENERAL;
      DE       : DICTIONARY_ENTRY;
      UNK_MNPC : DICT_IO.Count;

   begin
--  TEXT_IO.PUT_LINE("Entering LIST_NEIGHBORHOOD");

      if (Text_IO.Name (OUTPUT) = Text_IO.Name (Text_IO.Standard_Output)) then
         MM :=
           MAX_MEANING_PRINT_SIZE;   --  to keep from overflowing screen line
      else
         MM := MAX_MEANING_SIZE;
      end if;

      UNKNOWN_SEARCH (HEAD (INPUT_WORD, MAX_STEM_SIZE), UNK_MNPC);

-- TEXT_IO.PUT_LINE("UNK_MNPC = " & INTEGER'IMAGE(INTEGER(UNK_MNPC)));
      if Integer (UNK_MNPC) > 0 then

         if WORDS_MODE (DO_ANSI_FORMATTING) then
            Text_IO.Put (Format_Reset);
            Text_IO.Put (Format_Inverse);
         end if;
         Text_Io.New_Line(OUTPUT);
         Text_IO.Put_Line
           (OUTPUT,
            "----------  Entries in GENERAL Dictionary around the UNKNOWN  ----------");

         if WORDS_MODE (DO_ANSI_FORMATTING) then
            Text_IO.Put (Format_Reset);
         end if;
         PAUSE (OUTPUT);
         for MN in
           DICT_IO.Count (Integer (UNK_MNPC) - 5) ..
             DICT_IO.Count (Integer (UNK_MNPC) + 3)
         loop

            LIST_ENTRY (OUTPUT, D_K, MN);

         end loop;
      end if;

-- TEXT_IO.PUT_LINE("Leaving LIST_NEIGHBORHOOD");

   end LIST_NEIGHBORHOOD;

end LIST_PACKAGE;
