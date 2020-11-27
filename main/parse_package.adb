with Text_IO;                 use Text_IO;
with STRINGS_PACKAGE;         use STRINGS_PACKAGE;
with WORD_PARAMETERS;         use WORD_PARAMETERS;
with DEVELOPER_PARAMETERS;    use DEVELOPER_PARAMETERS;
with INFLECTIONS_PACKAGE;     use INFLECTIONS_PACKAGE;
with DICTIONARY_PACKAGE;      use DICTIONARY_PACKAGE;
with ADDONS_PACKAGE;          use ADDONS_PACKAGE;
with WORD_SUPPORT_PACKAGE;    use WORD_SUPPORT_PACKAGE;
with PREFACE;
with WORD_PACKAGE;            use WORD_PACKAGE;
with LIST_PACKAGE;            use LIST_PACKAGE;
with TRICKS_PACKAGE;          use TRICKS_PACKAGE;
with CONFIG;                  use CONFIG;
with ENGLISH_SUPPORT_PACKAGE; use ENGLISH_SUPPORT_PACKAGE;
with SEARCH_ENGLISH;
with Arabic2Roman;
with Words_Help;              use Words_Help;

with Ada.Exceptions;

--pragma Elaborate (WORD_PARAMETERS);
with Ada.Wide_Text_IO;
with Ada.Characters.Conversions;


package body Parse_Package is 

procedure PARSE (COMMAND_LINE : String := "") is

   STORAGE_ERROR_COUNT : Integer := 0;

   J, K, L             : Integer := 0;
   
   LINE, BLANK_LINE : String (1 .. INPUT_LINE_LENGTH) := (others => ' ');

   PA : PARSE_ARRAY (1 .. 150) :=          (others => NULL_PARSE_RECORD);
      
   SYNCOPE_MAX                   : constant                       := 20;
   NO_SYNCOPE                    : Boolean                        := False;
   TRICKS_MAX                    : constant                       := 40;
   SYPA : PARSE_ARRAY (1 .. SYNCOPE_MAX) := (others => NULL_PARSE_RECORD);
   TRPA : PARSE_ARRAY (1 .. TRICKS_MAX)  := (others => NULL_PARSE_RECORD);
   PA_LAST, SYPA_LAST, TRPA_LAST : Integer                        := 0;
     
   procedure PARSE_LINE (INPUT_LINE : String) is
      L             : Integer            := TRIM (INPUT_LINE)'LAST;
      W             : String (1 .. L)    := (others => ' ');
      Arabic_String : String (1 .. L+1)  := (others => '|');
      Arabic_Present : Boolean           := False;                
      Arabic_J       : Integer           := 1;

   begin

      LINE (1 .. L) := TRIM (INPUT_LINE);

      ELIMINATE_NOT_LETTERS :
      begin
         for I in 1 .. L loop
            case LINE (I) is
               when 'A' .. 'Z' =>
                  Arabic_String (I) := 'z';
               when 'a' .. 'z' =>
                  Arabic_String (I) := 'z';
               when '-' =>
                  Arabic_String (I) := LINE (I);
               when '.' =>
                  null;
               when '0' .. '9' =>
                  Arabic_String (I) := LINE (I);
                  Arabic_Present    := True;
               when '_' =>
                  Arabic_String (I) := LINE (I);
                  LINE (I)          := ' ';
               when ',' =>
                  Arabic_String (I) := '_';
                  LINE (I)          := ' ';
               when others =>
                  LINE (I) := ' ';
            end case;
         end loop;
      end ELIMINATE_NOT_LETTERS;

      --  Skip over leading and intervening blanks, looking for comments
      --  Punctuation, numbers, and special characters were cleared above
      J := 1;
      K := 0;
      OVER_LINE :
      while J <= L loop
         for I in K + 1 .. L loop
            exit when LINE (J) in 'A' .. 'Z';
            exit when LINE (J) in 'a' .. 'z';
            if I < L and then LINE (I .. I + 1) = "--"
            then  --  the rest of the line is comment
               exit OVER_LINE;
            end if;
            J := I + 1;
         end loop;

      --------------------------BEGIN ROMAN NUMERALS--------------------------
         -- Intercept Arabic numerals here; a bit messy, but we can avoid
         -- changing the rest of the procedure
         if WORDS_MODE (DO_ARABIC_NUMERALS) and Arabic_Present then
          
            if arabic_J = 1 and then (Arabic_String(1) /= 'z')
              then
                 if words_mode(WRITE_OUTPUT_TO_FILE)   
                 then Text_IO.New_Line(Output); 
                 else 
                 Text_IO.New_Line(current_Output); 
                 end if; 
            end if;
            
            if WORDS_MODE (WRITE_OUTPUT_TO_FILE) then

                     Arabic2Roman.Arabic2Roman
                       (OUTPUT, Arabic_String((Arabic_J) .. J));
                  else
                     Arabic2Roman.Arabic2Roman
                       (Current_Output, Arabic_String ((Arabic_J) .. J));
                  end if;
 
            Arabic_J := (J);
         end if;
      ----------------------------END ROMAN NUMERALS--------------------------

         exit when J > L;              --  Kludge

         FOLLOWED_BY_PERIOD := False;
         CAPITALIZED        := False;
         ALL_CAPS           := False;

         --  Extract the word
         for I in J .. L loop

         --  Although I have removed punctuation above, it may not always be so
            if LINE (I) = '.' then
               FOLLOWED_BY_PERIOD := True;
               exit;
            end if;

            exit when
              ((LINE (I) not in 'A' .. 'Z') and (LINE (I) not in 'a' .. 'z'));
            W (I) := LINE (I);
            K     := I;

         end loop;

         -- Determine whether all caps (a couple user options change behavior
         -- when there's a cap);

         if W (J) in 'A' .. 'Z' and then K - J >= 1
           and then W (J + 1) in 'a' .. 'z'
         then
            CAPITALIZED := True;
         end if;

         ALL_CAPS := True;
         for I in J .. K loop
            if W (I) = LOWER_CASE (W (I)) then
               ALL_CAPS := False;
               exit;
            end if;
         end loop;

         for I in J .. K - 1 loop               --  Kludge for QVAE
            if W (I) = 'Q' and then W (I + 1) = 'V' then
               W (I + 1) := 'U';
            end if;
            if W (I) = 'q' and then W (I + 1) = 'v'
            then                    -- SPR:  added lower case condition because
               W (I + 1) :=         -- the above resulted in inconsistent output
                 'u';      
            end if;
         end loop;

         if LANGUAGE = ENGLISH_TO_LATIN then

            PARSE_LINE_ENGLISH_TO_LATIN :
--  Since we do only one English word per line
            declare
               INPUT_WORD : constant String     := W (J .. K);
               POFS       : PART_OF_SPEECH_TYPE := X;
            begin

--  Extract from the rest of the line Should do AUX here
--  !!!!!!!!!!!!!!!!!!!!!!!!
               EXTRACT_POFS :
               begin
                  PART_OF_SPEECH_TYPE_IO.Get (LINE (K + 1 .. L), POFS, L);
--TEXT_IO.PUT_LINE("In EXTRACT   " & LINE(K+1..L));
               exception
                  when others =>
                     POFS := X;
               end EXTRACT_POFS;
--PART_OF_SPEECH_TYPE_IO.PUT(POFS);
--TEXT_IO.NEW_LINE;

               SEARCH_ENGLISH (INPUT_WORD, POFS);

               exit OVER_LINE;

            end PARSE_LINE_ENGLISH_TO_LATIN;

         elsif LANGUAGE = LATIN_TO_ENGLISH then

    XXX_MEANING_COUNTER := 1;  YYY_MEANING_COUNTER := 1; NNN_MEANING_COUNTER := 1;  RRR_MEANING_COUNTER := 1; PPP_MEANING_COUNTER := 1;

            PARSE_WORD_LATIN_TO_ENGLISH :
            declare
               INPUT_WORD         : constant String := W (J .. K);
               ENTERING_PA_LAST   : Integer         := 0;
               ENTERING_TRPA_LAST : Integer         := 0;
               HAVE_DONE_ENCLITIC : Boolean         := False;

               procedure PASS (INPUT_WORD : String);


               
               procedure ENCLITIC is

                  SAVE_DO_ONLY_FIXES : Boolean := WORDS_MDEV (DO_ONLY_FIXES);
                  ENCLITIC_LIMIT     : Integer := 4;                       
                  TRY : constant String := LOWER_CASE (INPUT_WORD);
                  
               begin
                             
--TEXT_IO.PUT_LINE("Entering ENCLITIC  HAVE DONE = " & BOOLEAN'IMAGE(HAVE_DONE_ENCLITIC));
   --if WORDS_MODE(TRIM_OUTPUT)  and (PA_LAST > 0)  sathen    return;   end if;
                  if HAVE_DONE_ENCLITIC then
                     return;
                  end if;

                  ENTERING_PA_LAST := PA_LAST;
                  if PA_LAST > 0 then
                     ENCLITIC_LIMIT := 1;
                  end if;
                  LOOP_OVER_ENCLITIC_TACKONS :
                  for I in 1 .. ENCLITIC_LIMIT
                  loop   --  If have parse, only do que of que, ne, ve, (est)

                     REMOVE_A_TACKON :
                     declare
                        LESS : constant String :=
                          SUBTRACT_TACKON (TRY, TACKONS (I));
                        --SUBTRACT_TACKON(INPUT_WORD, TACKONS(I));
                        SAVE_PA_LAST : Integer := 0;
                     begin
--TEXT_IO.PUT_LINE("In ENCLITIC     LESS/TACKON  = " & LESS & "/" & TACKONS(I).TACK);
                        if LESS /= TRY then       --  LESS is less
                           --WORDS_MODE(DO_FIXES) := FALSE;
                           WORD_PACKAGE.WORD (LESS, PA, PA_LAST);
--TEXT_IO.PUT_LINE("In ENCLITICS after WORD NO_FIXES  LESS = " & LESS & "   PA_LAST = " & INTEGER'IMAGE(PA_LAST));

                           if PA_LAST = 0 then

                              SAVE_PA_LAST := PA_LAST;
                              TRY_SLURY
                                (LESS, PA, PA_LAST);
                              if SAVE_PA_LAST /= 0 then
                                 if (PA_LAST - 1) - SAVE_PA_LAST = SAVE_PA_LAST
                                 then
                                    PA_LAST := SAVE_PA_LAST;
                                 end if;
                              end if;

                           end if;

                           --  Do not SYNCOPE if there is a verb TO_BE or
                           --  compound already there I do this here and
                           --  below, it might be combined but it works now
                           for I in 1 .. PA_LAST loop
                              --PARSE_RECORD_IO.PUT(PA(I)); TEXT_IO.NEW_LINE;
                              if PA (I).IR.QUAL.POFS = V
                                and then PA (I).IR.QUAL.V.CON = (5, 1)
                              then
                                 NO_SYNCOPE := True;
                              end if;
                           end loop;

                           --TEXT_IO.PUT_LINE("In ENCLITICS after SLURY  LESS = " & LESS & "   PA_LAST = " & INTEGER'IMAGE(PA_LAST));
                           SYPA_LAST := 0;
                           if WORDS_MDEV (DO_SYNCOPE) and not NO_SYNCOPE then
                              SYNCOPE
                                (LESS, SYPA,
                                 SYPA_LAST);  --  Want SYNCOPE second to make cleaner LIST
--TEXT_IO.PUT_LINE("In ENCLITIC after SYNCOPE  LESS = " & LESS & "   SYPA_LAST = " & INTEGER'IMAGE(SYPA_LAST));
                              PA_LAST :=
                                PA_LAST +
                                SYPA_LAST;   --  Make syncope another array to avoid PA_LAST = 0 problems
                              PA (1 .. PA_LAST) :=
                                PA (1 .. PA_LAST - SYPA_LAST) &
                                SYPA
                                  (1 ..
                                       SYPA_LAST);  --  Add SYPA to PA
                              SYPA (1 .. SYNCOPE_MAX) :=
                                (1 .. SYNCOPE_MAX =>
                                   NULL_PARSE_RECORD);   --  Clean up so it does not repeat
                              SYPA_LAST := 0;
                           end if;
                           NO_SYNCOPE := False;
                           --  Restore FIXES
                           --WORDS_MODE(DO_FIXES) := SAVE_DO_FIXES;

                           WORDS_MDEV (DO_ONLY_FIXES) := True;
                           WORD (INPUT_WORD, PA, PA_LAST);
--TEXT_IO.PUT_LINE("In ENCLITICS after ONLY_FIXES  LESS = " & LESS & "   PA_LAST = " & INTEGER'IMAGE(PA_LAST));
                           WORDS_MDEV (DO_ONLY_FIXES) := SAVE_DO_ONLY_FIXES;

                           if PA_LAST > ENTERING_PA_LAST
                           then      --  have a possible word
                              PA_LAST := PA_LAST + 1;
                              PA (ENTERING_PA_LAST + 2 .. PA_LAST) :=
                                PA (ENTERING_PA_LAST + 1 .. PA_LAST - 1);
                              PA (ENTERING_PA_LAST + 1) :=
                                (TACKONS (I).TACK,
                                 ((TACKON, NULL_TACKON_RECORD), 0,
                                  NULL_ENDING_RECORD, X, X),
                                 ADDONS, DICT_IO.Count (TACKONS (I).MNPC));

                              HAVE_DONE_ENCLITIC := True;
                           end if;
                           exit LOOP_OVER_ENCLITIC_TACKONS;
                        end if;
                     end REMOVE_A_TACKON;
                  end loop LOOP_OVER_ENCLITIC_TACKONS;
               end ENCLITIC;

               procedure TRICKS_ENCLITIC is
                  TRY : constant String := LOWER_CASE (INPUT_WORD);
               begin
--TEXT_IO.PUT_LINE("Entering TRICKS_ENCLITIC    PA_LAST = " & INTEGER'IMAGE(PA_LAST));
   --if WORDS_MODE(TRIM_OUTPUT)  and (PA_LAST > 0)  then    return;   end if;
                  if HAVE_DONE_ENCLITIC then
                     return;
                  end if;

                  ENTERING_TRPA_LAST := TRPA_LAST;
                  LOOP_OVER_ENCLITIC_TACKONS :
                  for I in 1 .. 4 loop   --  que, ne, ve, (est)

                     REMOVE_A_TACKON :
                     declare
                        LESS : constant String :=
                        --SUBTRACT_TACKON(LOWER_CASE(INPUT_WORD), TACKONS(I));

                          SUBTRACT_TACKON (TRY, TACKONS (I));
                     begin
-- TEXT_IO.PUT_LINE("In TRICKS_ENCLITIC LESS/TACKON = " & LESS & "/" &
-- TACKONS(I).TACK);
                        if LESS /= TRY then       --  LESS is less
                           --PASS(LESS);
                           TRY_TRICKS
                             (LESS, TRPA, TRPA_LAST);
                           --TEXT_IO.PUT_LINE("In TRICKS_ENCLITICS after TRY_TRICKS  LESS = " & LESS & "   TRPA_LAST = " & INTEGER'IMAGE(TRPA_LAST));
                           if TRPA_LAST > ENTERING_TRPA_LAST
                           then      --  have a possible word
                              TRPA_LAST := TRPA_LAST + 1;
                              TRPA (ENTERING_TRPA_LAST + 2 .. TRPA_LAST) :=
                                TRPA (ENTERING_TRPA_LAST + 1 .. TRPA_LAST - 1);
                              TRPA (ENTERING_TRPA_LAST + 1) :=
                                (TACKONS (I).TACK,
                                 ((TACKON, NULL_TACKON_RECORD), 0,
                                  NULL_ENDING_RECORD, X, X),
                                 ADDONS, DICT_IO.Count (TACKONS (I).MNPC));
                           end if;
                           exit LOOP_OVER_ENCLITIC_TACKONS;
                        end if;
                     end REMOVE_A_TACKON;
                  end loop LOOP_OVER_ENCLITIC_TACKONS;
               end TRICKS_ENCLITIC;

               procedure PASS (INPUT_WORD : in String) is
--  This is the core logic of the program, everything else is details

                  SAVE_DO_FIXES      : Boolean := WORDS_MODE (DO_FIXES);
                  SAVE_DO_ONLY_FIXES : Boolean := WORDS_MDEV (DO_ONLY_FIXES);

               begin
-- TEXT_IO.PUT_LINE("Entering PASS with >" & INPUT_WORD);
                  --  Do straight WORDS without FIXES/TRICKS, is the word in
                  --  the dictionary
                  WORDS_MODE (DO_FIXES) :=
                    False;          --"saves" initial value of DO_TRICKs then sets no to ensure first pass does
                  ROMAN_NUMERALS
                    (INPUT_WORD, PA,
                     PA_LAST);  --  "restored" after calling tricks procedures
                  WORD (INPUT_WORD, PA, PA_LAST);

--TEXT_IO.PUT_LINE("SLURY-   PA_LAST = " & INTEGER'IMAGE(PA_LAST));
--for JK in 1..PA_LAST  loop
-- f PARSE_RECORD_IO.PUT(PA(JK)); TEXT_IO.NEW_LINE;
--end loop;

                  if PA_LAST = 0 then
                     TRY_SLURY
                       (INPUT_WORD, PA, PA_LAST);
                  end if;

                  --  Do not SYNCOPE if there is a verb TO_BE or compound
                  --  already there
                  for I in 1 .. PA_LAST loop
                     --PARSE_RECORD_IO.PUT(PA(I)); TEXT_IO.NEW_LINE;
                     if PA (I).IR.QUAL.POFS = V
                       and then PA (I).IR.QUAL.V.CON = (5, 1)
                     then
                        NO_SYNCOPE := True;
                     end if;
                  end loop;

----TEXT_IO.PUT_LINE("1  PASS_BLOCK for  " & INPUT_WORD & "   PA_LAST = " & INTEGER'IMAGE(PA_LAST));

                  --  Pure SYNCOPE
                  SYPA_LAST := 0;
                  if WORDS_MDEV (DO_SYNCOPE) and not NO_SYNCOPE then
                     SYNCOPE (INPUT_WORD, SYPA, SYPA_LAST);
                     PA_LAST :=
                       PA_LAST +
                       SYPA_LAST;   --  Make syncope another array to avoid PA-LAST = 0 problems
                     PA (1 .. PA_LAST) :=
                       PA (1 .. PA_LAST - SYPA_LAST) &
                       SYPA
                         (1 ..
                              SYPA_LAST);  --  Add SYPA to PA
                     SYPA (1 .. SYNCOPE_MAX) :=
                       (1 .. SYNCOPE_MAX =>
                          NULL_PARSE_RECORD);   --  Clean up so it does not repeat
                     SYPA_LAST := 0;
                  end if;
                  NO_SYNCOPE := False;

--TEXT_IO.PUT_LINE("2  PASS_BLOCK for  " & INPUT_WORD & "   PA_LAST = " & INTEGER'IMAGE(PA_LAST));

                  --  There may be a vaild simple parse, if so it is most
                  --  probable But I have to allow for the possibility that
                  --  -que is answer, not colloque V
                  ENCLITIC;

                  --  Restore FIXES
                  WORDS_MODE (DO_FIXES) := SAVE_DO_FIXES;
--TEXT_IO.PUT_LINE("3  PASS_BLOCK for  " & INPUT_WORD & "   PA_LAST = " & INTEGER'IMAGE(PA_LAST));

                  --  Now with only fixes
                  if PA_LAST = 0 and then WORDS_MODE (DO_FIXES) then
                     WORDS_MDEV (DO_ONLY_FIXES) := True;
                     --TEXT_IO.PUT_LINE("3a PASS_BLOCK for  " & INPUT_WORD & "   PA_LAST = " & INTEGER'IMAGE(PA_LAST));
                     WORD (INPUT_WORD, PA, PA_LAST);
                     --TEXT_IO.PUT_LINE("3b PASS_BLOCK for  " & INPUT_WORD & "   PA_LAST = " & INTEGER'IMAGE(PA_LAST));
                     SYPA_LAST := 0;
                     if WORDS_MDEV (DO_SYNCOPE) and not NO_SYNCOPE then
                        SYNCOPE (INPUT_WORD, SYPA, SYPA_LAST);
                        --TEXT_IO.PUT_LINE("3c PASS_BLOCK for  " & INPUT_WORD & "   PA_LAST = " & INTEGER'IMAGE(PA_LAST));
                        PA_LAST :=
                          PA_LAST +
                          SYPA_LAST;   --  Make syncope another array to avoid PA-LAST = 0 problems
                        PA (1 .. PA_LAST) :=
                          PA (1 .. PA_LAST - SYPA_LAST) &
                          SYPA
                            (1 ..
                                 SYPA_LAST);  --  Add SYPA to PA
                        SYPA (1 .. SYNCOPE_MAX) :=
                          (1 .. SYNCOPE_MAX =>
                             NULL_PARSE_RECORD);   --  Clean up so it does not repeat
                        SYPA_LAST := 0;
                     end if;
                     NO_SYNCOPE := False;

--TEXT_IO.PUT_LINE("4  PASS_BLOCK for  " & INPUT_WORD & "   PA_LAST = " & INTEGER'IMAGE(PA_LAST));
                     ENCLITIC;

--TEXT_IO.PUT_LINE("5  PASS_BLOCK for  " & INPUT_WORD & "   PA_LAST = " & INTEGER'IMAGE(PA_LAST));
                     WORDS_MDEV (DO_ONLY_FIXES) := SAVE_DO_ONLY_FIXES;
                  end if;
--TEXT_IO.PUT_LINE("6  PASS_BLOCK for  " & INPUT_WORD & "   PA_LAST = " & INTEGER'IMAGE(PA_LAST));

               end PASS;

            begin   --  PARSE
             --  XXX_MEANING := Null_Special_Meaning_Array;
             --  XXX_MEANING_COUNTER := 1;
               PASS_BLOCK :
               begin
                  PA_LAST     := 0;

                  PASS (INPUT_WORD);

               end PASS_BLOCK;

--TEXT_IO.PUT_LINE("After PASS_BLOCK for  " & INPUT_WORD & "   PA_LAST = " & INTEGER'IMAGE(PA_LAST));

               --if (PA_LAST = 0) or DO_TRICKS_ANYWAY  then    --  WORD failed, try to modify the word
               if (PA_LAST = 0)
                 and then not
                 (WORDS_MODE (IGNORE_UNKNOWN_NAMES) and CAPITALIZED)
               then
                  --  WORD failed, try to modify the word
--TEXT_IO.PUT_LINE("WORDS fail me");
                  if WORDS_MODE (DO_TRICKS) then
--TEXT_IO.PUT_LINE("DO_TRICKS      PA_LAST    TRPA_LAST  " & INTEGER'IMAGE(PA_LAST) & "   " & INTEGER'IMAGE(TRPA_LAST));
                     WORDS_MODE (DO_TRICKS) :=
                       False;  --  Turn it off so wont be circular
                     TRY_TRICKS
                       (INPUT_WORD, TRPA, TRPA_LAST);
--TEXT_IO.PUT_LINE("DONE_TRICKS    PA_LAST    TRPA_LAST  " & INTEGER'IMAGE(PA_LAST) & "   " & INTEGER'IMAGE(TRPA_LAST));
                     if TRPA_LAST = 0 then
                        TRICKS_ENCLITIC;
                     end if;
                     WORDS_MODE (DO_TRICKS) := True;   --  Turn it back on
                  end if;

                  PA_LAST :=
                    PA_LAST +
                    TRPA_LAST;   --  Make TRICKS another array to avoid PA-LAST = 0 problems
                  PA (1 .. PA_LAST) :=
                    PA (1 .. PA_LAST - TRPA_LAST) &
                    TRPA (1 .. TRPA_LAST);  --  Add SYPA to PA
                  TRPA (1 .. TRICKS_MAX) :=
                    (1 .. TRICKS_MAX =>
                       NULL_PARSE_RECORD);   --  Clean up so it does not repeat
                  TRPA_LAST := 0;

               end if;

               --TEXT_IO.PUT_LINE("After TRICKS " & INTEGER'IMAGE(PA_LAST));

--======================================================================

--  At this point we have done what we can with individual words Now see if
--  there is something we can do with word combinations For this we have to
--  look ahead

               if PA_LAST > 0
               then    --  But PA may be killed by ALLOW in LIST_STEMS
                  if WORDS_MODE (DO_COMPOUNDS) and
                    not (CONFIGURATION = ONLY_MEANINGS)
                  then
                     COMPOUNDS_WITH_SUM :
                     declare
                        NW : String (1 .. 2_500) := (others => ' ');
                        NK : Integer             := 0;

                        COMPOUND_TENSE : INFLECTIONS_PACKAGE.TENSE_TYPE := X;
                        COMPOUND_TVM   : INFLECTIONS_PACKAGE
                          .TENSE_VOICE_MOOD_RECORD;
                        PPL_ON : Boolean := False;

                        SUM_INFO : VERB_RECORD :=
                          ((5, 1), (X, ACTIVE, X), 0, X);

--  ESSE_INFO : VERB_RECORD := ((5, 1),
--                              (PRES, ACTIVE, INF),
--                               0,
--                               X);

                        PPL_INFO : VPAR_RECORD := ((0, 0), X, X, X, (X, X, X));

                        SUPINE_INFO : SUPINE_RECORD := ((0, 0), X, X, X);

                        procedure LOOK_AHEAD is
                           J : Integer := 0;
                        begin
                           for I in K + 2 .. L loop
                              --  Although I have removed punctuation above, it
                              --  may not always be so
                              exit when
                                (LINE (I) = ' ' or LINE (I) = ',' or
                                 LINE (I) = '-' or LINE (I) = ';' or
                                 LINE (I) = ':' or LINE (I) = '.' or
                                 LINE (I) = '(' or LINE (I) = '[' or
                                 LINE (I) = '{' or LINE (I) = '<' or
                                 LINE (I) = ')' or LINE (I) = ']' or
                                 LINE (I) = '}' or LINE (I) = '>');
                              J      := J + 1;
                              NW (J) := LINE (I);
                              NK     := I;
                           end loop;
                        end LOOK_AHEAD;

                        function NEXT_WORD return String is
                        begin
                           return TRIM (NW);
                        end NEXT_WORD;

                        function IS_SUM (T : String) return Boolean is
                              SA : constant array
                             (MOOD_TYPE range IND .. SUB,
                              TENSE_TYPE range PRES .. FUTP,
                              NUMBER_TYPE range S .. P,
                              PERSON_TYPE range 1 .. 3) of String (1 .. 9) :=
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

                        begin
                           if T = "" then
                              return False;
                           elsif T (T'FIRST) /= 's' and T (T'FIRST) /= 'e' and
                             T (T'FIRST) /= 'f'
                           then
                              return False;
                           end if;
                           for L in MOOD_TYPE range IND .. SUB loop
                              for K in TENSE_TYPE range PRES .. FUTP loop
                                 for J in NUMBER_TYPE range S .. P loop
                                    for I in PERSON_TYPE range 1 .. 3 loop
                                       if TRIM (T) = TRIM (SA (L, K, J, I))
                                       then
                                          SUM_INFO :=
                                            ((5, 1), (K, ACTIVE, L), I, J);
                                          return
                                            True;     --  Only one of the forms can agree
                                       end if;
                                    end loop;
                                 end loop;
                              end loop;
                           end loop;
                           return False;
                        end IS_SUM;

                        function IS_ESSE (T : String) return Boolean is
                        begin
                           return TRIM (T) = "esse";
                        end IS_ESSE;

                        function IS_FUISSE (T : String) return Boolean is
                        begin
                           return TRIM (T) = "fuisse";
                        end IS_FUISSE;

                        function IS_IRI (T : String) return Boolean is
                        begin
                           return TRIM (T) = "iri";
                        end IS_IRI;

                     begin

                        --  Look ahead for sum
                        LOOK_AHEAD;
                        if IS_SUM (NEXT_WORD)
                        then                 --  On NEXT_WORD = sum, esse, iri

                           for I in 1 .. PA_LAST loop    --  Check for PPL
                              if PA (I).IR.QUAL.POFS = VPAR
                                and then PA (I).IR.QUAL.VPAR.CS = NOM
                                and then PA (I).IR.QUAL.VPAR.NUMBER =
                                  SUM_INFO.NUMBER
                                and then
                                ((PA (I).IR.QUAL.VPAR.TENSE_VOICE_MOOD =
                                  (PERF, PASSIVE, PPL)) or
                                 (PA (I).IR.QUAL.VPAR.TENSE_VOICE_MOOD =
                                  (FUT, ACTIVE, PPL)) or
                                 (PA (I).IR.QUAL.VPAR.TENSE_VOICE_MOOD =
                                  (FUT, PASSIVE, PPL)))
                              then

                                 --  There is at least one hit, fix PA, and
                                 --  advance J over the sum
                                 K := NK;

                              end if;
                           end loop;

                           if K = NK then      --  There was a PPL hit
                              CLEAR_PAS_NOM_PPL :
                              declare
                                 J : Integer := PA_LAST;
                              begin
                                 while J >= 1
                                 loop        --  Sweep backwards to kill empty suffixes
                                    if
                                      ((PA (J).IR.QUAL.POFS = PREFIX)
                                       and then (PPL_ON))
                                    then
                                       null;
                                    elsif
                                      ((PA (J).IR.QUAL.POFS = SUFFIX)
                                       and then (PPL_ON))
                                    then
                                       null;
                                    elsif
                                      ((PA (J).IR.QUAL.POFS = TACKON)
                                       and then (PPL_ON))
                                    then
                                       null;

                                    elsif PA (J).IR.QUAL.POFS = VPAR
                                      and then PA (J).IR.QUAL.VPAR.CS = NOM
                                      and then PA (J).IR.QUAL.VPAR.NUMBER =
                                        SUM_INFO.NUMBER
                                    then

                                       if PA (J).IR.QUAL.VPAR
                                           .TENSE_VOICE_MOOD =
                                         (PERF, PASSIVE, PPL)
                                       then
                                          PPL_ON := True;

                                          case SUM_INFO.TENSE_VOICE_MOOD.TENSE
                                          is  --  Allows PERF for sum
                                             when PRES | PERF =>
                                                COMPOUND_TENSE := PERF;
                                             when IMPF | PLUP =>
                                                COMPOUND_TENSE := PLUP;
                                             when FUT =>
                                                COMPOUND_TENSE := FUTP;
                                             when others =>
                                                COMPOUND_TENSE := X;
                                          end case;
                                          COMPOUND_TVM :=
                                            (COMPOUND_TENSE, PASSIVE,
                                             SUM_INFO.TENSE_VOICE_MOOD.MOOD);

                                          PPL_INFO :=
                                            (PA (J).IR.QUAL.VPAR
                                               .CON,   --  In this case, there is 1
                                             PA (J).IR.QUAL.VPAR
                                               .CS,    --  although several different
                                             PA (J).IR.QUAL.VPAR
                                               .NUMBER,--  dictionary entries may fit
                                             PA (J).IR.QUAL.VPAR
                                               .GENDER,--  all have same PPL_INFO
                                             PA (J).IR.QUAL.VPAR
                                               .TENSE_VOICE_MOOD);
                                          PPP_MEANING(PPP_MEANING_COUNTER) :=
                                            HEAD
                                              ("PERF PASSIVE PPL + verb TO_BE => PASSIVE perfect system",
                                               MAX_MEANING_SIZE); PPP_MEANING_COUNTER := PPP_MEANING_COUNTER + 1;

                                       elsif PA (J).IR.QUAL.VPAR
                                           .TENSE_VOICE_MOOD =
                                         (FUT, ACTIVE, PPL)
                                       then
                                          PPL_ON         := True;
                                          COMPOUND_TENSE :=
                                            SUM_INFO.TENSE_VOICE_MOOD.TENSE;
                                          COMPOUND_TVM :=
                                            (COMPOUND_TENSE, ACTIVE,
                                             SUM_INFO.TENSE_VOICE_MOOD.MOOD);

                                          PPL_INFO :=
                                            (PA (J).IR.QUAL.VPAR
                                               .CON,   --  In this case, there is 1
                                             PA (J).IR.QUAL.VPAR
                                               .CS,    --  although several different
                                             PA (J).IR.QUAL.VPAR
                                               .NUMBER,--  dictionary entries may fit
                                             PA (J).IR.QUAL.VPAR
                                               .GENDER,--  all have same PPL_INFO
                                             PA (J).IR.QUAL.VPAR
                                               .TENSE_VOICE_MOOD);
                                          PPP_MEANING(PPP_MEANING_COUNTER) :=
                                            HEAD
                                              ("FUT ACTIVE PPL + verb TO_BE => ACTIVE Periphrastic - about to, going to",
                                               MAX_MEANING_SIZE); PPP_MEANING_COUNTER := PPP_MEANING_COUNTER + 1;

                                       elsif PA (J).IR.QUAL.VPAR
                                           .TENSE_VOICE_MOOD =
                                         (FUT, PASSIVE, PPL)
                                       then
                                          PPL_ON         := True;
                                          COMPOUND_TENSE :=
                                            SUM_INFO.TENSE_VOICE_MOOD.TENSE;
                                          COMPOUND_TVM :=
                                            (COMPOUND_TENSE, PASSIVE,
                                             SUM_INFO.TENSE_VOICE_MOOD.MOOD);

                                          PPL_INFO :=
                                            (PA (J).IR.QUAL.VPAR
                                               .CON,   --  In this case, there is 1
                                             PA (J).IR.QUAL.VPAR
                                               .CS,    --  although several different
                                             PA (J).IR.QUAL.VPAR
                                               .NUMBER,--  dictionary entries may fit
                                             PA (J).IR.QUAL.VPAR
                                               .GENDER,--  all have same PPL_INFO
                                             PA (J).IR.QUAL.VPAR
                                               .TENSE_VOICE_MOOD);
                                          PPP_MEANING(PPP_MEANING_COUNTER) :=
                                            HEAD
                                              ("FUT PASSIVE PPL + verb TO_BE => PASSIVE Periphrastic - should/ought/had to",
                                               MAX_MEANING_SIZE); PPP_MEANING_COUNTER := PPP_MEANING_COUNTER + 1;

                                       end if;
                                    else
                                       PA (J .. PA_LAST - 1) :=
                                         PA (J + 1 .. PA_LAST);
                                       PA_LAST := PA_LAST - 1;
                                       PPL_ON  := False;
                                    end if;
                                    J := J - 1;
                                 end loop;
                              end CLEAR_PAS_NOM_PPL;

                              PA_LAST      := PA_LAST + 1;
                              PA (PA_LAST) :=
                                (HEAD ("PPL+" & NEXT_WORD, MAX_STEM_SIZE),
                                 ((V,
                                   (PPL_INFO.CON, COMPOUND_TVM,
                                    SUM_INFO.PERSON, SUM_INFO.NUMBER)),
                                  0, NULL_ENDING_RECORD, X, A),
                                 PPP, NULL_MNPC);

                           end if;

                        elsif IS_ESSE (NEXT_WORD) or IS_FUISSE (NEXT_WORD)
                        then     --  On NEXT_WORD

                           for I in 1 .. PA_LAST loop    --  Check for PPL
                              if PA (I).IR.QUAL.POFS = VPAR
                                and then
                                ((
                                  (PA (I).IR.QUAL.VPAR.TENSE_VOICE_MOOD =
                                   (PERF, PASSIVE, PPL)) and
                                  IS_ESSE (NEXT_WORD)) or
                                 ((PA (I).IR.QUAL.VPAR.TENSE_VOICE_MOOD =
                                   (FUT, ACTIVE, PPL)) or
                                  (PA (I).IR.QUAL.VPAR.TENSE_VOICE_MOOD =
                                   (FUT, PASSIVE, PPL))))
                              then

                                 --  There is at least one hit, fix PA, and
                                 --  advance J over the sum
                                 K := NK;

                              end if;
                           end loop;

                           if K = NK then      --  There was a PPL hit
                              CLEAR_PAS_PPL :
                              declare
                                 J : Integer := PA_LAST;
                              begin
                                 while J >= 1
                                 loop        --  Sweep backwards to kill empty suffixes
                                    if
                                      ((PA (J).IR.QUAL.POFS = PREFIX)
                                       and then (PPL_ON))
                                    then
                                       null;
                                    elsif
                                      ((PA (J).IR.QUAL.POFS = SUFFIX)
                                       and then (PPL_ON))
                                    then
                                       null;
                                    elsif
                                      ((PA (J).IR.QUAL.POFS = TACKON)
                                       and then (PPL_ON))
                                    then
                                       null;

                                    elsif PA (J).IR.QUAL.POFS = VPAR then

                                       if PA (J).IR.QUAL.VPAR
                                           .TENSE_VOICE_MOOD =
                                         (PERF, PASSIVE, PPL)
                                       then
                                          PPL_ON := True;

                                          COMPOUND_TVM := (PERF, PASSIVE, INF);

                                          PPL_INFO :=
                                            (PA (J).IR.QUAL.VPAR
                                               .CON,   --  In this case, there is 1
                                             PA (J).IR.QUAL.VPAR
                                               .CS,    --  although several different
                                             PA (J).IR.QUAL.VPAR
                                               .NUMBER,--  dictionary entries may fit
                                             PA (J).IR.QUAL.VPAR
                                               .GENDER,--  all have same PPL_INFO
                                             PA (J).IR.QUAL.VPAR
                                               .TENSE_VOICE_MOOD);
                                          PPP_MEANING(PPP_MEANING_COUNTER) :=
                                            HEAD
                                              ("PERF PASSIVE PPL + esse => PERF PASSIVE INF",
                                               MAX_MEANING_SIZE); 
                                               PPP_MEANING_COUNTER := PPP_MEANING_COUNTER + 1;

                                       elsif PA (J).IR.QUAL.VPAR
                                           .TENSE_VOICE_MOOD =
                                         (FUT, ACTIVE, PPL)
                                       then
                                          PPL_ON   := True;
                                          PPL_INFO :=
                                            (PA (J).IR.QUAL.VPAR
                                               .CON,   --  In this case, there is 1
                                             PA (J).IR.QUAL.VPAR
                                               .CS,    --  although several different
                                             PA (J).IR.QUAL.VPAR
                                               .NUMBER,--  dictionary entries may fit
                                             PA (J).IR.QUAL.VPAR
                                               .GENDER,--  all have same PPL_INFO
                                             PA (J).IR.QUAL.VPAR
                                               .TENSE_VOICE_MOOD);
                                          if IS_ESSE (NEXT_WORD) then
                                             COMPOUND_TVM :=
                                               (FUT, ACTIVE, INF);
                                             PPP_MEANING(PPP_MEANING_COUNTER) :=
                                               HEAD
                                                 ("FUT ACTIVE PPL + esse => PRES Periphastic/FUT ACTIVE INF - be about/going to",
                                                  MAX_MEANING_SIZE); 
                                                  PPP_MEANING_COUNTER := PPP_MEANING_COUNTER + 1;
                                             -- also peri COMPOUND_TVM :=
                                             -- (PRES, ACTIVE, INF);
                                          else   --  fuisse
                                             COMPOUND_TVM :=
                                               (PERF, ACTIVE, INF);
                                             PPP_MEANING(PPP_MEANING_COUNTER) :=
                                               HEAD
                                                 ("FUT ACT PPL + fuisse => PERF ACT INF Periphrastic - to have been about/going to",
                                                  MAX_MEANING_SIZE); PPP_MEANING_COUNTER := PPP_MEANING_COUNTER + 1;
                                          end if;

                                       elsif PA (J).IR.QUAL.VPAR
                                           .TENSE_VOICE_MOOD =
                                         (FUT, PASSIVE, PPL)
                                       then
                                          PPL_ON := True;

                                          PPL_INFO :=
                                            (PA (J).IR.QUAL.VPAR
                                               .CON,   --  In this case, there is 1
                                             PA (J).IR.QUAL.VPAR
                                               .CS,    --  although several different
                                             PA (J).IR.QUAL.VPAR
                                               .NUMBER,--  dictionary entries may fit
                                             PA (J).IR.QUAL.VPAR
                                               .GENDER,--  all have same PPL_INFO
                                             PA (J).IR.QUAL.VPAR
                                               .TENSE_VOICE_MOOD);
                                          if IS_ESSE (NEXT_WORD) then
                                             COMPOUND_TVM :=
                                               (PRES, PASSIVE, INF);
                                             PPP_MEANING(PPP_MEANING_COUNTER) :=
                                               HEAD
                                                 ("FUT PASSIVE PPL + esse => PRES PASSIVE INF",
                                                  MAX_MEANING_SIZE); 
                                                  PPP_MEANING_COUNTER := PPP_MEANING_COUNTER + 1;
                                             -- also peri COMPOUND_TVM :=
                                             -- (PRES, ACTIVE, INF);
                                          else   --  fuisse
                                             COMPOUND_TVM :=
                                               (PERF, PASSIVE, INF);
                                             PPP_MEANING(PPP_MEANING_COUNTER) :=
                                               HEAD
                                                 ("FUT PASSIVE PPL + fuisse => PERF PASSIVE INF Periphrastic - about to, going to",
                                                  MAX_MEANING_SIZE); 
                                                  PPP_MEANING_COUNTER := PPP_MEANING_COUNTER + 1;
                                          end if;

                                       end if;
                                    else
                                       PA (J .. PA_LAST - 1) :=
                                         PA (J + 1 .. PA_LAST);
                                       PA_LAST := PA_LAST - 1;
                                       PPL_ON  := False;
                                    end if;
                                    J := J - 1;
                                 end loop;
                              end CLEAR_PAS_PPL;

                              PA_LAST      := PA_LAST + 1;
                              PA (PA_LAST) :=
                                (HEAD ("PPL+" & NEXT_WORD, MAX_STEM_SIZE),
                                 ((V, (PPL_INFO.CON, COMPOUND_TVM, 0, X)), 0,
                                  NULL_ENDING_RECORD, X, A),
                                 PPP, NULL_MNPC);

                           end if;

                        elsif IS_IRI (NEXT_WORD)
                        then              --  On NEXT_WORD = sum, esse, iri
                           --  Look ahead for sum

                           for J in 1 .. PA_LAST loop    --  Check for SUPINE
                              if PA (J).IR.QUAL.POFS = SUPINE
                                and then PA (J).IR.QUAL.SUPINE.CS = ACC
                              then
                                 --  There is at least one hit, fix PA, and
                                 --  advance J over the iri
                                 K := NK;

                              end if;
                           end loop;

                           if K = NK then      --  There was a SUPINE hit
                              CLEAR_PAS_SUPINE :
                              declare
                                 J : Integer := PA_LAST;
                              begin
                                 while J >= 1
                                 loop        --  Sweep backwards to kill empty suffixes
                                    if
                                      ((PA (J).IR.QUAL.POFS = PREFIX)
                                       and then (PPL_ON))
                                    then
                                       null;
                                    elsif
                                      ((PA (J).IR.QUAL.POFS = SUFFIX)
                                       and then (PPL_ON))
                                    then
                                       null;
                                    elsif
                                      ((PA (J).IR.QUAL.POFS = TACKON)
                                       and then (PPL_ON))
                                    then
                                       null;

                                    elsif PA (J).IR.QUAL.POFS = SUPINE
                                      and then PA (J).IR.QUAL.SUPINE.CS = ACC
                                    then

                                       PPL_ON      := True;
                                       SUPINE_INFO :=
                                         (PA (J).IR.QUAL.SUPINE.CON,
                                          PA (J).IR.QUAL.SUPINE.CS,
                                          PA (J).IR.QUAL.SUPINE.NUMBER,
                                          PA (J).IR.QUAL.SUPINE.GENDER);

                                       PA_LAST      := PA_LAST + 1;
                                       PA (PA_LAST) :=
                                         (HEAD ("SUPINE + iri", MAX_STEM_SIZE),
                                          ((V,
                                            (SUPINE_INFO.CON,
                                             (FUT, PASSIVE, INF), 0, X)),
                                           0, NULL_ENDING_RECORD, X, A),
                                          PPP, NULL_MNPC);
                                       PPP_MEANING(PPP_MEANING_COUNTER) :=
                                         HEAD
                                           ("SUPINE + iri => FUT PASSIVE INF - to be about/going/ready to be ~",
                                            MAX_MEANING_SIZE); 
                                            PPP_MEANING_COUNTER := PPP_MEANING_COUNTER + 1;

                                       K := NK;

                                    else
                                       PA (J .. PA_LAST - 1) :=
                                         PA (J + 1 .. PA_LAST);
                                       PA_LAST := PA_LAST - 1;
                                       PPL_ON  := False;
                                    end if;
                                    J := J - 1;
                                 end loop;
                              end CLEAR_PAS_SUPINE;
                           end if;

                        end if;       --  On NEXT_WORD = sum, esse, iri

                     end COMPOUNDS_WITH_SUM;
                  end if;       --  On WORDS_MODE(DO_COMPOUNDS)

--========================================================================
               end if;

-- TEXT_IO.PUT_LINE("Before LISTing STEMS (PA_LAST > 0 to start) PA_LAST = " &
-- INTEGER'IMAGE(PA_LAST));

XXX_MEANING_COUNTER := 0;  YYY_MEANING_COUNTER := 0; NNN_MEANING_COUNTER := 0;  RRR_MEANING_COUNTER := 0; PPP_MEANING_COUNTER := 0;

               if WORDS_MODE (WRITE_OUTPUT_TO_FILE) then
                  LIST_STEMS (OUTPUT, INPUT_WORD, INPUT_LINE, PA, PA_LAST);
               else
                  LIST_STEMS
                    (Current_Output, INPUT_WORD, INPUT_LINE, PA, PA_LAST);
               end if;

-- TEXT_IO.PUT_LINE("After LISTing STEMS (PA_LAST > 0 to start) PA_LAST = " &
-- INTEGER'IMAGE(PA_LAST));

               PA_LAST := 0;

            end PARSE_WORD_LATIN_TO_ENGLISH;

         end if;

----------------------------------------------------------------------
----------------------------------------------------------------------

         J := K + 1;    --  In case it is end of line and we don't look for ' '

         exit when WORDS_MDEV (DO_ONLY_INITIAL_WORD);

      end loop OVER_LINE;        --  Loop on line

   exception
      --   Have STORAGE_ERROR check in WORD too  ?????????????
      when Storage_Error =>    --  I want to again, at least twice
         if WORDS_MDEV (DO_PEARSE_CODES) then
            Text_IO.Put ("00 ");
         end if;
         Text_IO.Put_Line
           (    --  ERROR_FILE,
         "STORAGE_ERROR Exception in WORDS, try again");
         STORAGE_ERROR_COUNT := STORAGE_ERROR_COUNT + 1;
         if STORAGE_ERROR_COUNT >= 4 then
            raise;
         end if;
         PA_LAST := 0;
      when GIVE_UP =>
         PA_LAST := 0;
         raise;
      when others =>    --  I want to try to get on with the next line
         Text_IO.Put_Line
           (    --  ERROR_FILE,
         "Exception in PARSE_LINE processing " & INPUT_LINE);
         if WORDS_MODE (WRITE_UNKNOWNS_TO_FILE) then
            if WORDS_MDEV (DO_PEARSE_CODES) then
               Text_IO.Put (UNKNOWNS, "00 ");
            end if;
            Text_IO.Put (UNKNOWNS, INPUT_LINE (J .. K));
            Text_IO.Set_Col (UNKNOWNS, 30);
            Text_IO.Put_Line (UNKNOWNS, "    ========   ERROR      ");
         end if;
         PA_LAST := 0;
   end PARSE_LINE;

procedure CHANGE_LANGUAGE(C : CHARACTER) is
begin
if UPPER_CASE(C) = 'L' then
  LANGUAGE := LATIN_TO_ENGLISH;
  PREFACE.PUT_LINE("Language changed to " & LANGUAGE_TYPE'IMAGE(LANGUAGE));
elsif UPPER_CASE(C) = 'E' then
  if ENGLISH_DICTIONARY_AVAILABLE(GENERAL)  then
    LANGUAGE:= ENGLISH_TO_LATIN;
    PREFACE.PUT_LINE("Language changed to " & LANGUAGE_TYPE'IMAGE(LANGUAGE));
    PREFACE.PUT_LINE("Input a single English word (+ part of speech - N, ADJ, V, PREP, ...)");
  else
    PREFACE.PUT_LINE("No English dictionary available");
  end if;
else
  PREFACE.PUT_LINE("Bad LANGUAGE input - no change, remains " & LANGUAGE_TYPE'IMAGE(LANGUAGE));
end if;
exception
when others  =>
  PREFACE.PUT_LINE("Bad LANGUAGE input - no change, remains " & LANGUAGE_TYPE'IMAGE(LANGUAGE));
end CHANGE_LANGUAGE;
   
begin --  PARSE
--  All Rights Reserved - William Armstrong Whitaker

   if METHOD = COMMAND_LINE_INPUT then
      if TRIM (COMMAND_LINE) /= "" then
         PARSE_LINE (COMMAND_LINE);
      end if;

   else

      PREFACE.PUT_LINE
        ("Copyright (c) William Whitaker 1993-2006 - free for any use");
      PREFACE.PUT_LINE
        ("with modifications and additions 2008-2020, see github.com/spr93");
      PREFACE.PUT_LINE
        ("In memoriam, Gen. William Whitaker, Chair of DoD Working Group responsible for");
      PREFACE.PUT_LINE
        ("establishing the Ada language and accomplished amateur Latin lexicographer,");
      PREFACE.PUT_LINE
        ("who gave this comprehensive and free dictionary to the world.");

      PREFACE.NEW_LINE;
      
      -- Begin restrictions report
      if CONFIGURATION = ONLY_MEANINGS or CL_Arguments /= Null_CL_Arguments then
         if WORDS_MODE(DO_ANSI_FORMATTING) then
            Preface.PUT(Format_Reset);
            Preface.Put(Format_Inverse);
         end if;
         
         Preface.PUT("Restrictions enabled:");
        
         if WORDS_MODE(DO_ANSI_FORMATTING) then
            Preface.PUT(Format_Reset);
            Preface.PUT(Format_Bold);
         end if;
         
         Preface.NEW_LINE;
         
         if CONFIGURATION = ONLY_MEANINGS then
            Preface.PUT_LINE("- MEANINGS ONLY:   Inflections will not display in Latin-to-English mode"); end if;
         if CL_ARGUMENTS(ENGLISH_ONLY)then
            Preface.Put_Line("- ENGLISH ONLY :   The program will not enter Latin-to-English mode"); end if;
         if CL_ARGUMENTS(LATIN_ONLY) then
            Preface.Put_Line("- LATIN ONLY   :   The program will not enter English-to-Latin mode"); end if; 
         if CL_ARGUMENTS(READ_ONLY) then
            Preface.Put_Line("- READ ONLY    :   Options cannot be changed; no output to file"); end if;
         if CL_ARGUMENTS(NO_FILES) then
            Preface.Put_Line("- NO FILES     :   The program will not load files for translation"); end if;                
         if CL_ARGUMENTS(NO_EXIT) then
            Preface.Put_Line("- NO EXIT      :   User cannot instruct the program to exit; SIGINT ignored"); end if;
        
         if WORDS_MODE(DO_ANSI_FORMATTING) then
            Preface.Put(Format_Reset);
         end if;
         
      Preface.New_Line;
      end if;
      -- End restrictions report
      
    --English-to-Latin mode instructions
    If CL_ARGUMENTS(ENGLISH_ONLY) 
      and then ENGLISH_DICTIONARY_AVAILABLE(GENERAL)
      then LANGUAGE := ENGLISH_TO_LATIN;
         
    elsif CL_ARGUMENTS(English_ONLY)  then
         Preface.Put_Line("English dictionary not available.  Cannot run in English-to-Latin only mode.");
         raise GIVE_UP; 
         
    elsif ENGLISH_DICTIONARY_AVAILABLE (GENERAL) 
        and then not CL_ARGUMENTS(ENGLISH_ONLY) 
        and then not CL_Arguments(LATIN_ONLY)
      then
          PREFACE.PUT_LINE ("English-to-Latin mode available");
          PREFACE.PUT_LINE
            ("   " & CHANGE_LANGUAGE_CHARACTER &
             "E changes to English-to-Latin mode; " &
          CHANGE_LANGUAGE_CHARACTER & "L changes back");
          PREFACE.NEW_LINE;
    end if;
      
    case LANGUAGE is
                                   
       when LATIN_TO_ENGLISH =>
            PREFACE.PUT 
              ("Input a word or line of Latin and press ENTER");
            
       when ENGLISH_TO_LATIN =>     
            PREFACE.PUT
              ("Input a word or line of English and press ENTER");
            PREFACE.NEW_LINE;
            PREFACE.PUT
              ("   or input a word <space> and part of speech restriction [ADJ, ADV, N, V]]");
           
      end case;    

      PREFACE.NEW_LINE;

    if CL_Arguments(NO_FILES) = False then 
       PREFACE.PUT_LINE
         ("   or input " & START_FILE_CHARACTER &
            " and the name of a file containing words or lines");
    end if; 
    
    if CL_Arguments(READ_ONLY) = False Then
       PREFACE.PUT_LINE
         ("   or input " & CHANGE_PARAMETERS_CHARACTER &
            " to change program options");
    end if; 
      
     PREFACE.NEW_LINE;
      
     PREFACE.PUT_LINE ("Input " & HELP_CHARACTER & " to get help");
     
      if CL_Arguments(NO_EXIT) = False then 
       PREFACE.PUT_LINE
         ("Two empty lines (just a RETURN/ENTER) from the keyboard exits the program");
         PREFACE.NEW_LINE;
      end if; 

      GET_INPUT_LINES :
      loop
         
         GET_INPUT_LINE :
         begin                    --  Block to manipulate file of lines
            
               PA := (others => NULL_PARSE_RECORD); -- clear PA to prevent exceptions in LIST_STEM
                                                    -- which can occur after a huge result fills the
                                                    -- PA (e.g., arcule will return a full array once
                                                    -- then cause an exception the next time without this)
                                                    
               SYPA := (others => NULL_PARSE_RECORD);
               TRPA := (others => NULL_PARSE_RECORD);
               PA_LAST := 0; SYPA_LAST := 0; TRPA_LAST  := 0;
               J := 0; K := 0; L := 0; 
               LINE := BLANK_LINE;
           
               if (Name (Current_Input) = Name (Standard_Input)) then
               SCROLL_LINE_NUMBER :=
               Integer (Text_IO.Line (Text_IO.Standard_Output));
                   
               PREFACE.NEW_LINE;
               PREFACE.PUT ("=>");
         end if;
            
            if METHOD = INTERACTIVE
              and then WORDS_MODE(DO_UNICODE_INPUT) then
               Get_Unicode(LINE, L);
            else
               Get_Line(LINE,L);
            end if;
            
            if (L = 0) or else (TRIM (LINE (1 .. L)) = "") then
               if CL_Arguments(NO_EXIT) then
                  null;
               Elsif (Name (Current_Input) = Name (Standard_Input))
                 and then not CL_Arguments(NO_EXIT)
               then   --  INPUT is keyboard
                  PREFACE.PUT ("Blank exits =>");
                  
                  if METHOD = INTERACTIVE 
                  and then WORDS_MODE(DO_UNICODE_INPUT) then
                  Get_Unicode(LINE, L);
                  else 
                  Get_Line(LINE,L);
                  end if;
                
                  if (L = 0) or else (TRIM (LINE (1 .. L)) = "")
                  then  -- Two in a row
                     exit;
                  end if;
                  elsif End_Of_File (Current_Input)
                  then
                  Exit; 
               end if;
            end if;
    
            if (TRIM (LINE (1 .. L)) /= "")
            then            -- Not a blank line so L(1) (in file input)

               if LINE (1) = START_FILE_CHARACTER
                 and then not CL_Arguments(NO_FILES)
                 and then not CL_Arguments(READ_ONLY)
               then    --  To begin file of words
                  
                  if (Name (Current_Input) /= Name (Standard_Input)) then
                     Text_IO.Put_Line
                       ("Cannot have file of words (@FILE) in an @FILE");
                  
                  elsif 
                    WORDS_MODE(DO_UNICODE_INPUT) then
                      Parse_Unicode_File (TRIM (LINE (2 .. L)));
                    null;
                  else 
                   Text_IO.Open
                       (INPUT, Text_IO.In_File, TRIM (LINE (2 .. L)));
                     Text_IO.Set_Input (INPUT);
                  end if;

               elsif LINE (1) = CHANGE_PARAMETERS_CHARACTER
                 and then (Name (Current_Input) = Name (Standard_Input))
                 and then not CONFIG.SUPPRESS_PREFACE
                 and then not CL_Arguments(READ_ONLY) 
               then
                  CHANGE_PARAMETERS;

               elsif LINE (1) = HELP_CHARACTER
                 and then (Name (Current_Input) = Name (Standard_Input))
                 and then not CONFIG.SUPPRESS_PREFACE 
               then 
                  SHOW_HELP (UPPER_CASE(Trim(Line (2..L))));
              
               elsif LINE (1) = CHANGE_LANGUAGE_CHARACTER 
                 and then not (CL_Arguments(ENGLISH_ONLY) or CL_Arguments(LATIN_ONLY))
                 and then (NAME(CURRENT_INPUT) = NAME(STANDARD_INPUT)) 
               then
                   CHANGE_LANGUAGE (LINE (2));
               
               elsif --  CONFIGURATION = DEVELOPER_VERSION  and then    --  Allow anyone to do it
               LINE (1) = CHANGE_DEVELOPER_MODES_CHARACTER
                 and then (Name (Current_Input) = Name (Standard_Input))
                 and then not CONFIG.SUPPRESS_PREFACE
                 and then not CL_Arguments(READ_ONLY) 
               then
                   CHANGE_DEVELOPER_MODES;
               
               else
                  if (Name (Current_Input) /= Name (Standard_Input)) then
                     PREFACE.NEW_LINE;
                     PREFACE.PUT_LINE (LINE (1 .. L));
                  end if;
                  if WORDS_MODE (WRITE_OUTPUT_TO_FILE) then
                     if not CONFIG.SUPPRESS_PREFACE then
                        New_Line (OUTPUT);
                        Text_IO.Put_Line (OUTPUT, LINE (1 .. L));
                     end if;
                  end if;
                     PARSE_LINE (LINE (1 .. L));
               
               end if;
            end if;

         exception
            when Name_Error | Use_Error =>
               if (Name (Current_Input) /= Name (Standard_Input)) then
                  Set_Input (Standard_Input);
                  Close (INPUT);
               end if;
                 Put_Line ("Unknown or unacceptable file name. Try Again");
                                          
            when End_Error =>          --  The end of the input file resets to CON:
               if CL_Arguments(NO_EXIT) 
                  then null;
               elsif (Name (Current_Input) /= Name (Standard_Input)) then
                  Set_Input (Standard_Input);
                  Close (INPUT);
                  if METHOD = COMMAND_LINE_FILES then
                     raise GIVE_UP;
                  end if;
               else
                  if CL_Arguments(NO_EXIT) 
                  then null;
                  
                  else Put_Line ("Raised END_ERROR (may be inappropriate line terminator)");
                     raise GIVE_UP;
                     end if; 
            end if;
                                          
            when Status_Error =>                 --  The end of the input file resets to CON:
                 Put_Line ("Raised STATUS_ERROR");
         end GET_INPUT_LINE;                     --  end Block to manipulate file of lines

      end loop GET_INPUT_LINES;                  --  Loop on lines
    
      return; 
   end if;     --  On command line input

exception

   when Storage_Error =>    --  Have tried at least twice, fail
      PREFACE.PUT_LINE ("STORAGE_ERROR Exception in PARSE");
   when GIVE_UP =>
      PREFACE.PUT_LINE ("Giving up!");
   when Catch_Me: Others =>
    if CL_Arguments(NO_EXIT) 
        then Parse;
        else 
         PREFACE.PUT_LINE ("Unexpected exception raised in PARSE");
         Put_Line(Ada.Exceptions.Exception_Message(Catch_Me));
         Put_Line(Ada.Exceptions.Exception_Information(Catch_Me));
    end if;
        
end PARSE;

      
 --  RELIES ON ADA2020 FEATURE WIDE_CHARACTERS.HANDLING.TO_BASIC  
   procedure Parse_Unicode_File (W_Input_String : String) is
      
     use Ada.Wide_Text_IO;

     pragma Wide_Character_Encoding(UTF8);
                     begin 

     Open(W_INPUT, In_File, W_Input_String);
                     WORDS_MODE(DO_UNICODE_INPUT) := False;  
                     Method := COMMAND_LINE_INPUT;
      
                     while not End_Of_File(W_Input) loop
                     
                     PARSE( (Ada.Characters.Conversions.To_String 
                                      --  (Ada.Wide_Characters.Handling.To_Basic
                                        (GET_Line(W_INPUT)))); -- );
                     end loop;
                     
                     Close(W_Input);
                     WORDS_MODE(DO_UNICODE_INPUT) := True; 
                     Method := INTERACTIVE;
                     exception
                     when Ada.Wide_Text_IO.End_Error | Ada.Wide_Text_IO.Status_Error | Ada.Wide_Text_IO.Name_Error  =>
                      --  WORDS_MODE(DO_UNICODE_INPUT) := False; 
                        Method := INTERACTIVE;
                     if Is_Open(W_INPUT) then
                        Close(W_Input);
                     end if; 
                     when others =>      
                      --  WORDS_MODE(DO_UNICODE_INPUT) := False; 
                        if Is_Open(W_INPUT) then
                         Close(W_Input);
                        end if; 
                       raise;   
  
     end Parse_Unicode_File;  
   
   
end Parse_Package;

   
   
