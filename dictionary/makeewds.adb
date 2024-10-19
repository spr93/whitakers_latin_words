with Text_IO;
with Ada.Exceptions;
with Ada.Command_Line;
with STRINGS_PACKAGE;         use STRINGS_PACKAGE;
with LATIN_FILE_NAMES;        use LATIN_FILE_NAMES;
with INFLECTIONS_PACKAGE;     use INFLECTIONS_PACKAGE;
with DICTIONARY_PACKAGE;      use DICTIONARY_PACKAGE;
with LINE_STUFF;              use LINE_STUFF;
with ENGLISH_SUPPORT_PACKAGE; use ENGLISH_SUPPORT_PACKAGE;
with WEED;
with WEED_ALL;
with Dictionary_Form;

procedure MAKEEWDS is

   package INTEGER_IO is new Text_IO.Integer_IO (Integer);

   use Text_IO;
   use INTEGER_IO;
   use STEM_KEY_TYPE_IO;
   use DICTIONARY_ENTRY_IO;
   use PART_ENTRY_IO;
   use PART_OF_SPEECH_TYPE_IO;
   use KIND_ENTRY_IO;
   use TRANSLATION_RECORD_IO;
   use AGE_TYPE_IO;
   use AREA_TYPE_IO;
   use GEO_TYPE_IO;
   use FREQUENCY_TYPE_IO;
   use SOURCE_TYPE_IO;
   use EWDS_RECORD_IO;

   PORTING  : constant Boolean := False;
   CHECKING : constant Boolean := True;

   D_K : DICTIONARY_KIND := XXX;       --  ######################

   START_STEM_1 : constant         := 1;
   START_STEM_2 : constant         := START_STEM_1 + MAX_STEM_SIZE + 1;
   START_STEM_3 : constant         := START_STEM_2 + MAX_STEM_SIZE + 1;
   START_STEM_4 : constant         := START_STEM_3 + MAX_STEM_SIZE + 1;
   START_PART   : constant         := START_STEM_4 + MAX_STEM_SIZE + 1;
   START_TRAN   : constant Integer :=
     START_PART + Integer (PART_ENTRY_IO.DEFAULT_WIDTH + 1);
   FINISH_LINE : constant Integer :=
     START_TRAN + TRANSLATION_RECORD_IO.DEFAULT_WIDTH - 1;

   LINE_NUMBER : Integer := 0;

   subtype WORD_TYPE is String (1 .. MAX_MEANING_SIZE);
   subtype LINE_TYPE is String (1 .. 400);

   N : Integer := 0;

   INPUT, OUTPUT, CHECK : Text_IO.File_Type;
   DE                   : DICTIONARY_ENTRY;

   NULL_WORD_TYPE, BLANK_WORD : WORD_TYPE := (others => ' ');
   S, LINE, BLANK_LINE        : LINE_TYPE := (others => ' ');
   L, LL, LAST                : Integer   := 0;

   EWA : EWDS_ARRAY (1 .. 40) := (others => NULL_EWDS_RECORD);

   EWR : EWDS_RECORD := NULL_EWDS_RECORD;

   --  First we supplement MEAN with singles of any hyphenated words In
   --  principle this could be done in the main EXTRACT, much same logic/code
   --  However this is difficult code for an old man, EXTRACT was hard when I
   --  was a bit younger And I cannot remember anything about it. Separating
   --  them out makes it much easier to test

   function ADD_HYPHENATED (S : String) return String is

      --------  I tried to do something with hyphenated but so far it does not work  ----------

      --  Find hyphenated words and add them to MEAN with a / connector, right
      --  before the parse so one has both the individual words (may be more
      --  than two) and a single combined word counting-board -> counting
      --  board/countingboard
      T : String (1 .. MAX_MEANING_SIZE * 2 + 20) :=
        (others => ' ');   --  Cannot be bigger
      WORD_START : Integer := 1;
      WORD_END   : Integer := 0;
      I, J, JMAX : Integer := 0;
      HYPHENATED : Boolean := False;

      WW : Integer := 0;     --  For debug

   begin
--PUT_LINE("S    " & INTEGER'IMAGE(LINE_NUMBER) & "   " & INTEGER'IMAGE(S'FIRST) & "  " & INTEGER'IMAGE(S'LAST));
--PUT_LINE(S);
      while I < S'LAST loop
         I        := I + 1;
         J        := J + 1;
         WORD_END := 0;
--PUT(INTEGER'IMAGE(I) & "-");

         --  First clear away or ignore all the non-words stuff
         if S (I) = '|' then     --  Skip continuation |'s
            WORD_START := I + 1;
            T (J)      := S (I);
            J          := J + 1;
            JMAX       := JMAX + 1;
            null;
            I := I + 1;
--PUT_LINE("|||    " & INTEGER'IMAGE(LINE_NUMBER) & "    " & S(I) & '_' & S(WORD_START..S'LAST));
         elsif S (I) = '"' then     --  Skip "'s
            WORD_START := I + 1;
            T (J)      := S (I);
            J          := J + 1;
            JMAX       := JMAX + 1;
            null;
            I := I + 1;
--PUT_LINE('"' &   "   " & INTEGER'IMAGE(LINE_NUMBER) & "    ->" & S(WORD_START..S'LAST));
         else
            if S (I) = '(' then    --  (...) not to be parsed
               T (J) := S (I);
               J     := J + 1;
               JMAX  := JMAX + 1;
               I     := I + 1;
               while S (I) /= ')' loop
                  T (J) := S (I);
                  J     := J + 1;
                  JMAX  := JMAX + 1;
                  I     := I + 1;
               end loop;
               WORD_START := I + 2;   --  Skip };
               WORD_END   := 0;
            elsif S (I) = '[' then    --  (...) not to be parsed
               T (J) := S (I);
               J     := J + 1;
               JMAX  := JMAX + 1;
               I     := I + 1;

               -- Put_Line("I is " & I'Image & " and S'Range is is " &
               -- S'First'Image & " .. " & S'Last'Image);

               while S (I - 1 .. I) /= "=>" and S (I - 1 .. I) /= "]" loop
                  T (J) := S (I);
                  J     := J + 1;
                  JMAX  := JMAX + 1;
                  I     := I + 1;
                  exit when I = S'Last;
               end loop;

               WORD_START := I + 2;
               WORD_END   := 0;

            end if;
            --  Finished with the non-word stuff

            if (S (I) = '-') then
               WORD_END := I - 1;

--              if  (I /= S'FIRST)  and then    --  Not -word
--               ( (S(I-1) /= ' ') and
--                 (S(I-1) /= '/')  )  then
--                 HYPHENATED := TRUE;
--              end if;
--PUT_LINE("---    " & INTEGER'IMAGE(LINE_NUMBER) & "   " & INTEGER'IMAGE(I) &
--"  " & INTEGER'IMAGE(WORD_START) & "  " & INTEGER'IMAGE(WORD_END) & "   ->" & S(WORD_START..WORD_END));
            end if;

            if S (I) = ' ' or S (I) = '/' or S (I) = ',' or S (I) = ';' or
              S (I) = '!' or S (I) = '?' or S (I) = '+' or S (I) = '*' or
              S (I) = '"' or S (I) = '('
            then
               WORD_END := I - 1;

--PUT_LINE(INTEGER'IMAGE(LINE_NUMBER) & "    NNN " & S(I) & "  " & INTEGER'IMAGE(I) & "  "
--& INTEGER'IMAGE(WORD_START)& "   " & INTEGER'IMAGE(WORD_END) & "    " & S(WORD_START..WORD_END));
               if HYPHENATED then
                  T (J) := '/';
                  J     := J + 1;
                  JMAX  := JMAX + 1;
                  for K in WORD_START .. WORD_END loop
                     if S (K) /= '-' then
                        T (J) := S (K);
                        J     := J + 1;
                        JMAX  := JMAX + 1;
                     end if;
                  end loop;
                  HYPHENATED := False;
               end if;

            end if;

            if  --WORD_END /= 0  and then
              (S (I) = ' ' or S (I) = '/') then
               WORD_START := I + 1;
               WORD_END   := 0;
            end if;

--PUT_LINE(INTEGER'IMAGE(LINE_NUMBER) & "    TTT " & S(I) & "  " &  INTEGER'IMAGE(I) &
--"  " & INTEGER'IMAGE(WORD_START) & "   " & INTEGER'IMAGE(WORD_END) & "    " & S(WORD_START..WORD_END));

         end if;  --  On '|'

         --  Set up the output to return
--PUT('|' & INTEGER'IMAGE(J) & '/' & INTEGER'IMAGE(I));
         exit when J not in S'Range;
         T (J) := S (I);
         JMAX  := JMAX + 1;

      end loop;  --  Over S'RANGE

--PUT_LINE("RRR    ->" & INTEGER'IMAGE(LINE_NUMBER) & "   " & T(1..JMAX));
      return T (1 .. JMAX);

   exception
      when Catch_Me : others =>
         Put_Line
           ("ADD_HYPHENATED  Exception    LINE = " &
            Integer'IMAGE (LINE_NUMBER));
         Put_Line (S);
         PUT (DE);
         New_Line;
         Put_Line (Ada.Exceptions.Exception_Name (Catch_Me));
         Put_Line (Ada.Exceptions.Exception_Information (Catch_Me));
         Put_Line (Ada.Exceptions.Exception_Message (Catch_Me));
         return T (1 .. JMAX);
   end ADD_HYPHENATED;

   procedure EXTRACT_WORDS
     (S   : in     String; POFS : in PART_OF_SPEECH_TYPE; N : out Integer;
      EWA :    out EWDS_ARRAY)
   is
      I, J, JS, K, L, M, IM, IC : Integer := 0;
      START_SEMI, END_SEMI      : Integer := 1;
      --  Have to expand type to take care of hyphenated
      subtype X_MEANING_TYPE is String (1 .. MAX_MEANING_SIZE * 2 + 20);
      NULL_X_MEANING_TYPE : constant X_MEANING_TYPE := (others => ' ');
      SEMI, COMMA         : X_MEANING_TYPE          := NULL_X_MEANING_TYPE;

      SM1, SM2 : Integer := 0;

      WW : Integer := 0;    --  For debug
   begin
--NEW_LINE(2);
--PUT_LINE("MEAN  " & INTEGER'IMAGE(LINE_NUMBER) & "  =>" & S);
--PUT_LINE("MEAN=>" & INTEGER'IMAGE(S'FIRST) & "  " & INTEGER'IMAGE(S'LAST) & "|::::::::");
      I  := 1;    --  Element Position in line, per SEMI
      J  := 1;    --  Position in word
      K  := 0;    --  SEMI - Division in line
      L  := 1;    --  Position in MEAN, for EXTRACTing SEMI
      M  := 1;    --  COMMA in SEMI
      N  := 1;    --  Word number
      IM := 0;   --  Position in SEMI
      IC := 0;   --  Position in COMMA

      EWA (N) := NULL_EWDS_RECORD;

      -- Slightly disparage extension
      if S (S'First) = '|' then
         K := 3;
      end if;

      while L <= S'LAST loop  --  loop over MEAN
         if S (L) = ' ' then  --  Clear initial blanks
            L := L + 1;

         end if;

         SEMI := NULL_X_MEANING_TYPE;
         IM   := 1;
         SM1  := 1;
         SM2  := 0;
         exit when L not in S'Range;
         EXTRACT_SEMI :
         loop

            exit when L not in S'Range;

            --PUT('/');
            --PUT(S(L));
            if S (L) in '0' .. '9' then
               null;         --  Ignore numbers
            elsif S (L) = '|' then
               null;          --  Ignore continuation flag | as word

            elsif S (L) = ';' then     --  Division Terminator
--PUT(':');
               K   := K + 1;
               SM2 := IM - 1;
               --PUT('+');
               L := L + 1;  --  Clear ;
               exit;
            elsif S (L) = '(' then  --  Skip (...)  !
               --PUT('[');
               while S (L) /= ')' loop
--PUT('+');
--PUT(INTEGER'IMAGE(L));
--PUT(S(L));
                  exit when L = S'LAST;  -- Run out
                  L := L + 1;
               end loop;
               --              L := L + 1;    --  Clear the ')'
--PUT('^');
--PUT(INTEGER'IMAGE(L));
--PUT(S(L));
               if L > S'LAST then
                  L   := S'LAST;
                  SM2 := IM;
               else
                  if S (L) = ';' then  --  );
                     SM2 := IM - 1;
                     exit EXTRACT_SEMI;
                  end if;
               end if;
               --PUT(']');

               if L >= S'LAST then  --  Ends in )
                  --PUT('!');
                  SM2 := IM;
                  exit;
               end if;
               --PUT('+');
               --L := L + 1;    --  Clear the ')'
            elsif L = S'LAST then
               --PUT('|');
               SM2 := IM;
               L   := L + 1;     --  To end the loop
               exit;

            else
               SEMI (IM) := S (L);
               SM2       := IM;
               IM        := IM + 1;
            end if;
--PUT('+');
            --IM := IM + 1;  --  To next character
            L := L + 1;  --  To next character
         end loop EXTRACT_SEMI;

         WW := 10;

--if LINE_NUMBER = 8399  then
--NEW_LINE;
--PUT_LINE("NEW SEMI=>" & SEMI(SM1..SM2) & "|::::::::");
--PUT_LINE("NEW SEMI INDEX=>" & INTEGER'IMAGE(SM1) & "  " & INTEGER'IMAGE(SM2) & "|::::::::");
--end if;

         PROCESS_SEMI :
         declare
            ST                     : constant String := TRIM (SEMI);
            SM                     : String (ST'FIRST .. ST'LAST) := ST;
            START_COMMA, END_COMMA : Integer                      := 0;
         begin
            if ST'LENGTH > 0 then
               COMMA := NULL_X_MEANING_TYPE;
               IM    := SM'FIRST;
               M     := 0;

               --I := SM'FIRST;
               --while  I <= ST'LAST  loop

--PUT(S(I));
--PUT('*');

               --COMMA := NULL_X_MEANING_TYPE;

               IC := 1;
               LOOP_OVER_SEMI :
               while IM <= SM'LAST loop
                  COMMA := NULL_X_MEANING_TYPE;
                  WW    := 20;
                  FIND_COMMA :
                  loop

                     --PUT(INTEGER'IMAGE(IM) & " ( " & SM(IM));
                     if SM (IM) = '(' then  --  Skip (...)  !
                        while SM (IM) /= ')' loop
                           IM := IM + 1;
                        end loop;
                        IM := IM + 1;    --  Clear the ')'
                        --        IM := IM + 1;    --  Go to next character
                     --PUT_LINE("Cleared (+" & "  IM = " & INTEGER'IMAGE(IM));
                        if IM >= END_SEMI then
                           --PUT_LINE("exit on SM'LAST  "  & INTEGER'IMAGE(SM'LAST) & "  I = " & INTEGER'IMAGE(IM));
                           exit;
                        end if;
                        --PUT_LINE("No exit on SM'LAST  "  & INTEGER'IMAGE(SM'LAST) & "  I = " & INTEGER'IMAGE(IM) & "|" & SM(IM) & "|");
                        if (SM (IM) = ';') or (SM (IM) = ',') then
                     --PUT_LINE("Found ;, COMMA  IM = " & INTEGER'IMAGE(IM));
                           --  Found COMMA
                           M  := M + 1;
                           IC := 1;
                           IM := IM + 1;       --  Clear ;,
                           exit;
                        elsif SM (IM) = ' ' then
                        --PUT_LINE("Found blank -  IM = " & INTEGER'IMAGE(IM));
                           IM := IM + 1;
                        --PUT_LINE("Found blank +  IM = " & INTEGER'IMAGE(IM));
                        end if;
                        --PUT_LINE("------------------------");
                     end if;
                     if SM (IM) = '[' then  --  Take end of [=>]
                        while SM (IM) /= '>' loop
                           exit when SM (IM) = ']'; --  If no >
                           IM := IM + 1;
                           exit when IM not in SM'Range;
                        end loop;
         --   Put_Line(SM & "|" & IM'Image & "|" & IC'Image & "|" & M'Image);

                        IM := IM + 1;    --  Clear the '>' or ']'

                        --     Put_Line("IM is: " & IM'Image & "|" & "M is " & "|" &  M'Image  & "|" &  "IC is "  & "|" &  "SM is " & SM  & "|" & "(SM last is " & SM'Last'Image & " )");
                        if IM < SM'Last and then SM (IM) = ';' then
                           --  Found COMMA
                           M  := M + 1;
                           IC := 1;

                           IM := IM + 1;       --  Clear ;
                           exit;
                        elsif IM < SM'Last and then SM (IM) = ' ' then
                           IM := IM + 1;
                        end if;

                     end if;          --  But could be 2 =>!
                     --PUT_LINE("Through ()[] I = " & INTEGER'IMAGE(I));
                     exit when IM > SM'LAST;

--PUT(INTEGER'IMAGE(IM) & " ) " & SM(IM));
                     if SM (IM) = ',' then
                        --  Foumd COMMA
                        M  := M + 1;
                        IC := 1;
                        IM := IM + 1;       --  Clear ,
                        exit;
                     elsif IM >= SM'LAST or IM = S'LAST then
                        --  Found COMMA
                        COMMA (IC) := SM (IM);
                        M          := M + 1;
                        IC         := 1;
                        exit;
                     else
                        COMMA (IC) := SM (IM);
                        IM         := IM + 1;
                        IC         := IC + 1;

                     end if;
                     --PUT(INTEGER'IMAGE(IM) & " ! " & SM(IM));

                  end loop FIND_COMMA;
                  --PUT_LINE("COMMA " & INTEGER'IMAGE(LINE_NUMBER) & INTEGER'IMAGE(IM) &  "=>" & TRIM(COMMA));
                  IM := IM + 1;

                  WW := 30;

                  PROCESS_COMMA :
                  declare
                     CT             : constant String := TRIM (COMMA);
                     CS             : String (CT'FIRST .. CT'LAST) := CT;
                     PURE           : Boolean                      := True;
                     W_START, W_END : Integer                      := 0;
                  begin
                     WW := 31;
                     --PUT_LINE("PROCESS COMMA " & INTEGER'IMAGE(LINE_NUMBER) & INTEGER'IMAGE(CT'FIRST) & INTEGER'IMAGE(CT'LAST) &  "=>" & TRIM(COMMA));

                     --   PUT_LINE(CS);
                     if CT'LENGTH > 0 then   --  Is COMMA non empty
                        --  Are there any blanks?
                        --  If not then it is a pure word Or words with /
                        for IP in CS'RANGE loop
                           if CS (IP) = ' ' then
                              PURE := False;
                           end if;
                        end loop;

                        WW := 32;

                        --  Check for WEED words and eliminate them
                        W_START := CS'FIRST;
                        W_END   := CS'LAST;
                        for IW in CS'RANGE loop
                           --PUT('-');
                           --PUT(CS(IW));
                           -- PUT(CS);
                           -- NEW_LINE;
                           if (CS (IW) = '(') or (CS (IW) = '[') then
                              WW      := 33;
                              W_START := IW + 1;
                           else
                              WW := 34;
                              if (CS (IW) = ' ') or (CS (IW) = '_') or
                                (CS (IW) = '-') or (CS (IW) = ''') or
                                (CS (IW) = '!') or (CS (IW) = '/') or
                                (CS (IW) = ':') or (CS (IW) = '.') or
                                (CS (IW) = '!') or (CS (IW) = ')') or
                                (CS (IW) = ']') or (IW = CS'LAST)
                              then
                                 --PUT_LINE("HIT  "  & CS(IW) & "  IW = " & INTEGER'IMAGE(IW) & "  CS'LAST = " & INTEGER'IMAGE(CS'LAST));
                                 WW := 35;
                                 if IW = CS'LAST then
                                    W_END := IW;
                                 elsif IW /= CS'FIRST then
                                    W_END := IW - 1;
                                 end if;

                                 WW := 36;
                                 --  KLUDGE
                                 if CS (W_START) = '"' then
                                    WW      := 361;
                                    W_START := W_START + 1;
                                    WW      := 362;
                                 elsif CS (W_END) = '"' then
                                    WW    := 364;
                                    W_END := W_END - 1;
                                    WW    := 365;
                                 end if;

                                 WW := 37;

--  PUT_LINE(INTEGER'IMAGE(LINE_NUMBER) & "WEEDing " & INTEGER'IMAGE(W_START) &
--  " " & INTEGER'IMAGE(W_END) & " " & CS(W_START..W_END) );
                                 WEED_ALL (CS (W_START .. W_END), POFS);
                                 if not PURE then
                                    WEED (CS (W_START .. W_END), POFS);
                                 end if;
                                 W_START := IW + 1;
                              end if;
                              WW := 38;
                           end if;
                           WW := 39;
                        end loop;          --  On CS'RANGE

                        --PUT_LINE(INTEGER'IMAGE(LINE_NUMBER) & "WEED done");

                        WW := 40;
                        --  Main process of COMMA
                        IC := 1;
                        J  := 1;
                        while IC <= CS'LAST loop

                           --PUT_LINE(CS & " : " &  CS(IC));

                           if CS (IC) = '"' or      --  Skip all "
                             CS (IC) = '(' or      --  Skip initial (
                           --CS(IC) = '-'  or      --  Skip hyphen -> one word!

                             CS (IC) = '?' or      --  Ignore ?

                             CS (IC) = '~' or      --  Ignore about ~

                             CS (IC) = '*' or
                             CS (IC) =
                               '%' or      --  Ignore percent unless word

                             CS (IC) = '.' or      --  Ignore ...

                             CS (IC) = '\' or      --  Ignore weed

                             (CS (IC) in '0' .. '9')
                           then --  Skip numbers
                              IC := IC + 1;
                              WW := 50;

                              ----PUT('-');
                           else

                              if
                           --            S(IC) = ','  or       --  Terminators
                        --            S(IC) = '.'  or       --  Would be typo
                  --            S(IC) = ';'  or       --  Should catch at SEMI
                                 CS (IC) = '/' or CS (IC) = ' ' or
                                CS (IC) =
                                  ''' or    --  Ignore all ' incl 's  ???

                                CS (IC) =
                                  '-' or    --  Hyphen causes 2 words  XXX

                                CS (IC) = '+' or    --  Plus causes 2 words

                                CS (IC) =
                                  '_' or    --  Underscore causes 2 words

                                CS (IC) = '=' or    --  = space/terminates

                                CS (IC) = '>' or
                                CS (IC) = ')' or CS (IC) = ']' or
                                CS (IC) = '!' or CS (IC) = '?' or
                                CS (IC) = '+' or CS (IC) = ':' or CS (IC) = ']'
                              then  --  Found word
                                 WW := 60;
--PUT('/');
                                 EWA (N).SEMI := K;
                                 if PURE then
                                    if K = 1 then
                                       EWA (N).KIND := 15;
                                    else
                                       EWA (N).KIND := 10;
                                    end if;
                                 else
                                    EWA (N).KIND := 0;
                                 end if;
                                 WW := 70;
--PUT_LINE("====1  K J = " & INTEGER'IMAGE(K) & "  " & INTEGER'IMAGE(J) & "  ." & EWA(N).W(1..J-1) & ".");
                                 N := N + 1;       --  Start new word in COMMA
                                 IC      := IC + 1;
                                 J       := 1;
                                 EWA (N) := NULL_EWDS_RECORD;

                              elsif           --  Order of if important
                              IC = CS'LAST then  --  End, Found word
--PUT('!');
                                 EWA (N).W (J) := CS (IC);
                                 EWA (N).SEMI  := K;
                                 if PURE then
                                    if K = 1 then
                                       EWA (N).KIND := 15;
                                    else
                                       EWA (N).KIND := 10;
                                    end if;
                                 else
                                    EWA (N).KIND := 0;
                                 end if;
--PUT_LINE("====2  K J = " & INTEGER'IMAGE(K) & "  " & INTEGER'IMAGE(J) & "  ." & EWA(N).W(1..J) & ".");
                                 N := N + 1;       --  Start new word/COMMA

                                 EWA (N) := NULL_EWDS_RECORD;
                                 exit;

                              else
                                 WW := 80;
                                 --PUT('+');
                                 EWA (N).W (J) := CS (IC);
                                 J             := J + 1;
                                 IC            := IC + 1;
                              end if;
                           end if;
                           WW := 90;
                        end loop;

                     end if;  -- On COMMA being empty
                  end PROCESS_COMMA;
                  --PUT_LINE("COMMA Processed ");

               end loop LOOP_OVER_SEMI;

               --PUT_LINE("LOOP OVER SEMI Processed ");

            end if;    -- On ST'LENGTH > 0
            --PUT_LINE("LOOP OVER SEMI after ST'LENGTH  0 ");
         end PROCESS_SEMI;

--PUT_LINE("SEMI Processed ");
-- I = "  & INTEGER'IMAGE(I)
--& "  S(I) = " & S(I)
--);
         if (L < S'LAST) and then (S (L) = ';') then             --  ??????
--PUT_LINE("Clear  L = " & INTEGER'IMAGE(L));
            L := L + 1;
         end if;

         JS := L;    --  Odd but necessary    ?????
         for J in L .. S'LAST loop
            exit when J >= S'LAST;
            if S (J) = ' ' then
               L := L + 1;
            else
               exit;
            end if;
         end loop;
         START_SEMI := L;

--PUT_LINE("SEMI Processed Completely   L = "  & INTEGER'IMAGE(L)  & "  S'LAST = " & INTEGER'IMAGE(S'LAST));

         exit when L >= S'LAST;
      end loop;   --  loop over MEAN

      DROP_DUPES :
      for Z in EWA'Range loop
         if EWA (Z).W /= NULL_EWORD then

            INNER_LOOP :
            for ZZ in (Z + 1) .. EWA'Last loop
               if EWA (Z).W = EWA (ZZ).W then
                  EWA (ZZ).W := NULL_EWORD;
               end if;
            end loop INNER_LOOP;

         end if;
      end loop DROP_DUPES;

      --PUT_LINE("SEMI loop Processed");

      if N <= EWA'Last and then EWA (N) = NULL_EWDS_RECORD then
         N := N - 1;   --  Clean up danglers
      end if;
      if N <= EWA'Last and then N > 1 and then EWA (N) = NULL_EWDS_RECORD
      then   --  AGAIN!!!!!!
         N := N - 1;   --  Clean up danglers
      end if;

   exception
      when Catch_Me : others =>
         if (S (S'LAST) /= ')') or (S (S'LAST) /= ']') then    --  KLUDGE
            New_Line;
            Put_Line
              ("Extract Exception    WW = " & Integer'IMAGE (WW) &
               "    LINE = " & Integer'IMAGE (LINE_NUMBER));
            Put_Line (S);
            PUT (DE);
            New_Line;
         end if;

         Put_Line (Ada.Exceptions.Exception_Name (Catch_Me));
         Put_Line (Ada.Exceptions.Exception_Information (Catch_Me));
         Put_Line (Ada.Exceptions.Exception_Message (Catch_Me));
   end EXTRACT_WORDS;

begin

   -- Process command-line arguments
   if Ada.Command_Line.Argument_Count = 1 then
      for YY in 1 .. TRIM (Ada.Command_Line.Argument (1))'length loop
         case Upper_Case (TRIM (Ada.Command_Line.Argument (1)) (YY)) is

            when '-' =>
               exit when YY > 3;

            when 'G' =>
               D_K := GENERAL;
               Put_Line ("Working on GENERAL dictionary");

            when 'S' =>
               D_K := SPECIAL;
               Put_Line ("Working on GENERAL dictionary");

            when others =>
               New_Line;
               Put_Line
                 ("====== UNKNOWN COMMAND-LINE ARGUMENT(S) - RUNNING INTERACTIVELY  ======");
               New_Line;
               exit;
         end case;
      end loop;
   end if;   --  End command-line argument processing

   if D_K = XXX then

      Put_Line ("Takes a DICTLINE.D_K and produces a EWDSLIST.D_K ");
      Put ("What dictionary to list, GENERAL or SPECIAL  =>");

      Get_Line (LINE, LAST);
      if LAST > 0 then
         if TRIM (LINE (1 .. LAST)) (1) = 'G'
           or else TRIM (LINE (1 .. LAST)) (1) = 'g'
         then
            D_K := GENERAL;
            --  LINE_NUMBER := LINE_NUMBER + 1; -- Because of ESSE DICTFILE
            --  line -- no longer
         elsif TRIM (LINE (1 .. LAST)) (1) = 'S'
           or else TRIM (LINE (1 .. LAST)) (1) = 's'
         then
            D_K := SPECIAL;
         else
            Put_Line ("No such dictionary");
            raise Text_IO.Data_Error;
         end if;
      end if;
   end if;

   --PUT_LINE("OPENING   " &
   --     ADD_FILE_NAME_EXTENSION(DICT_LINE_NAME, DICTIONARY_KIND'IMAGE(D_K)));

   Open
     (INPUT, In_File,
      ADD_FILE_NAME_EXTENSION (DICT_LINE_NAME, DICTIONARY_KIND'IMAGE (D_K)));
   --PUT_LINE("OPEN");

   if not PORTING then
      --PUT_LINE("CREATING");

      Create
        (OUTPUT, Out_File,
         ADD_FILE_NAME_EXTENSION ("EWDSLIST", DICTIONARY_KIND'IMAGE (D_K)));

      if CHECKING then
         Create (CHECK, Out_File, "CHECKEWD.");
      end if;

      --PUT_LINE("CREATED");
   end if;

   --  Now do the rest
   OVER_LINES :
   while not End_Of_File (INPUT) loop
      S := BLANK_LINE;
      Get_Line (INPUT, S, LAST);
      if TRIM (S (1 .. LAST)) /= "" then       --  If non-blank line
         L := 0;

         FORM_DE :
         begin

            DE.STEMS (1) := S (START_STEM_1 .. MAX_STEM_SIZE);
            --NEW_LINE; PUT(DE.STEMS(1));
            DE.STEMS (2) :=
              S (START_STEM_2 .. START_STEM_2 + MAX_STEM_SIZE - 1);
            DE.STEMS (3) :=
              S (START_STEM_3 .. START_STEM_3 + MAX_STEM_SIZE - 1);
            DE.STEMS (4) :=
              S (START_STEM_4 .. START_STEM_4 + MAX_STEM_SIZE - 1);
            --PUT('#'); PUT(INTEGER'IMAGE(L)); PUT(INTEGER'IMAGE(LAST));
            --PUT('@');
            GET (S (START_PART .. LAST), DE.PART, L);
            --PUT('%'); PUT(INTEGER'IMAGE(L)); PUT(INTEGER'IMAGE(LAST));
            --PUT('&'); PUT(S(L+1..LAST)); PUT('3');
            --GET(S(L+1..LAST), DE.PART.POFS, DE.KIND, L);
            Get (S (L + 1 .. LAST), DE.TRAN.AGE, L);
            Get (S (L + 1 .. LAST), DE.TRAN.AREA, L);
            Get (S (L + 1 .. LAST), DE.TRAN.GEO, L);
            Get (S (L + 1 .. LAST), DE.TRAN.FREQ, L);
            Get (S (L + 1 .. LAST), DE.TRAN.SOURCE, L);
            DE.MEAN := HEAD (S (L + 2 .. LAST), MAX_MEANING_SIZE);
            --  Note that this allows initial blanks L+2 skips over the SPACER,
            --  required because this is STRING, not ENUM

         exception
            when others =>
               New_Line;
               Put_Line ("GET Exception  LAST = " & Integer'IMAGE (LAST));
               Put_Line (S (1 .. LAST));
               INTEGER_IO.Put (LINE_NUMBER);
               New_Line;
               PUT (DE);
               New_Line;
         end FORM_DE;

         LINE_NUMBER := LINE_NUMBER + 1;

         if DE.PART.POFS = V and then DE.PART.V.CON.WHICH = 8 then
            --  V 8 is a kludge for variant forms of verbs that have regular
            --  forms elsewhere
            null;
         else

            --  Extract words
            EXTRACT_WORDS
              (ADD_HYPHENATED (TRIM (DE.MEAN)), DE.PART.POFS, N, EWA);

            --      EWORD_SIZE    : constant := 38;
            --      AUX_WORD_SIZE : constant := 9;
            --      LINE_NUMBER_WIDTH : constant := 10;
            --
            --      type EWDS_RECORD is
            --        record
            --          POFS : PART_OF_SPEECH_TYPE := X;
            --          W    : STRING(1..EWORD_SIZE);
            --          AUX  : STRING(1..AUX_WORD_SIZE);
            --          N    : INTEGER;
            --        end record;

            for I in 1 .. N loop
               if TRIM (EWA (I).W)'LENGTH /= 0 then
                  EWR.W    := HEAD (TRIM (EWA (I).W), EWORD_SIZE);
                  EWR.AUX  := HEAD ("", AUX_WORD_SIZE);
                  EWR.N    := LINE_NUMBER;
                  EWR.POFS := DE.PART.POFS;
                  EWR.FREQ := DE.TRAN.FREQ;
                  EWR.SEMI := EWA (I).SEMI;
                  EWR.KIND := EWA (I).KIND;
                  EWR.RANK :=
                    80 - FREQUENCY_TYPE'POS (EWR.FREQ) * 10 + EWR.KIND +
                    (EWR.SEMI - 1) * (-3);
                  if EWR.FREQ = INFLECTIONS_PACKAGE.N then
                     EWR.RANK := EWR.RANK + 25;
                  end if;
                  --PUT(EWA(I)); NEW_LINE;
                  --PUT(EWR); NEW_LINE;
                  PUT (OUTPUT, EWR);

                  --                SET_COL(OUTPUT, 71);
                  --                INTEGER_IO.PUT(OUTPUT, I, 2);

                  New_Line (OUTPUT);

                  if CHECKING then
                     --  Now make the CHECK file

                     Put (CHECK, EWR.W);
                     Set_Col (CHECK, 25);
                     declare
                        DF : constant String := Dictionary_Form (DE);
                        II : Integer         := 1;
                     begin
                        if DF'LENGTH > 0 then
                           while DF (II) /= ' ' and DF (II) /= '.' and
                             DF (II) /= ','
                           loop
                              Put (CHECK, DF (II));
                              II := II + 1;
                              exit when II = 19;
                           end loop;
                        end if;
                     end;
                     Set_Col (CHECK, 44);
                     Put (CHECK, EWR.N, 6);
                     Put (CHECK, ' ');
                     Put (CHECK, EWR.POFS);
                     Put (CHECK, ' ');
                     Put (CHECK, EWR.FREQ);
                     Put (CHECK, ' ');
                     Put (CHECK, EWR.SEMI, 5);
                     Put (CHECK, ' ');
                     Put (CHECK, EWR.KIND, 5);
                     Put (CHECK, ' ');
                     Put (CHECK, EWR.RANK, 5);
                     Put (CHECK, ' ');
                     Put (CHECK, DE.MEAN);

                     New_Line (CHECK);
                  end if;
               end if;

            end loop;

         end if;  --  If non-blank line
      end if;
   end loop OVER_LINES;

   Put_Line ("NUMBER_OF_LINES = " & Integer'IMAGE (LINE_NUMBER));

   if not PORTING then
      Close (OUTPUT);
      if CHECKING then
         Close (CHECK);
      end if;
   end if;

exception
   when Text_IO.Data_Error =>
      null;
   when others =>
      Put_Line (S (1 .. LAST));
      INTEGER_IO.Put (LINE_NUMBER);
      New_Line;
      Close (OUTPUT);
      if CHECKING then
         Close (CHECK);
      end if;

end MAKEEWDS;
