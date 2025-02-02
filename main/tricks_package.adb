with Text_IO;
with STRINGS_PACKAGE;      use STRINGS_PACKAGE;
with WORD_PARAMETERS;      use WORD_PARAMETERS;
with DEVELOPER_PARAMETERS; use DEVELOPER_PARAMETERS;
with INFLECTIONS_PACKAGE;  use INFLECTIONS_PACKAGE;
with WORD_SUPPORT_PACKAGE; use WORD_SUPPORT_PACKAGE;
with WORD_PACKAGE;         use WORD_PACKAGE;

package body TRICKS_PACKAGE is

   function IS_A_VOWEL (C : Character) return Boolean is
   begin
      if Lower_Case (C) = 'a' or Lower_Case (C) = 'e' or
         Lower_Case (C) = 'i' or Lower_Case (C) = 'o' or Lower_Case (C) = 'u' or
         Lower_Case (C) = 'y'
      then
         return True;
      else
         return False;
      end if;
   end IS_A_VOWEL;

  -- SPR:  NOTE that this and the following two functions can be called, directly or indirectly,
  --       in situations where it's not efficient to convert the whole string to upper case first
   function A_ROMAN_DIGIT (CHAR : Character) return Boolean is
   begin
      case CHAR is
         when 'M' |
           'm' =>
            return
              True;
         when 'D' | 'd' =>
            return True;
         when 'C' | 'c' =>
            return True;
         when 'L' | 'l' =>
            return True;
         when 'X' | 'x' =>
            return True;
         when 'U' | 'u' | 'V' | 'v' =>
            return True;
         when 'I' |
           'i' =>   -- SPR:  Medieval exception for 'j' as 'i' in the last place of a numeral handled by ONLY_ROMAN_DIGITS
            return True;
         when others =>
            return False;
      end case;
   end A_ROMAN_DIGIT;

   function VALUE (CHAR : Character) return Natural is
   begin
      case CHAR is
         when 'M' | 'm' =>
            return 1_000;
         when 'D' | 'd' =>
            return 500;
         when 'C' | 'c' =>
            return 100;
         when 'L' | 'l' =>
            return 50;
         when 'X' | 'x' =>
            return 10;
         when 'U' | 'u' | 'V' | 'v' =>
            return 5;
         when 'I' | 'i' | 'J' | 'j' =>
            return 1;
         when others =>
            return 0;
      end case;
   end VALUE;

   function ONLY_ROMAN_DIGITS (S : String) return Boolean is
   begin

      for I in S'range loop
         --  SPECIAL CASE: Medieval scripts sometimes use "j" for "i" at the
         --  end of a numeral. Limit the exception to the last place because
         --  there are no significant traditions where every J is equivalent
         --  to an I (unlike U/V).
         if I = S'Last and (S (I) = 'J' or S (I) = 'j') then
            null;
         elsif not A_ROMAN_DIGIT (S (I)) then
            return False;
         end if;

      end loop;
      return True;
   end ONLY_ROMAN_DIGITS;

   function ROMAN_NUMBER (STRING_RAW : String) return Natural is
      --  Determines and returns the value of a Roman numeral, or 0 if invalid
      TOTAL : Natural := 0;
      INVALID : exception;
      J            : Integer         := 0;
      STRING_UPPER : constant String := Upper_Case (STRING_RAW);

   begin
      if ONLY_ROMAN_DIGITS (STRING_RAW) then

--
--NUMERALS IN A STRING ARE ADDED: CC = 200 ; CCX = 210.
--ONE NUMERAL TO THE LEFT of A LARGER NUMERAL IS SUBTRACTED FROM THAT NUMBER: IX = 9
--
--SUBTRACT ONLY A SINGLE LETTER FROM A SINGLE NUMERAL.
--VIII FOR 8, NOT IIX; 19 IS XIX, NOT IXX.
--
--SUBTRACT ONLY POWERS of TEN, SUCH AS I, X, or C.
--NOT VL FOR 45, BUT XLV.
--
--DON'T SUBTRACT A LETTER FROM ANOTHER LETTER MORE THAN TEN TIMES GREATER.
--ONLY SUBTRACT I FROM V or X, and X FROM L or C.
--NOT IL FOR 49, BUT XLIX. MIM is ILLEGAL.
--
--ONLY IF ANY NUMERAL PRECEEDING IS AT LEAST TEN TIMES LARGER.
--NOT VIX FOR 14, BUT XIV.
--NOT  IIX, BUT VIII.
--ONLY IF ANY NUMERAL FOLLOWING IS SMALLER.
--NOT XCL FOR 140, BUT CXL.
--
         J := STRING_UPPER'LAST;

         EVALUATE : -- loop
         while J >= STRING_UPPER'FIRST loop
--
--Legal in the Ones position
--  I
--  II
--  III
--  IIII    IV
--  V
--  VI
--  VII
--  VIII
--  VIIII   IX
--
--
            --  Ones; only position where I/J equivalence allowed (see comment
            --  in ONLY_ROMAN_DIGITS, above).
            if (STRING_UPPER (J) = 'I' or STRING_UPPER (J) = 'J') then
               TOTAL := TOTAL + 1;
               J     := J - 1;
               exit EVALUATE when J < STRING_UPPER'FIRST;
               while STRING_UPPER (J) = 'I' loop
                  TOTAL := TOTAL + 1;
                  if TOTAL >= 5 then
                     raise INVALID;
                  end if;
                  J := J - 1;
                  exit EVALUATE when J < STRING_UPPER'FIRST;
               end loop;
            end if;

            if (STRING_UPPER (J) = 'V') or (STRING_UPPER (J) = 'U') then
               TOTAL := TOTAL + 5;
               J     := J - 1;
               exit EVALUATE when J < STRING_UPPER'FIRST;

               if TOTAL = 5
                 and then (STRING_UPPER (J) = 'I' or STRING_UPPER (J) = 'J')
               then
                  TOTAL := TOTAL - 1;
                  J     := J - 1;
                  exit EVALUATE when J < STRING_UPPER'FIRST;
               end if;

               if STRING_UPPER (J) = 'I' or STRING_UPPER (J) = 'J' or
                 STRING_UPPER (J) = 'V' or STRING_UPPER (J) = 'U'
               then
                  raise INVALID;
               end if;
            end if;

--
--Legal in the tens position
--  X
--  XX
--  XXX
--  XXXX    XL
--  L
--  LX
--  LXX
--  LXXX
--  LXXXX   XC
--

            --  Tens
            if STRING_UPPER (J) = 'X' then
               TOTAL := TOTAL + 10;
               J     := J - 1;
               exit EVALUATE when J < STRING_UPPER'FIRST;
               while STRING_UPPER (J) = 'X' loop
                  TOTAL := TOTAL + 10;
                  if TOTAL >= 50 then
                     raise INVALID;
                  end if;
                  J := J - 1;
                  exit EVALUATE when J < STRING_UPPER'FIRST;
               end loop;
               if STRING_UPPER (J) = 'I' and TOTAL = 10 then
                  TOTAL := TOTAL - 1;
                  J     := J - 1;
                  exit EVALUATE when J < STRING_UPPER'FIRST;
               end if;
               if STRING_UPPER (J) = 'I' or STRING_UPPER (J) = 'V' or
                 STRING_UPPER (J) = 'U'
               then
                  raise INVALID;
               end if;
            end if;

            if STRING_UPPER (J) = 'L' then
               TOTAL := TOTAL + 50;
               J     := J - 1;
               exit EVALUATE when J < STRING_UPPER'FIRST;

               if STRING_UPPER (J) = 'X' and TOTAL <= 59 then
                  TOTAL := TOTAL - 10;
                  J     := J - 1;
                  exit EVALUATE when J < STRING_UPPER'FIRST;
               end if;
               if STRING_UPPER (J) = 'I' or STRING_UPPER (J) = 'V' or
                 STRING_UPPER (J) = 'U' or STRING_UPPER (J) = 'X' or
                 STRING_UPPER (J) = 'L'
               then
                  raise INVALID;
               end if;

               if STRING_UPPER (J) = 'C' then
                  TOTAL := TOTAL + 100;
                  J     := J - 1;
                  exit EVALUATE when J < STRING_UPPER'FIRST;
                  if STRING_UPPER (J) = 'X' and TOTAL = 100 then
                     TOTAL := TOTAL - 10;
                     J     := J - 1;
                     exit EVALUATE when J < STRING_UPPER'FIRST;
                  end if;
               end if;

               if STRING_UPPER (J) = 'I' or STRING_UPPER (J) = 'V' or
                 STRING_UPPER (J) = 'U' or STRING_UPPER (J) = 'X' or
                 STRING_UPPER (J) = 'L'
               then
                  raise INVALID;
               end if;
            end if;

            if STRING_UPPER (J) = 'C' then
               TOTAL := TOTAL + 100;
               J     := J - 1;
               exit EVALUATE when J < STRING_UPPER'FIRST;
               while STRING_UPPER (J) = 'C' loop
                  TOTAL := TOTAL + 100;
                  if TOTAL >= 500 then
                     raise INVALID;
                  end if;
                  J := J - 1;
                  exit EVALUATE when J < STRING_UPPER'FIRST;
               end loop;
               if STRING_UPPER (J) = 'X' and TOTAL <= 109 then
                  TOTAL := TOTAL - 10;
                  J     := J - 1;
                  exit EVALUATE when J < STRING_UPPER'FIRST;
               end if;
               if STRING_UPPER (J) = 'I' or STRING_UPPER (J) = 'V' or
                 STRING_UPPER (J) = 'U' or STRING_UPPER (J) = 'X' or
                 STRING_UPPER (J) = 'L'
               then
                  raise INVALID;
               end if;
            end if;

            if STRING_UPPER (J) = 'D' then
               TOTAL := TOTAL + 500;
               J     := J - 1;
               exit EVALUATE when J < STRING_UPPER'FIRST;
               if STRING_UPPER (J) = 'C' and TOTAL <= 599 then
                  TOTAL := TOTAL - 100;
                  J     := J - 1;
                  exit EVALUATE when J < STRING_UPPER'FIRST;
               end if;
               if STRING_UPPER (J) = 'M' then
                  TOTAL := TOTAL + 1_000;
                  J     := J - 1;
                  exit EVALUATE when J < STRING_UPPER'FIRST;
               end if;
               if STRING_UPPER (J) = 'C' and TOTAL <= 1_099 then
                  TOTAL := TOTAL - 100;
                  J     := J - 1;
                  exit EVALUATE when J < STRING_UPPER'FIRST;
               end if;
               if STRING_UPPER (J) = 'I' or STRING_UPPER (J) = 'V' or
                 STRING_UPPER (J) = 'U' or STRING_UPPER (J) = 'X' or
                 STRING_UPPER (J) = 'L' or STRING_UPPER (J) = 'C' or
                 STRING_UPPER (J) = 'D'
               then
                  raise INVALID;
               end if;
            end if;

            if STRING_UPPER (J) = 'M' then
               TOTAL := TOTAL + 1_000;
               J     := J - 1;
               exit EVALUATE when J < STRING_UPPER'FIRST;
               while STRING_UPPER (J) = 'M' loop
                  TOTAL := TOTAL + 1_000;
                  if TOTAL >= 5_000 then
                     raise INVALID;
                  end if;
                  J := J - 1;
                  exit EVALUATE when J < STRING_UPPER'FIRST;
               end loop;
               if STRING_UPPER (J) = 'C' and TOTAL <= 1_099 then
                  TOTAL := TOTAL - 100;
                  J     := J - 1;
                  exit EVALUATE when J < STRING_UPPER'FIRST;
               end if;
               if STRING_UPPER (J) = 'I' or STRING_UPPER (J) = 'V' or
                 STRING_UPPER (J) = 'U' or STRING_UPPER (J) = 'X' or
                 STRING_UPPER (J) = 'L' or STRING_UPPER (J) = 'C' or
                 STRING_UPPER (J) = 'D'
               then
                  raise INVALID;
               end if;
            end if;

         end loop EVALUATE;

      end if;  --  On Only Roman digits

      return TOTAL;
   exception
      when INVALID =>
         return 0;
      when Constraint_Error =>
         return 0;
   end ROMAN_NUMBER;

   function BAD_ROMAN_NUMBER (S : String) return Natural is
      --  Determines and returns the value of a Roman numeral, or 0 if invalid
      --  This seems to allow all of Caesar's. Actually there are no rules if
      --  you look at some of the 12-15 century stuff
      TOTAL            : Integer := 0;
      DECREMENTED_FROM : Integer := 0;

   begin

      --  Already known that all the characters may be valid numerals Loop over
      --  the string to check validity, start with second place
      --PUT_LINE(" In function BAD_ROMAN_NUMBER ");
      --PUT_LINE(" BEFORE LOOP      S = " & S);
      TOTAL            := VALUE (S (S'LAST));
      DECREMENTED_FROM := VALUE (S (S'LAST));
      for I in reverse S'FIRST .. S'LAST - 1 loop

         if VALUE (S (I)) < VALUE (S (I + 1)) then
            --  Decrement
            TOTAL            := TOTAL - VALUE (S (I));
            DECREMENTED_FROM := VALUE (S (I + 1));
         elsif VALUE (S (I)) = VALUE (S (I + 1)) then
            if VALUE (S (I)) < DECREMENTED_FROM then
               TOTAL := TOTAL - VALUE (S (I));   --  IIX = 8 !
            else
               TOTAL := TOTAL + VALUE (S (I));
            end if;
         elsif VALUE (S (I)) > VALUE (S (I + 1)) then
            TOTAL            := TOTAL + VALUE (S (I));
            DECREMENTED_FROM := VALUE (S (I + 1));
         end if;
      end loop;
      if TOTAL > 0 then
         return TOTAL;
      else
         return 0;
      end if;

   exception
      when others =>
         return 0;
   end BAD_ROMAN_NUMBER;

   function Roman_Numeral_Age (S : in String) return AGE_TYPE is
   begin
   -- Principle: When translating FROM Latin (this function), identify a specific or extreme age only in very clear cases to avoid a false sense of certainty.

      if S (S'Last) = 'j' or S (S'Last) = 'J' then
         return F; -- terminal j is medieval form
    elsif ( ROMAN_NUMBER (S) in 1 .. 500 or ROMAN_NUMBER (S) = 600 or ROMAN_NUMBER (S) = 700 or
            ROMAN_NUMBER (S) = 800 or ROMAN_NUMBER (S) = 900 or ROMAN_NUMBER (S) = 10_000   ) then
         return
           X; -- valid subtractive form within the range of numbers used at all times; no further tests can disambiguate (see comment above)
      elsif BAD_ROMAN_NUMBER (S) in 1 .. 300
      then    -- MUST come after Roman_Number test because Bad_Roman_Number does BOTH subtractive and additive arithmetic
         return
           B;  -- small numbers, additive forms => archaic through classical period => mark as "early" (see comment above)
       elsif BAD_ROMAN_NUMBER (S) > 1_000 Then
    return F;   -- medieval
    else
      return X;
    end if;

   end Roman_Numeral_Age;

   procedure ROMAN_NUMERALS
     (INPUT_WORD : String; PA : in out PARSE_ARRAY; PA_LAST : in out Integer)
   is

      W              : constant String  := TRIM (INPUT_WORD);
      ROMAN_NUMBER_W : constant Integer := ROMAN_NUMBER (W);

   begin
      if ONLY_ROMAN_DIGITS (W) and then (ROMAN_NUMBER_W /= 0) then
         PA_LAST      := PA_LAST + 1;
         PA (PA_LAST) :=
           (STEM => HEAD (W, MAX_STEM_SIZE),
            IR   =>
              (QUAL =>
                 (POFS => NUM,
                  NUM  =>
                    (DECL => (2, 0), CS => X, NUMBER => X, GENDER => X,
                     SORT => CARD)),

               KEY => 0, ENDING => NULL_ENDING_RECORD,
               AGE => Roman_Numeral_Age (W), FREQ => A),
            D_K => RRR, MNPC => NULL_MNPC);
         RRR_MEANING (RRR_MEANING_COUNTER) :=
           HEAD
             (Integer'IMAGE (ROMAN_NUMBER_W) & " as a ROMAN NUMERAL;",
              MAX_MEANING_SIZE);
         RRR_MEANING_COUNTER := RRR_MEANING_COUNTER + 1;
      else
         null;    --  Is not ROMAN NUMERAL, so go on and try something else
      end if;
   end ROMAN_NUMERALS;

   procedure SYNCOPE
     (W : String; PA : in out PARSE_ARRAY; PA_LAST : in out Integer)
   is
      S : constant String (1 .. W'LENGTH) := Lower_Case (W);
      PA_SAVE                   : Integer                         := PA_LAST;
      SYNCOPE_INFLECTION_RECORD : INFLECTION_RECORD := NULL_INFLECTION_RECORD;
      --     ((V, ((0, 0), (X, X, X), 0, X, X)), 0, NULL_ENDING_RECORD, X, A);
   begin

      --  Syncopated forms (see Gildersleeve and Lodge, 131)

      YYY_MEANING := Null_Special_Meaning_Array;

      --  This one has to go first -- special for 3 4 ivi => ii , in perfect
      --  (esp. for V 3 4) This is handled in WORDS as syncope It seems to
      --  appear in texts as alternative stems ii and ivi
      for I in reverse S'FIRST .. S'LAST - 1 loop
         if (S (I .. I + 1) = "ii") then
            PA_LAST      := PA_LAST + 1;
            PA (PA_LAST) :=
              ("Syncope  ii => ivi", SYNCOPE_INFLECTION_RECORD, YYY,
               NULL_MNPC);
            WORD (S (S'FIRST .. I) & "v" & S (I + 1 .. S'LAST), PA, PA_LAST);
            if PA_LAST > PA_SAVE + 1 then
               exit;
            end if;
         end if;
         PA_LAST := PA_SAVE;     --  No luck, or it would have exited above
      end loop;
      if PA_LAST > PA_SAVE + 1 and then PA (PA_LAST).IR.QUAL.POFS = V
        and then
         --PA(PA_LAST).IR.QUAL.V.CON = (3, 4)/(6, 1) and then
         PA (PA_LAST).IR.KEY = 3
      then          --  Perfect system
         YYY_MEANING (YYY_MEANING_COUNTER) :=
           HEAD
             ("Syncopated perfect ivi can drop 'v' without contracting vowel ",
              MAX_MEANING_SIZE);
         YYY_MEANING_COUNTER := YYY_MEANING_COUNTER + 1;
         return;
      else
         PA_LAST := PA_SAVE;
      end if;

      -- avis => as, evis => es, ivis => is, ovis => os in perfect
      for I in reverse S'FIRST .. S'LAST - 2 loop     --  Need isse
         if
           ((S (I .. I + 1) = "as") or (S (I .. I + 1) = "es") or
            (S (I .. I + 1) = "is") or (S (I .. I + 1) = "os"))
         then
            --TEXT_IO.PUT_LINE("SYNCOPE vis   S = " & S & "    PA_SAVE = " & INTEGER'IMAGE(PA_SAVE));
            PA_LAST      := PA_LAST + 1;
            PA (PA_LAST) :=
              ("Syncope   s => vis", SYNCOPE_INFLECTION_RECORD, YYY,
               NULL_MNPC);
            --TEXT_IO.PUT_LINE("SYNCOPE vis   S+ = " & S(S'FIRST..I) & "vi" & S(I+1..S'LAST) & "  " & INTEGER'IMAGE(PA_LAST));
            WORD (S (S'FIRST .. I) & "vi" & S (I + 1 .. S'LAST), PA, PA_LAST);
            --TEXT_IO.PUT_LINE("SYNCOPE vis   DONE "  & "    PA_LAST = " & INTEGER'IMAGE(PA_LAST));
            if PA_LAST > PA_SAVE + 1 then
               exit;               --  Exit loop here if SYNCOPE found hit
            end if;
         end if;
         PA_LAST := PA_SAVE;     --  No luck, or it would have exited above
      end loop;
      --  Loop over the resulting solutions
      if PA_LAST > PA_SAVE + 1 and then PA (PA_LAST).IR.QUAL.POFS = V
        and then PA (PA_LAST).IR.KEY = 3
      then          --  Perfect system
         YYY_MEANING (YYY_MEANING_COUNTER) :=
           HEAD
             ("Syncopated perfect often drops the 'v' and contracts vowel ",
              MAX_MEANING_SIZE);
         YYY_MEANING_COUNTER := YYY_MEANING_COUNTER + 1;
      end if;
      --  end loop; -- over resulting solutions
      if PA_LAST > PA_SAVE + 1 then

         return;

      else
         PA_LAST := PA_SAVE;
      end if;

      -- aver => ar, ever => er, in perfect
      for I in reverse S'FIRST + 1 .. S'LAST - 2 loop
         if
           ((S (I .. I + 1) = "ar") or (S (I .. I + 1) = "er") or
            (S (I .. I + 1) = "or"))
         then
            PA_LAST      := PA_LAST + 1;
            PA (PA_LAST) :=
              ("Syncope   r => v.r", SYNCOPE_INFLECTION_RECORD, YYY,
               NULL_MNPC);
            WORD (S (S'FIRST .. I) & "ve" & S (I + 1 .. S'LAST), PA, PA_LAST);
            if PA_LAST > PA_SAVE + 1 then
               exit;
            end if;
         end if;
         PA_LAST := PA_SAVE;     --  No luck, or it would have exited above
      end loop;

      if PA_LAST > PA_SAVE + 1 and then PA (PA_LAST).IR.QUAL.POFS = V
        and then PA (PA_LAST).IR.KEY = 3
      then          --  Perfect system
         YYY_MEANING (YYY_MEANING_COUNTER) :=
           HEAD
             ("Syncopated perfect often drops the 'v' and contracts vowel ",
              MAX_MEANING_SIZE);
         YYY_MEANING_COUNTER := YYY_MEANING_COUNTER + 1;
         return;
      else
         PA_LAST := PA_SAVE;
      end if;

      -- iver => ier,  in perfect
      for I in reverse S'FIRST .. S'LAST - 3 loop
         if (S (I .. I + 2) = "ier") then
            PA_LAST      := PA_LAST + 1;
            PA (PA_LAST) :=
              ("Syncope  ier=>iver", SYNCOPE_INFLECTION_RECORD, YYY,
               NULL_MNPC);
            WORD (S (S'FIRST .. I) & "v" & S (I + 1 .. S'LAST), PA, PA_LAST);
            if PA_LAST > PA_SAVE + 1 then
               exit;
            end if;
         end if;
         PA_LAST := PA_SAVE;     --  No luck, or it would have exited above
      end loop;
      if PA_LAST > PA_SAVE + 1 and then PA (PA_LAST).IR.QUAL.POFS = V
        and then PA (PA_LAST).IR.KEY = 3
      then          --  Perfect system
         YYY_MEANING (YYY_MEANING_COUNTER) :=
           HEAD
             ("Syncopated perfect often drops the 'v' and contracts vowel ",
              MAX_MEANING_SIZE);
         YYY_MEANING_COUNTER := YYY_MEANING_COUNTER + 1;
         return;
      else
         PA_LAST := PA_SAVE;
      end if;

--         -- sis => s, xis => x, in perfect
      for I in reverse S'FIRST .. S'LAST - 2 loop
         if ((S (I) = 's') or (S (I) = 'x')) then
            PA_LAST      := PA_LAST + 1;
            PA (PA_LAST) :=
              ("Syncope s/x => +is", SYNCOPE_INFLECTION_RECORD, YYY,
               NULL_MNPC);
            WORD (S (S'FIRST .. I) & "is" & S (I + 1 .. S'LAST), PA, PA_LAST);
            if PA_LAST > PA_SAVE + 1 then
               exit;               --  Exit loop here if SYNCOPE found hit
            end if;
         end if;
         PA_LAST := PA_SAVE;     --  No luck, or it would have exited above
      end loop;
      --  Loop over the resulting solutions
      if PA_LAST > PA_SAVE + 1 and then PA (PA_LAST).IR.QUAL.POFS = V
        and then PA (PA_LAST).IR.KEY = 3
      then          --  Perfect system
         YYY_MEANING (YYY_MEANING_COUNTER) :=
           HEAD
             ("Syncopated perfect sometimes drops the 'is' after 's' or 'x' ",
              MAX_MEANING_SIZE);
         YYY_MEANING_COUNTER := YYY_MEANING_COUNTER + 1;
         return;
      else
         PA_LAST := PA_SAVE;
      end if;

      --  end loop; -- over resulting solutions
      if PA_LAST > PA_SAVE + 1 then

         return;

      else
         PA_LAST := PA_SAVE;
      end if;

      PA (PA_LAST + 1) := NULL_PARSE_RECORD;     --  Just to clear the trys

   exception
      when others =>
         PA_LAST          := PA_SAVE;
         PA (PA_LAST + 1) := NULL_PARSE_RECORD;     --  Just to clear the trys

   end SYNCOPE;

   procedure TRY_TRICKS
     (W : String; PA : in out PARSE_ARRAY; PA_LAST : in out Integer)
   is
      --  Since the chances are 1/1000 that we have one, Ignore the possibility
      --  of two in the same word That is called lying with statistics
      S       : constant String (1 .. W'LENGTH) := W;
      PA_SAVE : Integer                         := PA_LAST;

      procedure TWORD
        (W : String; PA : in out PARSE_ARRAY; PA_LAST : in out Integer)
      is
      begin
         WORD_PACKAGE.WORD (W, PA, PA_LAST);
         SYNCOPE (W, PA, PA_LAST);
      end TWORD;

      procedure FLIP (X1, X2 : String; EXPLANATION : String := "") is
         --  At the begining of input word, replaces X1 by X2
         PA_SAVE : Integer := PA_LAST;
      begin
         if S'LENGTH >= X1'LENGTH + 2
           and then S (S'FIRST .. S'FIRST + X1'LENGTH - 1) = X1
         then
            PA_LAST      := PA_LAST + 1;
            PA (PA_LAST) :=
              (HEAD ("Word mod " & X1 & "/" & X2, MAX_STEM_SIZE),
               NULL_INFLECTION_RECORD, XXX, NULL_MNPC);
            TWORD (X2 & S (S'FIRST + X1'LENGTH .. S'LAST), PA, PA_LAST);
            if (PA_LAST > PA_SAVE + 1)
              and then (PA (PA_LAST - 1).IR.QUAL.POFS /= TACKON)
            then
               if EXPLANATION = "" then
                  XXX_MEANING (XXX_MEANING_COUNTER) :=
                    HEAD
                      ("An initial '" & X1 & "' may have replaced usual '" &
                       X2 & "'",
                       MAX_MEANING_SIZE);
                  XXX_MEANING_COUNTER := XXX_MEANING_COUNTER + 1;
               else
                  XXX_MEANING (XXX_MEANING_COUNTER) :=
                    HEAD (EXPLANATION, MAX_MEANING_SIZE);
                  XXX_MEANING_COUNTER := XXX_MEANING_COUNTER + 1;
               end if;
               return;
            else
               PA_LAST := PA_SAVE;
            end if;
         end if;
         PA_LAST := PA_SAVE;
      end FLIP;

      procedure FLIP_FLOP (X1, X2 : String; EXPLANATION : String := "") is
         --  At the begining of input word, replaces X1 by X2 - then X2 by
         --  X1 To be used only when X1 and X2 start with the same letter
         --  because it will be called from a point where the first letter
         --  is established
         PA_SAVE : Integer := PA_LAST;
      begin
--TEXT_IO.PUT_LINE("FLIP_FLOP called    " & X1 & "  " & X2);
         if S'LENGTH >= X1'LENGTH + 2
           and then S (S'FIRST .. S'FIRST + X1'LENGTH - 1) = X1
         then
            PA_LAST      := PA_LAST + 1;
            PA (PA_LAST) :=
              (HEAD ("Word mod " & X1 & "/" & X2, MAX_STEM_SIZE),
               NULL_INFLECTION_RECORD, XXX, NULL_MNPC);
            --TEXT_IO.PUT_LINE("INSERTED MODIFICATION");
            --TEXT_IO.PUT_LINE("Trying " & X2 & S(S'FIRST+X1'LENGTH..S'LAST));
            TWORD (X2 & S (S'FIRST + X1'LENGTH .. S'LAST), PA, PA_LAST);
            if (PA_LAST > PA_SAVE + 1)
              and then (PA (PA_LAST - 1).IR.QUAL.POFS /= TACKON)
            then
               --TEXT_IO.PUT_LINE("FLIP_FLOP worked");
               if EXPLANATION = "" then
                  XXX_MEANING (XXX_MEANING_COUNTER) :=
                    HEAD
                      ("An initial '" & X1 & "' may be rendered by '" & X2 &
                       "'",
                       MAX_MEANING_SIZE);
                  XXX_MEANING_COUNTER := XXX_MEANING_COUNTER + 1;
               else
                  XXX_MEANING (XXX_MEANING_COUNTER) :=
                    HEAD (EXPLANATION, MAX_MEANING_SIZE);
                  XXX_MEANING_COUNTER := XXX_MEANING_COUNTER + 1;
               end if;
               return;
            else
               PA_LAST := PA_SAVE;
            end if;
         end if;
         --TEXT_IO.PUT_LINE("FLIPF failed");
         --TEXT_IO.PUT_LINE("Try FFLOP");

         if S'LENGTH >= X2'LENGTH + 2
           and then S (S'FIRST .. S'FIRST + X2'LENGTH - 1) = X2
         then
            --TEXT_IO.PUT_LINE("Trying FFLOP");
            PA_LAST      := PA_LAST + 1;
            PA (PA_LAST) :=
              (HEAD ("Word mod " & X2 & "/" & X1, MAX_STEM_SIZE),
               NULL_INFLECTION_RECORD, XXX, NULL_MNPC);
            --TEXT_IO.PUT_LINE("Trying " & X1 & S(S'FIRST+X2'LENGTH..S'LAST));
            TWORD (X1 & S (S'FIRST + X2'LENGTH .. S'LAST), PA, PA_LAST);
            if (PA_LAST > PA_SAVE + 1)
              and then (PA (PA_LAST - 1).IR.QUAL.POFS /= TACKON)
            then
               --TEXT_IO.PUT_LINE("FFLOP worked");
               if EXPLANATION = "" then
                  XXX_MEANING (XXX_MEANING_COUNTER) :=
                    HEAD
                      ("An initial '" & X2 & "' may be rendered by '" & X1 &
                       "'",
                       MAX_MEANING_SIZE);
                  XXX_MEANING_COUNTER := XXX_MEANING_COUNTER + 1;
               else
                  XXX_MEANING (XXX_MEANING_COUNTER) :=
                    HEAD (EXPLANATION, MAX_MEANING_SIZE);
                  XXX_MEANING_COUNTER := XXX_MEANING_COUNTER + 1;
               end if;
               return;
            else
               PA_LAST := PA_SAVE;
            end if;

         end if;
         --TEXT_IO.PUT_LINE("FFLIP failed");
         PA_LAST := PA_SAVE;
      end FLIP_FLOP;

      procedure INTERNAL (X1, X2 : String; EXPLANATION : String := "") is
         --  Replaces X1 with X2 anywhere in word and tries it for validity
         PA_SAVE : Integer := PA_LAST;
      begin
         for I in S'FIRST .. S'LAST - X1'LENGTH + 1 loop
            if S (I .. I + X1'LENGTH - 1) = X1 then
               PA_LAST := PA_LAST + 1;

               if X2 = "" then
                  PA (PA_LAST) :=
                    (HEAD ("Word mod letter H " & X1, MAX_STEM_SIZE),
                     NULL_INFLECTION_RECORD, XXX, NULL_MNPC);
               else
                  PA (PA_LAST) :=
                    (HEAD ("Word mod " & X1 & "/" & X2, MAX_STEM_SIZE),
                     NULL_INFLECTION_RECORD, XXX, NULL_MNPC);
               end if;
               TWORD
                 (S (S'FIRST .. I - 1) & X2 & S (I + X1'LENGTH .. S'LAST), PA,
                  PA_LAST);
               if (PA_LAST > PA_SAVE + 1)
                 and then (PA (PA_LAST - 1).IR.QUAL.POFS /= TACKON)
               then
                  if EXPLANATION = "" then
                     if X1 = "h" and then X2 = "" then
                        XXX_MEANING (XXX_MEANING_COUNTER) :=
                          HEAD
                            ("The letter H may be dropped or added; it may be silent or pronounced",
                             MAX_MEANING_SIZE);
                        XXX_MEANING_COUNTER := XXX_MEANING_COUNTER + 1;
                     else
                        XXX_MEANING (XXX_MEANING_COUNTER) :=
                          HEAD
                            ("An internal '" & X1 &
                             "' might be rendered by '" & X2 & "'",
                             MAX_MEANING_SIZE);
                        XXX_MEANING_COUNTER := XXX_MEANING_COUNTER + 1;
                     end if;
                  else
                     XXX_MEANING (XXX_MEANING_COUNTER) :=
                       HEAD (EXPLANATION, MAX_MEANING_SIZE);
                     XXX_MEANING_COUNTER := XXX_MEANING_COUNTER + 1;
                  end if;
                  return;
               else
                  PA_LAST := PA_SAVE;
               end if;
            end if;
         end loop;
         PA_LAST := PA_SAVE;
      end INTERNAL;

      procedure ADJ_TERMINAL_IIS (EXPLANATION : String := "") is
         PA_SAVE                  : Integer            := PA_LAST;
         I                        : Integer            := 0;
         TRICK_TRANSLATION_RECORD : TRANSLATION_RECORD :=
           NULL_TRANSLATION_RECORD;
      begin
         if S'LENGTH > 3 and then S (S'LAST - 1 .. S'LAST) = "is"
         then   --  Terminal 'is'
            PA_LAST                       := PA_LAST + 1;
            TRICK_TRANSLATION_RECORD.FREQ := C;
            PA (PA_LAST)                  :=
              (HEAD (" iis -> is", MAX_STEM_SIZE), NULL_INFLECTION_RECORD, XXX,
               NULL_MNPC);
            WORD (S (S'FIRST .. S'LAST - 2) & "iis", PA, PA_LAST);
            if (PA_LAST > PA_SAVE + 1) then
               I := PA_LAST;
               while I > PA_SAVE + 1 loop
                  if PA (I).IR.QUAL.POFS = ADJ
                    and then PA (I).IR.QUAL.ADJ.DECL = (1, 1)
                    and then
                    ((PA (I).IR.QUAL.ADJ.CS = DAT) or
                     (PA (I).IR.QUAL.ADJ.CS = ABL))
                    and then PA (I).IR.QUAL.ADJ.NUMBER = P
                  then
                     null;       --  Only for ADJ 1 1 DAT/ABL P
                  else
                     PA (I .. PA_LAST - 1) := PA (I + 1 .. PA_LAST);
                     PA_LAST               := PA_LAST - 1;
                  end if;
                  I := I - 1;
               end loop;
            end if;
            if (PA_LAST > PA_SAVE + 1) then
               if EXPLANATION = "" then
                  XXX_MEANING (XXX_MEANING_COUNTER) :=
                    HEAD
                      ("A Terminal 'iis' on ADJ 1 1 DAT/ABL P might drop 'i'",
                       MAX_MEANING_SIZE);
                  XXX_MEANING_COUNTER := XXX_MEANING_COUNTER + 1;
               else
                  XXX_MEANING (XXX_MEANING_COUNTER) :=
                    HEAD (EXPLANATION, MAX_MEANING_SIZE);
                  XXX_MEANING_COUNTER := XXX_MEANING_COUNTER + 1;
               end if;
               return;
            else
               PA_LAST := PA_SAVE;
            end if;
         end if;
         PA_LAST := PA_SAVE;
      end ADJ_TERMINAL_IIS;

      procedure DOUBLE_CONSONANTS (EXPLANATION : String := "") is
         PA_SAVE : Integer := PA_LAST;
      begin
         --  Medieval often replaced a classical doubled consonant with single
         --  The problem is to take possible medieval words and double (all)
         --  (isolated) consonants
         for I in S'FIRST + 1 .. S'LAST - 1
         loop  --  probably dont need to go to end
            if (not IS_A_VOWEL (S (I)))
              and then (IS_A_VOWEL (S (I - 1)) and IS_A_VOWEL (S (I + 1)))
            then
               PA_LAST      := PA_LAST + 1;
               PA (PA_LAST) :=
                 (HEAD
                    ("Word mod " & S (I) & " => " & S (I) & S (I),
                     MAX_STEM_SIZE),
                  NULL_INFLECTION_RECORD, XXX, NULL_MNPC);
               TWORD
                 (S (S'FIRST .. I) & S (I) & S (I + 1 .. S'LAST), PA, PA_LAST);
               --TEXT_IO.PUT_LINE(S(S'FIRST..I) & S(I) & S(I+1..S'LAST));
               if (PA_LAST > PA_SAVE + 1)
                 and then (PA (PA_LAST - 1).IR.QUAL.POFS /= TACKON)
               then
                  if EXPLANATION = "" then
                     XXX_MEANING (XXX_MEANING_COUNTER) :=
                       HEAD
                         ("A doubled consonant may be rendered by just the single" &
                          "  MEDIEVAL",
                          MAX_MEANING_SIZE);
                     XXX_MEANING_COUNTER := XXX_MEANING_COUNTER + 1;
                  else
                     XXX_MEANING (XXX_MEANING_COUNTER) :=
                       HEAD (EXPLANATION, MAX_MEANING_SIZE);
                     XXX_MEANING_COUNTER := XXX_MEANING_COUNTER + 1;
                  end if;
                  return;
               else
                  PA_LAST := PA_SAVE;
               end if;

            end if;
         end loop;
         PA_LAST := PA_SAVE;
      end DOUBLE_CONSONANTS;

      procedure TWO_WORDS (EXPLANATION : String := "") is
         --  This procedure examines the word to determine if it is made up of
         --  two separate inflectted words They are usually an adjective and a
         --  noun or two nouns
         PA_SAVE, PA_SECOND       : Integer := PA_LAST;
         NUM_HIT_ONE, NUM_HIT_TWO : Boolean := False;
         --MID : INTEGER := S'LENGTH/2;
         I, I_MID         : Integer := 0;
         REMEMBER_SYNCOPE : Boolean := False;
         procedure WORDS_NO_SYNCOPE
           (W : String; PA : in out PARSE_ARRAY; PA_LAST : in out Integer)
         is
         begin
            if WORDS_MDEV (DO_SYNCOPE) then
               REMEMBER_SYNCOPE        := True;
               WORDS_MDEV (DO_SYNCOPE) := False;
            end if;
            WORD_PACKAGE.WORD (W, PA, PA_LAST);
            if REMEMBER_SYNCOPE then
               WORDS_MDEV (DO_SYNCOPE) := True;
            end if;
         end WORDS_NO_SYNCOPE;

         function COMMON_PREFIX (S : String) return Boolean is
         --  Common prefixes that have corresponding words (prepositions
         --  usually) which could confuse TWO_WORDS. We wish to reject these.
         begin
            if S = "dis" or S = "ex" or S = "in" or S = "per" or S = "prae" or
              S = "pro" or S = "re" or S = "si" or S = "sub" or S = "super" or
              S = "trans"
            then
               return True;
            else
               return False;
            end if;
         end COMMON_PREFIX;

      begin
--TEXT_IO.PUT_LINE("Entering TWO_WORDS  PA_LAST = " & INTEGER'IMAGE(PA_LAST));
         --if S(S'FIRST) /= 'q'  then    --  qu words more complicated

         if S'LENGTH < 5 then    --  Dont try on too short words
            return;
         end if;

         I :=
           2;    --  Smallest is re-publica, but that killed by PREFIX, meipsum
         OUTER_LOOP :
         while I < S'LENGTH - 2 loop

            PA_LAST      := PA_LAST + 1;
            PA (PA_LAST) :=
              (HEAD ("Two words", MAX_STEM_SIZE), NULL_INFLECTION_RECORD, XXX,
               NULL_MNPC);
            --TEXT_IO.PUT_LINE("Setting PA TWO_WORDS  PA_LAST = " & INTEGER'IMAGE(PA_LAST));

            while I < S'LENGTH - 2 loop
               --TEXT_IO.PUT_LINE("Trying  " & S(S'FIRST..S'FIRST+I-1));
               if not COMMON_PREFIX (S (S'FIRST .. S'FIRST + I - 1)) then
                  WORDS_NO_SYNCOPE
                    (S (S'FIRST .. S'FIRST + I - 1), PA, PA_LAST);
                  if (PA_LAST > PA_SAVE + 1) then
                     I_MID := I;
                     for J in PA_SAVE + 1 .. PA_LAST loop
                        if PA (J).IR.QUAL.POFS = NUM then
                           NUM_HIT_ONE := True;
                           exit;
                        end if;
                     end loop;

                     --TEXT_IO.PUT_LINE("HIT first  " & S(S'FIRST..I_MID-1) & "  PA_LAST = " & INTEGER'IMAGE(PA_LAST));
                     --PARSE_RECORD_IO.PUT(PA(PA_LAST)); TEXT_IO.NEW_LINE;

                     exit;
                  end if;
               end if;
               I := I + 1;
            end loop;

            if (PA_LAST > PA_SAVE + 1) then
               null;
               --TEXT_IO.PUT_LINE("Confirm first  " & S(S'FIRST..I_MID) & "    PA_LAST =" & INTEGER'IMAGE(PA_LAST));
            else
               --TEXT_IO.PUT_LINE("No possible first  " & S(S'FIRST..I_MID));
               PA_LAST := PA_SAVE;
               return;
            end if;

            --  Now for second word
            --TEXT_IO.PUT_LINE("Looking for second  >" & S(I_MID+1..S'LAST));
            PA_LAST      := PA_LAST + 1;
            PA (PA_LAST) := NULL_PARSE_RECORD;     --  Separator
            PA_SECOND    := PA_LAST;
            WORDS_NO_SYNCOPE (S (I_MID + 1 .. S'LAST), PA, PA_LAST);
            if (PA_LAST > PA_SECOND)
              and then       --  No + 1 since XXX taken care of above

              (PA (PA_LAST - 1).IR.QUAL.POFS /= TACKON)
            then
               for J in PA_SECOND .. PA_LAST loop
                  if PA (J).IR.QUAL.POFS = NUM then
                     NUM_HIT_TWO := True;
                     exit;
                  end if;
               end loop;

               --TEXT_IO.PUT_LINE("Found       second  " & S(I_MID+1..S'LAST) & "  PA_LAST = " & INTEGER'IMAGE(PA_LAST));

               if EXPLANATION = "" then

                  if WORDS_MODE (TRIM_OUTPUT)
                    and then
                     --  Should check that cases correspond
                     (NUM_HIT_ONE and NUM_HIT_TWO)
                  then
                     --  Clear out any non-NUM if we are in TRIM
                     for J in PA_SAVE + 1 .. PA_LAST loop
                        if PA (J).D_K in GENERAL .. UNIQUE
                          and then PA (J).IR.QUAL.POFS /= NUM
                        then
                           PA (J .. PA_LAST - 1) := PA (J + 1 .. PA_LAST);
                           PA_LAST               := PA_LAST - 1;
                        end if;
                     end loop;

                     XXX_MEANING (XXX_MEANING_COUNTER) :=
                       HEAD
                         ("Likely a compound number    " &
                          S (S'FIRST .. S'FIRST + I - 1) & " + " &
                          S (S'FIRST + I .. S'LAST),
                          MAX_MEANING_SIZE);
                     XXX_MEANING_COUNTER := XXX_MEANING_COUNTER + 1;

                  else
                     XXX_MEANING (XXX_MEANING_COUNTER) :=
                       HEAD
                         ("May be 2 words combined (" &
                          S (S'FIRST .. S'FIRST + I - 1) & "+" &
                          S (S'FIRST + I .. S'LAST) &
                          ") - if not obvious, probably incorrect",
                          MAX_MEANING_SIZE);
                     XXX_MEANING_COUNTER := XXX_MEANING_COUNTER + 1;
                  end if;
               else
                  XXX_MEANING (XXX_MEANING_COUNTER) :=
                    HEAD (EXPLANATION, MAX_MEANING_SIZE);
                  XXX_MEANING_COUNTER := XXX_MEANING_COUNTER + 1;
               end if;

               --TEXT_IO.PUT_LINE("Returing from 2WDS  PA_SAVE+1 = " & INTEGER'IMAGE(PA_SAVE+1) & "  " & PA(PA_SAVE+1).STEM);

               return;
            else
               PA_LAST := PA_SAVE;
            end if;

            I := I + 1;
         end loop OUTER_LOOP;

         PA_LAST := PA_SAVE;   --  No success, so reset to clear the TRICK PA

         --  I could try to check cases/gender/number for matches Discard all
         --  that do not have a match ADJ, N, NUM But that is probably being
         --  too pedantic for a case which may be sloppy
      end TWO_WORDS;

   --------------------------------------------------------------------------
   --------------------------------------------------------------------------
   --------------------------------------------------------------------------
   --------------------------------------------------------------------------

   begin
      --  These things might be genericized, at least the PA(1) assignments
--TEXT_IO.PUT_LINE("TRICKS called");

      XXX_MEANING := Null_Special_Meaning_Array;

      --  If there is no satisfaction from above, we will try further

      case S (S'FIRST) is

         when 'a' =>

            FLIP_FLOP ("adgn", "agn");
            if PA_LAST > 0 then
               return;
            end if;
            FLIP_FLOP ("adsc", "asc");
            if PA_LAST > 0 then
               return;
            end if;
            FLIP_FLOP ("adsp", "asp");
            if PA_LAST > 0 then
               return;
            end if;
            FLIP_FLOP ("arqui", "arci");
            if PA_LAST > 0 then
               return;
            end if;
            FLIP_FLOP ("arqu", "arcu");
            if PA_LAST > 0 then
               return;
            end if;
            FLIP ("ae", "e");
            if PA_LAST > 0 then
               return;
            end if;
            FLIP ("al", "hal");
            if PA_LAST > 0 then
               return;
            end if;
            FLIP ("am", "ham");
            if PA_LAST > 0 then
               return;
            end if;
            FLIP ("ar", "har");
            if PA_LAST > 0 then
               return;
            end if;
            FLIP ("aur", "or");
            if PA_LAST > 0 then
               return;
            end if;

         when 'd' =>

            FLIP ("dampn", "damn");
            if PA_LAST > 0 then
               return;
            end if;
            FLIP_FLOP ("dij", "disj");       --  OLD p.543
            if PA_LAST > 0 then
               return;
            end if;
            FLIP_FLOP ("dir", "disr");       --  OLD p.556
            if PA_LAST > 0 then
               return;
            end if;
            FLIP_FLOP ("dir", "der");        --  OLD p.547
            if PA_LAST > 0 then
               return;
            end if;
            FLIP_FLOP ("del", "dil");        --  OLD p.507/543
            if PA_LAST > 0 then
               return;
            end if;

         when 'e' =>

            FLIP_FLOP ("ecf", "eff");
            if PA_LAST > 0 then
               return;
            end if;
            FLIP_FLOP ("ecs", "exs");
            if PA_LAST > 0 then
               return;
            end if;
            FLIP_FLOP ("es", "ess");
            if PA_LAST > 0 then
               return;
            end if;
            FLIP_FLOP ("ex", "exs");
            if PA_LAST > 0 then
               return;
            end if;

            FLIP ("eid", "id");
            if PA_LAST > 0 then
               return;
            end if;
            FLIP ("el", "hel");
            if PA_LAST > 0 then
               return;
            end if;
            FLIP ("e", "ae");
            if PA_LAST > 0 then
               return;
            end if;

         when 'f' =>

            FLIP_FLOP ("faen", "fen");
            if PA_LAST > 0 then
               return;
            end if;

            FLIP_FLOP ("faen", "foen");
            if PA_LAST > 0 then
               return;
            end if;

            FLIP_FLOP ("fed", "foed");
            if PA_LAST > 0 then
               return;
            end if;

            FLIP_FLOP ("fet", "foet");
            if PA_LAST > 0 then
               return;
            end if;

            FLIP ("f", "ph");
            if PA_LAST > 0 then
               return;
            end if;  -- Try lead then all

         when 'g' =>

            FLIP ("gna", "na");
            if PA_LAST > 0 then
               return;
            end if;

         when 'h' =>

            FLIP ("har", "ar");
            if PA_LAST > 0 then
               return;
            end if;
            FLIP ("hal", "al");
            if PA_LAST > 0 then
               return;
            end if;
            FLIP ("ham", "am");
            if PA_LAST > 0 then
               return;
            end if;
            FLIP ("hel", "el");
            if PA_LAST > 0 then
               return;
            end if;
            FLIP ("hol", "ol");
            if PA_LAST > 0 then
               return;
            end if;
            FLIP ("hum", "um");
            if PA_LAST > 0 then
               return;
            end if;

         when 'i' =>

            -- for some forms of eo the stem "i" grates with an "is..." ending
            if S'LENGTH > 1 and then S (S'FIRST .. S'FIRST + 1) = "is" then
               PA (1) :=
                 ("Word mod is => iis", NULL_INFLECTION_RECORD, XXX,
                  NULL_MNPC);
               PA_LAST := 1;
               TWORD ("i" & S (S'FIRST .. S'LAST), PA, PA_LAST);
            end if;
            if (PA_LAST > PA_SAVE + 1)
              and then (PA (PA_LAST - 1).IR.QUAL.POFS /= TACKON)
              and then PA (PA_LAST).IR.QUAL.POFS = V
              and then PA (PA_LAST).IR.QUAL.V.CON = (6, 1)
            then  --    Check it is V 6 1 eo
               XXX_MEANING (XXX_MEANING_COUNTER) :=
                 HEAD
                   ("Some forms of eo stem 'i' grates with an 'is...' ending, so 'is' -> 'iis' ",
                    MAX_MEANING_SIZE);
               XXX_MEANING_COUNTER := XXX_MEANING_COUNTER + 1;
               return;
            else
               PA_LAST := 0;
            end if;

         when 'k' =>

            FLIP ("k", "c");
            if PA_LAST > 0 then
               return;
            end if;
            FLIP ("c", "k");
            if PA_LAST > 0 then
               return;
            end if;

         when 'l' =>

            FLIP_FLOP ("lub", "lib");
            if PA_LAST > 1 then
               return;
            end if;

         when 'm' =>

            FLIP_FLOP ("mani", "manu");
            if PA_LAST > 1 then
               return;
            end if;

         when 'n' =>

            FLIP ("na", "gna");
            if PA_LAST > 0 then
               return;
            end if;

            FLIP_FLOP ("nihil", "nil");
            if PA_LAST > 0 then
               return;
            end if;

         when 'o' =>

            FLIP_FLOP ("obf", "off");
            if PA_LAST > 1 then
               return;
            end if;
            FLIP_FLOP ("obc", "occ");
            if PA_LAST > 1 then
               return;
            end if;
            FLIP_FLOP ("obt", "opt");
            if PA_LAST > 1 then
               return;
            end if;
            FLIP_FLOP ("obs", "ops");
            if PA_LAST > 1 then
               return;
            end if;
            FLIP ("ol", "hol");
            if PA_LAST > 0 then
               return;
            end if;
            FLIP ("opp", "op");
            if PA_LAST > 1 then
               return;
            end if;
            FLIP ("or", "aur");
            if PA_LAST > 0 then
               return;
            end if;

         when 'p' =>

            FLIP ("ph", "f");
            if PA_LAST > 0 then
               return;
            end if;  -- Try lead then all
            FLIP_FLOP ("pre", "prae");
            if PA_LAST > 1 then
               return;
            end if;

         when 's' =>

            FLIP_FLOP ("subsc", "susc");
            if PA_LAST > 0 then
               return;
            end if;
            FLIP_FLOP ("subsp", "susp");
            if PA_LAST > 0 then
               return;
            end if;

            FLIP_FLOP ("subc", "susc");
            if PA_LAST > 0 then
               return;
            end if;
            FLIP_FLOP ("succ", "susc");
            if PA_LAST > 0 then
               return;
            end if;

            FLIP_FLOP ("subt", "supt");
            if PA_LAST > 0 then
               return;
            end if;
            FLIP_FLOP ("subt", "sust");
            if PA_LAST > 0 then
               return;
            end if;

            FLIP_FLOP ("subst", "sust");   -- SPR: White (1880)
            if PA_LAST > 0 then
               return;
            end if;

            FLIP_FLOP ("subr", "surr");     -- SPR: White (1880)
            if PA_LAST > 0 then
               return;
            end if;

         when 't' =>

            FLIP_FLOP ("transv", "trav");
            if PA_LAST > 0 then
               return;
            end if;
--            FLIP("trig",  "tric");
--            if PA_LAST > 0  then
--               return; end if;

         when 'u' =>

            FLIP ("ul", "hul");
            if PA_LAST > 0 then
               return;
            end if;
            FLIP ("uol", "vul");
            if PA_LAST > 0 then
               return;
            end if;  --  u is not v for this purpose

         when 'y' =>

            FLIP ("y", "i");
            if PA_LAST > 0 then
               return;
            end if;

         when 'z' =>

            FLIP ("z", "di");
            if PA_LAST > 0 then
               return;
            end if;

         when others =>
            null;

      end case;   --  case on first letter

      INTERNAL ("ae", "e");
      if PA_LAST > 0 then
         return;
      end if;

      INTERNAL ("bul", "bol");
      if PA_LAST > 0 then
         return;
      end if;
      INTERNAL ("bol", "bul");
      if PA_LAST > 0 then
         return;
      end if;

      INTERNAL ("cl", "cul");
      if PA_LAST > 0 then
         return;
      end if;

      INTERNAL ("cu", "quu");
      if PA_LAST > 0 then
         return;
      end if;

      INTERNAL ("f", "ph");
      if PA_LAST > 0 then
         return;
      end if;
      INTERNAL ("ph", "f");
      if PA_LAST > 0 then
         return;
      end if;

      INTERNAL ("h", "");
      if PA_LAST > 0 then
         return;
      end if;

      INTERNAL ("oe", "e");
      if PA_LAST > 0 then
         return;
      end if;

      INTERNAL ("vul", "vol");
      if PA_LAST > 0 then
         return;
      end if;
      INTERNAL ("vol", "vul");
      if PA_LAST > 0 then
         return;
      end if;
      INTERNAL ("uol", "vul");
      if PA_LAST > 0 then
         return;
      end if;

      ADJ_TERMINAL_IIS;
      if PA_LAST > 0 then
         return;
      end if;

      ---------------------------------------------------------------

      if WORDS_MDEV (DO_MEDIEVAL_TRICKS) then
         --      Medieval  ->  Classic

         --  Harrington/Elliott 1.1.1

         INTERNAL ("col", "caul");
         if PA_LAST > 0 then
            return;
         end if;

         --TEXT_IO.PUT_LINE("Trying com -> con");
--INTERNAL("com",  "con");   if PA_LAST > 0  then return; end if;   --  My own

         --INTERNAL("cl",  "cul");   if PA_LAST > 0  then return; end if;

         --  Harrington/Elliott    1.3

         INTERNAL ("e", "ae");
         if PA_LAST > 0 then
            return;
         end if;

         INTERNAL ("o", "u");
         if PA_LAST > 0 then
            return;
         end if;

         INTERNAL ("i", "y");
         if PA_LAST > 0 then
            return;
         end if;

         --  Harrington/Elliott 1.3.1

         INTERNAL ("ism", "sm");
         if PA_LAST > 0 then
            return;
         end if;

         INTERNAL ("isp", "sp");
         if PA_LAST > 0 then
            return;
         end if;

         INTERNAL ("ist", "st");
         if PA_LAST > 0 then
            return;
         end if;

         INTERNAL ("iz", "z");
         if PA_LAST > 0 then
            return;
         end if;

         INTERNAL ("esm", "sm");
         if PA_LAST > 0 then
            return;
         end if;

         INTERNAL ("esp", "sp");
         if PA_LAST > 0 then
            return;
         end if;

         INTERNAL ("est", "st");
         if PA_LAST > 0 then
            return;
         end if;

         INTERNAL ("ez", "z");
         if PA_LAST > 0 then
            return;
         end if;

         --  Harrington/Elliott    1.4

         INTERNAL ("di", "z");
         if PA_LAST > 0 then
            return;
         end if;

         INTERNAL ("f", "ph");
         if PA_LAST > 0 then
            return;
         end if;

         INTERNAL ("is", "ix");
         if PA_LAST > 0 then
            return;
         end if;

         INTERNAL ("b", "p");
         if PA_LAST > 0 then
            return;
         end if;

         INTERNAL ("d", "t");
         if PA_LAST > 0 then
            return;
         end if;

         INTERNAL ("v", "b");
         if PA_LAST > 0 then
            return;
         end if;

         INTERNAL ("v", "f");
         if PA_LAST > 0 then
            return;
         end if;

         INTERNAL ("v", "f");
         if PA_LAST > 0 then
            return;
         end if;

         INTERNAL ("s", "x");
         if PA_LAST > 0 then
            return;
         end if;

         --  Harrington/Elliott 1.4.1

         INTERNAL ("ci", "ti");
         if PA_LAST > 0 then
            return;
         end if;

         --  Harrington/Elliott 1.4.2

         INTERNAL ("nt", "nct");
         if PA_LAST > 0 then
            return;
         end if;

         INTERNAL ("s", "ns");
         if PA_LAST > 0 then
            return;
         end if;

         --  Others

         INTERNAL ("ch", "c");
         if PA_LAST > 0 then
            return;
         end if;

         INTERNAL ("c", "ch");
         if PA_LAST > 0 then
            return;
         end if;

         INTERNAL ("th", "t");
         if PA_LAST > 0 then
            return;
         end if;

         INTERNAL ("t", "th");
         if PA_LAST > 0 then
            return;
         end if;

         DOUBLE_CONSONANTS;

      end if;   --  Medieval Tricks
      ---------------------------------------------------------------

      if not (WORDS_MODE (IGNORE_UNKNOWN_NAMES) and CAPITALIZED)
      then   --  Don't try on Names
         if WORDS_MDEV (DO_TWO_WORDS) then
            TWO_WORDS;
         end if;
      end if;

    --  May be Roman numeral formed without strict smaller-large subtraction rules
    --  SPR: Removed user-facing language that called these "ill formed" and "bad";
    --  see sources in Arabic2Roman.ads
      if ONLY_ROMAN_DIGITS (W) then

         PA_LAST := 1;   -- add 4 spaces because code assumes array length
         PA (1)  :=
           ("Roman numeral?    ", NULL_INFLECTION_RECORD, XXX, NULL_MNPC);
         XXX_MEANING (XXX_MEANING_COUNTER) := NULL_MEANING_TYPE;

         RRR_MEANING (RRR_MEANING_COUNTER) :=
           HEAD
             (Integer'IMAGE (BAD_ROMAN_NUMBER (W)) &
              " as a ROMAN NUMERAL formed without using strict subtraction rules (?);",
              MAX_MEANING_SIZE);
         RRR_MEANING_COUNTER := RRR_MEANING_COUNTER + 1;
         PA_LAST             := PA_LAST + 1;
         PA (PA_LAST)        :=
           (STEM => HEAD (W, MAX_STEM_SIZE),
            IR   =>
              (QUAL =>
                 (POFS => NUM,
                  NUM  =>
                    (DECL => (2, 0), CS => X, NUMBER => X, GENDER => X,
                     SORT => CARD)),

               KEY => 0, ENDING => NULL_ENDING_RECORD,
               AGE => Roman_Numeral_Age (W), FREQ => D),
            D_K => RRR, MNPC => NULL_MNPC);
         return;
      end if;

   exception
      when others =>    --  I want to ignore anything that happens in TRICKS
         PA_LAST          := PA_SAVE;
         PA (PA_LAST + 1) := NULL_PARSE_RECORD;     --  Just to clear the trys
         Format (OUTPUT, INVERSE);
         Text_IO.Put_Line ("Exception in TRY_TRICKS processing " & W);
         Format (OUTPUT, RESET);
   end TRY_TRICKS;

   procedure TRY_SLURY
     (W : String; PA : in out PARSE_ARRAY; PA_LAST : in out Integer)
   is
      --  Since the chances are 1/1000 that we have one, Ignore the possibility
      --  of two in the same word That is called lying with statistics
      S       : constant String (1 .. W'LENGTH) := W;
      PA_SAVE : Integer                         := PA_LAST;

      procedure TWORD
        (W : String; PA : in out PARSE_ARRAY; PA_LAST : in out Integer)
      is
         SAVE_USE_PREFIXES : Boolean := WORDS_MDEV (USE_PREFIXES);
      begin
         WORDS_MDEV (USE_PREFIXES) := False;
         WORD_PACKAGE.WORD (W, PA, PA_LAST);
         SYNCOPE (W, PA, PA_LAST);
         WORDS_MDEV (USE_PREFIXES) := SAVE_USE_PREFIXES;
      end TWORD;

      procedure FLIP (X1, X2 : String; EXPLANATION : String := "") is
         --  At the begining of input word, replaces X1 by X2
         PA_SAVE : Integer := PA_LAST;
      begin
         if S'LENGTH >= X1'LENGTH + 2
           and then S (S'FIRST .. S'FIRST + X1'LENGTH - 1) = X1
         then
            PA_LAST      := PA_LAST + 1;
            PA (PA_LAST) :=
              (HEAD ("Word mod " & X1 & "/" & X2, MAX_STEM_SIZE),
               NULL_INFLECTION_RECORD, XXX, NULL_MNPC);
            TWORD (X2 & S (S'FIRST + X1'LENGTH .. S'LAST), PA, PA_LAST);
            if (PA_LAST > PA_SAVE + 1)
              and then (PA (PA_LAST - 1).IR.QUAL.POFS /= TACKON)
            then
               if EXPLANATION = "" then
                  XXX_MEANING (XXX_MEANING_COUNTER) :=
                    HEAD
                      ("An initial '" & X1 & "' may be rendered by '" & X2 &
                       "'",
                       MAX_MEANING_SIZE);
                  XXX_MEANING_COUNTER := XXX_MEANING_COUNTER + 1;
               else
                  XXX_MEANING (XXX_MEANING_COUNTER) :=
                    HEAD (EXPLANATION, MAX_MEANING_SIZE);
                  XXX_MEANING_COUNTER := XXX_MEANING_COUNTER + 1;
               end if;
               return;
            else
               PA_LAST := PA_SAVE;
            end if;
         end if;
         PA_LAST := PA_SAVE;
      end FLIP;

      procedure FLIP_FLOP (X1, X2 : String; EXPLANATION : String := "") is
         --  At the begining of input word, replaces X1 by X2 - then X2 by
         --  X1 To be used only when X1 and X2 start with the same letter
         --  because it will be called from a point where the first letter
         --  is established
         PA_SAVE : Integer := PA_LAST;
      begin
         if S'LENGTH >= X1'LENGTH + 2
           and then S (S'FIRST .. S'FIRST + X1'LENGTH - 1) = X1
         then
            PA_LAST      := PA_LAST + 1;
            PA (PA_LAST) :=
              (HEAD ("Word mod " & X1 & "/" & X2, MAX_STEM_SIZE),
               NULL_INFLECTION_RECORD, XXX, NULL_MNPC);
            TWORD (X2 & S (S'FIRST + X1'LENGTH .. S'LAST), PA, PA_LAST);
            if (PA_LAST > PA_SAVE + 1)
              and then (PA (PA_LAST - 1).IR.QUAL.POFS /= TACKON)
            then
               if EXPLANATION = "" then
                  XXX_MEANING (XXX_MEANING_COUNTER) :=
                    HEAD
                      ("An initial '" & X1 & "' may be rendered by '" & X2 &
                       "'",
                       MAX_MEANING_SIZE);
                  XXX_MEANING_COUNTER := XXX_MEANING_COUNTER + 1;
               else
                  XXX_MEANING (XXX_MEANING_COUNTER) :=
                    HEAD (EXPLANATION, MAX_MEANING_SIZE);
                  XXX_MEANING_COUNTER := XXX_MEANING_COUNTER + 1;
               end if;
               return;
            else
               PA_LAST := PA_SAVE;
            end if;

         elsif S'LENGTH >= X2'LENGTH + 2
           and then S (S'FIRST .. S'FIRST + X2'LENGTH - 1) = X2
         then
            PA_LAST      := PA_LAST + 1;
            PA (PA_LAST) :=
              (HEAD ("Word mod " & X2 & "/" & X1, MAX_STEM_SIZE),
               NULL_INFLECTION_RECORD, XXX, NULL_MNPC);
            TWORD (X1 & S (S'FIRST + X2'LENGTH .. S'LAST), PA, PA_LAST);
            if (PA_LAST > PA_SAVE + 1)
              and then (PA (PA_LAST - 1).IR.QUAL.POFS /= TACKON)
            then
               if EXPLANATION = "" then
                  XXX_MEANING (XXX_MEANING_COUNTER) :=
                    HEAD
                      ("An initial '" & X1 & "' may be rendered by '" & X2 &
                       "'",
                       MAX_MEANING_SIZE);
                  XXX_MEANING_COUNTER := XXX_MEANING_COUNTER + 1;
               else
                  XXX_MEANING (XXX_MEANING_COUNTER) :=
                    HEAD (EXPLANATION, MAX_MEANING_SIZE);
                  XXX_MEANING_COUNTER := XXX_MEANING_COUNTER + 1;
               end if;
               return;
            else
               PA_LAST := PA_SAVE;
            end if;

         end if;
         PA_LAST := PA_SAVE;
      end FLIP_FLOP;

      procedure SLUR (X1 : String; EXPLANATION : String := "") is
         PA_SAVE : Integer := PA_LAST;
         SL      : Integer := X1'LENGTH;
      begin
         if S'LENGTH >= X1'LENGTH + 2 then
            if S (S'FIRST .. S'FIRST + X1'LENGTH - 1) = X1
              and then   --  Initial  X1
            not IS_A_VOWEL (S (S'FIRST + SL))
            then
               PA_LAST      := PA_LAST + 1;
               PA (PA_LAST) :=
                 (HEAD
                    ("Slur " & X1 & "/" & X1 (X1'First .. SL - 1) & "~",
                     MAX_STEM_SIZE),
                  NULL_INFLECTION_RECORD, XXX, NULL_MNPC);
               TWORD
                 (X1 (X1'First .. SL - 1) & S (S'FIRST + SL) &
                  S (S'FIRST + SL .. S'LAST),
                  PA, PA_LAST);
               if (PA_LAST > PA_SAVE + 1)
                 and then (PA (PA_LAST - 1).IR.QUAL.POFS /= TACKON)
               then
                  if EXPLANATION = "" then
                     XXX_MEANING (XXX_MEANING_COUNTER) :=
                       HEAD
                         ("An initial '" & X1 & "' may be rendered by " &
                          X1 (X1'First .. X1'LAST - 1) & "~",
                          MAX_MEANING_SIZE);
                     XXX_MEANING_COUNTER := XXX_MEANING_COUNTER + 1;
                  else
                     XXX_MEANING (XXX_MEANING_COUNTER) :=
                       HEAD (EXPLANATION, MAX_MEANING_SIZE);
                     XXX_MEANING_COUNTER := XXX_MEANING_COUNTER + 1;
                  end if;
                  return;
               else
                  PA_LAST := PA_SAVE;
               end if;

            elsif (S (S'FIRST .. S'FIRST + SL - 1) = X1 (X1'First .. SL - 1))
              and then (S (S'FIRST + SL - 1) = S (S'FIRST + SL))
              and then   --  double letter
            not IS_A_VOWEL (S (S'FIRST + SL))
            then
               PA_LAST      := PA_LAST + 1;
               PA (PA_LAST) :=
                 (HEAD
                    ("Slur " & X1 (X1'First .. SL - 1) & "~" & "/" & X1,
                     MAX_STEM_SIZE),
                  NULL_INFLECTION_RECORD, XXX, NULL_MNPC);
               TWORD (X1 & S (S'FIRST + SL .. S'LAST), PA, PA_LAST);
               if (PA_LAST > PA_SAVE + 1)
                 and then (PA (PA_LAST - 1).IR.QUAL.POFS /= TACKON)
               then
                  if EXPLANATION = "" then
                     XXX_MEANING (XXX_MEANING_COUNTER) :=
                       HEAD
                         ("An initial '" & X1 (X1'First .. SL - 1) & "~" &
                          "' may be rendered by " & X1,
                          MAX_MEANING_SIZE);
                     XXX_MEANING_COUNTER := XXX_MEANING_COUNTER + 1;
                  else
                     XXX_MEANING (XXX_MEANING_COUNTER) :=
                       HEAD (EXPLANATION, MAX_MEANING_SIZE);
                     XXX_MEANING_COUNTER := XXX_MEANING_COUNTER + 1;
                  end if;
                  return;
               else
                  PA_LAST := PA_SAVE;
               end if;

            end if;
         end if;
         PA_LAST := PA_SAVE;
      end SLUR;

   begin

      --  If there is no satisfaction from above, we will try further

      if S (S'FIRST) = 'a' then

         FLIP_FLOP ("abs", "aps");
         if PA_LAST > 0 then
            return;
         end if;
         FLIP_FLOP ("acq", "adq");
         if PA_LAST > 0 then
            return;
         end if;
         FLIP_FLOP ("ante", "anti");
         if PA_LAST > 0 then
            return;
         end if;
         FLIP_FLOP ("auri", "aure");
         if PA_LAST > 0 then
            return;
         end if;
         FLIP_FLOP ("auri", "auru");
         if PA_LAST > 0 then
            return;
         end if;
         SLUR ("ad");
         if PA_LAST > 0 then
            return;
         end if;

      elsif S (S'FIRST) = 'c' then

         FLIP_FLOP ("circum", "circun");
         if PA_LAST > 0 then
            return;
         end if;
         FLIP_FLOP ("con", "com");
         if PA_LAST > 0 then
            return;
         end if;
         FLIP ("co", "com");
         if PA_LAST > 0 then
            return;
         end if;
         FLIP ("co", "con");
         if PA_LAST > 0 then
            return;
         end if;
         FLIP_FLOP ("conl", "coll");
         if PA_LAST > 0 then
            return;
         end if;

      elsif S (S'FIRST) = 'f' then

         FLIP ("f", "ph");
         if PA_LAST > 0 then
            return;
         end if;

      elsif S (S'FIRST) = 'i' then

         SLUR ("in");
         if PA_LAST > 1 then
            return;
         end if;

         FLIP_FLOP ("inb", "imb");
         if PA_LAST > 1 then
            return;
         end if;
         FLIP_FLOP ("inp", "imp");
         if PA_LAST > 1 then
            return;
         end if;

      elsif S (S'FIRST) = 'n' then

         FLIP ("nun", "non");
         if PA_LAST > 0 then
            return;
         end if;

      elsif S (S'FIRST) = 'o' then

         SLUR ("ob");
         if PA_LAST > 0 then
            return;
         end if;

      elsif S (S'FIRST) = 'q' then

         FLIP_FLOP ("quadri", "quadru");
         if PA_LAST > 0 then
            return;
         end if;

      elsif S (S'FIRST) = 's' then

         FLIP ("se", "ce");     --  Latham
         if PA_LAST > 0 then
            return;
         end if;

      end if;   --  if on first letter

      ---------------------------------------------

   exception
      when others =>    --  I want to ignore anything that happens in SLURY
         PA_LAST          := PA_SAVE;
         PA (PA_LAST + 1) := NULL_PARSE_RECORD; -- Clear the TRYs
         Format (OUTPUT, INVERSE);
         Text_IO.Put_Line
           (    --  ERROR_FILE,
         "Exception in TRY_SLURY processing " & W);
         Format (OUTPUT, RESET);
   end TRY_SLURY;

end TRICKS_PACKAGE;
