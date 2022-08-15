with STRINGS_PACKAGE;      use STRINGS_PACKAGE;
with WORD_PARAMETERS;      use WORD_PARAMETERS;
with INFLECTIONS_PACKAGE;  use INFLECTIONS_PACKAGE;
with DICTIONARY_PACKAGE;   use DICTIONARY_PACKAGE;
with UNIQUES_PACKAGE;      use UNIQUES_PACKAGE;
with DEVELOPER_PARAMETERS; use DEVELOPER_PARAMETERS;
with WORD_SUPPORT_PACKAGE; use WORD_SUPPORT_PACKAGE;
with ADDONS_PACKAGE;

procedure LIST_SWEEP (PA : in out PARSE_ARRAY; PA_LAST : in out Integer) is
   --  This procedure is supposed to process the output PARSE_ARRAY at PA level
   --  before it get turned into SIRAA and DMNPCA in LIST_PACKAGE Since it does
   --  only PARSE_ARRAY it is just checking INFLECTIONS, not DICTIONARY (except
   --  PRON/PACKs, as noted below)

   use DICT_IO;

   PR, OPR  : PARSE_RECORD     := NULL_PARSE_RECORD;
   DE       : DICTIONARY_ENTRY := NULL_DICTIONARY_ENTRY;
   I, J, JJ : Integer          := 0;
   DIFF_J   : Integer          := 0;

   NOT_ONLY_ARCHAIC  : Boolean := False;
   NOT_ONLY_MEDIEVAL : Boolean := False;

   Has_Qu_Pron : Boolean := False;

   function ALLOWED_STEM (PR : PARSE_RECORD) return Boolean is
      ALLOWED : Boolean := True;   --  modify as necessary and return it

   begin
--DEBUG: TEXT_IO.PUT("ALLOWED? >"); PARSE_RECORD_IO.PUT(PR); TEXT_IO.NEW_LINE;
      if PR.D_K not in GENERAL .. LOCAL then
      return True;
      elsif PR.IR.QUAL.POFS /= V then  -- SPR:  This section previously included a few static checks that were intended
        return True;                   -- to verify new dictionary entries.  I've read Whitaker's explanations repeatedly--
                                       -- those checks were designed to allow only dictionary forms (e.g., amicus, amici)
                                       -- and I have no idea how that could be useful for entry checking (aside from maybe double-checking
                                       -- new INFLECTS entries, but the INFLECTS have been complete for a very long time).
                                       -- In any event, DICTPAGE reports dictionary forms if that's important for some reason.
                                       -- The remaining checks apply only to verbs; the /= V condition is an artifact of removed non-verb checks.

      end if; 

      DICT_IO.Read (DICT_FILE (PR.D_K), DE, PR.MNPC);


            --  Check for Verb 3 1 dic/duc/fac/fer shortened imperative See G&L
            --  130.5
      declare
               STEM       : constant String := TRIM (PR.STEM);
               LAST_THREE : String (1 .. 3);
            begin
               if (PR.IR.QUAL.V = ((3, 1), (PRES, ACTIVE, IMP), 2, S)) and
                 (PR.IR.ENDING.SIZE = 0)
               then    --  For this special case
                  if STEM'LENGTH >= 3 then
                     LAST_THREE := STEM (STEM'LAST - 2 .. STEM'LAST);
                     if (LAST_THREE = "dic") or (LAST_THREE = "duc") or
                       (LAST_THREE = "fac") or (LAST_THREE = "fer")
                     then
                        null;
                     else
                        ALLOWED := False;
                     end if;
                  elsif STEM'Length = 2
                  then      -- SPR:  This change prevents TRIM_OUTPUT from dropping some very short words
                     null;  --       E.g., it will drop "em" (imperative of emo).
                  else
                     ALLOWED := False;
                  end if;
               end if;
            end;

            --  Check for Verb Imperative being in permitted person
            if (PR.IR.QUAL.V.TENSE_VOICE_MOOD.MOOD = IMP) then
               if (PR.IR.QUAL.V.TENSE_VOICE_MOOD.TENSE = PRES) and
                 (PR.IR.QUAL.V.PERSON = 2)
               then
                  null;
               elsif (PR.IR.QUAL.V.TENSE_VOICE_MOOD.TENSE = FUT) and
                 (PR.IR.QUAL.V.PERSON = 2 or PR.IR.QUAL.V.PERSON = 3)
               then
                  null;
               else
         --DEBUG: PUT("IMP not in permitted person  "); PUT(PR.IR); NEW_LINE;
                  ALLOWED := False;
               end if;
            end if;

            --  Check for V IMPERS and demand that only 3rd person -- ???????
            if (DE.PART.V.KIND = IMPERS) then
               if (PR.IR.QUAL.V.PERSON = 3) then
                  null;
               else
         --DEBUG: PUT("IMPERS not in 3rd person     "); PUT(PR.IR); NEW_LINE;
                  ALLOWED := False;
               end if;
            end if;

            --  Check for V DEP and demand PASSIVE
            if (DE.PART.V.KIND = DEP) then
               --DEBUG: TEXT_IO.PUT("DEP  ");
               if (PR.IR.QUAL.V.TENSE_VOICE_MOOD.VOICE = ACTIVE) and
                 (PR.IR.QUAL.V.TENSE_VOICE_MOOD.MOOD = INF) and
                 (PR.IR.QUAL.V.TENSE_VOICE_MOOD.TENSE = FUT)
               then
                  --DEBUG: TEXT_IO.PUT("PASSIVE  ");
                  --DEBUG: TEXT_IO.PUT("DEP    FUT INF not in ACTIVE "); PUT(PR.IR); TEXT_IO.NEW_LINE;
                  ALLOWED := True;
               elsif (PR.IR.QUAL.V.TENSE_VOICE_MOOD.VOICE = ACTIVE) and
                 (PR.IR.QUAL.V.TENSE_VOICE_MOOD.MOOD in IND .. INF)
               then
                  --DEBUG: TEXT_IO.PUT("ACTIVE  ");
                  --DEBUG: TEXT_IO.PUT("DEP    not in PASSIVE     NOT ALLOWED   "); PUT(PR.IR); TEXT_IO.NEW_LINE;
                  ALLOWED := False;
               else
                  --DEBUG: TEXT_IO.PUT("??????  ");
                  null;
               end if;
            end if;

            --  Check for V SEMIDEP and demand PASSIVE ex Perf
            if (DE.PART.V.KIND = SEMIDEP) then
               if (PR.IR.QUAL.V.TENSE_VOICE_MOOD.VOICE = PASSIVE) and
                 (PR.IR.QUAL.V.TENSE_VOICE_MOOD.TENSE in PRES .. FUT) and
                 (PR.IR.QUAL.V.TENSE_VOICE_MOOD.MOOD in IND .. IMP)
               then
         --DEBUG: PUT("SEMIDEP    Pres not in ACTIVE "); PUT(PR.IR); NEW_LINE;
                  ALLOWED := False;
               elsif (PR.IR.QUAL.V.TENSE_VOICE_MOOD.VOICE = ACTIVE) and
                 (PR.IR.QUAL.V.TENSE_VOICE_MOOD.TENSE in PERF .. FUTP) and
                 (PR.IR.QUAL.V.TENSE_VOICE_MOOD.MOOD in IND .. IMP)
               then
         --DEBUG: PUT("SEMIDEP    Perf not in PASSIVE "); PUT(PR.IR); NEW_LINE;
                  ALLOWED := False;
               else
                  null;
               end if;
            end if;
        
      return ALLOWED;


   end ALLOWED_STEM;

   -----------------------------------------------------------

   procedure ORDER_PARSE_ARRAY (SL : in out PARSE_ARRAY; DIFF_J : out Integer)
   is

      HITS            : Integer := 0;
      SL_LAST         : Integer := SL'LAST;
      SL_LAST_INITIAL : Integer := SL_LAST;
      SM              : PARSE_RECORD;
      --DE, ODE : DICTIONARY_ENTRY;
      HAS_NOUN_ABBREVIATION : Boolean := False;
      NOT_ONLY_VOCATIVE     : Boolean := False;
      NOT_ONLY_LOCATIVE     : Boolean := False;

      function DEPR (PR : PARSE_RECORD) return DICTIONARY_ENTRY is
         DE : DICTIONARY_ENTRY;

      begin
--DEBUG: TEXT_IO.PUT("DEPR  "); PARSE_RECORD_IO.PUT(PR); TEXT_IO.NEW_LINE;
         if PR.MNPC = NULL_MNPC then
            return NULL_DICTIONARY_ENTRY;
         else
            if PR.D_K in GENERAL .. LOCAL then
               --if PR.MNPC /= OMNPC  then
               DICT_IO.Set_Index (DICT_FILE (PR.D_K), PR.MNPC);
               DICT_IO.Read (DICT_FILE (PR.D_K), DE);
               --OMNPC := PR.MNPC;
               --ODE := DE;
               --else
               --DE := ODE;
               --end if;
            elsif PR.D_K = UNIQUE then
               DE := UNIQUES_DE (PR.MNPC);
            end if;
         end if;

--DEBUG: TEXT_IO.PUT_LINE("Returning from DEPR   MNPC = " & INTEGER'IMAGE(INTEGER(PR.MNPC)) & "  ");
--DEBUG: DICTIONARY_ENTRY_IO.PUT(DE); TEXT_IO.NEW_LINE;
         return DE;
      end DEPR;

   begin

      if SL'LENGTH = 0 then
         DIFF_J := 0;
         return;
      end if;

      --  Bubble sort since this list should usually be very small (1-5)
      HIT_LOOP :
      loop
         HITS := 0;

         --------------------------------------------------

         SWITCH :
         declare

            function "<" (LEFT, RIGHT : QUALITY_RECORD) return Boolean is
            begin
--                    if LEFT.POFS = RIGHT.POFS  and then
--                    LEFT.POFS = PRON        and then
--                    LEFT.PRON.DECL.WHICH = 1    then
--                       return (LEFT.PRON.DECL.VAR < RIGHT.PRON.DECL.VAR);
--                    else
               return INFLECTIONS_PACKAGE."<" (LEFT, RIGHT);
--                  end if;
            end "<";

            function EQU (LEFT, RIGHT : QUALITY_RECORD) return Boolean is
            begin

--                    if LEFT.POFS = RIGHT.POFS  and then
--                    LEFT.POFS = PRON        and then
--                    LEFT.PRON.DECL.WHICH = 1    then
--
--                       return (LEFT.PRON.DECL.VAR = RIGHT.PRON.DECL.VAR);
--                    else
--
               return INFLECTIONS_PACKAGE."=" (LEFT, RIGHT);
               --               end if;

            end EQU;

            function MEANING (PR : PARSE_RECORD) return MEANING_TYPE is
            begin
               return DEPR (PR).MEAN;
            end MEANING;

         begin
            --  Need to remove duplicates in ARRAY_STEMS This sort is very
            --  sloppy One problem is that it can mix up some of the order of
            --  PREFIX, XXX, LOC I ought to do this for every set of results
            --  from different approaches not just in one fell swoop at the
            --  end !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            INNER_LOOP :
            for I in SL'FIRST .. SL_LAST - 1 loop
               --  Maybe < = on PR.STEM - will have to make up "<" -- Actually
               --  STEM and PART -- and check that later in print
               if SL (I + 1).D_K > SL (I).D_K
                 or else  --  Let DICT.LOC list first

                 (SL (I + 1).D_K = SL (I).D_K
                  and then SL (I + 1).MNPC < SL (I).MNPC)
                 or else

                 (SL (I + 1).D_K = SL (I).D_K
                  and then SL (I + 1).MNPC = SL (I).MNPC
                  and then SL (I + 1).IR.QUAL < SL (I).IR.QUAL)
                 or else

                 (SL (I + 1).D_K = SL (I).D_K
                  and then SL (I + 1).MNPC = SL (I).MNPC
                  and then EQU (SL (I + 1).IR.QUAL, SL (I).IR.QUAL)
                  and then MEANING (SL (I + 1)) < MEANING (SL (I)))
                 or else   --  | is > letter

                 (SL (I + 1).D_K = SL (I).D_K
                  and then SL (I + 1).MNPC = SL (I).MNPC
                  and then EQU (SL (I + 1).IR.QUAL, SL (I).IR.QUAL)
                  and then MEANING (SL (I + 1)) = MEANING (SL (I))
                  and then SL (I + 1).IR.ENDING.SIZE < SL (I).IR.ENDING.SIZE)
                 or else

                 (SL (I + 1).D_K = SL (I).D_K
                  and then SL (I + 1).MNPC = SL (I).MNPC
                  and then EQU (SL (I + 1).IR.QUAL, SL (I).IR.QUAL)
                  and then MEANING (SL (I + 1)) = MEANING (SL (I))
                  and then SL (I + 1).IR.ENDING.SIZE = SL (I).IR.ENDING.SIZE
                  and then INFLECTIONS_PACKAGE."<"
                    (SL (I + 1).IR.QUAL, SL (I).IR.QUAL))
               then

                  SM         := SL (I);
                  SL (I)     := SL (I + 1);
                  SL (I + 1) := SM;
                  HITS       := HITS + 1;

               end if;

            end loop INNER_LOOP;

         end SWITCH;
         --------------------------------------------------

         exit when HITS = 0;
      end loop HIT_LOOP;

      --  Fix up the Archaic/Medieval
      if WORDS_MODE (TRIM_OUTPUT) then
         --  Remove those inflections if MDEV and there is other valid
--DEBUG: TEXT_IO.PUT_LINE("SCANNING FOR TRIM   SL'FIRST = " & INTEGER'IMAGE(SL'FIRST) & "   SL'LAST = " & INTEGER'IMAGE(SL'LAST) );
--DEBUG:         for I in SL'FIRST..SL_LAST  loop
--DEBUG:         PARSE_RECORD_IO.PUT(SL(I)); TEXT_IO.NEW_LINE;
--DEBUG:         end loop;

         --  Check to see if we can afford to TRIM, if there will be something
         --  left over
         for I in SL'FIRST .. SL_LAST loop

--DEBUG: TEXT_IO.PUT_LINE("SCANNING FOR TRIM   I = " & INTEGER'IMAGE(I) & "  INFL AGE = " & AGE_TYPE'IMAGE(SL(I).IR.AGE));

            if SL (I).D_K in GENERAL .. LOCAL then

               DICT_IO.Set_Index (DICT_FILE (SL (I).D_K), SL (I).MNPC);

               --DEBUG: TEXT_IO.PUT(INTEGER'IMAGE(INTEGER(SL(I).MNPC)));

               DICT_IO.Read (DICT_FILE (SL (I).D_K), DE);

--DEBUG: DICTIONARY_ENTRY_IO.PUT(DE); TEXT_IO.NEW_LINE;

               if ((SL (I).IR.AGE = X) or else (SL (I).IR.AGE > A)) and
                 ((DE.TRAN.AGE = X) or else (DE.TRAN.AGE > A))
               then
                  NOT_ONLY_ARCHAIC := True;
               end if;
               if
                 ((SL (I).IR.AGE = X)
                  or else (SL (I).IR.AGE < F)) and     --  Or E????
                 ((DE.TRAN.AGE = X) or else (DE.TRAN.AGE < F))
               then     --  Or E????
                  NOT_ONLY_MEDIEVAL := True;
               end if;

               if SL (I).IR.QUAL.POFS = N
                 and then SL (I).IR.QUAL.N.DECL = (9, 8)
               then
                  HAS_NOUN_ABBREVIATION := True;
--DEBUG: TEXT_IO.PUT_LINE("Has noun abbreviation   I = " & INTEGER'IMAGE(I));
                  --       elsif SL(I).IR.QUAL.POFS = ADJ  and then
                  --          SL(I).IR.QUAL.ADJ.DECL = (9, 8) then
                  --         HAS_ADJECTIVE_ABBREVIATION := TRUE;
                  --       elsif SL(I).IR.QUAL.POFS = V  and then
                  --          SL(I).IR.QUAL.V.CON = (9, 8) then
                  --         HAS_VERB_ABBREVIATION := TRUE;
               end if;
            end if;
         end loop;

         --  We order and trim within a subset SL, but have to correct the big
         --  set PA also Kill not ALLOWED first, then check the remaining from
         --  the top I am assuming there is no trimming of FIXES for AGE/...
         I := SL_LAST;
         while I >= SL'FIRST loop
            if
              (not ALLOWED_STEM
                 (SL (I)) or               --  Remove not ALLOWED_STEM & null

               (PA (I) = NULL_PARSE_RECORD)) then
--DEBUG: TEXT_IO.PUT_LINE("Not ALLOWED   SL_LAST = " & INTEGER'IMAGE(SL_LAST) & "  J = " & INTEGER'IMAGE(I));
               SL (I .. SL_LAST - 1) := SL (I + 1 .. SL_LAST);
               SL_LAST               := SL_LAST - 1;
               TRIMMED               := True;
--DEBUG: TEXT_IO.PUT_LINE("Not ALLOWED end  SL_LAST = " & INTEGER'IMAGE(SL_LAST) & "  J = " & INTEGER'IMAGE(I));
            end if;
            I := I - 1;
         end loop;

         I := SL_LAST;
         while I >= SL'FIRST loop
      --DEBUG: TEXT_IO.PUT_LINE("TRIMMING FOR TRIM   I = " & INTEGER'IMAGE(I));
            if (NOT_ONLY_ARCHAIC and WORDS_MDEV (OMIT_ARCHAIC))
              and then SL (I).IR.AGE = A
            then
               SL (I .. SL_LAST - 1) := SL (I + 1 .. SL_LAST);
               SL_LAST               := SL_LAST - 1;
               --DEBUG: TEXT_IO.PUT_LINE("Archaic        SL_LAST = " & INTEGER'IMAGE(SL_LAST) & "  I = " & INTEGER'IMAGE(I));
               TRIMMED := True;
            elsif (NOT_ONLY_MEDIEVAL and WORDS_MDEV (OMIT_MEDIEVAL))
              and then SL (I).IR.AGE >= F
            then
               SL (I .. SL_LAST - 1) := SL (I + 1 .. SL_LAST);
               SL_LAST               := SL_LAST - 1;
               --DEBUG: TEXT_IO.PUT_LINE("Medieval       SL_LAST = " & INTEGER'IMAGE(SL_LAST) & "  I = " & INTEGER'IMAGE(I));
               TRIMMED := True;
            end if;
            I := I - 1;
         end loop;

         I := SL_LAST;

----------------------------------------------------------------------------
----------------------------------------------------------------------------
----------------------------------------------------------------------------
----------------------------------------------------------------------------
----------------------------------------------------------------------------
----Whitaker's original comments:
----Big problem.  This area has been generating exceptions.
----At least one difficulty is that suffixes change POFS.
----So one has a N inflection (SL) but a V DE
----When the program checks for VOC, it wants a N
----and then asks about KIND (P, N, T,...)
----But the DE (v) does not have those
----The solution would be to fix ADD SUFFIX to do something about passing the ADDON KIND
----I do not want to face that now
----It is likely that all this VOC/LOC is worthless anyway.  Maybe lower FREQ in INFLECTS
----
----A further complication is that GNAT and AO [SPR: I think he means OA (ObjectAda)] give different results (AO no exception)
----That is probably because the program is in error and the result therefore unspecified

--  SPR: Should be solved; see long comment below

         I := SL_LAST;
--DEBUG: PUT_LINE("Checking VOC/LOC SL_LAST = " & INTEGER'IMAGE(SL_LAST));
         while I >= SL'FIRST loop
            --  Check for Vocative being person/name and Locative a place/area
--DEBUG: TEXT_IO.PUT_LINE("Looping down on I I = " & INTEGER'IMAGE(I));
            if (SL (I).IR.QUAL.POFS = N) then
--DEBUG: TEXT_IO.PUT_LINE("N found I = " & INTEGER'IMAGE(I));
--DEBUG: PARSE_RECORD_IO.PUT(SL(I)); TEXT_IO.NEW_LINE;
               if NOT_ONLY_VOCATIVE and then (SL (I).IR.QUAL.N.CS = VOC)
                 and then
                 ((DEPR (SL (I)).PART.N.KIND /= N) and
                  (DEPR (SL (I)).PART.N.KIND /= P))
               then
--DEBUG: TEXT_IO.PUT_LINE("N VOC not a P or N I = " & INTEGER'IMAGE(I));
                  SL (I .. SL_LAST - 1) := SL (I + 1 .. SL_LAST);
                  SL_LAST               := SL_LAST - 1;
                  TRIMMED               := True;
               elsif NOT_ONLY_LOCATIVE and then (SL (I).IR.QUAL.N.CS = LOC)
                 and then
                 ((DEPR (SL (I)).PART.N.KIND /= L) and
                  (DEPR (SL (I)).PART.N.KIND /= W))
               then
--DEBUG: TEXT_IO.PUT_LINE("N LOC not a W or L ");
                  SL (I .. SL_LAST - 1) := SL (I + 1 .. SL_LAST);
                  SL_LAST               := SL_LAST - 1;
                  TRIMMED               := True;
               end if;
            end if;
            I := I - 1;
         end loop;
--DEBUG: TEXT_IO.PUT_LINE("Checked VOC/LOC");

         --  Cutting viciously here
         I := SL_LAST;
         while I >= SL'FIRST loop
            if (SL (I).IR.QUAL.POFS = ADJ) then
               if NOT_ONLY_VOCATIVE and then (SL (I).IR.QUAL.ADJ.CS = VOC) then
                  SL (I .. SL_LAST - 1) := SL (I + 1 .. SL_LAST);
                  SL_LAST               := SL_LAST - 1;
                  TRIMMED               := True;
               elsif NOT_ONLY_LOCATIVE and then (SL (I).IR.QUAL.ADJ.CS = LOC)
               then
                  SL (I .. SL_LAST - 1) := SL (I + 1 .. SL_LAST);
                  SL_LAST               := SL_LAST - 1;
                  TRIMMED               := True;
               end if;
            end if;
            I := I - 1;
         end loop;

         I := SL_LAST;
         while I >= SL'FIRST loop
            if (SL (I).IR.QUAL.POFS = VPAR) then
               if NOT_ONLY_VOCATIVE and then (SL (I).IR.QUAL.VPAR.CS = VOC)
               then
                  SL (I .. SL_LAST - 1) := SL (I + 1 .. SL_LAST);
                  SL_LAST               := SL_LAST - 1;
                  TRIMMED               := True;
               elsif NOT_ONLY_LOCATIVE and then (SL (I).IR.QUAL.VPAR.CS = LOC)
               then
                  SL (I .. SL_LAST - 1) := SL (I + 1 .. SL_LAST);
                  SL_LAST               := SL_LAST - 1;
                  TRIMMED               := True;
               end if;
            end if;
            I := I - 1;
         end loop;

         --  This is really working much too hard! just to kill Roman numeral
         --  for three single letters Also strange in that code depends on
         --  dictionary knowledge
         I := SL_LAST;
         while I >= SL'FIRST loop
            if HAS_NOUN_ABBREVIATION and then (ALL_CAPS and FOLLOWED_BY_PERIOD)
            then
               if (SL (I).IR.QUAL.POFS /= N) or
                 ((SL (I).IR.QUAL /= (N, ((9, 8), X, X, M))) and
                  (TRIM (SL (I).STEM)'LENGTH = 1
                   and then
                   (SL (I).STEM (1) =
                    'A' or        -- (because 'a' is a lexical unit)

                    SL (I).STEM (1) = 'C' or
                    SL (I).STEM (1) = 'D' or
                  --SL(I).STEM(1) = 'K'  or      -- What about V?
                  SL (I).STEM (1) = 'L' or
                    SL (I).STEM (1) = 'M'
                     --SL(I).STEM(1) = 'N'  or
                     --SL(I).STEM(1) = 'P'  or
                     --SL(I).STEM(1) = 'Q'  or
                     --SL(I).STEM(1) = 'T'
                     )))
               then
                  SL (I .. SL_LAST - 1) := SL (I + 1 .. SL_LAST);
                  SL_LAST               := SL_LAST - 1;
                  TRIMMED               := True;
               end if;
            end if;
            I := I - 1;
         end loop;

      end if;   --  On TRIM

      DIFF_J := SL_LAST_INITIAL - SL_LAST;

   end ORDER_PARSE_ARRAY;

begin                               --  LIST_SWEEP

--DEBUG: DICT_IO.READ(DICT_FILE(GENERAL), DE, 31585); DICTIONARY_ENTRY_IO.PUT(DE);
--DEBUG: TEXT_IO.PUT_LINE("#########");

   if PA'LENGTH = 0 then
      return;
   end if;

   SET_PRONOUN_Var_KIND :
   declare
      DE : DICTIONARY_ENTRY;
   begin
      for I in 1 .. PA_LAST loop
         if PA (I).D_K = GENERAL then
            DICT_IO.Set_Index (DICT_FILE (PA (I).D_K), PA (I).MNPC);
            DICT_IO.Read (DICT_FILE (PA (I).D_K), DE);

            if DE.PART.POFS = PRON and then DE.PART.PRON.DECL.WHICH = 1 then

               if PA (I).IR.QUAL.POFS = PRON
                 and then PA (I).IR.QUAL.PRON.DECL.WHICH = 1
               then

                  PA (I).IR.QUAL.PRON.KIND     := DE.PART.PRON.KIND;
                  PA (I).IR.QUAL.PRON.DECL.VAR :=
                    PRONOUN_KIND_TYPE'POS (DE.PART.PRON.KIND);
                  Has_Qu_Pron := True;
               elsif PA (I).IR.QUAL.POFS = PACK
                 and then PA (I).IR.QUAL.PACK.DECL.WHICH = 1
               then
                  PA (I).IR.QUAL.PACK.KIND     := DE.PART.PRON.KIND;
                  PA (I).IR.QUAL.PACK.DECL.VAR :=
                    PRONOUN_KIND_TYPE'POS (DE.PART.PRON.KIND);
                  Has_Qu_Pron := True;

               end if;
            elsif DE.PART.POFS = PACK and then DE.PART.PACK.DECL.WHICH = 1 then

               if PA (I).IR.QUAL.POFS = PACK
                 and then PA (I).IR.QUAL.PACK.DECL.WHICH = 1
               then

                  PA (I).IR.QUAL.PACK.KIND     := DE.PART.PACK.KIND;
                  PA (I).IR.QUAL.PACK.DECL.VAR :=
                    PRONOUN_KIND_TYPE'POS (DE.PART.PACK.KIND);
                  Has_Qu_Pron := True;
               elsif PA (I).IR.QUAL.POFS = PRON
                 and then PA (I).IR.QUAL.PRON.DECL.WHICH = 1
               then

                  PA (I).IR.QUAL.PRON.KIND     := DE.PART.PACK.KIND;
                  PA (I).IR.QUAL.PRON.DECL.VAR :=
                    PRONOUN_KIND_TYPE'POS (DE.PART.PACK.KIND);
                  Has_Qu_Pron := True;

               end if;

            end if;
         end if;
      end loop;
   end SET_PRONOUN_Var_KIND;

   ---------------------------------------------------

   --  NEED TO REMOVE DISALLOWED BEFORE DOING ANYTHING - BUT WITHOUT REORDERING

   --  The problem I seem to have to face first, if not the first problem,
   --  is the situation in which there are several sets of identical IRs with
   --  different MNPC These may be variants with some other stem (e.g., K=3)
   --  not affecting the (K=1) word Or they might be identical forms with
   --  different meanings (| additional meanings) I need to group such
   --  common inflections - and pass this on somehow

   -- SPR: Most of the "first problems" now are handled by the
   -- next_meaning_same, next_form_same, etc. variables in list_package.
   --      It's not pretty, but it handles almost almost all the corner cases I've identified. ...except qu- PRONs, which are
   --      handled in a combination of routines here and in list_package.
   --
   --      The qu- pronouns cause the most trouble.   It takes a 5-dimensional array to fully characterize
   --      their inflections (see INFLECTS.LAT for the explanation) AND THEN they have the special PACKON feature
   --      (-que, -libet, etc.).  Making things even more complicated are two structural issues.  First, Col. Whitaker maximized
   --      flexibility by separating the parse and dictionary arrays (see explanation of those structures in his list_package
   --      comments). Second, he used variant records to reduce duplication and use storage efficiently.  But this approach means that
   --      parsing qu- pronouns uniquely require information from the dictionary array (namely, the actual meanings, not just the MNPC,
   --      to prevent duplicates). It also requires treating dictionary objects from ADDONs (PACKONs) as if they were inflection
   --      objects.  The variant records make this especially tricky because but the ADDON record variants don't store the information
   --      we need in a way that maps 1:1 to PRONs.  See "Problem  Encountered with the Variant in Ada," Cryptologic Quarterly (1988)
   --      (NSA Transparency Case 63853, DOCID 3929124); Whitaker's note above re generating exceptions due to POFS variants.
   --
   --      The upshot is that we must either change objects and data structures just for qu- stems or run some complicated conditional
   --      procedures.  I'm not willing to descend into the object-oriented hell of refactoring the data structures or creating
   --      even more special qu- pronoun objects.
   --
   --      It appears Col. Whitaker didn't want to either. He minimized the processing required by truncating the output of qu- pronoun
   --      information.  Put "quique" in his version--it returns only quique PRON, has no promoun_kind information, does not
   --      differentiate the possible meanings given the pronoun_kind, returns no dict_form, and duplicates all but one of the
   --      MEANS.  Try it in this version, you'll get comprehensive results.
   --
   --      This change sets adds routines here and in list_package; it also defines a couple helper functions in dictionary_package and
   --      the inflections package.  The new routines address four areas:  First, that qu- pronouns require special treatment when
   --      eliminating duplicates and sorting the results.  Col. Whitaker had routines in both list_sweep and list_package to address
   --      aspects of these sweep-and-sort issues.  Several of those routines are removed, one is replaced, and one is added.
   --      The net changes localize the qu-pronoun-specific code.  They add some overhead, but most of the additional processing
   --      is triggered only when qu-pronouns exist in the results.  Second, Col. Whitaker's dictform largely ignored qu- pronouns.
   --      Now qu-pronoun results include proper dictforms.  They also provide KIND information (relative, indefinite, adjectival, etc.).
   --      Third, list_package's output procedure needs special logic to eliminate duplicate output.  The new dictforms help list_package
   --      determine which FORMs are duplicates.  Finally, list_package needs a handful of changes to prevent duplicate or omitted output
   --      because the volume of qu-pronoun results and the inability to use MNPCs to determine duplicates lead to variety of output problems.
   --      The new code adds overhead here by adding new checks before outputting a result, but I suspect these new changes eliminate output
   --      problems that can arise under different circumstances.  See the comment attached to the Saved_Meaning_J code.

--DEBUG: TEXT_IO.PUT_LINE("PA before SWEEPING in LIST_SWEEP     PA_LAST = " & INTEGER'IMAGE(PA_LAST));
--DEBUG: for I in 1..PA_LAST  loop
--DEBUG: PARSE_RECORD_IO.PUT(PA(I)); TEXT_IO.NEW_LINE;
--DEBUG: end loop;

   SWEEPING :
   --  To remove disallowed stems/inflections and resulting dangling fixes
   declare
      FIX_ON  : Boolean := False;
      PW_ON   : Boolean := False;
      P_FIRST : Integer := 1;
      P_LAST  : Integer := 0;
      subtype XONS is PART_OF_SPEECH_TYPE range TACKON .. SUFFIX;

   begin

--DEBUG: TEXT_IO.NEW_LINE;
--DEBUG: TEXT_IO.PUT_LINE("SWEEPING    ======================================");
--DEBUG: TEXT_IO.NEW_LINE;
--DEBUG: TEXT_IO.PUT("{");
      J := PA_LAST;

      while J >= 1 loop        --  Sweep backwards over PA

         --           if (not ALLOWED_STEM(PA(J))   or                 --  Remove not ALLOWED_STEM & null
--               (PA(J) = NULL_PARSE_RECORD))  then         --  and close ranks
         --DEBUG: TEXT_IO.PUT_LINE("Removing dis ALLOWED STEM J = " &
         --DEBUG: INTEGER'IMAGE(J));
         --               PA(J..PA_LAST-1) := PA(J+1..PA_LAST);     --  null if J = PA_LAST
         --              PA_LAST := PA_LAST - 1;
         --              P_LAST := P_LAST - 1;
         --              TRIMMED := TRUE;

         if ((PA (J).D_K in ADDONS .. YYY) or (PA (J).IR.QUAL.POFS in XONS))
           and then (PW_ON)
         then               --  first FIX/TRICK after regular
            FIX_ON  := True;
            PW_ON   := False;
            P_FIRST := J + 1;

            --DEBUG: TEXT_IO.PUT_LINE("SWEEP  FIX/TRICK  J = " & INTEGER'IMAGE(J) & "  P_FIRST = " & INTEGER'IMAGE(P_FIRST) &
            --DEBUG: "  P_LAST = " & INTEGER'IMAGE(P_LAST));

            JJ := J;

            while PA (JJ + 1).IR.QUAL.POFS = PA (JJ).IR.QUAL.POFS loop
               P_LAST := JJ + 1;
               exit when JJ + 1 > PA'Last; -- prevent infinite loop
            end loop;

            ----Order internal to this set of inflections
--DEBUG:   TEXT_IO.PUT_LINE("SWEEP INTERNAL J = " & INTEGER'IMAGE(J) & " P_FIRST = " &
--DEBUG:   INTEGER'IMAGE(P_FIRST) & " P_LAST = " & INTEGER'IMAGE(P_LAST) & " DIFF_J = " &
--DEBUG:   INTEGER'IMAGE(DIFF_J) & " PA_LAST = " & INTEGER'IMAGE(PA_LAST));

            ORDER_PARSE_ARRAY (PA (P_FIRST .. P_LAST), DIFF_J);
            PA (P_LAST - DIFF_J + 1 .. PA_LAST - DIFF_J) :=
              PA (P_LAST + 1 .. PA_LAST);
            PA_LAST := PA_LAST - DIFF_J;

--DEBUG:  TEXT_IO.PUT_LINE("SWEEP INTERNAL end J = " & INTEGER'IMAGE(J) & " P_FIRST = " &
--DEBUG:  INTEGER'IMAGE(P_FIRST) & " P_LAST = " & INTEGER'IMAGE(P_LAST) & " DIFF_J = " &
--DEBUG:  INTEGER'IMAGE(DIFF_J) & " PA_LAST = " & INTEGER'IMAGE(PA_LAST));
            P_FIRST := 1;
            P_LAST  := 0;

         elsif ((PA (J).D_K in ADDONS .. YYY) or (PA (J).IR.QUAL.POFS in XONS))
           and then (FIX_ON)
         then               --  another FIX
--DEBUG: TEXT_IO.PUT_LINE("SWEEP  Another FIX/TRICK  J = " & INTEGER'IMAGE(J));
            null;

         elsif ((PA (J).D_K in ADDONS .. YYY) or (PA (J).IR.QUAL.POFS = X))
           and then  --  Kills TRICKS stuff

           (not PW_ON)
         then
   --DEBUG: TEXT_IO.PUT_LINE("Killing Tricks stuff  J = " & INTEGER'IMAGE(J));

            PA (P_LAST - DIFF_J + 1 .. PA_LAST - DIFF_J) :=
              PA (P_LAST + 1 .. PA_LAST);
            PA_LAST := PA_LAST - DIFF_J;

            P_LAST := P_LAST - 1;

         else
--DEBUG: TEXT_IO.PUT_LINE("SWEEP else J = " & INTEGER'IMAGE(J) & " P_LAST = " &
--DEBUG: INTEGER'IMAGE(P_LAST));
--DEBUG: for I in 1..PA_LAST  loop
--DEBUG: PARSE_RECORD_IO.PUT(PA(I)); TEXT_IO.NEW_LINE;
            --DEBUG: end loop;

            PW_ON  := True;
            FIX_ON := False;
            if P_LAST <= 0 then
               P_LAST := J;
            end if;
            if J = 1 then
--DEBUG: TEXT_IO.PUT_LINE("SWEEP  J = 1     P_LAST = " & INTEGER'IMAGE(P_LAST));
               ORDER_PARSE_ARRAY (PA (1 .. P_LAST), DIFF_J);
               PA (P_LAST - DIFF_J + 1 .. PA_LAST - DIFF_J) :=
                 PA (P_LAST + 1 .. PA_LAST);
               PA_LAST := PA_LAST - DIFF_J;
--DEBUG: TEXT_IO.PUT_LINE("SWEEP  J = 1 end    PA_LAST = " & INTEGER'IMAGE(PA_LAST) & "  DIFF_J = " & INTEGER'IMAGE(DIFF_J));
            end if;

         end if; --  check PART

         J := J - 1;

      end loop;  --  loop sweep over PA

   end SWEEPING;

   OPR := PA (1);
   --  Last chance to weed out duplicates
   J := 2;
   COMPRESS_LOOP :
   loop
      exit when J > PA_LAST;
      PR := PA (J);
      if PR /= OPR then
         SUPRESS_KEY_CHECK :
         declare
            function "<=" (A, B : in PARSE_RECORD) return Boolean is
            begin                             --  !!!!!!!!!!!!!!!!!!!!!!!!!!

               if A.IR.QUAL = B.IR.QUAL and then A.MNPC = B.MNPC then
                  return True;
               else
                  return False;
               end if;
            end "<=";

         begin

            if ((PR.D_K /= XXX) and (PR.D_K /= YYY) and (PR.D_K /= PPP)) then
               if (PR <= OPR) then
                  PA (J .. PA_LAST - 1) :=
                    PA (J + 1 .. PA_LAST);  --  Shift PA down 1
                  PA_LAST := PA_LAST - 1;   --  because found key duplicate

                  -- SPR: The elsif that follows stops duplicative entries for
                  -- suffixes with multiple stems SPR: (for example, 'ludica');
                  -- should also prevent other weird duplicate entries
               elsif
                 (J + 1 <=
                  PA_LAST) -- Must include range check or exceptions will be raised
                  -- in some situations (e.g., 'fame')

               then
                  if (PA (J) = PA (J + 1)) then
                     PA (J .. PA_LAST - 1) := PA (J + 1 .. PA_LAST);
                     PA_LAST               := PA_LAST - 1;
                  end if;
               end if;

            else
               J := J + 1;
            end if;

         end SUPRESS_KEY_CHECK;
      else
         J := J + 1;

      end if;

      OPR := PR;

   end loop COMPRESS_LOOP;

   if Has_Qu_Pron then
      Qu_PA_Fix :
      declare

         Qu_PA, OPA : PARSE_ARRAY (PA'Range) := (others => NULL_PARSE_RECORD);
         Qu_Next, Next : Integer                := PA'First;
         De_J          : DICTIONARY_ENTRY       := NULL_DICTIONARY_ENTRY;
         De_K          : DICTIONARY_ENTRY       := NULL_DICTIONARY_ENTRY;
      begin
         -- separate Qu PRON/PACKs from everything else
         for I in PA'range loop                             -- I loop
            if
              ((PA (I).IR.QUAL.POFS = PRON or PA (I).IR.QUAL.POFS = PACK)
               and then Qr_Pack_To_PRON (PA (I).IR.QUAL).DECL.WHICH = 1)
              or else
              (PA (I).D_K = ADDONS
               and then ADDONS_PACKAGE.MEANS (Integer (PA (I).MNPC)) (1 .. 6) =
                 "PACKON")
            then
               Qu_PA (Qu_Next) := PA (I);
               Qu_Next         := Qu_Next + 1;
            else
               OPA (Next) := PA (I);
               Next       := Next + 1;
            end if;
         end loop; -- I

         for J in Qu_PA'FIRST .. (Qu_PA'Last - 1)
         loop     -- remove duplicate PRON 1 X and PACKs

            if Qu_PA (J) /= NULL_PARSE_RECORD then
               for K in J + 1 .. Qu_PA'Last loop

                  if Qu_PA (J).D_K = GENERAL and then Qu_PA (K).D_K = GENERAL
                    and then Qr_Pack_To_PRON (Qu_PA (J).IR.QUAL) =
                      Qr_Pack_To_PRON (Qu_PA (K).IR.QUAL)
                  then

                     De_K := NULL_DICTIONARY_ENTRY;
                     De_J := NULL_DICTIONARY_ENTRY;

                     DICT_IO.Set_Index
                       (DICT_FILE (Qu_PA (K).D_K), Qu_PA (K).MNPC);
                     DICT_IO.Read (DICT_FILE (Qu_PA (K).D_K), De_K);

                     DICT_IO.Set_Index
                       (DICT_FILE (Qu_PA (J).D_K), Qu_PA (J).MNPC);
                     DICT_IO.Read (DICT_FILE (Qu_PA (J).D_K), De_J);

                     if De_J.STEMS (1) = De_K.STEMS (1)
                       and then De_J.MEAN = De_K.MEAN
                     then
                        Qu_PA (K) := NULL_PARSE_RECORD;
                     end if;

                  elsif Qu_PA (J).D_K = ADDONS and then Qu_PA (K).D_K = ADDONS
                    and then
                    (Qu_PA (J).MNPC = Qu_PA (K).MNPC
                     or else
                     (ADDONS_PACKAGE.MEANS (Integer (Qu_PA (J).MNPC)) =
                      ADDONS_PACKAGE.MEANS (Integer (Qu_PA (K).MNPC))))
                  then
                     Qu_PA (K) := NULL_PARSE_RECORD;

                  end if;

               end loop; -- K
            end if;   -- J /= null_parse_record

         end loop; -- J

         PA := (others => NULL_PARSE_RECORD);

         Next := PA'First;

         for Z in PRONOUN_KIND_TYPE'Range loop
            for X in CASE_TYPE'Range loop
               for L in Qu_PA'range loop
                  for M in NUMBER_TYPE'range loop
                     for N in GENDER_TYPE'range loop
                        if Qu_PA (L) /= NULL_PARSE_RECORD then
                           if
                             (Qu_PA (L).IR.QUAL.POFS = PRON
                              and then Qu_PA (L).IR.QUAL.PRON.KIND = Z
                              and then Qu_PA (L).IR.QUAL.PRON.CS = X
                              and then Qu_PA (L).IR.QUAL.PRON.NUMBER = M
                              and then Qu_PA (L).IR.QUAL.PRON.GENDER = N)
                              -- or
                              --   ( Qu_PA(L).Ir.Qual.Pofs = Pack
                              --    and then Qu_PA(L).Ir.Qual.Pack.Kind = Z )
                              or
                             Qu_PA (L).D_K = ADDONS
                           then
                              PA (Next) := Qu_PA (L);
                              Next      := Next + 1;
                              Qu_PA (L) := NULL_PARSE_RECORD;

                           end if;
                        end if;
                     end loop; -- N
                  end loop; -- M
               end loop; -- L
            end loop; -- X
         end loop; -- Z

         for M in OPA'Range loop
            if OPA (M) /= NULL_PARSE_RECORD then
               PA (Next) := OPA (M);
               Next      := Next + 1;
               exit when Next > PA'Last;
            end if;
         end loop; -- M (2nd)

      end Qu_PA_Fix;
   end if;  -- has_qu_pron

   for I in 1 .. PA_LAST loop

      if PA (I).IR.QUAL.POFS = V then
         if PA (I).IR.QUAL.V.CON = (3, 4)
         then --  Fix V 3 4 to be 4th conjugation
            PA (I).IR.QUAL.V.CON := (4, 1);
         end if;
      end if;
   end loop;

--DEBUG:     declare -- debug block
--DEBUG:        De : DICTIONARY_ENTRY;
--DEBUG:        begin
--DEBUG:      TEXT_IO.PUT_LINE("PA after COMPRESS -- ABOUT TO LEAVE LIST_STEMS    PA_LAST = "  & INTEGER'IMAGE(PA_LAST));
--DEBUG:      for I in 1..PA_LAST  loop
--DEBUG:        PARSE_RECORD_IO.PUT(PA(I)); TEXT_IO.NEW_LINE;
--DEBUG:        if PA(I).D_K = General then
--DEBUG:        DICT_IO.SET_INDEX(DICT_FILE(PA(I).D_K), PA(I).MNPC);
--DEBUG:        DICT_IO.READ(DICT_FILE(PA(I).D_K), DE);
--DEBUG:        end if;
--DEBUG:        Text_IO.Put_Line(DE.MEAN);
--DEBUG:        end loop;
--DEBUG:      end;  -- debug block

end LIST_SWEEP;
