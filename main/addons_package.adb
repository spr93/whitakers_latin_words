with STRINGS_PACKAGE;      use STRINGS_PACKAGE;
with DEVELOPER_PARAMETERS; use DEVELOPER_PARAMETERS;
with PREFACE;

package body ADDONS_PACKAGE is
   use PART_OF_SPEECH_TYPE_IO;
   use TARGET_ENTRY_IO;
   use STEM_KEY_TYPE_IO;

   function EQU (C, D : Character) return Boolean is
   begin
      if (D = 'u') or (D = 'v') then
         if (C = 'u') or (C = 'v') then
            return True;
         else
            return False;
         end if;
      else
         return C = D;
      end if;
   end EQU;

   function EQU (S, T : String) return Boolean is
   begin
      if S'LENGTH /= T'LENGTH then
         return False;
      end if;

      for I in 1 .. S'LENGTH loop
         if not EQU (S (S'FIRST + I - 1), T (T'FIRST + I - 1)) then
            return False;
         end if;
      end loop;

      return True;
   end EQU;

   procedure LOAD_ADDONS (FILE_NAME : in String) is
      use TACKON_ENTRY_IO;
      use PREFIX_ENTRY_IO;
      use SUFFIX_ENTRY_IO;

      S                                : String (1 .. 100);
      L, LAST, TIC, PRE, SUF, TAC, PAC : Integer                  := 0;
      ADDONS_FILE                      : Text_IO.File_Type;
      POFS                             : PART_OF_SPEECH_TYPE;
      DE : DICTIONARY_ENTRY := NULL_DICTIONARY_ENTRY;
      MEAN                             : MEANING_TYPE := NULL_MEANING_TYPE;
      M                                : Integer                  := 1;
      --TG : TARGET_ENTRY;
      TN : TACKON_ENTRY;
      PM : PREFIX_ITEM;
      TS : STEM_TYPE;

      procedure GET_NO_COMMENT_LINE
        (F : in Text_IO.File_Type; S : out String; LAST : out Integer)
      is
         T : String (1 .. 250) := (others => ' ');
         L : Integer           := 0;
      begin
         LAST := 0;
         while not End_Of_File (F) loop
            Get_Line (F, T, L);
            if L >= 2
              and then
              (HEAD (TRIM (T), 250) (1 .. 2) = "--" or
               HEAD (TRIM (T), 250) (1 .. 2) = "  ")
            then
               null;
            else
               S ((S'First) .. L) := T (1 .. L);
               LAST               := L;
               exit;
            end if;
         end loop;
      end GET_NO_COMMENT_LINE;

      procedure EXTRACT_FIX
        (S : in String; XFIX : out FIX_TYPE; XC : out Character)
      is
         ST : constant String := TRIM (S);
         L  : Integer         := ST'LENGTH;
         J  : Integer         := 0;
      begin
         for I in 1 .. L loop
            J := I;
            exit when ((I < L) and then (ST (I + 1) = ' '));
         end loop;
         XFIX := HEAD (ST (1 .. J), MAX_FIX_SIZE);
         if J = L then     --  there is no CONNECT CHARACTER
            XC := ' ';
            return;
         else
            for I in J + 1 .. L loop
               if ST (I) /= ' ' then
                  XC := ST (I);
                  exit;
               end if;
            end loop;
         end if;
         return;
      end EXTRACT_FIX;

   begin
      Open (ADDONS_FILE, In_File, FILE_NAME);
      PREFACE.PUT ("ADDONS");
      PREFACE.PUT ("  loading ");

      while not End_Of_File (ADDONS_FILE) loop

         DE := NULL_DICTIONARY_ENTRY;
         GET_NO_COMMENT_LINE (ADDONS_FILE, S, LAST);
--TEXT_IO.PUT_LINE(S(1..LAST));
         Get (S (1 .. LAST), POFS, L);
         case POFS is
            when TACKON =>
               TS           := HEAD (TRIM (S (L + 1 .. LAST)), MAX_STEM_SIZE);
               DE.STEMS (1) := TS;

               Get_Line (ADDONS_FILE, S, LAST);
               GET (S (1 .. LAST), TN, L);
               Get_Line (ADDONS_FILE, S, LAST);
               MEAN := HEAD (S (1 .. LAST), MAX_MEANING_SIZE);

               if TN.BASE.POFS = PACK
                 and then
                 (TN.BASE.PACK.DECL.WHICH = 1 or TN.BASE.PACK.DECL.WHICH = 2)
                 and then MEAN (1 .. 9) = "PACKON w/"
               then
                  PAC                := PAC + 1;
                  PACKONS (PAC).POFS := POFS;
                  PACKONS (PAC).TACK := TS;
                  PACKONS (PAC).ENTR := TN;
                  PACKONS (PAC).MNPC := M;
                  MEANS (M)          := MEAN;
                  M                  := M + 1;

               else
                  TAC                := TAC + 1;
                  TACKONS (TAC).POFS := POFS;
                  TACKONS (TAC).TACK := TS;
                  TACKONS (TAC).ENTR := TN;
                  TACKONS (TAC).MNPC := M;
                  MEANS (M)          := MEAN;
                  M                  := M + 1;
               end if;

               NUMBER_OF_PACKONS := PAC;
               NUMBER_OF_TACKONS := TAC;

            when PREFIX =>

               EXTRACT_FIX (S (L + 1 .. LAST), PM.FIX, PM.CONNECT);
               Get_Line (ADDONS_FILE, S, LAST);
               GET (S (1 .. LAST), PM.ENTR, L);
               Get_Line (ADDONS_FILE, S, LAST);
               MEAN := HEAD (S (1 .. LAST), MAX_MEANING_SIZE);

               if PM.ENTR.ROOT = PACK then
                  TIC                   := TIC + 1;
                  TICKONS (TIC).POFS    := POFS;
                  TICKONS (TIC).FIX     := PM.FIX;
                  TICKONS (TIC).CONNECT := PM.CONNECT;
                  TICKONS (TIC).ENTR    := PM.ENTR;
                  TICKONS (TIC).MNPC    := M;
                  MEANS (M)             := MEAN;
                  M                     := M + 1;

               else
                  PRE                    := PRE + 1;
                  PREFIXES (PRE).POFS    := POFS;
                  PREFIXES (PRE).FIX     := PM.FIX;
                  PREFIXES (PRE).CONNECT := PM.CONNECT;
                  PREFIXES (PRE).ENTR    := PM.ENTR;
                  DE.MEAN                := MEAN;
                  PREFIXES (PRE).MNPC    := M;
                  MEANS (M)              := MEAN;
                  M                      := M + 1;
               end if;

               NUMBER_OF_TICKONS  := TIC;
               NUMBER_OF_PREFIXES := PRE;

            when SUFFIX =>
               SUF                 := SUF + 1;
               SUFFIXES (SUF).POFS := POFS;
--TEXT_IO.PUT_LINE(S(1..LAST));
               EXTRACT_FIX
                 (S (L + 1 .. LAST), SUFFIXES (SUF).FIX,
                  SUFFIXES (SUF).CONNECT);
--TEXT_IO.PUT("@1");
               Get_Line (ADDONS_FILE, S, LAST);
--TEXT_IO.PUT("@2");
--TEXT_IO.PUT_LINE(S(1..LAST) & "<");
--TEXT_IO.PUT("@2");
               GET (S (1 .. LAST), SUFFIXES (SUF).ENTR, L);
--TEXT_IO.PUT("@3");
               Get_Line (ADDONS_FILE, S, LAST);
--TEXT_IO.PUT("@4");
               MEAN := HEAD (S (1 .. LAST), MAX_MEANING_SIZE);
--TEXT_IO.PUT("@5");
               SUFFIXES (SUF).MNPC := M;
               MEANS (M)           := MEAN;
               M                   := M + 1;

               NUMBER_OF_SUFFIXES := SUF;

            when others =>
               Text_IO.Put_Line ("Bad ADDON");
               Text_IO.Put_Line (S (1 .. LAST));
               raise Text_IO.Data_Error;
         end case;

      end loop;

      PREFACE.PUT (TAC, 1);
      PREFACE.PUT ("+");
      PREFACE.PUT (PAC, 2);
      PREFACE.PUT (" tackons ");
      PREFACE.PUT (TIC, 1);
      PREFACE.PUT ("+");
      PREFACE.PUT (PRE, 3);
      PREFACE.PUT (" prefixes  ");
      PREFACE.PUT (SUF, 3);
      PREFACE.PUT (" suffixes ");

      PREFACE.SET_COL (60);
      PREFACE.PUT_LINE ("--  loaded correctly");
      Close (ADDONS_FILE);

   exception
      when Text_IO.Name_Error =>
         PREFACE.PUT_LINE ("No ADDONS file ");
         null;
      when Text_IO.Data_Error =>
         PREFACE.PUT_LINE (S (1 .. LAST));
         PREFACE.PUT_LINE ("No further ADDONS read ");
         Close (ADDONS_FILE);
      when others =>
         PREFACE.PUT_LINE ("Exception in LOAD_ADDONS");
         PREFACE.PUT_LINE (S (1 .. LAST));
   end LOAD_ADDONS;

   function SUBTRACT_TACKON (W : String; X : TACKON_ITEM) return String is
      WD : constant String  := TRIM (W);
      L  : constant Integer := WD'LENGTH;
      XF : constant String  := TRIM (X.TACK);
      Z  : constant Integer := XF'LENGTH;
   begin
--PUT_LINE("In SUB TACKON " & INTEGER'IMAGE(L) & INTEGER'IMAGE(Z));
      if WORDS_MDEV (USE_TACKONS) and then L > Z
        and then
         --WD(L-Z+1..L) = XF(1..Z)  then
         EQU (WD (L - Z + 1 .. L), XF (1 .. Z))
      then
--PUT("In SUBTRACT_TACKON we got a hit   "); PUT_LINE(X.TACK);
         return WD (1 .. L - Z);
      else
--PUT("In SUBTRACT_TACKON    NO    hit   "); PUT_LINE(X.TACK);
         return W;
      end if;
   end SUBTRACT_TACKON;

   function SUBTRACT_PREFIX (W : String; X : PREFIX_ITEM) return STEM_TYPE is
      WD : constant String  := TRIM (W);
      XF : constant String  := TRIM (X.FIX);
      Z  : constant Integer := XF'LENGTH;
      ST : STEM_TYPE        := HEAD (WD, MAX_STEM_SIZE);
   begin
      if WORDS_MDEV (USE_PREFIXES) and then X /= NULL_PREFIX_ITEM
        and then WD'LENGTH > Z and then
         --WD(1..Z) = XF(1..Z)  and then
         EQU (WD (1 .. Z), XF (1 .. Z))
        and then ((X.CONNECT = ' ') or (WD (Z + 1) = X.CONNECT))
      then
         ST (1 .. WD'LENGTH - Z)                 := WD (Z + 1 .. WD'LAST);
         ST (WD'LENGTH - Z + 1 .. MAX_STEM_SIZE) :=
           NULL_STEM_TYPE (WD'LENGTH - Z + 1 .. MAX_STEM_SIZE);
      end if;
--PUT_LINE("SUBTRACT_PREFIX  " & X.FIX & " FROM " & WD & "  returns " & ST);
      return ST;
   end SUBTRACT_PREFIX;

   function SUBTRACT_SUFFIX (W : String; X : SUFFIX_ITEM) return STEM_TYPE is
      WD : constant String  := TRIM (W);
      L  : constant Integer := WD'LENGTH;
      XF : constant String  := TRIM (X.FIX);
      Z  : constant Integer := XF'LENGTH;
      ST : STEM_TYPE        := HEAD (WD, MAX_STEM_SIZE);
   begin
--PUT_LINE("In SUBTRACT_SUFFIX  Z = " & INTEGER'IMAGE(Z) &
--"  CONNECT >" & X.CONNECT & '<');
      if WORDS_MDEV (USE_SUFFIXES) and then X /= NULL_SUFFIX_ITEM
        and then WD'LENGTH > Z and then
         --WD(L-Z+1..L) = XF(1..Z)  and then
         EQU (WD (L - Z + 1 .. L), XF (1 .. Z))
        and then ((X.CONNECT = ' ') or (WD (L - Z) = X.CONNECT))
      then
--PUT_LINE("In SUBTRACT_SUFFIX we got a hit");
         ST (1 .. WD'LENGTH - Z)                 := WD (1 .. WD'LENGTH - Z);
         ST (WD'LENGTH - Z + 1 .. MAX_STEM_SIZE) :=
           NULL_STEM_TYPE (WD'LENGTH - Z + 1 .. MAX_STEM_SIZE);
      end if;
--PUT_LINE("SUBTRACT_SUFFIX  " & X.FIX & " FROM " & WD & "  returns " & ST);
      return ST;
   end SUBTRACT_SUFFIX;

   function ADD_PREFIX
     (STEM : STEM_TYPE; PREFIX : PREFIX_ITEM) return STEM_TYPE
   is
      FPX : constant String := TRIM (PREFIX.FIX) & STEM;
   begin
      if WORDS_MDEV (USE_PREFIXES) then
         return HEAD (FPX, MAX_STEM_SIZE);
      else
         return STEM;
      end if;
   end ADD_PREFIX;

   function ADD_SUFFIX
     (STEM : STEM_TYPE; SUFFIX : SUFFIX_ITEM) return STEM_TYPE
   is
      FPX : constant String := TRIM (STEM) & SUFFIX.FIX;
   begin
      if WORDS_MDEV (USE_SUFFIXES) then
         return HEAD (FPX, MAX_STEM_SIZE);
      else
         return STEM;
      end if;
   end ADD_SUFFIX;

   package body TARGET_ENTRY_IO is
      use NOUN_ENTRY_IO;
      use PRONOUN_ENTRY_IO;
      use PROPACK_ENTRY_IO;
      use ADJECTIVE_ENTRY_IO;
      use NUMERAL_ENTRY_IO;
      use ADVERB_ENTRY_IO;
      use VERB_ENTRY_IO;

      SPACER : Character := ' ';

      NOUN      : NOUN_ENTRY;
      PRONOUN   : PRONOUN_ENTRY;
      PROPACK   : PROPACK_ENTRY;
      ADJECTIVE : ADJECTIVE_ENTRY;
      NUMERAL   : NUMERAL_ENTRY;
      ADVERB    : ADVERB_ENTRY;
      VERB      : VERB_ENTRY;

      procedure GET (F : in File_Type; P : out TARGET_ENTRY) is
         PS : TARGET_POFS_TYPE := X;
      begin
         Get (F, PS);
         Get (F, SPACER);
         case PS is
            when N =>
               GET (F, NOUN);
               --GET(F, NOUN_KIND);
               P := (N, NOUN);  --, NOUN_KIND);
            when PRON =>
               GET (F, PRONOUN);
               --GET(F, PRONOUN_KIND);
               P := (PRON, PRONOUN);  --, PRONOUN_KIND);
            when PACK =>
               GET (F, PROPACK);
               --GET(F, PROPACK_KIND);
               P := (PACK, PROPACK);  --, PROPACK_KIND);
            when ADJ =>
               GET (F, ADJECTIVE);
               P := (ADJ, ADJECTIVE);
            when NUM =>
               GET (F, NUMERAL);
               --GET(F, NUMERAL_VALUE);
               P := (NUM, NUMERAL);  --, NUMERAL_VALUE);
            when ADV =>
               GET (F, ADVERB);
               P := (ADV, ADVERB);
            when V =>
               GET (F, VERB);
               --GET(F, VERB_KIND);
               P := (V, VERB);  --, VERB_KIND);
            when X =>
               P := (POFS => X);
         end case;
         return;
      end GET;

      procedure GET (P : out TARGET_ENTRY) is
         PS : TARGET_POFS_TYPE := X;
      begin
         Get (PS);
         Get (SPACER);
         case PS is
            when N =>
               GET (NOUN);
               --GET(NOUN_KIND);
               P := (N, NOUN);  --, NOUN_KIND);
            when PRON =>
               GET (PRONOUN);
               --GET(PRONOUN_KIND);
               P := (PRON, PRONOUN);  --, PRONOUN_KIND);
            when PACK =>
               GET (PROPACK);
               --GET(PROPACK_KIND);
               P := (PACK, PROPACK);  --, PROPACK_KIND);
            when ADJ =>
               GET (ADJECTIVE);
               P := (ADJ, ADJECTIVE);
            when NUM =>
               GET (NUMERAL);
               --GET(NUMERAL_VALUE);
               P := (NUM, NUMERAL);  --, NUMERAL_VALUE);
            when ADV =>
               GET (ADVERB);
               P := (ADV, ADVERB);
            when V =>
               GET (VERB);
               --GET(VERB_KIND);
               P := (V, VERB);  --, VERB_KIND);
            when X =>
               P := (POFS => X);
         end case;
         return;
      end GET;

      procedure PUT (F : in File_Type; P : in TARGET_ENTRY) is
         C : Positive := Positive (Col (F));
      begin
         Put (F, P.POFS);
         Put (F, ' ');
         case P.POFS is
            when N =>
               PUT (F, P.N);
               --PUT(F, P.NOUN_KIND);
            when PRON =>
               PUT (F, P.PRON);
               --PUT(F, P.PRONOUN_KIND);
            when PACK =>
               PUT (F, P.PACK);
               --PUT(F, P.PROPACK_KIND);
            when ADJ =>
               PUT (F, P.ADJ);
            when NUM =>
               PUT (F, P.NUM);
               --PUT(F, P.NUMERAL_VALUE);
            when ADV =>
               PUT (F, P.ADV);
            when V =>
               PUT (F, P.V);
               --PUT(F, P.VERB_KIND);
            when others =>
               null;
         end case;
         Put
           (F,
            String'
              ((Integer (Col (F)) .. TARGET_ENTRY_IO.DEFAULT_WIDTH + C - 1 =>
                  ' ')));
         return;
      end PUT;

      procedure PUT (P : in TARGET_ENTRY) is
         C : Positive := Positive (Col);
      begin
         Put (P.POFS);
         Put (' ');
         case P.POFS is
            when N =>
               PUT (P.N);
               --PUT(P.NOUN_KIND);
            when PRON =>
               PUT (P.PRON);
               --PUT(P.PRONOUN_KIND);
            when PACK =>
               PUT (P.PACK);
               --PUT(P.PROPACK_KIND);
            when ADJ =>
               PUT (P.ADJ);
            when NUM =>
               PUT (P.NUM);
               --PUT(P.NUMERAL_VALUE);
            when ADV =>
               PUT (P.ADV);
            when V =>
               PUT (P.V);
               --PUT(P.VERB_KIND);
            when others =>
               null;
         end case;
         Put
           (String'
              ((Integer (Col) .. TARGET_ENTRY_IO.DEFAULT_WIDTH + C - 1 =>
                  ' ')));
         return;
      end PUT;

      procedure GET (S : in String; P : out TARGET_ENTRY; LAST : out Integer)
      is
         L  : Integer          := S'FIRST - 1;
         PS : TARGET_POFS_TYPE := X;
      begin
         Get (S, PS, L);
         L := L + 1;
         case PS is
            when N =>
               GET (S (L + 1 .. S'LAST), NOUN, LAST);
               --GET(S(L+1..S'LAST), NOUN_KIND, LAST);
               P := (N, NOUN);  --, NOUN_KIND);
            when PRON =>
               GET (S (L + 1 .. S'LAST), PRONOUN, LAST);
               --GET(S(L+1..S'LAST), PRONOUN_KIND, LAST);
               P := (PRON, PRONOUN);  --, PRONOUN_KIND);
            when PACK =>
               GET (S (L + 1 .. S'LAST), PROPACK, LAST);
               --GET(S(L+1..S'LAST), PROPACK_KIND, LAST);
               P := (PACK, PROPACK);  --, PROPACK_KIND);
            when ADJ =>
               GET (S (L + 1 .. S'LAST), ADJECTIVE, LAST);
               P := (ADJ, ADJECTIVE);
            when NUM =>
               GET (S (L + 1 .. S'LAST), NUMERAL, LAST);
               --GET(S(L+1..S'LAST), NUMERAL_VALUE, LAST);
               P := (NUM, NUMERAL);  --, NUMERAL_VALUE);
            when ADV =>
               GET (S (L + 1 .. S'LAST), ADVERB, LAST);
               P := (ADV, ADVERB);
            when V =>
               GET (S (L + 1 .. S'LAST), VERB, LAST);
               --GET(S(L+1..S'LAST), VERB_KIND, LAST);
               P := (V, VERB);  --, VERB_KIND);
            when X =>
               P := (POFS => X);
         end case;
         return;
      end GET;

      procedure PUT (S : out String; P : in TARGET_ENTRY) is
         L : Integer := S'FIRST - 1;
         M : Integer := 0;
      begin
         M := L + PART_OF_SPEECH_TYPE_IO.Default_Width;
         Put (S (L + 1 .. M), P.POFS);
         L     := M + 1;
         S (L) := ' ';
         case P.POFS is
            when N =>
               M := L + NOUN_ENTRY_IO.DEFAULT_WIDTH;
               PUT (S (L + 1 .. M), P.N);
--        M := L + NOUN_KIND_TYPE_IO.DEFAULT_WIDTH;
--        PUT(S(L+1..M), P.NOUN_KIND);
            when PRON =>
               M := L + PRONOUN_ENTRY_IO.DEFAULT_WIDTH;
               PUT (S (L + 1 .. M), P.PRON);
--        M := L + PRONOUN_KIND_TYPE_IO.DEFAULT_WIDTH;
--        PUT(S(L+1..M), P.PRONOUN_KIND);
            when PACK =>
               M := L + PROPACK_ENTRY_IO.DEFAULT_WIDTH;
               PUT (S (L + 1 .. M), P.PACK);
--        M := L + PRONOUN_KIND_TYPE_IO.DEFAULT_WIDTH;
--        PUT(S(L+1..M), P.PROPACK_KIND);
            when ADJ =>
               M := L + ADJECTIVE_ENTRY_IO.DEFAULT_WIDTH;
               PUT (S (L + 1 .. M), P.ADJ);
            when NUM =>
               M := L + NUMERAL_ENTRY_IO.DEFAULT_WIDTH;
               PUT (S (L + 1 .. M), P.NUM);
--        M := L + NUMERAL_VALUE_TYPE_IO_DEFAULT_WIDTH;
--        PUT(S(L+1..M), P.PRONOUN_KIND);
            when ADV =>
               M := L + ADVERB_ENTRY_IO.DEFAULT_WIDTH;
               PUT (S (L + 1 .. M), P.ADV);
            when V =>
               M := L + VERB_ENTRY_IO.DEFAULT_WIDTH;
               PUT (S (L + 1 .. M), P.V);
--        M := L + PRONOUN_KIND_TYPE_IO.DEFAULT_WIDTH;
--        PUT(S(L+1..M), P.PRONOUN_KIND);
            when others =>
               null;
         end case;
         S (M + 1 .. S'LAST) := (others => ' ');
      end PUT;

   end TARGET_ENTRY_IO;

   package body TACKON_ENTRY_IO is

      procedure GET (F : in File_Type; I : out TACKON_ENTRY) is
      begin
         GET (F, I.BASE);
      end GET;

      procedure GET (I : out TACKON_ENTRY) is
      begin
         GET (I.BASE);
      end GET;

      procedure PUT (F : in File_Type; I : in TACKON_ENTRY) is
      begin
         PUT (F, I.BASE);
      end PUT;

      procedure PUT (I : in TACKON_ENTRY) is
      begin
         PUT (I.BASE);
      end PUT;

      procedure GET (S : in String; I : out TACKON_ENTRY; LAST : out Integer)
      is
         L : Integer := S'FIRST - 1;
      begin
         GET (S (L + 1 .. S'LAST), I.BASE, LAST);
      end GET;

      procedure PUT (S : out String; I : in TACKON_ENTRY) is
         L : Integer := S'FIRST - 1;
         M : Integer := 0;
      begin
         M := L + TARGET_ENTRY_IO.DEFAULT_WIDTH;
         PUT (S (L + 1 .. M), I.BASE);
         S (S'FIRST .. S'LAST) := (others => ' ');
      end PUT;

   end TACKON_ENTRY_IO;

   package body PREFIX_ENTRY_IO is
      SPACER : Character := ' ';

      procedure GET (F : in File_Type; P : out PREFIX_ENTRY) is
      begin
         Get (F, P.ROOT);
         Get (F, SPACER);
         Get (F, P.TARGET);
      end GET;

      procedure GET (P : out PREFIX_ENTRY) is
      begin
         Get (P.ROOT);
         Get (SPACER);
         Get (P.TARGET);
      end GET;

      procedure PUT (F : in File_Type; P : in PREFIX_ENTRY) is
      begin
         Put (F, P.ROOT);
         Put (F, ' ');
         Put (F, P.TARGET);
      end PUT;

      procedure PUT (P : in PREFIX_ENTRY) is
      begin
         Put (P.ROOT);
         Put (' ');
         Put (P.TARGET);
      end PUT;

      procedure GET (S : in String; P : out PREFIX_ENTRY; LAST : out Integer)
      is
         L : Integer := S'FIRST - 1;
      begin
         Get (S (L + 1 .. S'LAST), P.ROOT, L);
         L := L + 1;
         Get (S (L + 1 .. S'LAST), P.TARGET, LAST);
      end GET;

      procedure PUT (S : out String; P : in PREFIX_ENTRY) is
         L : Integer := S'FIRST - 1;
         M : Integer := 0;
      begin
         M := L + PART_OF_SPEECH_TYPE_IO.Default_Width;
         Put (S (L + 1 .. M), P.ROOT);
         L     := M + 1;
         S (L) := ' ';
         M     := L + PART_OF_SPEECH_TYPE_IO.Default_Width;
         Put (S (L + 1 .. M), P.TARGET);
         S (M + 1 .. S'LAST) := (others => ' ');
      end PUT;

   end PREFIX_ENTRY_IO;

   package body SUFFIX_ENTRY_IO is
      SPACER : Character := ' ';

      procedure GET (F : in File_Type; P : out SUFFIX_ENTRY) is
      begin
         Get (F, P.ROOT);
         Get (F, SPACER);
         Get (F, P.ROOT_KEY);
         Get (F, SPACER);
         GET (F, P.TARGET);
         Get (F, SPACER);
         Get (F, P.TARGET_KEY);
      end GET;

      procedure GET (P : out SUFFIX_ENTRY) is
      begin
         Get (P.ROOT);
         Get (SPACER);
         Get (P.ROOT_KEY);
         Get (SPACER);
         GET (P.TARGET);
         Get (SPACER);
         Get (P.TARGET_KEY);
      end GET;

      procedure PUT (F : in File_Type; P : in SUFFIX_ENTRY) is
      begin
         Put (F, P.ROOT);
         Put (F, ' ');
         Put (F, P.ROOT_KEY, 2);
         Put (F, ' ');
         PUT (F, P.TARGET);
         Put (F, ' ');
         Put (F, P.TARGET_KEY, 2);
      end PUT;

      procedure PUT (P : in SUFFIX_ENTRY) is
      begin
         Put (P.ROOT);
         Put (' ');
         Put (P.ROOT_KEY, 2);
         Put (' ');
         PUT (P.TARGET);
         Put (' ');
         Put (P.TARGET_KEY, 2);
      end PUT;

      procedure GET (S : in String; P : out SUFFIX_ENTRY; LAST : out Integer)
      is
         L : Integer := S'FIRST - 1;
      begin
--TEXT_IO.PUT("#1" & INTEGER'IMAGE(L));
         Get (S (L + 1 .. S'LAST), P.ROOT, L);
--TEXT_IO.PUT("#2" & INTEGER'IMAGE(L));
         L := L + 1;
         Get (S (L + 1 .. S'LAST), P.ROOT_KEY, L);
--TEXT_IO.PUT("#3" & INTEGER'IMAGE(L));
         L := L + 1;
         GET (S (L + 1 .. S'LAST), P.TARGET, L);
--TEXT_IO.PUT("#4" & INTEGER'IMAGE(L));
         L := L + 1;
         Get (S (L + 1 .. S'LAST), P.TARGET_KEY, LAST);
--TEXT_IO.PUT("#5" & INTEGER'IMAGE(LAST));
      end GET;

      procedure PUT (S : out String; P : in SUFFIX_ENTRY) is
         L : Integer := S'FIRST - 1;
         M : Integer := 0;
      begin
         M := L + PART_OF_SPEECH_TYPE_IO.Default_Width;
         Put (S (L + 1 .. M), P.ROOT);
         L     := M + 1;
         S (L) := ' ';
         M     := L + 2;
         Put (S (L + 1 .. M), P.ROOT_KEY);
         L     := M + 1;
         S (L) := ' ';
         M     := L + TARGET_ENTRY_IO.DEFAULT_WIDTH;
         PUT (S (L + 1 .. M), P.TARGET);
         L     := M + 1;
         S (L) := ' ';
         M     := L + 2;
         Put (S (L + 1 .. M), P.TARGET_KEY);
         S (M + 1 .. S'LAST) := (others => ' ');
      end PUT;

   end SUFFIX_ENTRY_IO;

begin

   PREFIX_ENTRY_IO.DEFAULT_WIDTH :=
     PART_OF_SPEECH_TYPE_IO.Default_Width + 1 +
     PART_OF_SPEECH_TYPE_IO.Default_Width;
   TARGET_ENTRY_IO.DEFAULT_WIDTH :=
     PART_OF_SPEECH_TYPE_IO.Default_Width + 1 +
     NUMERAL_ENTRY_IO.DEFAULT_WIDTH; --  Largest

   SUFFIX_ENTRY_IO.DEFAULT_WIDTH :=
     PART_OF_SPEECH_TYPE_IO.Default_Width + 1 + 2 + 1 +
     TARGET_ENTRY_IO.DEFAULT_WIDTH + 1 + 2;
   TACKON_ENTRY_IO.DEFAULT_WIDTH := TARGET_ENTRY_IO.DEFAULT_WIDTH;

end ADDONS_PACKAGE;
