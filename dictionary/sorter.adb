with Text_IO;
with Direct_IO;
with STRINGS_PACKAGE;     use STRINGS_PACKAGE;
with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
with DICTIONARY_PACKAGE;  use DICTIONARY_PACKAGE;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings;         use Ada.Strings;

procedure SORTER is
   --  This program sorts a file of lines (strings) on 5 substrings Mx..Nx Sort
   --  by stringwise (different cases), numeric, or POS enumeration

   package BOOLEAN_IO is new Text_IO.Enumeration_IO (Boolean);
   use BOOLEAN_IO;
   package INTEGER_IO is new Text_IO.Integer_IO (Integer);
   use INTEGER_IO;
   package FLOAT_IO is new Text_IO.Float_IO (Float);
   use FLOAT_IO;
   use Text_IO;

   NAME_LENGTH    : constant                  := 80;
   ST, ENTER_LINE : String (1 .. NAME_LENGTH) := (others => ' ');
   LS, LAST       : Integer                   := 0;
   INPUT_NAME     : String (1 .. 80)          := (others => ' ');

   LINE_LENGTH : constant := 300;

   CURRENT_LENGTH : Integer := 0;
   subtype TEXT_TYPE is String (1 .. LINE_LENGTH);
   -- type LINE_TYPE is
   -- record
   --   CURRENT_LENGTH : CURRENT_LINE_LENGTH_TYPE := 0;
   --   TEXT : TEXT_TYPE;
   -- end record;
   package LINE_IO is new Direct_IO (TEXT_TYPE);
   use LINE_IO;
   BLANK_TEXT : TEXT_TYPE := (others => ' ');

   LINE_TEXT : TEXT_TYPE := BLANK_TEXT;
   OLD_LINE  : TEXT_TYPE := BLANK_TEXT;
   P_LINE    : TEXT_TYPE := BLANK_TEXT;

   type SORT_TYPE is (A, C, G, U, N, F, P, R, S);
   package SORT_TYPE_IO is new Text_IO.Enumeration_IO (SORT_TYPE);
   use SORT_TYPE_IO;

   type WAY_TYPE is (I, D);
   package WAY_TYPE_IO is new Text_IO.Enumeration_IO (WAY_TYPE);
   use WAY_TYPE_IO;

   INPUT  : Text_IO.File_Type;
   OUTPUT : Text_IO.File_Type;
   WORK   : LINE_IO.File_Type;

   CLARG_FILENAME : String := "NULLNULL.NUL";

   M1, M2, M3, M4, M5 : Natural := 1;
   N1, N2, N3, N4, N5 : Natural := LINE_LENGTH;
   Z1, Z2, Z3, Z4, Z5 : Natural := 0;

   S1, S2, S3, S4, S5 : SORT_TYPE := A;
   W1, W2, W3, W4, W5 : WAY_TYPE  := I;

   ENTRY_FINISHED : exception;

   --  For section numbering of large documents and standards
   type SECTION_TYPE is record
      FIRST_LEVEL  : Integer := 0;
      SECOND_LEVEL : Integer := 0;
      THIRD_LEVEL  : Integer := 0;
      FOURTH_LEVEL : Integer := 0;
      FIFTH_LEVEL  : Integer := 0;
   end record;

   NO_SECTION : constant SECTION_TYPE := (0, 0, 0, 0, 0);

   type APPENDIX_TYPE is
     (NONE, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V,
      W, X, Y, Z);
   package APPENDIX_IO is new Text_IO.Enumeration_IO (APPENDIX_TYPE);

   type APPENDIX_SECTION_TYPE is record
      APPENDIX : APPENDIX_TYPE := NONE;
      SECTION  : SECTION_TYPE  := NO_SECTION;
   end record;

   NO_APPENDIX_SECTION : constant APPENDIX_SECTION_TYPE :=
     (NONE, (0, 0, 0, 0, 0));

   -- DEBUG
   --  procedure PUT(OUTPUT : TEXT_IO.FILE_TYPE; S : SECTION_TYPE); procedure
   --  PUT(S : SECTION_TYPE); procedure GET(FROM : in STRING;
   --                   S : out SECTION_TYPE; LAST : out POSITIVE);
   --  function "<"(A, B : SECTION_TYPE) return BOOLEAN;
   --
   --  procedure PUT(OUTPUT : TEXT_IO.FILE_TYPE; S : APPENDIX_SECTION_TYPE);
   --  procedure PUT(S : APPENDIX_SECTION_TYPE); procedure GET(FROM : in
   --  STRING;
   --                   S : out APPENDIX_SECTION_TYPE; LAST : out POSITIVE);
   --  function "<"(A, B : APPENDIX_SECTION_TYPE) return BOOLEAN;
   -- DEBUG

   procedure PUT (OUTPUT : Text_IO.File_Type; S : SECTION_TYPE) is
      LEVEL : Integer := 0;

      procedure PUT_LEVEL (OUTPUT : Text_IO.File_Type; L : Integer) is
      begin
         if L > 9_999 then
            Put (OUTPUT, "****");
         elsif L > 999 then
            Put (OUTPUT, L, 4);
         elsif L > 99 then
            Put (OUTPUT, L, 3);
         elsif L > 9 then
            Put (OUTPUT, L, 2);
         elsif L >= 0 then
            Put (OUTPUT, L, 1);
         else
            Put (OUTPUT, "**");
         end if;
      end PUT_LEVEL;

   begin
      if S.FIFTH_LEVEL <= 0 then
         if S.FOURTH_LEVEL <= 0 then
            if S.THIRD_LEVEL <= 0 then
               if S.SECOND_LEVEL <= 0 then
                  LEVEL := 1;
               else
                  LEVEL := 2;
               end if;
            else
               LEVEL := 3;
            end if;
         else
            LEVEL := 4;
         end if;
      else
         LEVEL := 5;
      end if;

      if S.FIRST_LEVEL <= 9 then
         Put (OUTPUT, ' ');
      end if;
      PUT_LEVEL (OUTPUT, S.FIRST_LEVEL);
      if LEVEL = 1 then
         Put (OUTPUT, '.');
         Put (OUTPUT, '0');           --  To match the ATLAS index convention
      end if;
      if LEVEL >= 2 then
         Put (OUTPUT, '.');
         PUT_LEVEL (OUTPUT, S.SECOND_LEVEL);
      end if;
      if LEVEL >= 3 then
         Put (OUTPUT, '.');
         PUT_LEVEL (OUTPUT, S.THIRD_LEVEL);
      end if;
      if LEVEL >= 4 then
         Put (OUTPUT, '.');
         PUT_LEVEL (OUTPUT, S.FOURTH_LEVEL);
      end if;
      if LEVEL >= 5 then
         Put (OUTPUT, '.');
         PUT_LEVEL (OUTPUT, S.FIFTH_LEVEL);
      end if;
   end PUT;

   procedure PUT (S : SECTION_TYPE) is
      LEVEL : Integer := 0;

      procedure PUT_LEVEL (L : Integer) is
      begin
         if L > 9_999 then
            Put ("****");
         elsif L > 999 then
            Put (L, 4);
         elsif L > 99 then
            Put (L, 3);
         elsif L > 9 then
            Put (L, 2);
         elsif L >= 0 then
            Put (L, 1);
         else
            Put ("**");
         end if;
      end PUT_LEVEL;

   begin
      if S.FIFTH_LEVEL = 0 then
         if S.FOURTH_LEVEL = 0 then
            if S.THIRD_LEVEL = 0 then
               if S.SECOND_LEVEL = 0 then
                  LEVEL := 1;
               else
                  LEVEL := 2;
               end if;
            else
               LEVEL := 3;
            end if;
         else
            LEVEL := 4;
         end if;
      else
         LEVEL := 5;
      end if;

      if S.FIRST_LEVEL <= 9 then
         Put (' ');
      end if;
      PUT_LEVEL (S.FIRST_LEVEL);
      Put ('.');
      if LEVEL = 1 then
         Put ('0');           --  To match the ATLAS index convention
      end if;
      if LEVEL >= 2 then
         PUT_LEVEL (S.SECOND_LEVEL);
      end if;
      if LEVEL >= 3 then
         Put ('.');
         PUT_LEVEL (S.THIRD_LEVEL);
      end if;
      if LEVEL >= 4 then
         Put ('.');
         PUT_LEVEL (S.FOURTH_LEVEL);
      end if;
      if LEVEL >= 5 then
         Put ('.');
         PUT_LEVEL (S.FIFTH_LEVEL);
      end if;
   end PUT;

   procedure GET (FROM : in String; S : out SECTION_TYPE; LAST : out Integer)
   is
      L  : Integer := 0;
      FT : Integer := FROM'FIRST;
      LT : Integer := FROM'LAST;
   begin
      S := NO_SECTION;
      if TRIM (FROM)'LAST < FROM'FIRST then
         return;   --  Empty string, no data         --  Return default
      end if;

      Get (FROM, S.FIRST_LEVEL, L);
      if L + 1 >= LT then
         LAST := L;
         return;
      end if;
      Get (FROM (L + 2 .. LT), S.SECOND_LEVEL, L);
      if L + 1 >= LT then
         LAST := L;
         return;
      end if;
      Get (FROM (L + 2 .. LT), S.THIRD_LEVEL, L);
      if L + 1 >= LT then
         LAST := L;
         return;
      end if;
      Get (FROM (L + 2 .. LT), S.FOURTH_LEVEL, L);
      if L + 1 >= LT then
         LAST := L;
         return;
      end if;
      Get (FROM (L + 2 .. LT), S.FIFTH_LEVEL, L);
      LAST := L;
      return;
   exception
      when Text_IO.End_Error =>
         LAST := L;
         return;
      when Text_IO.Data_Error =>
         LAST := L;
         return;
      when others =>
         Put
           (" Unexpected exception in GET(FROM; SECTION_TYPE) with input =>");
         Put (FROM);
         New_Line;
         LAST := L;
         raise;
   end GET;

   function "<" (A, B : SECTION_TYPE) return Boolean is
   begin

      if A.FIRST_LEVEL > B.FIRST_LEVEL then
         return False;
      elsif A.FIRST_LEVEL < B.FIRST_LEVEL then
         return True;
      else
         if A.SECOND_LEVEL > B.SECOND_LEVEL then
            return False;
         elsif A.SECOND_LEVEL < B.SECOND_LEVEL then
            return True;
         else
            if A.THIRD_LEVEL > B.THIRD_LEVEL then
               return False;
            elsif A.THIRD_LEVEL < B.THIRD_LEVEL then
               return True;
            else
               if A.FOURTH_LEVEL > B.FOURTH_LEVEL then
                  return False;
               elsif A.FOURTH_LEVEL < B.FOURTH_LEVEL then
                  return True;
               else
                  if A.FIFTH_LEVEL > B.FIFTH_LEVEL then
                     return False;
                  elsif A.FIFTH_LEVEL < B.FIFTH_LEVEL then
                     return True;
                  else
                     return False;
                  end if;
               end if;
            end if;
         end if;
      end if;

      return False;

   end "<";

   procedure PUT (OUTPUT : Text_IO.File_Type; S : APPENDIX_SECTION_TYPE) is
      use APPENDIX_IO;
   begin
      Put (OUTPUT, S.APPENDIX);
      Put (OUTPUT, ' ');
      PUT (OUTPUT, S.SECTION);
   end PUT;

   procedure PUT (S : APPENDIX_SECTION_TYPE) is
      use APPENDIX_IO;
   begin
      Put (S.APPENDIX);
      Put (' ');
      PUT (S.SECTION);
   end PUT;

   procedure GET
     (FROM : in String; S : out APPENDIX_SECTION_TYPE; LAST : out Integer)
   is
      use APPENDIX_IO;
      L  : Integer := 0;
      FT : Integer := FROM'FIRST;
      LT : Integer := FROM'LAST;
   begin

      S := NO_APPENDIX_SECTION;
      if (FT = LT) or else (TRIM (FROM)'LENGTH = 0)
      then   --  Empty/blank string, no data
         Put ("@");
         return;                      --  Return default
      end if;

      --PUT_LINE("In GET =>" & FROM & '|');

      begin
         Get (FROM, S.APPENDIX, L);
         --PUT("A");
         if L + 1 >= LT then
            LAST := L;
            return;
         end if;
      exception
         when others =>
            S.APPENDIX := NONE;
            L          := FT - 2;
      end;

      GET (FROM (L + 2 .. LT), S.SECTION, L);
      --PUT("F");
      return;
   exception
      when Text_IO.End_Error =>
         LAST := L;
         return;
      when Text_IO.Data_Error =>
         LAST := L;
         return;
      when others =>
         Put
           (" Unexpected exception in GET(FROM; APPENDIX_SECTION_TYPE) with input =>");
         Put (FROM);
         New_Line;
         LAST := L;
         return;
   end GET;

   function "<" (A, B : APPENDIX_SECTION_TYPE) return Boolean is
   begin

      if A.APPENDIX > B.APPENDIX then
         return False;
      elsif A.APPENDIX < B.APPENDIX then
         return True;
      else
         if A.SECTION.FIRST_LEVEL > B.SECTION.FIRST_LEVEL then
            return False;
         elsif A.SECTION.FIRST_LEVEL < B.SECTION.FIRST_LEVEL then
            return True;
         else
            if A.SECTION.SECOND_LEVEL > B.SECTION.SECOND_LEVEL then
               return False;
            elsif A.SECTION.SECOND_LEVEL < B.SECTION.SECOND_LEVEL then
               return True;
            else
               if A.SECTION.THIRD_LEVEL > B.SECTION.THIRD_LEVEL then
                  return False;
               elsif A.SECTION.THIRD_LEVEL < B.SECTION.THIRD_LEVEL then
                  return True;
               else
                  if A.SECTION.FOURTH_LEVEL > B.SECTION.FOURTH_LEVEL then
                     return False;
                  elsif A.SECTION.FOURTH_LEVEL < B.SECTION.FOURTH_LEVEL then
                     return True;
                  else
                     if A.SECTION.FIFTH_LEVEL > B.SECTION.FIFTH_LEVEL then
                        return False;
                     elsif A.SECTION.FIFTH_LEVEL < B.SECTION.FIFTH_LEVEL then
                        return True;
                     else
                        return False;
                     end if;
                  end if;
               end if;
            end if;
         end if;
      end if;
   end "<";

   procedure PROMPT_FOR_ENTRY (ENTRY_NUMBER : String) is
   begin
      Put ("Give starting column and size of ");
      Put (ENTRY_NUMBER);
      Put_Line (" significant sort field ");
      Put ("  with optional sort type and way  => ");
   end PROMPT_FOR_ENTRY;

   procedure GET_ENTRY
     (MX, NX : out Natural; SX : out SORT_TYPE; WX : out WAY_TYPE)
   is
      M : Natural   := 1;
      N : Natural   := LINE_LENGTH;
      S : SORT_TYPE := A;
      W : WAY_TYPE  := I;
      Z : Natural   := 0;

      procedure ECHO_ENTRY is
      begin
         Text_IO.Put_Line
           ("M, N, S, W:  " & M'Image & ", " & N'Image & ", " & S'Image &
            ", " & W'Image);
         Put ("                    Sorting on LINE(");
         Put (M, 3);
         Put ("..");
         Put (N, 3);
         Put (")");
         Put ("  with S = ");
         Put (S);
         Put (" and W = ");
         Put (W);
         New_Line (2);
      end ECHO_ENTRY;

   begin

      M := 0;
      N := LINE_LENGTH;
      S := A;
      W := I;

      Get_Line (ENTER_LINE, LS);
      if LS = 0 then
         raise ENTRY_FINISHED;
      end if;
      INTEGER_IO.Get (ENTER_LINE (1 .. LS), M, LAST);
      begin
         INTEGER_IO.Get (ENTER_LINE (LAST + 1 .. LS), Z, LAST);
         if M = 0 or Z = 0 then
            Put_Line ("Start or size of zero, you must be kidding, aborting");
            raise Program_Error;
         elsif M + Z > LINE_LENGTH then
            Put_Line ("Size too large, going to end of line");
            N := LINE_LENGTH;
         else
            N := M + Z - 1;
         end if;
         SORT_TYPE_IO.Get (ENTER_LINE (LAST + 1 .. LS), S, LAST);
         WAY_TYPE_IO.Get (ENTER_LINE (LAST + 1 .. LS), W, LAST);
         MX := M;
         NX := N;
         SX := S;
         WX := W;

         ECHO_ENTRY;

         return;
      exception
         when Program_Error =>
            Put_Line ("PROGRAM_ERROR raised in GET_ENTRY");
            raise;
         when others =>
            MX := M;
            NX := N;
            SX := S;
            WX := W;
            ECHO_ENTRY;
            return;
      end;
   end GET_ENTRY;

   function IGNORE_SEPARATORS (S : String) return String is
      T : String (S'FIRST .. S'LAST) := Lower_Case (S);
   begin
      for I in S'FIRST + 1 .. S'LAST - 1 loop
         if (S (I - 1) /= '-' and then S (I - 1) /= '_')
           and then (S (I) = '-' or else S (I) = '_')
           and then (S (I + 1) /= '-' and then S (I + 1) /= '_')
         then
            T (I) := ' ';
         end if;
      end loop;
      return T;
   end IGNORE_SEPARATORS;

   function LTU (C, D : Character) return Boolean is
   begin
      if (D = 'v') then
         if (C < 'u') then
            return True;
         else
            return False;
         end if;
      elsif (D = 'j') then
         if (C < 'i') then
            return True;
         else
            return False;
         end if;
      elsif (D = 'V') then
         if (C < 'U') then
            return True;
         else
            return False;
         end if;
      elsif (D = 'J') then
         if (C < 'I') then
            return True;
         else
            return False;
         end if;
      else
         return C < D;
      end if;
   end LTU;

   function EQU (C, D : Character) return Boolean is
   begin
      if (D = 'u') or (D = 'v') then
         if (C = 'u') or (C = 'v') then
            return True;
         else
            return False;
         end if;
      elsif (D = 'i') or (D = 'j') then
         if (C = 'i') or (C = 'j') then
            return True;
         else
            return False;
         end if;
      elsif (D = 'U') or (D = 'V') then
         if (C = 'U') or (C = 'V') then
            return True;
         else
            return False;
         end if;
      elsif (D = 'I') or (D = 'J') then
         if (C = 'I') or (C = 'J') then
            return True;
         else
            return False;
         end if;
      else
         return C = D;
      end if;
   end EQU;

   function GTU (C, D : Character) return Boolean is
   begin
      if D = 'u' then
         if (C > 'v') then
            return True;
         else
            return False;
         end if;
      elsif D = 'i' then
         if (C > 'j') then
            return True;
         else
            return False;
         end if;
      elsif D = 'U' then
         if (C > 'V') then
            return True;
         else
            return False;
         end if;
      elsif D = 'I' then
         if (C > 'J') then
            return True;
         else
            return False;
         end if;
      else
         return C > D;
      end if;
   end GTU;

   function LTU (S, T : String) return Boolean is
   begin
      for I in 1 .. S'LENGTH loop   --  Not TRIMed, so same length
         if EQU (S (S'FIRST + I - 1), T (T'FIRST + I - 1)) then
            null;
         elsif GTU (S (S'FIRST + I - 1), T (T'FIRST + I - 1)) then
            return False;
         elsif LTU (S (S'FIRST + I - 1), T (T'FIRST + I - 1)) then
            return True;
         end if;
      end loop;
      return False;
   end LTU;

   function GTU (S, T : String) return Boolean is
   begin
      for I in 1 .. S'LENGTH loop
         if EQU (S (S'FIRST + I - 1), T (T'FIRST + I - 1)) then
            null;
         elsif LTU (S (S'FIRST + I - 1), T (T'FIRST + I - 1)) then
            return False;
         elsif GTU (S (S'FIRST + I - 1), T (T'FIRST + I - 1)) then
            return True;
         end if;
      end loop;
      return False;
   end GTU;

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

   function SLT
     (X, Y : String;         --  Make LEFT and RIGHT
      ST   : SORT_TYPE := A; WT : WAY_TYPE := I) return Boolean
   is
      AS     : String (X'RANGE) := X;
      BS     : String (Y'RANGE) := Y;
      MN, NN : Integer          := 0;
      FN, GN : Float            := 0.0;
      --FS, GS : SECTION_TYPE := NO_SECTION;
      FS, GS : APPENDIX_SECTION_TYPE := NO_APPENDIX_SECTION;
      PX, PY : PART_ENTRY;       --  So I can X here
      RX, RY : PART_OF_SPEECH_TYPE;   --  So I can X here
   begin
      if ST = A then
         AS := Lower_Case (AS);
         BS := Lower_Case (BS);
         if WT = I then
            return AS < BS;
         else
            return AS > BS;
         end if;

      elsif ST = C then
         if WT = I then
            return AS < BS;
         else
            return AS > BS;
         end if;

      elsif ST = G then
         AS := IGNORE_SEPARATORS (AS);
         BS := IGNORE_SEPARATORS (BS);
         if WT = I then
            return AS < BS;
         else
            return AS > BS;
         end if;

      elsif ST = U then
         AS := Lower_Case (AS);
         BS := Lower_Case (BS);
         if WT = I then
            return LTU (AS, BS);
         else
            return GTU (AS, BS);
         end if;

      elsif ST = N then
         INTEGER_IO.Get (AS, MN, LAST);
         INTEGER_IO.Get (BS, NN, LAST);
         if WT = I then
            return MN < NN;
         else
            return MN > NN;
         end if;

      elsif ST = F then
         FLOAT_IO.Get (AS, FN, LAST);
         FLOAT_IO.Get (BS, GN, LAST);
         if WT = I then
            return FN < GN;
         else
            return FN > GN;
         end if;

      elsif ST = P then
         PART_ENTRY_IO.GET (AS, PX, LAST);
         PART_ENTRY_IO.GET (BS, PY, LAST);
         if WT = I then
            return PX < PY;
         else
            return (not (PX < PY)) and (not (PX = PY));
         end if;

      elsif ST = R then
         PART_OF_SPEECH_TYPE_IO.Get (AS, RX, LAST);
         PART_OF_SPEECH_TYPE_IO.Get (BS, RY, LAST);
         if WT = I then
            return RX < RY;
         else
            return (not (RX < RY)) and (not (RX = RY));
         end if;

      elsif ST = S then
         --PUT_LINE("AS =>" & AS & '|');
         GET (AS, FS, LAST);
         --PUT_LINE("BS =>" & BS & '|');
         GET (BS, GS, LAST);
         --PUT_LINE("GOT AS & BS");
         if WT = I then
            return FS < GS;
         else
            return (not (FS < GS)) and (not (FS = GS));
         end if;

      else
         return False;
      end if;

   exception
      when others =>
         Text_IO.Put_Line ("exception in SLT    showing LEFT and RIGHT");
         Text_IO.Put_Line (X & "&");
         Text_IO.Put_Line (Y & "|");
         raise;

   end SLT;

   function SORT_EQUAL
     (X, Y : String; ST : SORT_TYPE := A; WT : WAY_TYPE := I) return Boolean
   is
      AS     : String (X'RANGE)      := X;
      BS     : String (Y'RANGE)      := Y;
      MN, NN : Integer               := 0;
      FN, GN : Float                 := 0.0;
      FS, GS : APPENDIX_SECTION_TYPE := NO_APPENDIX_SECTION;
      PX, PY : PART_ENTRY;
      RX, RY : PART_OF_SPEECH_TYPE;
   begin
      if ST = A then
         AS := Lower_Case (AS);
         BS := Lower_Case (BS);
         return AS = BS;

      elsif ST = C then
         return AS = BS;

      elsif ST = G then
         AS := IGNORE_SEPARATORS (AS);
         BS := IGNORE_SEPARATORS (BS);
         return AS = BS;

      elsif ST = U then
         AS := Lower_Case (AS);
         BS := Lower_Case (BS);
         return EQU (AS, BS);

      elsif ST = N then
         INTEGER_IO.Get (AS, MN, LAST);
         INTEGER_IO.Get (BS, NN, LAST);
         return MN = NN;

      elsif ST = F then
         FLOAT_IO.Get (AS, FN, LAST);
         FLOAT_IO.Get (BS, GN, LAST);
         return FN = GN;

      elsif ST = P then
         PART_ENTRY_IO.GET (AS, PX, LAST);
         PART_ENTRY_IO.GET (BS, PY, LAST);
         return PX = PY;

      elsif ST = R then
         PART_OF_SPEECH_TYPE_IO.Get (AS, RX, LAST);
         PART_OF_SPEECH_TYPE_IO.Get (BS, RY, LAST);
         return RX = RY;

      elsif ST = S then
         GET (AS, FS, LAST);
         GET (BS, GS, LAST);
         return FS = GS;

      else
         return False;
      end if;

   exception
      when others =>
         Text_IO.Put_Line ("exception in LT    showing LEFT and RIGHT");
         Text_IO.Put_Line (X & "|");
         Text_IO.Put_Line (Y & "|");
         raise;

   end SORT_EQUAL;

   function LT (LEFT, RIGHT : TEXT_TYPE) return Boolean is
   begin

      if SLT (LEFT (M1 .. N1), RIGHT (M1 .. N1), S1, W1) then
         return True;

      elsif SORT_EQUAL (LEFT (M1 .. N1), RIGHT (M1 .. N1), S1, W1) then
         if ((N2 > 0) and then SLT (LEFT (M2 .. N2), RIGHT (M2 .. N2), S2, W2))
         then
            return True;

         elsif
           ((N2 > 0)
            and then SORT_EQUAL (LEFT (M2 .. N2), RIGHT (M2 .. N2), S2, W2))
         then
            if
              ((N3 > 0)
               and then SLT (LEFT (M3 .. N3), RIGHT (M3 .. N3), S3, W3))
            then
               return True;

            elsif
              ((N3 > 0)
               and then SORT_EQUAL (LEFT (M3 .. N3), RIGHT (M3 .. N3), S3, W3))
            then
               if
                 ((N4 > 0)
                  and then SLT (LEFT (M4 .. N4), RIGHT (M4 .. N4), S4, W4))
               then
                  return True;

               elsif
                 ((N4 > 0)
                  and then SORT_EQUAL
                    (LEFT (M4 .. N4), RIGHT (M4 .. N4), S4, W4))
               then
                  if
                    ((N5 > 0)
                     and then SLT (LEFT (M5 .. N5), RIGHT (M5 .. N5), S5, W5))
                  then
                     return True;

                  end if;
               end if;
            end if;
         end if;
      end if;
      return False;
   exception
      when others =>
         Text_IO.Put_Line ("exception in LT    showing LEFT and RIGHT");
         Text_IO.Put_Line (LEFT & "|LEFT|");
         Text_IO.Put_Line (RIGHT & "|RIGHT|");
         raise;
   end LT;

   procedure OPEN_FILE_FOR_INPUT
     (INPUT  : in out Text_IO.File_Type;
      PROMPT :        String := "File for input => ")
   is
      LAST : Natural := 0;
   begin

      if CLARG_FILENAME /= "NULLNULL.NUL" then
         Text_IO.Put_Line ("Opening " & CLARG_FILENAME);
         Open (INPUT, In_File, CLARG_FILENAME);
         Text_IO.Put_Line ("Opened " & CLARG_FILENAME);

      else
         Text_IO.Put_Line ("Manual input mode");
         GET_INPUT_FILE :
         loop
            CHECK_INPUT :
            begin
               New_Line;

               Put (PROMPT);
               Get_Line (INPUT_NAME, LAST);
               Open (INPUT, In_File, INPUT_NAME (1 .. LAST));
               exit;
            exception
               when others =>
                  Put_Line ("   !!!!!!!!!  Try Again  !!!!!!!!");
            end CHECK_INPUT;
         end loop GET_INPUT_FILE;

      end if;

   end OPEN_FILE_FOR_INPUT;

   procedure CREATE_FILE_FOR_OUTPUT
     (OUTPUT : in out Text_IO.File_Type;
      PROMPT :        String := "File for output => ")
   is
      NAME : String (1 .. 80) := (others => ' ');
      LAST : Natural          := 0;
   begin

      if CLARG_FILENAME /= "NULLNULL.NUL" then

         Create (OUTPUT, Out_File, (CLARG_FILENAME (1 .. 8) & ".SRD"));

      else

         GET_OUTPUT_FILE :
         loop
            CHECK_OUTPUT :
            begin
               New_Line;

               Put (PROMPT);
               Get_Line (NAME, LAST);
               if TRIM (NAME (1 .. LAST))'LENGTH /= 0 then
                  Create (OUTPUT, Out_File, NAME (1 .. LAST));
               else
                  Create (OUTPUT, Out_File, TRIM (INPUT_NAME));
               end if;
               exit;
            exception
               when others =>
                  Put_Line ("!!!!!!!!!  Try Again  !!!!!!!!");
            end CHECK_OUTPUT;
         end loop GET_OUTPUT_FILE;

      end if;

   end CREATE_FILE_FOR_OUTPUT;

   function GRAPHIC (S : String) return String is
      T : String (1 .. S'LENGTH) := S;
   begin
      for I in S'RANGE loop
         if Character'POS (S (I)) < 32 then
            T (I) := ' ';
         end if;
      end loop;
      return T;
   end GRAPHIC;

   procedure PROCESS_COMMAND_LINE_ARGUMENTS is

   begin

      for ZZ in 1 .. Ada.Command_Line.Argument_Count loop

         for YY in 1 .. TRIM (Ada.Command_Line.Argument (1))'length loop
            case Upper_Case (TRIM (Ada.Command_Line.Argument (1)) (YY)) is
               when '-' =>
                  exit when YY > 3;

               when 'D' =>
                  M1 := 1;
                  N1 := M1 + (75 - 1);
                  S1 := A;
                  W1 := I;

                  M2 := 77;
                  N2 := M2 + (24 - 1);
                  S2 := P;
                  W2 := I;

                  M3 := 111;
                  N3 := M3 + (80 - 1);
                  S3 := A;
                  W3 := I;

                  M4 := 101;
                  N4 := M4 + (1 - 1);
                  S4 := A;
                  W4 := I;

                  M5 := 107;
                  N5 := M5 + (1 - 1);
                  S5 := A;
                  W5 := I;

                  CLARG_FILENAME := "DICTLINE.GEN";

               when 'E' =>

                  M1 := 1;
                  N1 := M1 + (24 - 1);
                  S1 := A;
                  W1 := I;

                  M2 := 1;
                  N2 := M2 + (24 - 1);
                  S2 := C;
                  W2 := I;

                  M3 := 51;
                  N3 := M3 + (6 - 1);
                  S3 := R;
                  W3 := I;

                  M4 := 72;
                  N4 := M4 + (5 - 1);
                  S4 := N;
                  W4 := D;

                  M5 := 58;
                  N5 := M5 + (1 - 1);
                  S5 := A;
                  W5 := I;

                  CLARG_FILENAME := "EWDSLIST.GEN";

               when 'S' =>

                  M1 := 1;
                  N1 := M1 + (18 - 1);
                  S1 := U;
                  W1 := I;

                  M2 := 20;
                  N2 := M2 + (24) - 1;
                  S2 := P;
                  W2 := I;

                  M3 := 1;
                  N3 := M3 + (18 - 1);
                  S3 := C;
                  W3 := I;

                  M4 := 1;
                  N4 := M4 + (56 - 1);
                  S4 := A;
                  W4 := I;

                  M5 := 58;
                  N5 := M5 + (1 - 1);
                  S5 := A;
                  W5 := I;

                  CLARG_FILENAME := "STEMLIST.GEN";

               when others =>
                  New_Line;
                  Put_Line
                    ("====== UNKNOWN COMMAND-LINE OPTION: " &
                     TRIM (Ada.Command_Line.Argument (1)) (YY) & " ======");
                  New_Line;
                  --exit;
                  return;
            end case;

         end loop;

      end loop; -- while =< argument_counter'length

      Text_IO.Put_Line ("Input file is " & CLARG_FILENAME);

   end PROCESS_COMMAND_LINE_ARGUMENTS;

begin

   if Ada.Command_Line.Argument_Count > 0 then

      PROCESS_COMMAND_LINE_ARGUMENTS;
      OPEN_FILE_FOR_INPUT (INPUT, CLARG_FILENAME);

   else
      New_Line;
      Put_Line ("Sorts a text file of lines four times on substrings M..N");
      Put_Line
        ("A)lphabetic (all case) C)ase sensitive, iG)nore seperators, U)i_is_vj,");
      Put_Line
        ("    iN)teger, F)loating point, S)ection, P)art entry, or paR)t of speech");
      Put_Line ("         I)ncreasing or D)ecreasing");
      New_Line;

      OPEN_FILE_FOR_INPUT (INPUT, "What file to sort from => ");
      New_Line;

      PROMPT_FOR_ENTRY ("first");
      begin
         GET_ENTRY (M1, N1, S1, W1);
      exception
         when Program_Error =>
            raise;
         when others =>
            null;
      end;

      begin
         PROMPT_FOR_ENTRY ("second");
         GET_ENTRY (M2, N2, S2, W2);
         PROMPT_FOR_ENTRY ("third");
         GET_ENTRY (M3, N3, S3, W3);
         PROMPT_FOR_ENTRY ("fourth");
         GET_ENTRY (M4, N4, S4, W4);
         PROMPT_FOR_ENTRY ("fifth");
         GET_ENTRY (M5, N5, S5, W5);
      exception
         when Program_Error =>
            raise;
         when ENTRY_FINISHED =>
            null;
         when Text_IO.Data_Error | Text_IO.End_Error =>
            null;
      end;

   end if; -- end detour for command line arguments

   --PUT_LINE("CREATING WORK FILE");
   New_Line;
   Create (WORK, Inout_File, "WORK.");
   Put_Line ("CREATED  WORK FILE");

   while not End_Of_File (INPUT) loop
      --begin
      Get_Line (INPUT, LINE_TEXT, CURRENT_LENGTH);
      --exception when others  =>
      --TEXT_IO.PUT_LINE("INPUT GET exception");
      --TEXT_IO.PUT_LINE(LINE_TEXT(1..CURRENT_LENGTH) & "|");
      --end;
      --PUT_LINE(LINE_TEXT(1..CURRENT_LENGTH));
      --PUT_LINE("=>" & HEAD(LINE_TEXT(1..CURRENT_LENGTH), LINE_LENGTH) & "|");
      if TRIM (LINE_TEXT (1 .. CURRENT_LENGTH)) /= "" then
         --begin
         Write (WORK, HEAD (LINE_TEXT (1 .. CURRENT_LENGTH), LINE_LENGTH));
         --exception when others  =>
         --TEXT_IO.PUT_LINE("WORK WRITE exception");
         --TEXT_IO.PUT_LINE(LINE_TEXT(1..CURRENT_LENGTH) & "|");
         --end;
      end if;
   end loop;
   Close (INPUT);

   Put_Line ("Begin sorting");

   LINE_HEAPSORT :
   declare

      L    : LINE_IO.Positive_Count := Size (WORK) / 2 + 1;
      IR   : LINE_IO.Positive_Count := Size (WORK);
      I, J : LINE_IO.Positive_Count;

   begin
      Text_IO.Put_Line
        ("SIZE OF WORK = " & Integer'IMAGE (Integer (Size (WORK))));
      MAIN :
      loop

         if L > 1 then
            L := L - 1;
            Read (WORK, LINE_TEXT, L);
            OLD_LINE := LINE_TEXT;
         else
            Read (WORK, LINE_TEXT, IR);
            OLD_LINE := LINE_TEXT;
            Read (WORK, LINE_TEXT, 1);
            Write (WORK, LINE_TEXT, IR);
            IR := IR - 1;
            if IR = 1 then
               Write (WORK, OLD_LINE, 1);
               exit MAIN;
            end if;
         end if;
         I := L;
         J := L + L;

         while J <= IR loop

            --  Text_IO.Put_Line("J is " & J'Image & "; IR is " & IR'image & ";
            --  " & " I is " & I'Image & " and L is " & L'Image); -- DEBUG
            if J < IR then
               Read (WORK, LINE_TEXT, J);
               Read (WORK, P_LINE, J + 1);
               --if LT (LINE.TEXT, P_LINE.TEXT)  then
               if LT (LINE_TEXT, P_LINE) then
                  J := J + 1;
               end if;
            end if;
            Read (WORK, LINE_TEXT, J);
            --if OLD_LINE.TEXT < LINE.TEXT  then
            if LT (OLD_LINE, LINE_TEXT) then
               Write (WORK, LINE_TEXT, I);
               I := J;
               J := J + J;
            else
               J := IR + 1;
            end if;
         end loop;
         Write (WORK, OLD_LINE, I);

      end loop MAIN;

   exception
      when Constraint_Error =>
         Put_Line ("HEAP CONSTRAINT_ERROR");
      when others =>
         Put_Line ("HEAP NON-CONSTRAINT_ERROR");
   end LINE_HEAPSORT;

   Put_Line ("Finished sorting in WORK");

   CREATE_FILE_FOR_OUTPUT (OUTPUT, "Where to put the output => ");

   --RESET(WORK);
   Set_Index (WORK, 1);
   while not End_Of_File (WORK) loop
      Read (WORK, LINE_TEXT);
      if TRIM (GRAPHIC (LINE_TEXT))'LENGTH > 0 then
         --PUT_LINE(TRIM(LINE_TEXT, RIGHT));
         Put_Line (OUTPUT, TRIM (LINE_TEXT, Right));
      end if;
   end loop;

   Close (WORK);
   Close (OUTPUT);

   -- Check and rename input/output using command-line argument
   if CLARG_FILENAME /= "NULLNULL.NUL" then
      declare
         CL_Input_File_Name         : String := CLARG_FILENAME;
         CL_Temp_File_Name : String := (CLARG_FILENAME (1 .. 8) & ".SRD");
         CL_Renamed_Input_File_Name : String :=
           (CLARG_FILENAME (1 .. 8) & ".OLD");
         Rename_Files_Exception : exception;
         CL_User_Input : Character := 'X';
         -- use Ada.Numerics;
         use Ada.Directories;
         In_Size  : Ada.Directories.File_Size;
         Out_Size : Ada.Directories.File_Size;

      begin
         In_Size  := Ada.Directories.Size (CL_Input_File_Name);
         Out_Size := Ada.Directories.Size (CL_Temp_File_Name);

         if Out_Size >= (In_Size - (In_Size / 10)) then

            if Ada.Directories.Exists (CL_Renamed_Input_File_Name) then
               Text_IO.Put_Line
                 ("CAUTION: " & CL_Renamed_Input_File_Name &
                  " already exists.");
               while (CL_User_Input /= 'y') and then (CL_User_Input /= 'Y')
               loop
                  Text_IO.Put_Line
                    ("Delete/overwrite the existing " &
                     CL_Renamed_Input_File_Name & " [y/n]?");
                  Text_IO.Get (CL_User_Input);
                  if (CL_User_Input = 'n') or (CL_User_Input = 'N') then
                     raise Rename_Files_Exception;
                  end if;
               end loop;
               if (CL_User_Input = 'y') or (CL_User_Input = 'Y') then
                  Text_IO.Put_Line ("Deleting " & CL_Renamed_Input_File_Name);
                  Ada.Directories.Delete_File (CL_Renamed_Input_File_Name);
               end if;
            end if;

            Ada.Directories.Rename
              (Old_Name => CL_Input_File_Name,
               New_Name => CL_Renamed_Input_File_Name);

            Text_IO.Put_Line
              ("The original input file " & CL_Input_File_Name &
               " HAS BEEN RENAMED " & CL_Renamed_Input_File_Name);

            Ada.Directories.Rename
              (Old_Name => CL_Temp_File_Name, New_Name => CL_Input_File_Name);
         else
            Text_IO.Put_Line
              ("The sorter output is not greater than or equal to the input.");
            Text_IO.Put_Line
              ("Please check any errors above and whether " &
               CL_Temp_File_Name & "contains valid output.");
            Text_IO.Put_Line
              ("if " & CL_Temp_File_Name &
               " contains valid output, then rename it " & CL_Input_File_Name);
            Text_IO.Put_Line
              ("before performing the next step in building the WORDS dictionary files.");
         end if;

      exception
         when Rename_Files_Exception =>
            Text_IO.Put_Line
              ("User decline to delete/overwrite the original " &
               CL_Renamed_Input_File_Name);
            Text_IO.Put_Line
              ("======== " & CL_Temp_File_Name &
               " CONTAINS THE SORTED OUTPUT ========");
         when others =>
            Text_IO.Put_Line ("Encountered an error.  Check: ");
            Text_IO.Put_Line
              ("(1) Whether " & CL_Temp_File_Name &
               " contains valid sorted output");
            Text_IO.Put_Line
              ("(2) Whether " & CL_Renamed_Input_File_Name &
               " already exists (and, if so, whether you have permission to delete it)");
            raise Text_IO.Device_Error;
      end; --block
   end if;
   -- END input/output check-and-rename when using command-line argument

   Put_Line ("Done!");
   New_Line;

exception
   when Program_Error =>
      Put_Line ("SORT terminated on a PROGRAM_ERROR");
      Close (OUTPUT);
   when Text_IO.Data_Error =>     --Terminate on primary start or size = 0
      Put_Line ("SORT terminated on a DATA_ERROR");
      Put_Line (LINE_TEXT);
      Close (OUTPUT);
   when Constraint_Error =>       --Terminate on blank line for file name
      Put_Line ("SORT terminated on a CONSTRAINT_ERROR");
      Close (OUTPUT);
   when Text_IO.Device_Error =>     --Ran out of space to write output file
      Put_Line ("SORT terminated on a DEVICE_ERROR");
      Delete (OUTPUT);
      CREATE_FILE_FOR_OUTPUT (OUTPUT, "Wherelse to put the output => ");
      Reset (WORK);
      while not End_Of_File (WORK) loop
         Read (WORK, LINE_TEXT);
         Put_Line (OUTPUT, LINE_TEXT);    --(1..LINE.CURRENT_LENGTH));
      end loop;
      Close (OUTPUT);
end SORTER;
