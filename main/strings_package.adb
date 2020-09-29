with TEXT_IO;         use TEXT_IO;
with Ada.Strings;
with Ada.Wide_Text_IO;
with Ada.Wide_Characters.Handling;
with Ada.Characters.Conversions;
with WORD_PARAMETERS; use WORD_PARAMETERS;
with LIST_PACKAGE;

   package body STRINGS_PACKAGE is
   
      function MAX(A, B : in INTEGER) return INTEGER is
      begin
         if A >= B  then 
            return A; end if; 
         return B;
      end MAX;
   
      function MIN(A, B : in INTEGER) return INTEGER is
      begin
         if A <= B  then 
            return A; end if; 
         return B;
      end MIN;
   
   
      function LOWER_CASE(C : in CHARACTER) return CHARACTER is
      begin
         if C in 'A'..'Z'  then
            return CHARACTER'VAL(CHARACTER'POS(C) + 32);
         else
            return C;
         end if;
      end LOWER_CASE;
   
      function LOWER_CASE(S : in STRING) return STRING is
         T : STRING(S'RANGE);
      begin
         for I in S'RANGE  loop
            T(I) := LOWER_CASE(S(I));
         end loop;
         return T;
      end LOWER_CASE;
   
   
      function UPPER_CASE(C : in CHARACTER) return CHARACTER is
      begin
         if C in 'a'..'z'  then
            return CHARACTER'VAL(CHARACTER'POS(C) - 32);
         else
            return C;
         end if;
      end UPPER_CASE;
   
      function UPPER_CASE(S : in STRING) return STRING is
         T : STRING(S'RANGE);
      begin
         for I in S'RANGE  loop
            T(I) := UPPER_CASE(S(I));
         end loop;
         return T;
      end UPPER_CASE;
   
   
      function TRIM(SOURCE : in STRING;
                    SIDE   : in TRIM_END := BOTH) return STRING is
      --  Removes leading and trailing blanks and returns a STRING staring at 1
      --  For a string of all blanks as input it returns NULL_STRING
         T : STRING(1..SOURCE'LENGTH) := SOURCE;
         FIRST: NATURAL := SOURCE'FIRST; 
         LAST : NATURAL := SOURCE'LAST;

      begin
         if SIDE /= RIGHT  then
            FIRST := SOURCE'LAST + 1;
            for I in SOURCE'RANGE  loop
               if SOURCE(I) /= ' '  then
                  FIRST := I;
                  exit;
               end if;
            end loop;
         else
            FIRST := SOURCE'FIRST;
         end if;
      
         if SIDE /= LEFT  then
            LAST := SOURCE'FIRST - 1;
            for I in reverse SOURCE'RANGE  loop
               if SOURCE(I) /= ' '  then
                  LAST := I;
                  exit;
               end if;
            end loop;
         else
            LAST := SOURCE'LAST;
         end if;
      
         if FIRST > LAST  then
            return NULL_STRING;
         else
            T(1..LAST-FIRST+1) := SOURCE(FIRST..LAST);
            return T(1..LAST-FIRST+1);
         end if;
      end TRIM;        
   
   
      function HEAD(SOURCE : in STRING; 
                    COUNT  : in NATURAL; 
                    PAD    : in CHARACTER := ' ') return STRING is
      --  Truncates or fills a string to exactly N in length
         T : STRING(1..COUNT) := (others => ' ');
      begin
         if COUNT < SOURCE'LENGTH  then
            T(1..COUNT) := SOURCE(SOURCE'FIRST..SOURCE'FIRST+COUNT-1);
         else
            T(1..SOURCE'LENGTH) := SOURCE(SOURCE'FIRST..SOURCE'LAST);
         end if;
         return T;
      end HEAD; 
   
   
      procedure GET_NON_COMMENT_LINE(F : in TEXT_IO.FILE_TYPE; 
                                     S : out STRING; LAST : out INTEGER) is
      --  Reads a text file and outs a string that is as much of the 
      --  first line encountered that is not a comment, that is not a comment   
      
         T : STRING(1..250) := (others => ' ');
         L, LX : INTEGER := 0;
      begin
         LAST := 0;
      FILE_LOOP:
         while not TEXT_IO.END_OF_FILE(F)  loop  --  Loop until data - Finish on EOF
            TEXT_IO.GET_LINE(F, T, L);
            if (HEAD(TRIM(T), 250)(1..2) = "  "  or 
                HEAD(TRIM(T), 250)(1..2) = "--")  then
               null;
            else
               LX := L;
            LINE_LOOP:
               for I in 2..L  loop
               --  Any leading comment does not get to here
                  if (T(I-1) = '-')  and  (T(I) = '-')  then   --  We have a comment
                     LX := I - 2;
                     exit FILE_LOOP;
                  end if;
               end loop LINE_LOOP;
               exit FILE_LOOP;
            end if;
         end loop FILE_LOOP;
         S(S'First..LX) := T(T'First..LX);
         LAST := LX;
      end GET_NON_COMMENT_LINE;
   
     
   procedure GET_UNICODE (LINE : in out String; L : in out Integer) is

         -- Converts unicode accented forms to basic ASCII 
         -- Useful for input that includes macrons.
         -- E.g., this causes 'ē' to be processed as 'e'
         -- SPR TO DO; WHEN MAKE SEPARATE PRAGMA ADA_2012 and pragma in_line
            
         W_Line : Wide_String := Ada.Wide_Text_IO.Get_Line;

         T_Line : String := Ada.Characters.Conversions.To_String (    
                            Ada.Wide_Characters.Handling.To_Basic(W_Line));
          
      begin
         
              if T_Line'Last <= INPUT_LINE_LENGTH then 
                LINE(T_LINE'RANGE) := T_LINE;
                L := T_Line'Length;
              else 
                Line := T_Line(T_Line'First..Line'Last);
                L := Line'Last;
              end if; 
               -- SPR to do:  add exception to fall back to old way
      
   exception 
      when Constraint_Error =>
         if WORDS_MODE (DO_ANSI_FORMATTING) then
            Text_IO.Put (OUTPUT, LIST_PACKAGE.Format_Reset);
            Text_IO.Put (OUTPUT, LIST_PACKAGE.Format_Inverse);
         end if;

         Text_IO.Put_Line(OUTPUT, "ERROR processing Unicode. Falling back to non-Unicode mode.");
         Text_IO.Put(OUTPUT, "If this resolves the problem, save the current parameters by entering " & CHANGE_PARAMETERS_CHARACTER);
           WORDS_MODE(DO_UNICODE_INPUT) := False;
          if WORDS_MODE (DO_ANSI_FORMATTING) then
            Text_IO.Put (OUTPUT, LIST_PACKAGE.Format_Reset);
         end if;
         New_Line;
      
      end GET_UNICODE; 
   
   
   end STRINGS_PACKAGE;  
