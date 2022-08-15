with WORD_PARAMETERS;      use WORD_PARAMETERS;
with DEVELOPER_PARAMETERS; use DEVELOPER_PARAMETERS;

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
   

  procedure Put_Pearse_Code (OUTPUT : in Text_IO.File_Type; Code : in Pearse_Code_Type) is
  begin
    if WORDS_MDEV(DO_PEARSE_CODES) then 
       Put(OUTPUT,Pearse_Code_Array(Code));
      end if;
    end Put_Pearse_Code;
   
    
  procedure Format (OUTPUT : in Text_IO.File_Type; Format : In Format_Command) is
   begin

      if WORDS_MODE(DO_ANSI_FORMATTING) and then
         not WORDS_MODE (WRITE_OUTPUT_TO_FILE) then
      
        Put (OUTPUT, Format_Reset);

         case Format is
            when UNDERLINE => Put (OUTPUT, Format_Underline);
            when INVERSE   => Put (OUTPUT, Format_Inverse);
            when FAINT     => if WORDS_MODE(DIM_EXAMPLES_TEXT) then
                               Put (OUTPUT, Format_Faint);
                                            end if;
            when BOLD      => Put (OUTPUT, Format_Bold);
            when RESET     => null;
         end case;

      end if;

   end Format;  
   
 
   end STRINGS_PACKAGE;  
