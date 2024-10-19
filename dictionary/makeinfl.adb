with Text_IO;
with STRINGS_PACKAGE;     use STRINGS_PACKAGE;
with LATIN_FILE_NAMES;    use LATIN_FILE_NAMES;
with INFLECTIONS_PACKAGE; use INFLECTIONS_PACKAGE;
with IO_Exceptions;

procedure MAKEINFL is
   package INTEGER_IO is new Text_IO.Integer_IO (Integer);
   use Text_IO;
   use INTEGER_IO;
   use STEM_KEY_TYPE_IO;
   use INFLECTION_RECORD_IO;
   use QUALITY_RECORD_IO;
   use ENDING_RECORD_IO;
   use AGE_TYPE_IO;
   use FREQUENCY_TYPE_IO;
   use LEL_SECTION_IO;

   PORTING : constant Boolean := True;    --FALSE for WAKEINFL;

   M, N               : Integer := 0;
   N1, N2, N3, N4, N5 : Integer := 0;

   OUTPUT                    : Text_IO.File_Type;
   INFLECTIONS_SECTIONS_FILE : LEL_SECTION_IO.File_Type;

   procedure FILE_INFLECTIONS_SECTIONS is
--  Reads the INFLECTS. file and prepares an inflections list Then it writes
--  that list into an array Loads the inflection array into a file for later
--  retrieval

      INFLECTIONS_FILE          : Text_IO.File_Type;
      INFLECTIONS_SECTIONS_FILE : LEL_SECTION_IO.File_Type;
      IR                        : INFLECTION_RECORD;
      LINE, BLANKS              : String (1 .. 100) := (others => ' ');
      LAST, L                   : Integer           := 0;
      SN                        : ENDING_SIZE_TYPE  := ENDING_SIZE_TYPE'FIRST;
      SX                        : Character         := ' ';

      type INFLECTION_ITEM;
      type INFLECTION_LIST is access INFLECTION_ITEM;

      type INFLECTION_ITEM is record
         IR   : INFLECTION_RECORD;
         SUCC : INFLECTION_LIST;
      end record;

      type LATIN_INFLECTIONS is
        array
          (Integer range 0 .. MAX_ENDING_SIZE,
           Character range ' ' .. 'z') of INFLECTION_LIST;
      NULL_LATIN_INFLECTIONS : LATIN_INFLECTIONS :=
        (others => (others => null));

      L_I : LATIN_INFLECTIONS := NULL_LATIN_INFLECTIONS;

      LEL                : LEL_SECTION := (others => NULL_INFLECTION_RECORD);
      J1, J2, J3, J4, J5 : Integer     := 0;

      procedure NULL_LEL is
      begin
         for I in LEL'RANGE loop
            LEL (I) := NULL_INFLECTION_RECORD;
         end loop;
      end NULL_LEL;

      procedure LOAD_INFLECTIONS_LIST is
      --  Takes the INFLECT. file and populates the L_I list of inflections
      --  indexed on ending size and last letter of ending
      begin
         Put_Line ("Begin  LOAD_INFLECTIONS_LIST");
         NUMBER_OF_INFLECTIONS := 0;

         L_I := NULL_LATIN_INFLECTIONS;
         Open (INFLECTIONS_FILE, In_File, INFLECTIONS_FULL_NAME);
         Text_IO.Put_Line ("INFLECTIONS file loading");
         while not End_Of_File (INFLECTIONS_FILE) loop

            READ_A_LINE :
            begin
               GET_NON_COMMENT_LINE (INFLECTIONS_FILE, LINE, LAST);

               if LAST > 0 then
                  GET (LINE (1 .. LAST), IR, L);
                  SN := IR.ENDING.SIZE;
                  if SN = 0 then
                     SX := ' ';
                  else
                     SX := IR.ENDING.SUF (SN);
                  end if;
                  L_I (SN, SX) := new INFLECTION_ITEM'(IR, L_I (SN, SX));
                  NUMBER_OF_INFLECTIONS := NUMBER_OF_INFLECTIONS + 1;
--TEXT_IO.PUT(INTEGER'IMAGE(NUMBER_OF_INFLECTIONS) & "  "); INFLECTION_RECORD_IO.PUT(IR); NEW_LINE;
               end if;
            exception
               when Constraint_Error | IO_Exceptions.Data_Error =>
                  Put_Line ("****" & LINE (1 .. LAST));
            end READ_A_LINE;

         end loop;
         Close (INFLECTIONS_FILE);
         Put_Line
           ("INFLECTIONS_LIST LOADED   " &
            Integer'IMAGE (NUMBER_OF_INFLECTIONS));

      end LOAD_INFLECTIONS_LIST;

      procedure LIST_TO_LEL_FILE is
         --  From ILC (=L_I) list of inflections, prepares the LEL inflections
         --  array
         I   : Integer           := 0;
         ILC : LATIN_INFLECTIONS := L_I;
      begin

         Create
           (INFLECTIONS_SECTIONS_FILE, Out_File, INFLECTIONS_SECTIONS_NAME);

         NULL_LEL;
         ILC :=
           L_I;                              --  Resetting the list to start over
         while ILC (0, ' ') /= null loop
            J5           := J5 + 1;
            LEL (J5)     := ILC (0, ' ').IR;
            ILC (0, ' ') := ILC (0, ' ').SUCC;
         end loop;
         Write (INFLECTIONS_SECTIONS_FILE, LEL, 5);
         N5 := J5;

         NULL_LEL;
         ILC :=
           L_I;                              --  Resetting the list to start over
         for CH in Character range 'a' .. 'z' loop
            for N in reverse 1 .. MAX_ENDING_SIZE loop
               while ILC (N, CH) /= null loop
                  if not
                    (ILC (N, CH).IR.QUAL.POFS = PRON
                     and then
                     (ILC (N, CH).IR.QUAL.PRON.DECL.WHICH = 1 or
                      ILC (N, CH).IR.QUAL.PRON.DECL.WHICH = 2))
                  then

                     if CH in INFLECTIONS_SECTION_1 then
                        J1       := J1 + 1;
                        LEL (J1) := ILC (N, CH).IR;

                     end if;
                  end if;
                  ILC (N, CH) := ILC (N, CH).SUCC;
               end loop;
            end loop;
         end loop;
         Write (INFLECTIONS_SECTIONS_FILE, LEL, 1);
         N1 := J1;

         NULL_LEL;
         ILC :=
           L_I;                              --  Resetting the list to start over
         for CH in Character range 'a' .. 'z' loop
            for N in reverse 1 .. MAX_ENDING_SIZE loop
               while ILC (N, CH) /= null loop
                  if not
                    (ILC (N, CH).IR.QUAL.POFS = PRON
                     and then
                     (ILC (N, CH).IR.QUAL.PRON.DECL.WHICH = 1 or
                      ILC (N, CH).IR.QUAL.PRON.DECL.WHICH = 2))
                  then

                     if CH in INFLECTIONS_SECTION_2 then
                        J2       := J2 + 1;
                        LEL (J2) := ILC (N, CH).IR;

                     end if;
                  end if;
                  ILC (N, CH) := ILC (N, CH).SUCC;
               end loop;
            end loop;
         end loop;
         Write (INFLECTIONS_SECTIONS_FILE, LEL, 2);
         N2 := J2;

         NULL_LEL;
         ILC :=
           L_I;                              --  Resetting the list to start over
         for CH in Character range 'a' .. 'z' loop
            for N in reverse 1 .. MAX_ENDING_SIZE loop
               while ILC (N, CH) /= null loop
                  if not
                    (ILC (N, CH).IR.QUAL.POFS = PRON
                     and then
                     (ILC (N, CH).IR.QUAL.PRON.DECL.WHICH = 1 or
                      ILC (N, CH).IR.QUAL.PRON.DECL.WHICH = 2))
                  then

                     if CH in INFLECTIONS_SECTION_3 then
                        J3       := J3 + 1;
                        LEL (J3) := ILC (N, CH).IR;

                     end if;
                  end if;
                  ILC (N, CH) := ILC (N, CH).SUCC;
               end loop;
            end loop;
         end loop;
         Write (INFLECTIONS_SECTIONS_FILE, LEL, 3);
         N3 := J3;

         NULL_LEL;
         ILC :=
           L_I;                              --  Resetting the list to start over
         for CH in Character range 'a' .. 'z' loop
            for N in reverse 1 .. MAX_ENDING_SIZE loop
               while ILC (N, CH) /= null loop
                  if not
                    (ILC (N, CH).IR.QUAL.POFS = PRON
                     and then
                     (ILC (N, CH).IR.QUAL.PRON.DECL.WHICH = 1 or
                      ILC (N, CH).IR.QUAL.PRON.DECL.WHICH = 2))
                  then

                     if (CH in INFLECTIONS_SECTION_4) then
                        J4       := J4 + 1;
                        LEL (J4) := ILC (N, CH).IR;

                     end if;
                  end if;
                  ILC (N, CH) := ILC (N, CH).SUCC;
               end loop;
            end loop;
         end loop;

         --  Now put the PACK in 4 -- Maybe it should be in 5 ????
         ILC :=
           L_I;                              --  Resetting the list to start over
         for CH in Character range 'a' .. 'z' loop
            for N in reverse 1 .. MAX_ENDING_SIZE loop
               while ILC (N, CH) /= null loop
                  if
                    (ILC (N, CH).IR.QUAL.POFS = PRON
                     and then
                     (ILC (N, CH).IR.QUAL.PRON.DECL.WHICH = 1 or
                      ILC (N, CH).IR.QUAL.PRON.DECL.WHICH = 2))
                  then  --  2 no longer PACK

                     J4       := J4 + 1;
                     LEL (J4) := ILC (N, CH).IR;

                  end if;
                  ILC (N, CH) := ILC (N, CH).SUCC;
               end loop;
            end loop;
         end loop;
         Write (INFLECTIONS_SECTIONS_FILE, LEL, 4);
         N4 := J4;

         Close (INFLECTIONS_SECTIONS_FILE);

      end LIST_TO_LEL_FILE;

   begin

      LOAD_INFLECTIONS_LIST;

      Text_IO.Set_Col (33);
      Text_IO.Put ("--  ");
      INTEGER_IO.Put (NUMBER_OF_INFLECTIONS);
      Text_IO.Put_Line (" entries    --  Loaded correctly");

      LIST_TO_LEL_FILE;                     --  Load arrays to file
      Text_IO.Put_Line ("File INFLECTS.SEC  --  Loaded");

   exception
      when others =>
         Text_IO.Put_Line ("Exception in FILE_INFLECTIONS_SECTIONS");
   end FILE_INFLECTIONS_SECTIONS;

begin

   Put_Line ("Produces INFLECTS.SEC file from INFLECTS.");

   FILE_INFLECTIONS_SECTIONS;

   if not PORTING then
      Put_Line
        ("using FILE_INFLECTIONS_SECTIONS, also produces INFLECTS.LIN file");

      Create (OUTPUT, Out_File, "INFLECTS.LIN");
   end if;

   ESTABLISH_INFLECTIONS_SECTION;

   LEL_SECTION_IO.Open
     (INFLECTIONS_SECTIONS_FILE, In_File, INFLECTIONS_SECTIONS_NAME);

   if not PORTING then
      for I in BEL'RANGE loop                     --  Blank endings
         if BEL (I) /= NULL_INFLECTION_RECORD then
            M := M + 1;
            PUT (OUTPUT, BEL (I).QUAL);
            Set_Col (OUTPUT, 50);
            Put (OUTPUT, BEL (I).KEY, 1);
            Set_Col (OUTPUT, 52);
            PUT (OUTPUT, BEL (I).ENDING);
            Set_Col (OUTPUT, 62);
            Put (OUTPUT, BEL (I).AGE);
            Set_Col (OUTPUT, 64);
            Put (OUTPUT, BEL (I).FREQ);
            New_Line (OUTPUT);
         end if;
      end loop;
   end if;

   for N in 1 .. 4 loop
      Read (INFLECTIONS_SECTIONS_FILE, LEL, LEL_SECTION_IO.Positive_Count (N));

      if not PORTING then
         for I in LEL'RANGE loop                     --  Non-blank endings
            if LEL (I) /= NULL_INFLECTION_RECORD then
               M := M + 1;
               PUT (OUTPUT, LEL (I).QUAL);
               Set_Col (OUTPUT, 50);
               Put (OUTPUT, LEL (I).KEY, 1);
               Set_Col (OUTPUT, 52);
               PUT (OUTPUT, LEL (I).ENDING);
               Set_Col (OUTPUT, 62);
               Put (OUTPUT, LEL (I).AGE);
               Set_Col (OUTPUT, 64);
               Put (OUTPUT, LEL (I).FREQ);
               New_Line (OUTPUT);
            end if;
         end loop;
      end if;

   end loop;

   New_Line;
   Put ("LINE_INFLECTIONS finds ");
   Put (M);
   Put_Line (" inflections");
   New_Line;

   for I in Character range ' ' .. ' ' loop
      INTEGER_IO.Put (0);
      Put ("    ");
      Put (I);
      Put ("    ");
      Put (BELF (0, I));
      Put ("  ");
      Put (BELL (0, I));
      Put ("    ");
      Put (BELL (0, I) - BELF (0, I) + 1);
      New_Line;
   end loop;
   New_Line;

   for I in Character range 'a' .. 'z' loop
      for N in reverse 1 .. MAX_ENDING_SIZE loop
         if (LELL (N, I) > 0) and then (LELF (N, I) <= LELL (N, I)) then
            Put (N);
            Put ("    ");
            Put (I);
            Put ("    ");
            Put (LELF (N, I));
            Put ("  ");
            Put (LELL (N, I));
            Put ("    ");
            Put (LELL (N, I) - LELF (N, I) + 1);
            New_Line;
         end if;
      end loop;
   end loop;
   New_Line;

   for I in Character range 'a' .. 'z' loop
      for N in reverse 1 .. MAX_ENDING_SIZE loop
         if (PELL (N, I) > 0) and then (PELF (N, I) <= PELL (N, I)) then
            Put (N);
            Put ("    ");
            Put (I);
            Put ("    ");
            Put (PELF (N, I));
            Put ("  ");
            Put (PELL (N, I));
            Put ("    ");
            Put (PELL (N, I) - PELF (N, I) + 1);
            New_Line;
         end if;
      end loop;
   end loop;
   New_Line;

   New_Line;
   Put (N5);
   Put ("    ");
   Put (N1);
   Put ("    ");
   Put (N2);
   Put ("    ");
   Put (N3);
   Put ("    ");
   Put (N4);
   Put ("    ");
   New_Line;

end MAKEINFL;
