with Ada.Command_Line;
with Text_IO;              use Text_IO;
with STRINGS_PACKAGE;      use STRINGS_PACKAGE;
with CONFIG;               use CONFIG;
with WORD_PARAMETERS;      use WORD_PARAMETERS;
with DEVELOPER_PARAMETERS; use DEVELOPER_PARAMETERS;
with WORD_PACKAGE;         use WORD_PACKAGE;
with Parse_Package;        use Parse_Package;
with Ada.Interrupts;       use Ada.Interrupts;
with Ada.Interrupts.Names; use Ada.Interrupts.Names;
with No_Exit_Handler;      use No_Exit_Handler;
with Words_Help;
with Ada.Environment_Variables;
with Ada.Directories;
with Ada.Wide_Text_IO;

procedure WORDS is

begin

   -- FIND DATA FILES
   -- If the always-necessary INFLECTS.SEC isn't in the working directory, see
   -- if we can find them
   if
     (not Ada.Directories.Exists ("INFLECTS.SEC")
      and then Ada.Environment_Variables.Exists ("LATINWORDS"))
   then
      Put_Line
        ("LATINWORDS environment variable set; using data files at " &
         Ada.Environment_Variables.Value ("LATINWORDS"));
      Ada.Directories.Set_Directory
        (Ada.Environment_Variables.Value ("LATINWORDS"));
   else
      CHECK_PATH_VARIABLE;
   end if;

   --  SIMPLE INTERACTIVE MODE
   if Ada.Command_Line.Argument_Count = 0 then
      METHOD           := INTERACTIVE;
      SUPPRESS_PREFACE := False;
      Set_Output (Standard_Output);
      INITIALIZE_WORD_PARAMETERS;
      INITIALIZE_DEVELOPER_PARAMETERS;
      INITIALIZE_WORD_PACKAGE;
      Parse;
      return;

      --  COMMAND-LINE ARGUMENTS ("new" style to modify interactive mode)
   elsif
     (TRIM (Ada.Command_Line.Argument (1)) (1) = '-' or
      TRIM (Ada.Command_Line.Argument (1)) (1) = HELP_CHARACTER)
   then

      New_Style_Arguments :
      declare
         Args_Exception : exception;

      begin

         for I in 1 .. Ada.Command_Line.Argument_Count loop

            for J in 2 .. TRIM (Ada.Command_Line.Argument (I))'length loop
               exit when J > 5; -- max 5 arguments (-E and -L conflict)
               case Upper_Case (TRIM (Ada.Command_Line.Argument (I)) (J)) is
                  when '-' =>
                     exit when J > 2;
                     exit;
                  when 'E' =>
                     if CL_Arguments (LATIN_ONLY) then
                        raise Args_Exception;
                     else
                        CL_Arguments (ENGLISH_ONLY) := True;
                     end if;
                  when 'L' =>
                     if CL_Arguments (ENGLISH_ONLY) then
                        raise Args_Exception;
                     else
                        CL_Arguments (LATIN_ONLY) := True;
                     end if;
                  when 'R' =>
                     CL_Arguments (READ_ONLY) := True;
                  when 'M' =>
                     CL_Arguments (MEANINGS_ONLY) := True;
                     CONFIGURATION                :=
                       ONLY_MEANINGS;      -- re-use this existing setting; nothing more to do to enforce meanings only
                  when 'N' =>
                     CL_Arguments (NO_FILES) := True;
                  when 'X' =>
                     CL_Arguments (NO_EXIT) := True;
                     pragma Unreserve_All_Interrupts;
                     Attach_Handler (No_Exit_Handler_Access, SIGINT);
                  when '?' | 'H' =>
                     raise Args_Exception; -- display arguments help and terminate
                  when others =>
                     New_Line;
                     Put_Line
                       ("====== UNKNOWN COMMAND-LINE OPTION: " &
                        TRIM (Ada.Command_Line.Argument (I)) (J) & " ======");
                     raise Args_Exception;
               end case;

            end loop;

            exit when I > 5;  -- max 5 arguments (-E and -L conflict)
         end loop; -- while =< argument_counter'length

         -- Still in the "elsif" statement => we're using the - / -- style
         -- arguments => INTERACTIVE MODE STARTUP:
         METHOD           := INTERACTIVE;
         SUPPRESS_PREFACE := False;
         Set_Output (Standard_Output);
         INITIALIZE_WORD_PARAMETERS;
         INITIALIZE_DEVELOPER_PARAMETERS;
         INITIALIZE_WORD_PACKAGE;

         if CL_Arguments (NO_EXIT) then
            loop
               Parse; -- return => infinite loop avoids more complex exception logic
            end loop;
         else
            Parse;
            return;
         end if;

      exception

         when Args_Exception =>     -- Parse may raise NO_EXCEPTION_EXCEPTION; handler is outside the block
            words_help.SHOW_HELP ("ARG");
            return;
      end New_Style_Arguments;

   end if; -- enclosing interactive mode

   -- NOT entering interactive mode; no '-'-style parameters; back to classic
   -- Words startup

   SUPPRESS_PREFACE := True;
   INITIALIZE_WORD_PARAMETERS;
   INITIALIZE_DEVELOPER_PARAMETERS;
   INITIALIZE_WORD_PACKAGE;

   --  choose a non-interactive mode: when 1 argument => either a simple Latin
   --  word or an input file. when 2 arguments => two words in-line OR language
   --  switch and word or input file when more arguments => command-line of
   --  words

   if Ada.Command_Line.Argument_Count > 1
     and then Ada.Command_Line.Argument (1) (1) = CHANGE_LANGUAGE_CHARACTER
   then

      CHANGE_LANGUAGE ('E');
      METHOD := COMMAND_LINE_INPUT;

      English_Command_Line_Input :
      declare
         Input_String   : String (1 .. 250) := (others => ' ');
         Length_Counter : Natural           := 0;
      begin
         for I in 2 .. (Ada.Command_Line.Argument_Count) loop
            exit when (Length_Counter + 1) +
              Ada.Command_Line.Argument (I)'length >
              250;

            --to get the same result as interactive mode and classic Words command-line, we pass a single string (because of POFS option)
            Input_String
              ((Length_Counter + 1) ..
                   (Length_Counter +
                    (Ada.Command_Line.Argument (I)'Length))) :=
              Ada.Command_Line.Argument (I);

            Length_Counter :=
              (Length_Counter + 1 + (Ada.Command_Line.Argument (I)'Length));
         end loop;

         Parse (TRIM (Input_String));
         return;
      end English_Command_Line_Input;

   elsif Ada.Command_Line.Argument_Count > 2 then

      METHOD := COMMAND_LINE_INPUT;

   else -- Ada.Command_Line.Argument_Count = 1 or 2 w/o language change

      SETUP_INPUT :
      declare
      begin

         if WORDS_MODE (DO_UNICODE_INPUT) then
            Ada.Wide_Text_IO.Open
              (W_INPUT, Ada.Wide_Text_IO.In_File,
               (Ada.Command_Line.Argument (1)));
            Ada.Wide_Text_IO.Close (W_INPUT);
         else

            Open (INPUT, In_File, TRIM (Ada.Command_Line.Argument (1)));
            Set_Input (INPUT);
            METHOD := COMMAND_LINE_FILES;
         end if;
      exception
         when Name_Error =>
            METHOD := COMMAND_LINE_INPUT;
      end SETUP_INPUT;

      if Ada.Command_Line.Argument_Count = 2
        and then METHOD /= COMMAND_LINE_INPUT

      then
         SETUP_OUTPUT :
         begin
            WORDS_MODE (DO_ANSI_FORMATTING) := False;
            Open (OUTPUT, Append_File, TRIM (Ada.Command_Line.Argument (2)));
            Set_Output (OUTPUT);
         exception
            when Name_Error =>
               Create (OUTPUT, Out_File, TRIM (Ada.Command_Line.Argument (2)));
               Set_Output (OUTPUT);
         end SETUP_OUTPUT;
      end if;

   end if;

   if METHOD = COMMAND_LINE_INPUT then

      for I in 1 .. Ada.Command_Line.Argument_Count loop
         declare
            INPUT_LINE : constant String :=
              TRIM (Ada.Command_Line.Argument (I));
         begin
            Parse (TRIM (INPUT_LINE));
         end; --block
      end loop;

   else

      METHOD := COMMAND_LINE_FILES;

      if WORDS_MODE (DO_UNICODE_INPUT) then
         Parse_Unicode_File ((Ada.Command_Line.Argument (1)));
      else
         Parse;
      end if;

      if Name (Current_Output) /= Name (Standard_Output) then
         Set_Output (Standard_Output);
         Close (OUTPUT);
         Put_Line
           ("Wrote output to file " & TRIM (Ada.Command_Line.Argument (2)));
      end if;

   end if;

end WORDS;
