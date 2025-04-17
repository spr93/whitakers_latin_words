--
-- LICENSE DETAILS AND RIGHTS GRANT AT BOTTOM OF FILE.
-- SUMMARY:
-- Copyright (c) 1993-2025 William Armstrong Whitaker and contributors.
-- BSD 2-clause
--
-- In memoriam, Col. Wm. Whitaker, Chair of DoD Working Group responsible for
-- establishing the Ada language and accomplished amateur Latin lexicographer,
-- who labored for years to create this comprehensive and free dictionary.
--

---------------------------------------------------
-- CORE:  Packages necessary for the core Words system; Whitaker's original
with TEXT_IO;              use TEXT_IO;
   -- Configuration packages
with CONFIG;               use CONFIG;               -- Important global variables, changes to which
with LATIN_FILE_NAMES;     use LATIN_FILE_NAMES;     --   must be carefully controlled
with WORD_PARAMETERS;      use WORD_PARAMETERS;      -- Common options; easily changeable by user
with DEVELOPER_PARAMETERS; use DEVELOPER_PARAMETERS; -- Uncommon options that Whitaker used primarily
                                                     --   for development - rarely set by user, mostly
                                                     --   simple control-flow changes; can be disabled in CONFIG
   -- Logic-and-interface packages
with WORD_PACKAGE;         use WORD_PACKAGE;         -- Initializes type system and data structures, loads dictionary data
with STRINGS_PACKAGE;      use STRINGS_PACKAGE;      -- Mostly formatting routines; mostly for user interface
with PARSE_PACKAGE;        use PARSE_PACKAGE;        -- Contains PARSE, the main control program

-- MORE:  Packages supporting additional user convenience options; mostly post-Whitaker
with Ada.Command_Line;                               -- Command-line options (see below)
with Ada.Wide_Text_IO;                               -- For Unicode handling (macron stripping)
with Words_Help;                                     -- Online help system
with Ada.Exceptions;                                 -- Provide information for unhandled errors
---------------------------------------------------

procedure WORDS is -- the main/startup procedure.  Handles I/O configuration and command-line options.

  procedure Initialize_Dictionary is -- Mostly related to file system operations (opening, loading, and creating files).
  begin                              -- Some options (METHODs and command-line options) modify, moot, or prohibit some
                                     -- of those operations so we defer them until after all options are set.
      Find_Data_And_Settings;
      INITIALIZE_WORD_PARAMETERS;
      INITIALIZE_DEVELOPER_PARAMETERS;
      INITIALIZE_WORD_PACKAGE;
  end Initialize_Dictionary;

begin

-- PART I:  INTERACTIVE MODES.  Self-contained modes that NEVER fall through to Part II.

   --  SIMPLE INTERACTIVE MODE -- when Words is run without parameters
   if Ada.Command_Line.Argument_Count = 0 then
      METHOD           := INTERACTIVE;
      Initialize_Dictionary;
      PARSE;
      return;

   --  COMMAND-LINE ARGUMENTS ("new" [post-Whitaker] options that modify interactive mode)
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
                       ONLY_MEANINGS;       -- this config.ads setting is all we need to enforce meanings only
                  when 'N' =>
                     CL_Arguments (NO_FILES) := True;
                  when 'X' =>
                     CL_Arguments (NO_EXIT) := True;
                  when '?' | 'H' | '-' =>
                     raise Args_Exception;  -- display arguments help and terminate
                  when others =>
                     New_Line;
                     Put_Line
                       ("====== UNKNOWN COMMAND-LINE OPTION: " &
                        TRIM (Ada.Command_Line.Argument (I)) (J) & " ======");
                     raise Args_Exception;
               end case;

            end loop;

            exit when I > 5;  -- max 5 arguments (-E and -L conflict)
         end loop; -- I in 1 .. Ada.Command_Line.Argument_Count

         -- Still in the "elsif" statement => we're using '-' style arguments
         -- arguments => INTERACTIVE MODE STARTUP:
         METHOD           := INTERACTIVE;
         Initialize_Dictionary;

         if CL_Arguments (NO_EXIT) then
            loop
               Parse;
            end loop;
         else
            Parse;
            return;
         end if;

      exception

         when Args_Exception =>
            Words_Help.SHOW_HELP (Current_Output, "ARG");
            return;
      end New_Style_Arguments;

   end if; -- enclosing interactive mode

----------------------------------------------------------------------------------------------------

 --  PART II:  NON-INTERACTIVE MODES--i.e., the command-line possibilities Whitaker created
 --            Steps:  A (set global options); B (load data files); C (parse dictionary queries)

  -- STEP A:   Set global options

   --  NOT entering interactive mode, so minimize screen output.
   SUPPRESS_PREFACE := True;

   --  Now choose a non-interactive mode: when 1 argument => either a simple Latin
   --  word or an input file. when 2 arguments => two words in-line OR language
   --  switch and word or input file when more arguments => command-line of Latin

  --  First possibility:  English->Latin command-line mode
   --  Words has never supported processing a file of English words after a command-line switch to English.
   --  Few users should want to do that because Word's isn't suitable for translating a lot of English
   --  to Latin in a non-interactive mode (getting a Latin word with the right connotation
   --  often requires interactively trying a few English words; dumping a lot of English into Words
   --  will almost always produce output that's be too long (or too TRIM'd) to be practical).
   --  We'd also need to jump through more hoops to detect and handle the POFS restriction option.

   if Ada.Command_Line.Argument_Count > 1
    and then Ada.Command_Line.Argument (1) (1) = CHANGE_LANGUAGE_CHARACTER
    and then ( Ada.Command_Line.Argument (1) (2) = 'e' or Ada.Command_Line.Argument (1) (2) ='E')
   then
      METHOD := COMMAND_LINE_INPUT;
      Initialize_Dictionary;
      CHANGE_LANGUAGE ('E');

      English_Command_Line_Input :
      declare
         Input_String   : String (1 .. 250) := (others => ' ');
         Length_Counter : Natural           := 0;
      begin
         for I in 2 .. (Ada.Command_Line.Argument_Count) loop
            exit when (Length_Counter + 1) +
              Ada.Command_Line.Argument (I)'length >
              250;

         -- In English->Latin mode, PARSE needs to receive a single string (due to POFS option)
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

 -- Possibility two:  >2 arguments without a switch to English->Latin => must be command-line Latin
   elsif Ada.Command_Line.Argument_Count > 2 then
      METHOD := COMMAND_LINE_INPUT;

 -- Remaining possibilities:  Command-line Latin or Latin command-line files.  Ambiguous until we check
 --                           whether the first argument is a valid input file name
   else -- (Ada.Command_Line.Argument_Count = 1 or 2)

    -- We've got to start doing some file I/O at this point, but not sure whether to open the file
    -- for Unicode processing (Wide_Text) or not.
    -- Go ahead and initialize now.  At this point, METHOD is NOT_YET_SET, which will
    -- suppress unnecessary file operations during initialization.
       METHOD := COMMAND_LINE_FILES;

    SETUP_INPUT :
      declare
      begin
         if WORDS_MODE (DO_UNICODE_INPUT) then
            Ada.Wide_Text_IO.Open
              (W_INPUT, Ada.Wide_Text_IO.In_File,
               Correct_File(TRIM(Ada.Command_Line.Argument (1))));
         --   Ada.Wide_Text_IO.Close (W_INPUT);
         else

            Open (INPUT, In_File, Correct_File(TRIM (Ada.Command_Line.Argument (1))));
            Set_Input (INPUT);
         end if;

      exception
         when Name_Error =>              -- First argument not a valid file name
         METHOD := COMMAND_LINE_INPUT;   -- => presume it's a (Latin) word to translate
      end SETUP_INPUT;

      if Ada.Command_Line.Argument_Count = 2
        and then METHOD /= COMMAND_LINE_INPUT  -- So we won't interfere with any ..._TO_OUTPUT_FILE parameters
      then
         SETUP_OUTPUT :
         begin
            WORDS_MODE (DO_ANSI_FORMATTING) := False;
            Open (OUTPUT, Append_File, Correct_File(TRIM (Ada.Command_Line.Argument (2))));
            Set_Output (OUTPUT);
         exception
            when Name_Error =>
               Create (OUTPUT, Out_File, Correct_File(TRIM (Ada.Command_Line.Argument (2))));
               Set_Output (OUTPUT);
      end SETUP_OUTPUT;
    elsif WORDS_MODE(HAVE_OUTPUT_FILE) and then not TEXT_IO.IS_OPEN(OUTPUT)
      then
      TEXT_IO.CREATE(OUTPUT, TEXT_IO.OUT_FILE, Correct_File(LATIN_FILE_NAMES.OUTPUT_FULL_NAME));
       -- else must be writing to Standard_Output (or whatever is right for the target platform), so no output setup needed.
      end if;

   end if;

  -- STEP B:  Load data files
  Initialize_Dictionary;

  -- STEP C:   Parse dictionary queries
   if METHOD = COMMAND_LINE_INPUT then
      for I in 1 .. Ada.Command_Line.Argument_Count loop
         declare
            INPUT_LINE : constant String :=
              TRIM (Ada.Command_Line.Argument (I));
         begin
              Parse (INPUT_LINE);
         end;
      end loop;

   else  -- METHOD := COMMAND_LINE_FILES;
      if WORDS_MODE (DO_UNICODE_INPUT) then
         Parse_Unicode_File ((Ada.Command_Line.Argument (1)));
      else
         Parse;
      end if;

     if Text_IO.Is_Open(Output) -- Must check Is_Open first or risk Status_Error
       and then Name (OUTPUT) /= Name (Standard_Output) then
       Put_Pearse_Code(OUTPUT,7);
       Put_Line
         (Standard_Output,"Wrote output to file " & Name(Output));
     end if;

   end if;

----------------------------------------------------------------------------------------------------

exception
  when Give_Up | FATAL_ERROR => return; -- handled errors
  when Catch_Me: Others =>
    Put_Line(Standard_Error,"Latin Words encountered a problem and cannot continue:");
    Put_Line(Standard_Error,Ada.Exceptions.Exception_Information(Catch_Me));
    Put_Line(Standard_Error,Ada.Exceptions.Exception_Message(Catch_Me));

end WORDS;

--
-- COPYRIGHT INFORMATION AND LICENSE GRANT
--
-- BACKGROUND:
--
-- SPR:  Whitaker developed and relased Words under a 1990s-style public domain license.
-- For example, he made the following statements in the documentation that he wrote concerning Words:
--   * "[Words is] freely available for anyone to use for any purpose. It may be converted
--      to other languages, used in pieces, or modified in any way without
--      further permission or notification."
--   * "The program source (in Ada) and dictionary are freely available for rehosting."
--
-- The BSD 2-clause license accurately captures the intent of Whitaker's license (that is,
-- (free for any commercial or non-commercial use,no attempt to control downstream use
-- or force a particular downstream licensing regime on derivatives) and is well known to modern audiences.
-- At a minimum, there is no conflict between the BSD 2-clause license and the license that Whitaker articulated.
--
-- See also discussion of license issues at https://github.com/mk270/whitakers-words/issues/118
--
-- The formal copyright and license grant follow.
--
--  LICENSE:
--
--  Copyright (c) 1993-2022 William Armstrong Whitaker and contributors.
--
--  Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
--
--  1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--
--  2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the
--     documentation and/or other materials provided with the distribution.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
--  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
--  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
--  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
--  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
--  EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--
