with Text_IO;              use Text_IO;
with CONFIG;               use CONFIG;
with WORD_PACKAGE;
with DEVELOPER_PARAMETERS; use DEVELOPER_PARAMETERS;
with STRINGS_PACKAGE;      use STRINGS_PACKAGE;


package body Words_Help is

   ---------
   -- PUT --
   ---------

   procedure PUT (HELP : in Main_Help_Type; Output : in Text_IO.File_Type) is

      Need_To_Reset : Boolean := False;

   begin
      New_Line;

      for Line_Counter in HELP'Range loop

         if WORDS_MDEV (PAUSE_IN_SCREEN_OUTPUT) and
           Line_Counter > OUTPUT_SCREEN_SIZE and
           (Line_Counter mod (OUTPUT_SCREEN_SIZE + 1)) = 0
         then
            if Need_To_Reset and WORDS_MODE (DO_ANSI_FORMATTING) then
               Format (Output, RESET);
            end if;

            WORD_PACKAGE.PAUSE (Output);

            if Need_To_Reset and WORDS_MODE (DO_ANSI_FORMATTING) then
               Format (Output, BOLD);
            end if;
         end if;

         if HELP (Line_Counter) = Skip_Next then
            New_Line;

         elsif HELP (Line_Counter) = Underline_Next then
            if WORDS_MODE (DO_ANSI_FORMATTING) then
               Format (Output, UNDERLINE);
            end if;
            Need_To_Reset := True;
         elsif HELP (Line_Counter) = Bold_Next then
            if WORDS_MODE (DO_ANSI_FORMATTING) then
               Format (Output, BOLD);
               Need_To_Reset := True;
            else
               Need_To_Reset := False;
            end if;
         else

            for I in HELP (Line_Counter)'First .. HELP (Line_Counter)'Last loop

               if (HELP (Line_Counter) (I) = '|') then
                  if Need_To_Reset then
                     case WORDS_MODE (DO_ANSI_FORMATTING) is
                        when True =>
                           Format (Output, RESET);

                        when False =>
                           New_Line;
                           for J in HELP'First .. (I - 1) loop
                              Put ('-');
                           end loop;
                     end case;

                     Need_To_Reset := False;
                  end if;
                  exit;

               else
                  Put (HELP (Line_Counter) (I));

                  if (I = HELP (Line_Counter)'last) and
                    WORDS_MODE (DO_ANSI_FORMATTING)
                  then
                     Format (Output, RESET);
                  end if;
               end if;

        end loop;

        New_Line;

         end if;

         --  end loop;
      end loop;

      New_Line;
   end PUT;

   ---------------
   -- SHOW_HELP --
   ---------------

   procedure SHOW_HELP (Line : in String; Output : in Text_IO.File_Type) is

      Help_Param : String (1 .. 3) := (others => ' ');

   begin

      if Line'LENGTH >= 3 then
         Help_Param := Line (Line'First .. (Line'First + 2));
      end if;

      if Help_Param = "COD" then
         PUT (CODES_HELP, Output);
      elsif Help_Param = "SOU" then
         PUT (SOURCE_HELP, Output);
      elsif Help_Param = "GRA" then
         PUT (GRAMMAR_HELP, Output);
      elsif Help_Param = "AGE" or Help_Param = "ERA" then
         PUT (AGE_HELP, Output);
      elsif Help_Param = "ARE" then
         PUT (AREA_HELP, Output);
      elsif Help_Param = "GEO" then
         PUT (GEO_HELP, Output);
      elsif Help_Param = "FRE" then
         PUT (FREQ_HELP, Output);
      elsif Help_Param = "MEA" or Help_Param = "ABB" then
         PUT (MEANING_HELP, Output);
      elsif Help_Param = "PEA" then
         PUT (PEARSE_HELP, Output);
      elsif Help_Param = "PAR" or Help_Param = "ARG" then
         PUT (Param_Help, Output);
      else
         PUT (GENERAL_HELP, Output);
      end if;

   exception
      when others =>
         null;

   end SHOW_HELP;

end Words_Help;
