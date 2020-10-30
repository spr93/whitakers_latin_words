with Text_IO;               use Text_IO;
with Ada.Strings.Unbounded;
with WORD_PARAMETERS;       use WORD_PARAMETERS;
with DEVELOPER_PARAMETERS;  use DEVELOPER_PARAMETERS;
with LIST_PACKAGE;          use LIST_PACKAGE;
with CONFIG;                use CONFIG;


package body Arabic2Roman is

  procedure Arabic2Roman
     (OUTPUT : in Ada.Text_IO.File_Type; INPUT_WORD : in String)
   is

      type Roman_Num_Record_Type is record
         Age_X              : Unbounded_String;
         Age_F              : Unbounded_String;
         Bar_Reminder       : Boolean           := False;
      end record;

      Null_Roman_Num_Record : Roman_Num_Record_Type := (Bar_Reminder => False,
                                                        others       => Null_Unbounded_String);

      Roman_Num_Record      : Roman_Num_Record_Type;

      Put_Additive          : Boolean           := False;
      Arabic_String         : String (1 .. 11)  := (others => ' ');
      Arabic_Build_Counter  : Integer           := 1;
      Bar_Length            : Integer           := 0;
      Input_Counter         : Integer           := INPUT_WORD'First;

      Is_Negative           : Boolean;

   begin

      for I in INPUT_WORD'range
      loop   --outermost loop ensures we catch everything where there are two numbers in row

         if Input_Counter > INPUT_WORD'Last then
            return;
         end if;

         Arabic_Build_Counter := 1;
         Is_Negative          := False;
         Arabic_String        :=
           (others => ' ');
         Put_Additive         := False;
         Bar_Length           := 0;
         Roman_Num_Record     := Null_Roman_Num_Record;


         if INPUT_WORD (Input_Counter) = '-' then
            Is_Negative := True;
            Input_Counter := Input_Counter + 1;
         end if;

         for J in Input_Counter .. INPUT_WORD'Last loop
            if Arabic_Build_Counter > 11
            then   -- too big; fast forward to the next number
               for Z in Input_Counter .. INPUT_WORD'Last loop
                  exit when INPUT_WORD(INPUT_Counter) = '|';
                  Input_Counter := Input_Counter + 1;
               end loop;
               Input_Counter := Input_Counter + 1;
              exit;  -- back to parse
            end if;

            case INPUT_WORD (Input_Counter)
            is  -- PARSE procedure send us a string with these sentinels: | (blank/space/delimiter), z (letter), #, or - (potential negative)
               when '0' .. '9' =>
                  Arabic_String (Arabic_Build_Counter) :=
                    INPUT_WORD (Input_Counter);
                  Arabic_Build_Counter := Arabic_Build_Counter + 1;
                  Input_Counter        := Input_Counter + 1;
               when '_' =>
                  Input_Counter := Input_Counter + 1;       -- this was a valid 000's delimiter (, or _) so ignore it
               when 'z' =>
                  Input_Counter := Input_Counter + 1;       -- there was a word or letter here, so ignore it
               when '|' => Input_Counter := Input_Counter + 1;
                  exit; -- a word or new number follows.
               when '.' =>
                  if (INPUT_WORD (Input_Counter + 1) in '0' .. '9') then
                     return;
                  else
                     Input_Counter := Input_Counter + 1;
                  end if;
               when '-' =>  -- "TRICK":  Accept negative numbers, convert to positive, and note that it's a neologism.
                  if Is_Negative and then WORDS_MODE (DO_TRICKS)
                  then  -- otherwise ignore negative entries
                     exit;
                  end if;
               when others =>
                  return;  -- we shouldn't see any other character.  if we do, something went wrong, so give up on converting to Roman numerals.
            end case;

         end loop;

         if (Integer_Test (Arabic_String) = True)
         then    -- if enclosing the rest of the procedure
            Arabic_Num :=
              Integer'Value
                (Arabic_String);  -- rest of procedure requires integer

            --quickly test and reject zeros
            if Arabic_Num = 0 then
                  -- New_Line (OUTPUT);
                  if WORDS_MDEV (DO_PEARSE_CODES) then
                     Put (OUTPUT, "03 ");
                  end if;
                     Format(OUTPUT, BOLD);
                  Put (OUTPUT, "nihil; nullum;");
                     Format(OUTPUT, Reset);
                  New_Line (OUTPUT);
               return;
            end if;

            -- build the numerals
            case Arabic_Num is
               when 1 .. 99_999 =>
                  Roman_Num_Record.Age_F := Generate_Subtractive (Arabic_Num);
               when 100_000 .. 999_999_999 =>
                  Roman_Num_Record.Age_F := Generate_Subtractive ((Arabic_Num / 100_000) mod 100_000);
                  Bar_Length := Length(Roman_Num_Record.Age_F)+2;
                  Roman_Num_Record.Age_F :=
                    "|" &
                    Roman_Num_Record.Age_F &
                    "|" & Generate_Subtractive (Arabic_Num mod 10_000);
                    Roman_Num_Record.Bar_Reminder := True;
               when others =>
                  return;
            end case;

            -- Is the number low enough to do an additive form?  is there a unique additive form?
            if Arabic_Num <= 100_000 then
               Roman_Num_Record.Age_X := Generate_Additive (Arabic_Num);

                if Roman_Num_Record.Age_X /= Roman_Num_Record.Age_F then
               Put_Additive := True;
               end if;
            end if;

            -- user note for OMIT_MEDIEVAL and OMIT_UNCOMMONG
            if ((WORDS_MDEV (OMIT_MEDIEVAL) = True) or
                (WORDS_MDEV (OMIT_UNCOMMON) = True))
                and then
                (Is_Negative
                or (Arabic_Num not in 1 .. 500 | 600 | 700 | 800 | 900 | 10_000) )
            then

               Put (OUTPUT, Arabic_String);
                   Set_Col (OUTPUT, 15);
                   Put_Line (OUTPUT, "========   UNKNOWN  ");
               New_Line;

               if WORDS_MDEV (DO_PEARSE_CODES) then
                  Put (OUTPUT, "06 ");
               end if;

               Format(Output,Inverse);
               Put (OUTPUT, "Uncommon medieval numeral forms exist, but OMIT_MEDIEVAL or OMIT_UNCOMMON ON");
               FORMAT (OUTPUT, RESET); New_Line(OUTPUT);
               return;
            end if; -- end medieval/uncommon exceptions

            -- BARS AND REMINDER FOR BIG NUMBERS
            if Roman_Num_Record.Bar_Reminder = True then

              if  WORDS_MDEV (DO_PEARSE_CODES) then
                  if WORDS_MODE (DO_ONLY_MEANINGS) = False
                    and then (not (CONFIGURATION = ONLY_MEANINGS))
                  then
                     Put (OUTPUT, "06 ");
                  else
                     Put
                       (OUTPUT,
                        "03 ");  -- Print as the meaning if only showing meanings;  disabling Arabic2Roman is the equivalent here
                  end if;
               end if;

               Format (OUTPUT, Inverse);
                Put
                 (OUTPUT,
                  "The -s and |s should be solid lines, forming a three-sided 'box'.");
               Format (OUTPUT, Reset);
               New_Line(OUTPUT);

               if Bar_Length > 0 then
                 if WORDS_MDEV (DO_PEARSE_CODES) then
                     Put(Output,"01 ");
                 end if;

                 for I in 1..Bar_Length loop
                    Put(OUTPUT,'_');
                 end loop;

                  New_Line(OUTPUT);
               end if;

            end if;

            if WORDS_MDEV (DO_PEARSE_CODES) then
               if WORDS_MODE (DO_ONLY_MEANINGS) = False
                 and then (not (CONFIGURATION = ONLY_MEANINGS))
               then
                  Put (OUTPUT, "01 ");
               else
                  Put
                    (OUTPUT,
                     "03 ");  -- Print as the meaning if only showing meanings; disabling Arabic2Roman is the equivalent here
               end if;
            end if;

            if (Put_Additive  and then Is_Negative = False and then
                Arabic_Num in 1 .. 500 | 600 | 700 | 800 | 900 | 10_000)
            then
               Put (OUTPUT, To_String (Roman_Num_Record.Age_X));
            else
               Put (OUTPUT, To_String (Roman_Num_Record.Age_F));
            end if;

            if WORDS_MODE (DO_ONLY_MEANINGS) = False
               and then (not (CONFIGURATION = ONLY_MEANINGS))
            then

               Set_Col (OUTPUT, 41);
               Put (OUTPUT, "CARD");

               if WORDS_MODE (SHOW_AGE) then
                  Set_Col (OUTPUT, 59);
                  -- since negatives are neologism, ignore the additive form
                  if Is_Negative then
                     Put (OUTPUT, "NeoLatin");

                  elsif Arabic_Num in 1 .. 500 | 600 | 700 | 800 | 900 | 10_000 then

                     case Put_Additive is
                        when True => Put
                                      (OUTPUT,
                                             "Archaic");  -- additive is different from subtractive; small or simple roman numerals
                        when False =>                       Put
                                      (OUTPUT,
                                             "Always");   --- additive and subtractive the same; small or simple roman numerals
                      end case;

                  else     -- long or complicated roman numerals => medieval
                     Put (OUTPUT, "Medieval");

                  end if;
               end if;

               if WORDS_MODE (SHOW_FREQUENCY) = True then
                  Set_Col (OUTPUT, 69);
                  if Is_Negative and Put_Additive = False then
                     Put (OUTPUT, "very rare");
                  elsif Is_Negative = False then
                     Put (OUTPUT, "mostfreq");
                  end if;
               end if;

            end if; -- end of meanings_only stuff

            Format (OUTPUT, RESET);
            New_Line (OUTPUT);

            if Is_Negative then
               Format(OUTPUT, FAINT);
               Put (OUTPUT, "~ negativum");
               Format(OUTPUT, Reset); New_Line(OUTPUT);
             end if;

            -- if enclosing dictionary-form line
            if
              (WORDS_MDEV (SHOW_DICTIONARY_CODES) or
                   WORDS_MODE (SHOW_FREQUENCY))
               and then
                WORDS_MODE (DO_ONLY_MEANINGS) = False
                and then
               CONFIGURATION /= ONLY_MEANINGS
            then

               if WORDS_MDEV (DO_PEARSE_CODES) then
                  Put (OUTPUT, "02 ");
               end if;

               Format(OUTPUT, UNDERLINE);

             if Is_Negative then -- subtractive, negative
                  if WORDS_MDEV (SHOW_DICTIONARY_CODES) = True then
                     Put (OUTPUT, "[HXXFQ]");
                  end if;
                  if WORDS_MODE (SHOW_FREQUENCY) = True then
                     Put (OUTPUT, "  very rare");
                  end if;
                  -- no additive negatives

             elsif (Put_Additive = True and then
                       Arabic_Num in 1 .. 500 | 600 | 700 | 800 | 900 | 10_000 )
               then
                  if WORDS_MDEV (SHOW_DICTIONARY_CODES) = True then
                     Put (OUTPUT, "[AXXAQ]");
                  end if;
                  if WORDS_MODE (SHOW_FREQUENCY) = True then
                     Put (OUTPUT, "  very frequent");
                  end if;

             elsif (Put_Additive = False and then
                 Arabic_Num in 1 .. 500 | 600 | 700 | 800 | 900 | 10_000 )
               then

                  if WORDS_MDEV (SHOW_DICTIONARY_CODES) = True then
                     Put (OUTPUT, "[XXXAQ]");
                  end if;
                  if WORDS_MODE (SHOW_FREQUENCY) = True then
                     Put (OUTPUT, "  very frequent");
                  end if;

             elsif Put_Additive then
                  if WORDS_MDEV (SHOW_DICTIONARY_CODES) = True then
                     Put (OUTPUT, "[FXXDQ]");
                  end if;
                  if WORDS_MODE (SHOW_FREQUENCY) = True then
                     Put (OUTPUT, "  lesser");
                  end if;
             else -- long or complicated number => medieval
                  if WORDS_MDEV (SHOW_DICTIONARY_CODES) = True then
                     Put (OUTPUT, "[FXXDQ]");
                  end if;
                  if WORDS_MODE (SHOW_FREQUENCY) = True then
                     Put (OUTPUT, "  lesser");
                  end if;
            end  if;
            Format (Output, Reset); New_Line(OUTPUT);
            end if; -- end of dictionary-form line

            -- meaning line:
            if WORDS_MDEV (DO_PEARSE_CODES) then
               Put(Output,"03 ");
            end if;

            Format(OUTPUT,BOLD);
            for C in Arabic_String'Range loop
               exit when Arabic_String (C) = ' ';
               Put (OUTPUT, Arabic_String (C));
            end loop;

            Put (OUTPUT, " as a ROMAN NUMERAL");
            Format(OUTPUT, RESET);

            New_Line (OUTPUT); -- end output of first result

            if Put_Additive
              and then Arabic_Num in 1 .. 500 | 600 | 700 | 800 | 900 | 10_000
              and then Is_Negative = False
              then -- only situation where we could have another result:
                   -- small number with both additive and subtractive
                   -- call this classical for lack of a better way to distinguish from additive

               if WORDS_MODE(DO_ANSI_FORMATTING) then  -- skip another before putting second result
                  New_Line(OUTPUT);
                  end if;

               if WORDS_MDEV (DO_PEARSE_CODES) then
                  if WORDS_MODE (DO_ONLY_MEANINGS) = False
                    and then (not (CONFIGURATION = ONLY_MEANINGS))
                  then
                     Put (OUTPUT, "01 ");
                  else
                     Put
                       (OUTPUT,
                        "03 ");  -- Print as the meaning if only showing meanings;  disabling Arabic2Roman is the equivalent here
                  end if;

               end if;

               Put (OUTPUT, To_String (Roman_Num_Record.Age_F));

               if WORDS_MODE (DO_ONLY_MEANINGS) = False
                 and then (not (CONFIGURATION = ONLY_MEANINGS))
               then
                  Set_Col (OUTPUT, 41);
                  Put (OUTPUT, "CARD");

                  if WORDS_MODE (SHOW_AGE) then
                     Set_Col (OUTPUT, 59);

                     Put (OUTPUT, "Classical");
                  end if;

                  if WORDS_MODE (SHOW_FREQUENCY) = True then
                     Set_Col (OUTPUT, 69);

                     Put (OUTPUT, "mostfreq");
                  end if;
                  New_Line (OUTPUT);
               end if;

               Format(OUTPUT, UNDERLINE);
               -- if enclosing dictionary line items
               if
                 (WORDS_MDEV (SHOW_DICTIONARY_CODES) or
                      WORDS_MODE (SHOW_FREQUENCY))
                 and
                   (not WORDS_MODE (DO_ONLY_MEANINGS) and then
                      CONFIGURATION /= ONLY_MEANINGS)
               then

                  if WORDS_MDEV (DO_PEARSE_CODES) then
                     Put (OUTPUT, "02 ");
                  end if;

                  if WORDS_MDEV (SHOW_DICTIONARY_CODES) = True then
                     Put (OUTPUT, "[CXXAQ]");
                  end if;

                  if WORDS_MODE (SHOW_FREQUENCY) = True then
                     Put (OUTPUT, "  very frequent");
                  end if;

                  Format(Output,Reset); New_Line (OUTPUT);

                if WORDS_MDEV (DO_PEARSE_CODES) then
                Put(Output,"03 ");
                  end if;

               Format(OUTPUT, BOLD);
                for C in Arabic_String'Range loop
                  exit when Arabic_String (C) = ' ';
                  Put (OUTPUT, Arabic_String (C));
               end loop;
               Put_Line (OUTPUT, " as a ROMAN NUMERAL");
                 Format(OUTPUT, Reset); New_Line (OUTPUT);
            end if;  -- if enclosing dictionary line items

          else New_Line(OUTPUT); -- we put the subtractive, then ended; so skip line
          end if; -- Put_Additive
         -- end of second output

         end if; -- enclosing statements requiring integer

      end loop;  -- end outermost loop

      return;

   end Arabic2Roman;

   function Generate_Additive (Arabic_Num : in Integer) return Unbounded_String
   is

      built_string   : Unbounded_String;
      Counter        : Integer          := 1;
      Frame          : Unbounded_String;
      Arabic_String2 : String           := Integer'Image (Arabic_Num);
   begin

      for I in reverse 2 .. (Arabic_String2'Length)
      loop  --  build from lowest Arabic digit, moving left (position 1 is blank to indicate positive)
         case Arabic_String2 (I) is

            when '0' =>
               null;
            when '1' =>
               built_string := Roman_Nums_CLASSICAL (Counter) & built_string;
            when '2' =>
               built_string :=
                 Roman_Nums_CLASSICAL (Counter) &
                 Roman_Nums_CLASSICAL (Counter) & built_string;
            when '3' =>
               built_string :=
                 Roman_Nums_CLASSICAL (Counter) &
                 Roman_Nums_CLASSICAL (Counter) &
                 Roman_Nums_CLASSICAL (Counter) & built_string;
            when '4' =>
               built_string :=
                 Roman_Nums_CLASSICAL (Counter) &
                 Roman_Nums_CLASSICAL (Counter) &
                 Roman_Nums_CLASSICAL (Counter) &
                 Roman_Nums_CLASSICAL (Counter) & built_string;
            when '5' =>
               built_string :=
                 Roman_Nums_CLASSICAL (Counter + 1) & built_string;
            when '6' =>
               built_string :=
                 Roman_Nums_CLASSICAL (Counter + 1) &
                 Roman_Nums_CLASSICAL (Counter) & built_string;
            when '7' =>
               built_string :=
                 Roman_Nums_CLASSICAL (Counter + 1) &
                 Roman_Nums_CLASSICAL (Counter) &
                 Roman_Nums_CLASSICAL (Counter) & built_string;
            when '8' =>
               built_string :=
                 Roman_Nums_CLASSICAL (Counter + 1) &
                 Roman_Nums_CLASSICAL (Counter) &
                 Roman_Nums_CLASSICAL (Counter) &
                 Roman_Nums_CLASSICAL (Counter) & built_string;
            when '9' =>
               built_string :=
                 Roman_Nums_CLASSICAL (Counter + 1) &
                 Roman_Nums_CLASSICAL (Counter) &
                 Roman_Nums_CLASSICAL (Counter) &
                 Roman_Nums_CLASSICAL (Counter) &
                 Roman_Nums_CLASSICAL (Counter) & built_string;
            when others =>
               null;
         end case;
         Counter :=
           Counter + 2;  -- Move two positions down the Roman numeral array each time
         exit when Counter > 11;  -- shouldnt ever hit this, but just in case
      end loop;

      if Arabic_String2'Length >= 8 then
         built_string := Frame & built_string;
         Counter      := 0;
      end if;
      return (built_string);
   end Generate_Additive;

   function Generate_Subtractive
     (Arabic_Num : in Integer) return Unbounded_String
   is

      Built_String : Unbounded_String;

      -- Rules get complex, especially starting at 4_000, so not using an array for readability and debugging purposes
   begin

      case (Arabic_Num / 10_000) mod 10 is
         when 0 =>
            null;
         when 1 =>
            Built_String := Built_String & To_Unbounded_String ("((I))");
         when 2 =>
            Built_String := Built_String & To_Unbounded_String ("((II))");
         when 3 =>
            Built_String := Built_String & To_Unbounded_String ("((III))");
         when 4 =>
            Built_String := Built_String & To_Unbounded_String ("((IV))");
         when 5 =>
            Built_String := Built_String & To_Unbounded_String ("((V))");
         when 6 =>
            Built_String := Built_String & To_Unbounded_String ("((VI))");
         when 7 =>
            Built_String := Built_String & To_Unbounded_String ("((VII))");
         when 8 =>
            Built_String := Built_String & To_Unbounded_String ("((VIII))");
         when 9 =>
            Built_String := Built_String & To_Unbounded_String ("((IX))");
         when others =>
            null;
      end case;

      case (Arabic_Num / 1_000) mod 10 is
         when 0 =>
            null;
         when 1 =>
            Built_String := Built_String & To_Unbounded_String ("M");
         when 2 =>
            Built_String := Built_String & To_Unbounded_String ("MM");
         when 3 =>
            Built_String := Built_String & To_Unbounded_String ("MMM");
         when 4 =>
            Built_String := Built_String & To_Unbounded_String ("M(V)");
         when 5 =>
            Built_String := Built_String & To_Unbounded_String ("(V)");
         when 6 =>
            Built_String := Built_String & To_Unbounded_String ("(VI)");
         when 7 =>
            Built_String := Built_String & To_Unbounded_String ("(VII)");
         when 8 =>
            Built_String := Built_String & To_Unbounded_String ("(VIII)");
         when 9 =>
            Built_String := Built_String & To_Unbounded_String ("(IX)");
         when others =>
            null;
      end case;

      case (Arabic_Num / 100) mod 10 is
         when 0 =>
            null;
         when 1 =>
            Built_String := Built_String & To_Unbounded_String ("C");
         when 2 =>
            Built_String := Built_String & To_Unbounded_String ("CC");
         when 3 =>
            Built_String := Built_String & To_Unbounded_String ("CCC");
         when 4 =>
            Built_String := Built_String & To_Unbounded_String ("CD");
         when 5 =>
            Built_String := Built_String & To_Unbounded_String ("D");
         when 6 =>
            Built_String := Built_String & To_Unbounded_String ("DC");
         when 7 =>
            Built_String := Built_String & To_Unbounded_String ("DCC");
         when 8 =>
            Built_String := Built_String & To_Unbounded_String ("DCCC");
         when 9 =>
            Built_String := Built_String & To_Unbounded_String ("CM");
         when others =>
            null;
      end case;

      -- Handle tens

      case (Arabic_Num / 10) mod 10 is
         when 0 =>
            null;
         when 1 =>
            Built_String := Built_String & To_Unbounded_String ("X");
         when 2 =>
            Built_String := Built_String & To_Unbounded_String ("XX");
         when 3 =>
            Built_String := Built_String & To_Unbounded_String ("XXX");
         when 4 =>
            Built_String := Built_String & To_Unbounded_String ("XL");
         when 5 =>
            Built_String := Built_String & To_Unbounded_String ("L");
         when 6 =>
            Built_String := Built_String & To_Unbounded_String ("LX");
         when 7 =>
            Built_String := Built_String & To_Unbounded_String ("LXX");
         when 8 =>
            Built_String := Built_String & To_Unbounded_String ("LXXX");
         when 9 =>
            Built_String := Built_String & To_Unbounded_String ("XC");
         when others =>
            null;
      end case;

      -- handle final digit
      case Arabic_Num mod 10 is
         when 0 =>
            null;
         when 1 =>
            Built_String := Built_String & To_Unbounded_String ("I");
         when 2 =>
            Built_String := Built_String & To_Unbounded_String ("II");
         when 3 =>
            Built_String := Built_String & To_Unbounded_String ("III");
         when 4 =>
            Built_String := Built_String & To_Unbounded_String ("IV");
         when 5 =>
            Built_String := Built_String & To_Unbounded_String ("V");
         when 6 =>
            Built_String := Built_String & To_Unbounded_String ("VI");
         when 7 =>
            Built_String := Built_String & To_Unbounded_String ("VII");
         when 8 =>
            Built_String := Built_String & To_Unbounded_String ("VIII");
         when 9 =>
            Built_String := Built_String & To_Unbounded_String ("IX");
         when others =>
            null;
      end case;
      return (Built_String);

   end Generate_Subtractive;

   function Integer_Test (Arabic_String : in String) return Boolean is
      -- Checks that PARSE and Arabic2Roman have left us with valid input
      -- and lets us handle exceptions silently so we don't interrupt PARSE
      -- if somehow nonsense characters have made it to this point
      type Valid_Integer is new Integer range -999_999_999 .. 999_999_999;
      Tester : Valid_Integer;
   begin
      Tester := Valid_Integer'Value (Arabic_String);
      return True;
   exception
      when others =>
         return False;
   end Integer_Test;

end Arabic2Roman; -- end of package
