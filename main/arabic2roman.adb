with WORD_PARAMETERS;      use WORD_PARAMETERS;
with DEVELOPER_PARAMETERS; use DEVELOPER_PARAMETERS;
with CONFIG;               use CONFIG;
with INFLECTIONS_PACKAGE;  use INFLECTIONS_PACKAGE;
with DICTIONARY_PACKAGE;   use DICTIONARY_PACKAGE;
with LIST_PACKAGE;

package body Arabic2Roman is

   procedure Arabic2Roman
     (OUTPUT : in Text_IO.File_Type; INPUT_WORD : in String)
   is

      type Roman_Num_Record_Type is record
         Additive     : Unbounded_String;
         Subtractive  : Unbounded_String;
         Bar_Reminder : Boolean := False;
      end record;

      Null_Roman_Num_Record : constant Roman_Num_Record_Type :=
        (Bar_Reminder => False, others => Null_Unbounded_String);

      Roman_Num_Record : Roman_Num_Record_Type;

      Put_Additive         : Boolean          := False;
      Arabic_String        : String (1 .. 11) := (others => ' ');
      Arabic_Build_Counter : Integer          := 1;
      Bar_Length           : Integer          := 0;
      Input_Counter        : Integer          := INPUT_WORD'First;

      Is_Negative       : Boolean       := False;
      Pearse_Adjustment : Text_IO.Count := 0;

      -- The following declarations let us re-use the DIRECT_IO routines
      -- instantiated in INFLECTIONS as well as most of the inflection and
      -- dictionary entry-related routines in LIST_PACKAGE.

      Inflect_Padding : String (1 .. QUALITY_RECORD_IO.DEFAULT_WIDTH) :=
        (others => ' ');

      DE : DICTIONARY_ENTRY :=
        (STEMS => NULL_STEMS_TYPE,
         PART  => (POFS => NUM, NUM => ((0, 0), X, 0)),
         TRAN  => NULL_TRANSLATION_RECORD, MEAN => NULL_MEANING_TYPE);

      IR : INFLECTION_RECORD :=
        (QUAL   => Null_Roman_Numeral_Qual_Record, KEY => 0,
         ENDING => NULL_ENDING_RECORD, AGE => X, FREQ => X);
      -- end re-use declarations

   begin

      for I in INPUT_WORD'range
      loop   --outermost loop ensures we catch everything where there are two numbers in row

         if Input_Counter > INPUT_WORD'Last then
            return;
         elsif Input_Counter > INPUT_WORD'First
         then                    -- reset all variables not used for loop control
            DE.TRAN              := NULL_TRANSLATION_RECORD;
            IR.AGE               := X;
            IR.FREQ              := X;
            Arabic_Build_Counter := 1;
            Is_Negative          := False;
            Arabic_String        := (others => ' ');
            Put_Additive         := False;
            Bar_Length           := 0;
            Roman_Num_Record     := Null_Roman_Num_Record;
         end if;

         for J in Input_Counter .. INPUT_WORD'Last loop
            if Arabic_Build_Counter > 11
            then   -- too big; fast forward to the next number
               for Z in Input_Counter .. INPUT_WORD'Last loop
                  exit when INPUT_WORD (Input_Counter) = '|';
                  Input_Counter := Input_Counter + 1;
               end loop;
               Input_Counter := Input_Counter + 1;
               exit;  -- back to parse
            end if;

            case INPUT_WORD (Input_Counter)
            is  -- PARSE procedure sent a string with these sentinels: | (blank/space/delimiter), z (letter), #, or - (potential negative)
               when '0' .. '9' =>
                  Arabic_String (Arabic_Build_Counter) :=
                    INPUT_WORD (Input_Counter);
                  Arabic_Build_Counter := Arabic_Build_Counter + 1;
                  Input_Counter        := Input_Counter + 1;
               when '_' =>
                  Input_Counter :=
                    Input_Counter +
                    1;       -- this was a valid 000's delimiter (, or _) so ignore it
               when 'z' =>
                  Input_Counter :=
                    Input_Counter +
                    1;       -- there was a word or letter here, so ignore it
               when '|' =>
                  Input_Counter := Input_Counter + 1;
                  exit; -- a word or new number follows.
               when '.' =>
                  if (INPUT_WORD (Input_Counter + 1) in '0' .. '9') then
                     return; -- ignore decimals
                  else
                     Input_Counter := Input_Counter + 1;
                  end if;
               when '-' =>

                  if not Is_Negative then
                     Input_Counter := Input_Counter + 1;
                     Is_Negative   := True;
                  else
                     exit; -- treat a scond negative sign as a terminator
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
               Put_Pearse_Code(Output,3);
               Format (OUTPUT, BOLD);
               Put (OUTPUT, "nihil; nullum");
               Format (OUTPUT, RESET);
               New_Line (OUTPUT);
               return;
            end if;

            -- build the numerals
            case Arabic_Num is
               when 1 .. 99_999 =>
                  Roman_Num_Record.Subtractive :=
                    Generate_Subtractive (Arabic_Num);
               when 100_000 .. 999_999_999 =>
                  Roman_Num_Record.Subtractive :=
                    Generate_Subtractive ((Arabic_Num / 100_000) mod 100_000);
                  Bar_Length := Length (Roman_Num_Record.Subtractive) + 2;
                  Roman_Num_Record.Subtractive :=
                    "|" & Roman_Num_Record.Subtractive & "|" &
                    Generate_Subtractive (Arabic_Num mod 10_000);
                  Roman_Num_Record.Bar_Reminder := True;
               when others =>
                  return;
            end case;

            -- Is the number low enough to do an additive form? Is there a
            -- unique additive form?
            if Arabic_Num <= 100_000 then
               Roman_Num_Record.Additive := Generate_Additive (Arabic_Num);
               if Roman_Num_Record.Additive /= Roman_Num_Record.Subtractive
               then
                  Put_Additive := True;
               end if;
            end if;

            -- DETERMINE AGE
            -- At this point we can determine the age information, which will
            -- define the output from here out
            case Is_Negative is
               when True =>
                  IR.AGE         := G;
                  IR.FREQ        := D;
                  DE.TRAN.AGE    := G;
                  DE.TRAN.FREQ   := F;
                  DE.TRAN.SOURCE := Q;
                  Put_Additive   :=
                    False; -- since negativum is at least medieval, don't do additive
               when False =>

                  -- Ada 2012 syntax version:
                  --   if Arabic_Num in 1 .. 500 | 600 | 700 | 800 | 900 | 10_000
                  -- Ada 2005 syntax:
                  if
                    (Arabic_Num in 1 .. 500 or Arabic_Num = 600 or
                     Arabic_Num = 700 or Arabic_Num = 800 or
                     Arabic_Num = 900 or Arabic_Num = 10_000)
                  then
                     case Put_Additive is
                        when True =>
                           DE.TRAN.AGE    := B;
                           DE.TRAN.FREQ   := E;
                           IR.AGE         := B;
                           DE.TRAN.SOURCE := Q;
                        when False =>
                           DE.TRAN.FREQ := A;
                     end case; -- additive
                  else
                     IR.AGE      := F;
                     DE.TRAN.AGE := F;
                  end if;
            end case;

         -- END DETERMINE AGE

            if WORDS_MDEV (OMIT_MEDIEVAL) = True and then DE.TRAN.AGE >= F then

               Put (OUTPUT, Arabic_String);
               Set_Col (OUTPUT, 35);
               Put_Line (OUTPUT, "========   UNKNOWN  ");
               New_Line (OUTPUT);
               Put_Pearse_Code(OUTPUT,6);
               Format (OUTPUT, INVERSE);
               Put
                 (OUTPUT,
                  "Uncommon medieval numeral forms exist, but OMIT_MEDIEVAL ON");
               Format (OUTPUT, RESET);
               New_Line (OUTPUT);
               return;
            end if; -- end medieval/uncommon exceptions

            -- BARS AND REMINDER FOR BIG NUMBERS
            if Roman_Num_Record.Bar_Reminder = True then

               Pearse_Code_Adjust_For_Meanings (Output, Alternative => 6);

               Format (OUTPUT, INVERSE);
               Put
                 (OUTPUT,
                  "The _s and |s below should be solid lines, forming a three-sided 'box'");
               Format (OUTPUT, RESET);
               New_Line (OUTPUT);

               if Bar_Length > 0 then
               Put_Pearse_Code(OUTPUT,1);

                  for I in 1 .. Bar_Length loop
                     Put (OUTPUT, '_');
                  end loop;

                  New_Line (OUTPUT);
               end if;

            end if;

-- begin inflection (here, meaning) line stuff
            if WORDS_MDEV (DO_PEARSE_CODES) then
               Pearse_Adjustment := 3; -- used to shift by three columns Pearse code length+space)
               Pearse_Code_Adjust_For_Meanings(Output, Alternative => 1);
            end if;

            if IR.AGE >= F then
               Put (OUTPUT, To_String (Roman_Num_Record.Subtractive));
            else
               Put (OUTPUT, To_String (Roman_Num_Record.Additive));
            end if;

            Text_IO.Set_Col (OUTPUT, 22 + Pearse_Adjustment);

            QUALITY_RECORD_IO.PUT (Inflect_Padding, IR.QUAL);

            Put (OUTPUT, Inflect_Padding);

            LIST_PACKAGE.PUT_INFLECTION_FLAGS
              (Output => OUTPUT, SR => (STEM => NULL_STEM_TYPE, IR => IR));

            New_Line (OUTPUT);

            if Is_Negative and WORDS_MODE (DO_EXAMPLES) then
               Format (OUTPUT, FAINT);
               Put (OUTPUT, "     ~ negativum");
               Format (OUTPUT, RESET);
               New_Line (OUTPUT);
            end if;

            LIST_PACKAGE.PUT_DICTIONARY_FORM
              (OUTPUT => OUTPUT, D_K => RRR, MNPC => NULL_MNPC, DE => DE);

            -- meaning line:
            Put_Pearse_Code(OUTPUT,3);

            Format (OUTPUT, BOLD);
            if Is_Negative then
               Put ('-');
            end if;

            for C in Arabic_String'Range loop
               exit when Arabic_String (C) = ' ';
               Put (OUTPUT, Arabic_String (C));
            end loop;

           Put (OUTPUT, " as a ROMAN NUMERAL;");

           if Is_Negative then
             Put(" [~ negativum];");
           end if;
          Format (OUTPUT, RESET);
          New_Line(Output);
        -- end output of first result


 -- SECOND RESULT

            if Put_Additive
               -- the only time we will ever output again is if we put additive
               -- above
               then
               DE.TRAN.SOURCE := X;
               DE.TRAN.AGE    := X;
               DE.TRAN.FREQ   := A;
               IR.FREQ        := X;
               IR.AGE         := X;

               Pearse_Code_Adjust_For_Meanings(Output, Alternative => 1);

               Put (OUTPUT, To_String (Roman_Num_Record.Subtractive));
               Text_IO.Set_Col (OUTPUT, 22 + Pearse_Adjustment);

               Put (OUTPUT, Inflect_Padding);
               LIST_PACKAGE.PUT_INFLECTION_FLAGS
                 (Output => OUTPUT, SR => (STEM => NULL_STEM_TYPE, IR => IR));

               Format (OUTPUT, RESET);
               New_Line (OUTPUT);

               LIST_PACKAGE.PUT_DICTIONARY_FORM
                 (OUTPUT => OUTPUT, D_K => RRR, MNPC => NULL_MNPC, DE => DE);

               -- meaning line:
               Put_Pearse_Code(OUTPUT,3);
         
               Format (OUTPUT, BOLD);
               for C in Arabic_String'Range loop
                  exit when Arabic_String (C) = ' ';
                  Put (OUTPUT, Arabic_String (C));
               end loop;

               Put_Line (OUTPUT, " as a ROMAN NUMERAL;");
			   Format (OUTPUT, RESET);
               New_Line (OUTPUT);

             else
             New_Line (OUTPUT); -- we put the subtractive, then ended; so skip line
            end if; -- Put_Additive
            -- end of second output

         end if; -- enclosing statements requiring integer

      end loop;  -- end outermost loop

      return;

   end Arabic2Roman;

   function Generate_Additive (Arabic_Num : in Integer) return Unbounded_String
   is

      built_string   : Unbounded_String;
      Counter        : Integer         := 1;
      Frame          : Unbounded_String;
      Arabic_String2 : constant String := Integer'Image (Arabic_Num);
   begin

      for I in reverse 2 .. (Arabic_String2'Length)
      loop  --  build from lowest Arabic digit, moving left (position 1 is blank to indicate positive)
         case Arabic_String2 (I) is
            when '1' =>
               built_string := Roman_Nums_CLASSICAL (Counter) & built_string;
            when '2' =>
               built_string :=
                 (2 * Roman_Nums_CLASSICAL (Counter)) & built_string;
            when '3' =>
               built_string :=
                 (3 * Roman_Nums_CLASSICAL (Counter)) & built_string;
            when '4' =>
               built_string :=
                 (4 * Roman_Nums_CLASSICAL (Counter)) & built_string;
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
                 (2 * Roman_Nums_CLASSICAL (Counter)) & built_string;
            when '8' =>
               built_string :=
                 Roman_Nums_CLASSICAL (Counter + 1) &
                 (3 * Roman_Nums_CLASSICAL (Counter)) & built_string;
            when '9' =>
               built_string :=
                 Roman_Nums_CLASSICAL (Counter + 1) &
                 (4 * Roman_Nums_CLASSICAL (Counter)) & built_string;
            when others =>
               null;
         end case;
         Counter :=
           Counter +
           2;           -- Move two positions down the Roman numeral array each time
         exit when Counter >
           11;          -- Can't hit this unless code modified to allow > 999_999_999
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

      -- Rules get complex starting at about 4_000, so no fancy stuff here;
      -- keep it understandable
   begin

      case (Arabic_Num / 10_000) mod 10 is
         when 1 =>
            Append(Built_String, "((I))");
         when 2 =>
            Append(Built_String, "((II))");
         when 3 =>
            Append(Built_String, "((III))");
         when 4 =>
            Append(Built_String, "((IV))");
         when 5 =>
            Append(Built_String, "((V))");
         when 6 =>
            Append(Built_String, "((VI))");
         when 7 =>
            Append(Built_String, "((VII))");
         when 8 =>
            Append(Built_String, "((VIII))");
         when 9 =>
            Append(Built_String, "((IX))");
         when others =>
            null;
      end case;

      case (Arabic_Num / 1_000) mod 10 is
         when 1 =>
            Append(Built_String, "M");
         when 2 =>
            Append(Built_String, "MM");
         when 3 =>
            Append(Built_String, "MMM");
         when 4 =>
            Append(Built_String, "M(V)");
         when 5 =>
            Append(Built_String, "(V)");
         when 6 =>
            Append(Built_String, "(VI)");
         when 7 =>
            Append(Built_String, "(VII)");
         when 8 =>
            Append(Built_String, "(VIII)");
         when 9 =>
            Append(Built_String, "(IX)");
         when others =>
            null;
      end case;

      case (Arabic_Num / 100) mod 10 is
         when 1 =>
            Append(Built_String, "C");
         when 2 =>
            Append(Built_String, "CC");
         when 3 =>
            Append(Built_String, "CCC");
         when 4 =>
            Append(Built_String, "CD");
         when 5 =>
            Append(Built_String, "D");
         when 6 =>
            Append(Built_String, "DC");
         when 7 =>
            Append(Built_String, "DCC");
         when 8 =>
            Append(Built_String, "DCCC");
         when 9 =>
            Append(Built_String, "CM");
         when others =>
            null;
      end case;

      case (Arabic_Num / 10) mod 10 is
         when 1 =>
            Append(Built_String, "X");
         when 2 =>
            Append(Built_String, "XX");
         when 3 =>
            Append(Built_String, "XXX");
         when 4 =>
            Append(Built_String, "XL");
         when 5 =>
            Append(Built_String, "L");
         when 6 =>
            Append(Built_String, "LX");
         when 7 =>
            Append(Built_String, "LXX");
         when 8 =>
            Append(Built_String, "LXXX");
         when 9 =>
            Append(Built_String, "XC");
         when others =>
            null;
      end case;

      case Arabic_Num mod 10 is
         when 1 =>
            Append(Built_String, "I");
         when 2 =>
            Append(Built_String, "II");
         when 3 =>
            Append(Built_String, "III");
         when 4 =>
            Append(Built_String, "IV");
         when 5 =>
            Append(Built_String, "V");
         when 6 =>
            Append(Built_String, "VI");
         when 7 =>
            Append(Built_String, "VII");
         when 8 =>
            Append(Built_String, "VIII");
         when 9 =>
            Append(Built_String, "IX");
         when others =>
            null;
      end case;
      return (Built_String);

   end Generate_Subtractive;

  procedure Pearse_Code_Adjust_For_Meanings (Output      : in Text_IO.File_Type;
                                             Alternative : in Pearse_Code_Type) is
   begin
     if WORDS_MDEV (DO_PEARSE_CODES) then
       if WORDS_MODE (DO_ONLY_MEANINGS) = False
         and then (not (CONFIGURATION = ONLY_MEANINGS))
       then
         Put(OUTPUT,Pearse_Code_Array(Alternative));
       else
         Put(OUTPUT,Pearse_Code_Array(3)); -- Print as the meaning if only showing meanings;
       end if;                             -- disabling Arabic2Roman is the equivalent here
     end if;
   end Pearse_Code_Adjust_For_Meanings;

   function Integer_Test (Arabic_String : in String) return Boolean is
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
