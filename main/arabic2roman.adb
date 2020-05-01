with Ada.Text_IO; use ADA.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Config; use Config;
with WORD_PARAMETERS; use WORD_PARAMETERS;
with DEVELOPER_PARAMETERS; use DEVELOPER_PARAMETERS;

package body Arabic2Roman is

   procedure Arabic2Roman (INPUT_WORD : in out String; Arabic_Process_All : in Boolean) is

   type Roman_Record_Type Is
      record
      Age_X : Unbounded_String := To_Unbounded_String("");
      Age_F : Unbounded_String := To_Unbounded_String("");
      Bar_Reminder : Boolean := False;
   end record;

   Roman_Num_Record :  Roman_Record_Type;

      Print_Medieval : Boolean := False;
      Arabic_String : String(1..11) := (others => ' ');
      Arabic_Build_Counter : Integer := 1;

      Input_Counter : Integer := Input_Word'First;

      Negative : Boolean;

   begin

   for I in INPUT_WORD'range loop --outmost loop ensures we catch input where there are two numbers in row
         Arabic_Build_Counter := 1;
         Negative := False;
         Arabic_String := (others => ' ' ); -- clear the string to build it again if we need to
         Print_Medieval := True;
         if Input_Counter > INPUT_WORD'Last then return; end if;

         if INPUT_WORD(Input_Counter) = '-' then
                if Input_Counter = INPUT_WORD'First then Negative := True;
                elsif Arabic_Process_All = False then return;
                end if;
               if Input_Counter < INPUT_WORD'Last then Input_Counter := Input_Counter + 1; end if; -- advance one space if we wont go out of bounds
         end if;


         for J in Input_Counter..INPUT_WORD'Last loop
            if Arabic_Build_Counter > 11 then   -- too big; fast forward to the next number
               for Z in Input_Counter..INPUT_WORD'Last loop
                  if INPUT_WORD(Input_Counter) = '|' then exit;
                  end if;
                 Input_Counter := Input_Counter + 1;
               end loop;
              exit;
            end if;

            case INPUT_WORD(Input_Counter) is  -- PARSE procedure leaves us with S (start of line), | (blank/space/delimiter), z (letter), #, or - (potential negative)
            when '0'..'9' => Arabic_String(Arabic_Build_Counter) := INPUT_WORD(Input_counter); Arabic_Build_Counter := Arabic_Build_Counter+1; Input_Counter := Input_Counter+1;
            when '_' => Input_Counter := Input_Counter + 1;       -- this was a valid 000's delimiter (, or _) so ignore it
            when 'z' => Input_Counter := Input_Counter + 1;       -- there was a word or number here, so ignore it
            when '|' => Input_Counter := Input_Counter + 1; exit; -- a word or new number follows.
            when '.' => if (INPUT_WORD(Input_Counter+1) in '0'..'9') then return; else Input_Counter := Input_Counter + 1; end if;
            when '-' =>  -- "TRICK":  Accept negative numbers, convert to positive, and note that it's a neologism.   if Negative = True then
                        if WORDS_MODE(DO_TRICKS) then exit; end if;
            when others => Return;  -- we shouldn't see any other character.  if we do, something went wrong, so give up on converting to Roman numerals.
            end Case;


         end loop;


   if (Integer_Test(Arabic_String) = True) then    -- if enclosing the rest of the procedure
    Arabic_Num := Integer'Value(Arabic_String);  -- rest of procedure requires integer

         if Negative = True then
                      if WORDS_MDEV(DO_PEARSE_CODES) then
                      PUT("06");
                     end if;
            Put_Line("NB:  Negative numbers are a neologism.");
            end if;



     if WORDS_MDEV(DO_PEARSE_CODES) then
         PUT( "06 ");
      end if;

      Put(Arabic_String);
      SET_COL(22);
      Put("ARABIC NUM");
      New_Line;

            --quickly test and reject zeros
         if Arabic_num = 0 then
               if WORDS_MDEV(DO_PEARSE_CODES) then
               PUT( "03 "); end if;
               Put_Line("nihil");
               New_Line;
               return; -- SPR:  is nulla better?  noun form of nulla isn't in the dictionary - is that right?
         end if;

         -- Do medieval form

         case Arabic_Num is
            when 1..99_999 => Roman_Num_Record.Age_F := Generate_Subtractive(Arabic_Num);
            when 100_000..999_999_999 =>
                   Roman_Num_Record.Age_F := "|" & Generate_Subtractive((Arabic_Num / 100_000) mod 100_000) & "|"
                                              & Generate_Subtractive(Arabic_Num mod 10_000);
               Roman_Num_Record.Bar_Reminder := True;
            when others => null;
         end case;



         -- Is the number low enough to do a non-medieval form?
         if Arabic_Num <= 100_000 then Roman_Num_Record.Age_X := Generate_Additive(Arabic_Num);

            if Roman_Num_Record.Age_X = Roman_Num_Record.Age_F then Print_Medieval := False;
               end if;

            if WORDS_MDEV(DO_PEARSE_CODES) then
               PUT("03");
               end if;

            Put_Line(To_String(Roman_Num_Record.Age_X) & "   NUM CARD    [XXAQC]");

         end if;


         if Print_Medieval = True then
                     if WORDS_MDEV(DO_PEARSE_CODES) then
                        PUT( "03 ");
                     end if;

            Put(To_String(Roman_Num_Record.Age_F));

            case Roman_Num_Record.Bar_Reminder is
                     when False =>  Put_line("   NUM CARD    [XXFQA]");
                     when True =>   Put("   NUM CARD    [XXFQC]"); Put_Line (""); Put_Line("     write bar across top of the numerals in the bars | | (three sided box, open bottom)");
            end Case;

         end if;


       Put_Line("");
       end if;  -- end if enclosing statements requiring integer


  end loop; -- end outermost loop

 end Arabic2Roman;

function Generate_Additive (Arabic_Num : in Integer) return Unbounded_String is

      built_string : Unbounded_String := To_Unbounded_String("");
      Counter : Integer;
      Frame : Unbounded_String := To_Unbounded_String("");
      Arabic_String2 : String := Integer'Image(Arabic_Num);
   begin

      Counter := 1;

      for I in Reverse 2..(Arabic_String2'Length) loop  --  build from lowest Arabic digit, moving left (position 1 is blank to indicate positive)
         case Arabic_String2(I) is

           when '0' => null;
           when '1' =>  Built_String := Roman_Nums_CLASSICAL(COUNTER) & Built_String;
           when '2' =>  Built_String := Roman_Nums_CLASSICAL(COUNTER) & Roman_Nums_CLASSICAL(COUNTER) &  Built_String;
           when '3' =>  Built_String := Roman_Nums_CLASSICAL(COUNTER) & Roman_Nums_CLASSICAL(COUNTER) & Roman_Nums_CLASSICAL(COUNTER) &  Built_String;
           when '4' =>  Built_String := Roman_Nums_CLASSICAL(COUNTER) & Roman_Nums_CLASSICAL(COUNTER) & Roman_Nums_CLASSICAL(COUNTER) & Roman_Nums_CLASSICAL(COUNTER) &  Built_String;
           when '5' =>  Built_String := Roman_Nums_CLASSICAL(Counter+1) &  Built_String;
           when '6' =>  Built_String := Roman_Nums_CLASSICAL(COUNTER+1) & Roman_Nums_CLASSICAL(COUNTER) &  Built_String;
           when '7' =>  Built_String := Roman_Nums_CLASSICAL(COUNTER+1) & Roman_Nums_CLASSICAL(COUNTER) & Roman_Nums_CLASSICAL(COUNTER) &  Built_String;
           when '8' =>  Built_String := Roman_Nums_CLASSICAL(COUNTER+1) & Roman_Nums_CLASSICAL(COUNTER) & Roman_Nums_CLASSICAL(COUNTER) & Roman_Nums_CLASSICAL(COUNTER) &  Built_String;
           when '9' =>  Built_String := Roman_Nums_CLASSICAL(COUNTER+1) & Roman_Nums_CLASSICAL(COUNTER) & Roman_Nums_CLASSICAL(COUNTER) & Roman_Nums_CLASSICAL(COUNTER) & Roman_Nums_CLASSICAL(COUNTER) &  Built_String;
           when others => null;
         end case;
         Counter := Counter+2;  -- Move two positions down the Roman numeral array each time
              if Counter > 11  then exit;
              end if;  -- shouldnt ever hit this, but just in case
      end loop;

      if Arabic_String2'Length >= 8 then Built_String := Frame & Built_String; Counter := 0; end if;
  return(Built_string);

  end Generate_Additive;


function Generate_Subtractive(Arabic_Num : in  Integer) return Unbounded_String is

      Built_String : Unbounded_String := To_Unbounded_String("");

   -- Rules get complex, especially starting at 4_000, so not using an array for readability and debugging purposes
begin

      case (Arabic_Num / 10_000) mod 10 Is
           when 0 => null;
           when 1 =>  Built_String := Built_String & To_Unbounded_String("((I))");
           when 2 => Built_String := Built_String & To_Unbounded_String("((II))");
           when 3 => Built_String := Built_String & To_Unbounded_String("((III))");
           when 4 => Built_String := Built_String & To_Unbounded_String("((IV))");
           when 5 => Built_String := Built_String & To_Unbounded_String("((V))");
           when 6 => Built_String := Built_String & To_Unbounded_String("((VI))");
           when 7 => Built_String := Built_String & To_Unbounded_String("((VII))");
           when 8 => Built_String := Built_String & To_Unbounded_String("((VIII))");
           When 9 => Built_String := Built_String & To_Unbounded_String("((IX))");
           when others => null;
      end Case;

      case (Arabic_Num / 1_000) mod 10 Is
           when 0 => null;
           when 1 =>  Built_String := Built_String & To_Unbounded_String("M");
           when 2 =>  Built_String := Built_String & To_Unbounded_String("MM");
           when 3 =>  Built_String := Built_String & To_Unbounded_String("MMM");
           when 4 =>  Built_String := Built_String & To_Unbounded_String("M(V)");
           when 5 =>  Built_String := Built_String & To_Unbounded_String("(V)");
           when 6 =>  Built_String := Built_String & To_Unbounded_String("(VI)");
           when 7 =>  Built_String := Built_String & To_Unbounded_String("(VII)");
           when 8 =>  Built_String := Built_String & To_Unbounded_String("(VIII)");
           when 9 =>  Built_String := Built_String & To_Unbounded_String("(IX)");
           when others => null;
      end Case;

      case (Arabic_Num / 100) mod 10  Is
           when 0 => null;
           when 1 =>  Built_String := Built_String & To_Unbounded_String("C");
           when 2 =>  Built_String := Built_String & To_Unbounded_String("CC");
           when 3 =>  Built_String := Built_String & To_Unbounded_String("CCC");
           when 4 =>  Built_String := Built_String & To_Unbounded_String("CD");
           when 5 => Built_String := Built_String & To_Unbounded_String("D");
           when 6 => Built_String := Built_String & To_Unbounded_String("DC");
           when 7 => Built_String := Built_String & To_Unbounded_String("DCC");
           when 8 => Built_String := Built_String & To_Unbounded_String("DCCC");
           when 9 => Built_String := Built_String & To_Unbounded_String("CM");
          when others => null;
      end Case;

      -- Handle tens

           case (Arabic_Num / 10) mod 10 Is
           when 0 => null;
           when 1 =>  Built_String := Built_String & To_Unbounded_String("X");
           when 2 =>  Built_String := Built_String & To_Unbounded_String("XX");
           when 3 =>  Built_String := Built_String & To_Unbounded_String("XXX");
           when 4 =>  Built_String := Built_String & To_Unbounded_String("XL");
           when 5 => Built_String := Built_String & To_Unbounded_String("L");
           when 6 => Built_String := Built_String & To_Unbounded_String("LX");
           when 7 => Built_String := Built_String & To_Unbounded_String("LXX");
           when 8 => Built_String := Built_String & To_Unbounded_String("LXXX");
           when 9 =>  Built_String := Built_String & To_Unbounded_String("XC");
           when others => Null;
     end Case;

      -- handle final digit
           case Arabic_Num Mod 10  is
      when 0 =>  null;
           when 1 => Built_String := Built_String & To_Unbounded_String("I");
           when 2 => Built_String := Built_String & To_Unbounded_String("II");
           when 3 => Built_String := Built_String & To_Unbounded_String("III");
           when 4 => Built_String := Built_String & To_Unbounded_String("IV");
           when 5 => Built_String := Built_String & To_Unbounded_String("V");
           when 6 => Built_String := Built_String & To_Unbounded_String("VI");
           when 7 => Built_String := Built_String & To_Unbounded_String("VII");
           when 8 => Built_String := Built_String & To_Unbounded_String("VIII");
           When 9 => Built_String := Built_String & To_Unbounded_String("IX");
           when others => Null;
     end case;
  return (Built_String);


end Generate_Subtractive;

function Integer_Test (Arabic_String : in String) return Boolean is
 -- Checks that PARSE and Arabic2Roman have left us with valid input
 -- and lets us handle exceptions silently so we don't interrupt PARSE
 -- if somehow nonsense characters have made it to this point
  type Valid_Integer is new Integer range -999_999_999..999_999_999;
  Tester : Valid_integer;

begin
      Tester := Valid_Integer'Value(Arabic_String);
      return True;
      exception
          when others =>
         return False;
end Integer_Test;


end Arabic2Roman; -- end of package
