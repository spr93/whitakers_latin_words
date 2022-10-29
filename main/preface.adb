with CONFIG;
with WORD_PARAMETERS;

package body PREFACE is

  procedure PUT (S : in String) is
  begin
    if not CONFIG.SUPPRESS_PREFACE then
      Text_IO.Put (Text_IO.Current_Output, S);
    end if;
  end PUT;

  procedure SET_COL (PC : in Text_IO.Positive_Count) is
  begin
    if not CONFIG.SUPPRESS_PREFACE then
      Text_IO.Set_Col (Text_IO.Current_Output, PC);
    end if;
  end SET_COL;

  procedure PUT_LINE (S : in String) is
  begin
    if not CONFIG.SUPPRESS_PREFACE then
      Text_IO.Put_Line (Text_IO.Current_Output, S);
    end if;
  end PUT_LINE;

  procedure NEW_LINE (SPACING : in Text_IO.Positive_Count := 1) is
  begin
    if not CONFIG.SUPPRESS_PREFACE then
      Text_IO.New_Line (Text_IO.Current_Output, SPACING);
    end if;
  end NEW_LINE;

  procedure PUT (N : in Integer; WIDTH : in Text_IO.Field := Integer'Width) is
    package INTEGER_IO is new Text_IO.Integer_IO (Integer);
  begin
    if not CONFIG.SUPPRESS_PREFACE then
      INTEGER_IO.Put (Text_IO.Current_Output, N, WIDTH);
    end if;
  end PUT;

  procedure Format (Format : in Strings_Package.Format_Command) is
    use STRINGS_PACKAGE;
  begin
    if WORD_PARAMETERS.WORDS_MODE(WORD_PARAMETERS.DO_ANSI_FORMATTING)
      and not Config.SUPPRESS_PREFACE
    -- Format procedure in STRINGS_PACKAGE tests for Write_Output_To_File, but preface messages
    -- are never written to file, so we don't want that option to suppress ANSI formatting
    then
      Text_IO.Put(Text_IO.Current_Output, Format_Reset);
         case Format is
            when UNDERLINE => Text_IO.Put(Text_IO.Current_Output, Format_Underline);
            when INVERSE   => Text_IO.Put(Text_IO.Current_Output, Format_Inverse);
            when BOLD      => Text_IO.Put(Text_IO.Current_Output, Format_Bold);
            when others    => null;
         end case;

    end if;
  end Format;

end PREFACE;
