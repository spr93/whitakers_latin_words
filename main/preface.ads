with Text_IO;
with STRINGS_PACKAGE;

package PREFACE is

  procedure PUT (S : in String);
  procedure SET_COL (PC : in Text_IO.Positive_Count);
  procedure PUT_LINE (S : in String);
  procedure NEW_LINE (SPACING : in Text_IO.Positive_Count := 1);
  procedure PUT (N : in Integer; WIDTH : in Text_IO.Field := Integer'Width);
  procedure Format (Format : in STRINGS_PACKAGE.Format_Command);
end PREFACE;
