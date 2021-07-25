with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;

package TRICKS_PACKAGE is

   procedure SYNCOPE
     (W : in String; PA : in out PARSE_ARRAY; PA_LAST : in out Integer);

   procedure TRY_TRICKS
     (W : in String; PA : in out PARSE_ARRAY; PA_LAST : in out Integer);

   procedure TRY_SLURY
     (W : in String; PA : in out PARSE_ARRAY; PA_LAST : in out Integer);

   procedure ROMAN_NUMERALS
     (INPUT_WORD : in     String; PA : in out PARSE_ARRAY;
      PA_LAST    : in out Integer);

end TRICKS_PACKAGE;
