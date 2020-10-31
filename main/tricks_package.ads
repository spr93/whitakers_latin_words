with DICTIONARY_PACKAGE; use DICTIONARY_PACKAGE;
package TRICKS_PACKAGE is

  procedure SYNCOPE(W : in STRING;
                    PA : in out PARSE_ARRAY; PA_LAST : in out INTEGER);           

  procedure TRY_TRICKS(W : in STRING; 
                       PA : in out PARSE_ARRAY; PA_LAST : in out INTEGER;   
                       LINE_NUMBER : in INTEGER; WORD_NUMBER : in INTEGER);

  procedure TRY_SLURY(W : in STRING;
                      PA : in out PARSE_ARRAY; PA_LAST : in out INTEGER;
                      LINE_NUMBER : in INTEGER; WORD_NUMBER : in INTEGER);   

  procedure ROMAN_NUMERALS(INPUT_WORD : in STRING;
                           PA : in out PARSE_ARRAY; PA_LAST : in out INTEGER);
  
end TRICKS_PACKAGE;
