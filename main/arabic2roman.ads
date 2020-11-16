with Ada.Text_IO;           use ADA.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

-- Forms and age codes guided by sources incl.
-- Wolfram Mathworld at http://mathworld.wolfram.com/RomanNumerals.html
-- and Christomalis, S., "Trends and Traditions in the History of Written Numerals" in The Shape of Script: How and Why Writing Systems Change (2012)
-- and Hunt, L. N.H., et al., "The Historical Roots of Elementary Mathematics" (1988)
-- and _especially_ the clear discussions in Menninger, K., "Number Words and Number Symbols: A Cultural History of Numbers" (Eng. tr. 1992).


package Arabic2Roman is

  procedure Arabic2Roman (OUTPUT : in Ada.Text_IO.File_Type ; INPUT_WORD : in String);


private
  Roman_Nums_CLASSICAL : constant array (1..11) of Unbounded_String  := (
                          1         => (To_Unbounded_String("I")),
                          2         => (To_Unbounded_String("V")),
                          3         => (To_Unbounded_String("X")),
                          4         => (To_Unbounded_String("L")),
                          5         => (To_Unbounded_String("C")),
                          6         => (To_Unbounded_String("|)")),
                          7         => (To_Unbounded_String("(|)")),
                          8         => (To_Unbounded_String("|))")),
                          9         => (To_Unbounded_String("((|))")),
                         10         => (To_Unbounded_String("|)))")),
                         11         => (To_Unbounded_String("(((|)))"))); -- 100_000
   -- Stop at 100_000 for classical period.  See also Pliny.

   Arabic_Num : Natural range 0..999_999_999;

   function Generate_Additive (Arabic_Num : in Integer) return Unbounded_String;
   function Generate_Subtractive (Arabic_Num : in Integer) return Unbounded_String;
   function Integer_Test (Arabic_String: in String) return Boolean;

   pragma Inline_Always(Generate_Additive);
   pragma Inline_Always(Generate_Subtractive);
   pragma Inline_Always(Integer_Test);

end  Arabic2Roman;
