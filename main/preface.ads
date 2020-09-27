with TEXT_IO;
package PREFACE is
  procedure PUT(S: in STRING);
  procedure SET_COL(PC : in TEXT_IO.POSITIVE_COUNT);
  procedure PUT_LINE(S : in STRING);
  procedure NEW_LINE(SPACING  : in TEXT_IO.POSITIVE_COUNT := 1);
  procedure PUT(N : in INTEGER; WIDTH : in TEXT_IO.FIELD := INTEGER'WIDTH);
end PREFACE;

