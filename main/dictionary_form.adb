with Strings_Package;     use Strings_Package;
with Inflections_Package; use Inflections_Package;
with Dictionary_Package;  use Dictionary_Package;
with TEXT_IO;


function Dictionary_Form(DE      : in     Dictionary_Entry) return String is

  Null_OX : constant String(1 .. 24) := (others => ' ');
  OX      : array (1 .. 4) of String (1 .. 24) := (others => Null_OX);
  Form    : String(1 .. 100) := (others => ' ');

  Fst     : array (Which_Type range 1 .. 5) of String(1 .. 3) :=
  ("1st", "2nd", "3rd", "4th", "5th");

  Not_Found: exception;

  function Add(Stem, Infl    : in     String) return String is
  begin
    return Head(Trim(Stem) & Trim(Infl), 24);
  end Add;

  procedure Add_Up(Factor  :        String) is
  begin
    Form := Head(Trim(Form) & Trim(Factor), 100);
  end Add_Up;

  procedure Add_to(Factor  :        String) is
  begin
    Form := Head(Trim(Form) & Factor, 100);
  end Add_to;

begin
  --DICTIONARY_ENTRY_IO.PUT(DE);
  --  So I can call with a NULL_DICTIONARY_ENTRY and not bomb
  if DE = Null_Dictionary_Entry then
    return "";
  end if;

  if (DE.Part.POFS = Prep) then
    return Trim(DE.Stems(1)) & "  " & Part_of_Speech_Type'Image(DE.Part.POFS) &
    "  " & Case_Type'Image(DE.Part.Prep.Obj);
  end if;
   
  if DE.Stems(2) = Null_Stem_Type and
  DE.Stems(3) = Null_Stem_Type and
  DE.Stems(4) = Null_Stem_Type and not
  (((DE.Part.POFS = N) and then (DE.Part.N.DEcl.Which = 9)) or
  ((DE.Part.POFS = Adj) and then
  ((DE.Part.Adj.DEcl.Which = 9) or
  (DE.Part.Adj.Co in Comp | Super))) or
  ((DE.Part.POFS = V) and then (DE.Part.V.Con = (9, 8))) or
  ((DE.Part.POFS = V) and then (DE.Part.V.Con = (9, 9))))
  then
      return Trim(DE.Stems(1)) & "  " & Part_of_Speech_Type'Image(DE.Part.POFS);
    --  For UNIQUES, CONJ, INTERJ, ...
  end if;

  if DE.Part.POFS = N then
    if DE.Part.N.DEcl.Which = 1 then
      if DE.Part.N.Decl.Var = 1 then
        OX(1) := Add(DE.Stems(1), "a");
        OX(2) := Add(DE.Stems(2), "ae");
      elsif DE.Part.N.Decl.Var = 6 then
        OX(1) := Add(DE.Stems(1), "e");
        OX(2) := Add(DE.Stems(2), "es");
      elsif DE.Part.N.Decl.Var = 7 then
        OX(1) := Add(DE.Stems(1), "es");
        OX(2) := Add(DE.Stems(2), "ae");
      elsif DE.Part.N.Decl.Var = 8 then
        OX(1) := Add(DE.Stems(1), "as");
        OX(2) := Add(DE.Stems(2), "ae");
      end if;

    elsif DE.Part.N.DEcl.Which = 2 then
      if DE.Part.N.Decl.Var = 1 then
        OX(1) := Add(DE.Stems(1), "us");
        OX(2) := Add(DE.Stems(2), "i");
      elsif DE.Part.N.Decl.Var = 2 then
        OX(1) := Add(DE.Stems(1), "um");
        OX(2) := Add(DE.Stems(2), "i");
      elsif DE.Part.N.Decl.Var = 3 then
        OX(1) := Add(DE.Stems(1), "");
        OX(2) := Add(DE.Stems(2), "i");
      elsif DE.Part.N.Decl.Var = 4 then
        if DE.Part.N.Gender = N then
          OX(1) := Add(DE.Stems(1), "um");
        else
          OX(1) := Add(DE.Stems(1), "us");
        end if;
        OX(2) := Add(DE.Stems(2), "(i)");
      elsif DE.Part.N.Decl.Var = 5 then
        OX(1) := Add(DE.Stems(1), "us");
        OX(2) := Add(DE.Stems(2), "");
      elsif DE.Part.N.Decl.Var = 6 then
        OX(1) := Add(DE.Stems(1), "os");
        OX(2) := Add(DE.Stems(2), "i");
      elsif DE.Part.N.Decl.Var = 7 then
        OX(1) := Add(DE.Stems(1), "");
        OX(2) := Add(DE.Stems(2), "yos/i");
      elsif DE.Part.N.Decl.Var = 8 then
        OX(1) := Add(DE.Stems(1), "on");
        OX(2) := Add(DE.Stems(2), "i");
      elsif DE.Part.N.Decl.Var = 9 then
        OX(1) := Add(DE.Stems(1), "us");
        OX(2) := Add(DE.Stems(2), "i");
      end if;

    elsif DE.Part.N.DEcl.Which = 3 then
      OX(1) := Add(DE.Stems(1), "");
      if DE.Part.N.Decl.Var in 7 | 9 then
        OX(2) := Add(DE.Stems(2), "os/is");
      else
        OX(2) := Add(DE.Stems(2), "is");
      end if;

    elsif DE.Part.N.DEcl.Which = 4 then
      if DE.Part.N.Decl.Var = 1 then
        OX(1) := Add(DE.Stems(1), "us");
        OX(2) := Add(DE.Stems(2), "us");
      elsif DE.Part.N.Decl.Var = 2 then
        OX(1) := Add(DE.Stems(1), "u");
        OX(2) := Add(DE.Stems(2), "us");
      elsif DE.Part.N.Decl.Var = 3 then
        OX(1) := Add(DE.Stems(1), "us");
        OX(2) := Add(DE.Stems(2), "u");
      elsif DE.Part.N.Decl.Var = 4 then
        OX(1) := Add(DE.Stems(1), "");
        OX(2) := Add(DE.Stems(2), "u");
      end if;

    elsif DE.Part.N.DEcl.Which = 5 then
      OX(1) := Add(DE.Stems(1), "es");
      OX(2) := Add(DE.Stems(2), "ei");

    elsif DE.Part.N.DEcl = (9, 8) then
      OX(1) := Add(DE.Stems(1), ".");
      OX(2) := Add(Null_OX, "abb.");

    elsif DE.Part.N.DEcl = (9, 9) then
      OX(1) := Add(DE.Stems(1), "");
      OX(2) := Add(Null_OX, "undeclined");

    else
      raise Not_Found;
    end if; --  N

  elsif DE.Part.POFS = PRON then
         
    if DE.Part.Pron.DEcl.Which = 1 then
            
      if Trim(De.Stems(1)) = "qu"  or Trim(De.Stems(1)) = "aliqu" then 
        OX(1) := Add(DE.Stems(1), "i");
        OX(2) := Add(DE.Stems(1), "ae");
        OX(3) := Add(DE.Stems(1), "od");    
      else   
      raise Not_Found;
      end if; 
         
    elsif DE.Part.Pron.DEcl.Which = 3 then
      OX(1) := Add(DE.Stems(1), "ic");
      OX(2) := Add(DE.Stems(1), "aec");
      if DE.Part.Pron.Decl.Var = 1 then
        OX(3) := Add(DE.Stems(1), "oc");
      elsif DE.Part.Pron.Decl.Var = 2 then
        OX(3) := Add(DE.Stems(1), "uc");
      end if;

    elsif DE.Part.Pron.DEcl.Which = 4 then
      if DE.Part.Pron.Decl.Var = 1 then
        OX(1) := Add(DE.Stems(1), "s");
        OX(2) := Add(DE.Stems(2), "a");
        OX(3) := Add(DE.Stems(1), "d");
      elsif DE.Part.Pron.Decl.Var = 2 then
        OX(1) := Add(DE.Stems(1), "dem");
        OX(2) := Add(DE.Stems(2), "adem");
        OX(3) := Add(DE.Stems(1), "dem");
      end if;

    elsif DE.Part.Pron.DEcl.Which = 5 then
      if DE.Part.Pron.Decl.Var = 1 then   
        OX(1) := Add(DE.Stems(1), "");    
        OX(2) := Add(DE.Stems(2), "ei");
      elsif DE.Part.Pron.Decl.Var = 2 then
        OX(1) := Add(DE.Stems(1), "");
        OX(2) := Add(DE.Stems(2), "ui");
      elsif DE.Part.Pron.Decl.Var = 3 then
        OX(1) := Add(DE.Stems(1), "os");
        OX(2) := Add(DE.Stems(2), "um (-i)");     
      end if;

    elsif DE.Part.Pron.DEcl.Which = 6 then
      OX(1) := Add(DE.Stems(1), "e");
      OX(2) := Add(DE.Stems(1), "a");
      if DE.Part.Pron.Decl.Var = 1 then
        OX(3) := Add(DE.Stems(1), "ud");
      elsif DE.Part.Pron.Decl.Var = 2 then
        OX(3) := Add(DE.Stems(1), "um");
      end if;

    elsif DE.Part.Adj.DEcl = (9, 8) then
      OX(1) := Add(DE.Stems(1), ".");
      OX(2) := Add(Null_OX, "abb.");

    elsif DE.Part.Pron.DEcl = (9, 9) then
      OX(1) := Add(DE.Stems(1), "");
      OX(2) := Add(Null_OX, "undeclined");
    
    else
      raise Not_Found;
    end if; --  PRON

  elsif DE.Part.POFS = Adj then

    if DE.Part.Adj.Co = Comp then
      if DE.Part.Adj.DEcl.Which = 5 then
        OX(1) := Add(DE.Stems(1), "us");
        OX(2) := Add(DE.Stems(1), "a");
        OX(3) := Add(DE.Stems(1), "um");

      else
        OX(1) := Add(DE.Stems(1), "or");
        OX(2) := Add(DE.Stems(1), "or");
        OX(3) := Add(DE.Stems(1), "us");
      end if;

    elsif DE.Part.Adj.Co = Super then
      OX(1) := Add(DE.Stems(1), "mus");
      OX(2) := Add(DE.Stems(1), "ma");
      OX(3) := Add(DE.Stems(1), "mum");

    elsif DE.Part.Adj.Co = Pos then
      if DE.Part.Adj.DEcl.Which = 1 then
        if DE.Part.Adj.Decl.Var = 1 then
          OX(1) := Add(DE.Stems(1), "us");
          OX(2) := Add(DE.Stems(2), "a");
          OX(3) := Add(DE.Stems(2), "um");

        elsif DE.Part.Adj.Decl.Var = 2 then
          OX(1) := Add(DE.Stems(1), "");
          OX(2) := Add(DE.Stems(2), "a");
          OX(3) := Add(DE.Stems(2), "um");

        elsif DE.Part.Adj.Decl.Var = 3 then
          OX(1) := Add(DE.Stems(1), "us");
          OX(2) := Add(DE.Stems(2), "a");
          OX(3) := Add(DE.Stems(2), "um (gen. -ius)");

        elsif DE.Part.Adj.Decl.Var = 4 then
          OX(1) := Add(DE.Stems(1), "");
          OX(2) := Add(DE.Stems(2), "a");
          OX(3) := Add(DE.Stems(2), "um");

        elsif DE.Part.Adj.Decl.Var = 5 then
          OX(1) := Add(DE.Stems(1), "us");
          OX(2) := Add(DE.Stems(2), "a");
          OX(3) := Add(DE.Stems(2), "ud");

        else
          raise Not_Found;
        end if;

      elsif DE.Part.Adj.DEcl.Which = 2 then
        if DE.Part.Adj.Decl.Var = 1 then
          OX(1) := Add(Null_OX, "-");
          OX(2) := Add(DE.Stems(1), "e");
          OX(3) := Add(Null_OX, "-");
        elsif DE.Part.Adj.Decl.Var = 2 then
          OX(1) := Add(Null_OX, "-");
          OX(2) := Add(Null_OX, "a");
          OX(3) := Add(Null_OX, "-");
        elsif DE.Part.Adj.Decl.Var = 3 then
          OX(1) := Add(DE.Stems(1), "es");
          OX(2) := Add(DE.Stems(1), "es");
          OX(3) := Add(DE.Stems(1), "es");
        elsif DE.Part.Adj.Decl.Var = 6 then
          OX(1) := Add(DE.Stems(1), "os");
          OX(2) := Add(DE.Stems(1), "os");
          OX(3) := Add(Null_OX, "-");
        elsif DE.Part.Adj.Decl.Var = 7 then
          OX(1) := Add(DE.Stems(1), "os");
          OX(2) := Add(Null_OX, "-");
          OX(3) := Add(Null_OX, "-");
        elsif DE.Part.Adj.Decl.Var = 8 then
          OX(1) := Add(Null_OX, "-");
          OX(2) := Add(Null_OX, "-");
          OX(3) := Add(DE.Stems(2), "on");
        end if;

      elsif DE.Part.Adj.DEcl.Which = 3 then
        if DE.Part.Adj.Decl.Var = 1 then
          OX(1) := Add(DE.Stems(1), "");
          OX(2) := Add(DE.Stems(2), "is (gen.)");
          OX(3) := Add(Null_OX, "");
        elsif DE.Part.Adj.Decl.Var = 2 then
          OX(1) := Add(DE.Stems(1), "is");
          OX(2) := Add(DE.Stems(2), "is");
          OX(3) := Add(DE.Stems(2), "e");
        elsif DE.Part.Adj.Decl.Var = 3 then
          OX(1) := Add(DE.Stems(1), "");
          OX(2) := Add(DE.Stems(2), "is");
          OX(3) := Add(DE.Stems(2), "e");
        elsif DE.Part.Adj.Decl.Var = 6 then
          OX(1) := Add(DE.Stems(1), "");
          OX(2) := Add(DE.Stems(2), "os (-is) (gen.)");
          OX(3) := Add(Null_OX, "");
        end if;

      elsif DE.Part.Adj.DEcl = (9, 8) then
        OX(1) := Add(DE.Stems(1), ".");
        OX(2) := Add(Null_OX, "abb.");

      elsif DE.Part.Adj.DEcl = (9, 9) then
        OX(1) := Add(DE.Stems(1), "");
        OX(2) := Add(Null_OX, "undeclined");

      else
        raise Not_Found;
      end if;

    elsif DE.Part.Adj.Co = X then
      if DE.Part.Adj.DEcl.Which = 1 then
        if DE.Part.Adj.Decl.Var = 1 then
          OX(1) := Add(DE.Stems(1), "us");
          OX(2) := Add(DE.Stems(2), "a -um");
          OX(3) := Add(DE.Stems(3), "or -or -us");
          OX(4) := Add(DE.Stems(4), "mus -a -um");
        elsif DE.Part.Adj.Decl.Var = 2 then
          OX(1) := Add(DE.Stems(1), "");
          OX(2) := Add(DE.Stems(2), "a -um");
          OX(3) := Add(DE.Stems(3), "or -or -us");
          OX(4) := Add(DE.Stems(4), "mus -a -um");
        end if;

      elsif DE.Part.Adj.DEcl.Which = 3 then
        if DE.Part.Adj.Decl.Var = 1 then
          OX(1) := Add(DE.Stems(1), "");
          OX(2) := Add(DE.Stems(2), "is (gen.)");
          OX(3) := Add(DE.Stems(3), "or -or -us");
          OX(4) := Add(DE.Stems(4), "mus -a -um");
        elsif DE.Part.Adj.Decl.Var = 2 then
          OX(1) := Add(DE.Stems(1), "is");
          OX(2) := Add(DE.Stems(2), "e");
          OX(3) := Add(DE.Stems(3), "or -or -us");
          OX(4) := Add(DE.Stems(4), "mus -a -um");
        elsif DE.Part.Adj.Decl.Var = 3 then
          OX(1) := Add(DE.Stems(1), "");
          OX(2) := Add(DE.Stems(2), "is -e");
          OX(3) := Add(DE.Stems(3), "or -or -us");
          OX(4) := Add(DE.Stems(4), "mus -a -um");
        end if;

      elsif DE.Part.Adj.DEcl.Which = 9 then
        OX(1) := Add(DE.Stems(1), "");
        OX(2) := Add(Null_OX, "undeclined");
        OX(3) := Add(DE.Stems(3), "or -or -us");
        OX(4) := Add(DE.Stems(4), "mus -a -um");

      else
        raise Not_Found;
      end if;

    else
      raise Not_Found;
    end if;

  elsif (DE.Part.POFS = Adv) and then (DE.Part.Adv.Co = X) then
    OX(1) := Add(DE.Stems(1), "");
    OX(2) := Add(DE.Stems(2), "");
    OX(3) := Add(DE.Stems(3), "");


  elsif DE.Part.POFS = V then

    if DE.Part.V.Kind = DEp then --  all DEP
      OX(3) := Add(Null_OX, "DEP"); --  Flag for later use
      OX(4) := Add(DE.Stems(4), "us sum");
      if DE.Part.V.Con.Which = 1 then
        OX(1) := Add(DE.Stems(1), "or");
        OX(2) := Add(DE.Stems(2), "ari");
      elsif DE.Part.V.Con.Which = 2 then
        OX(1) := Add(DE.Stems(1), "eor");
        OX(2) := Add(DE.Stems(2), "eri");
      elsif DE.Part.V.Con.Which = 3 then
        OX(1) := Add(DE.Stems(1), "or");
        --  Would be wrong for 3 3, but no 3 3 DEP
        if DE.Part.V.Con.Var = 4 then
          OX(2) := Add(DE.Stems(2), "iri");
        else
          OX(2) := Add(DE.Stems(2), "i");
        end if;
        --            elsif DE.PART.V.CON.WHICH = 4  then   --  4th amy be 3,4 or 4,1
        --              OX(1) := ADD(DE.STEMS(1), "or");    --  depending on where in code
        --              OX(2) := ADD(DE.STEMS(2), "iri");   --  In practice there is no problem
      else
        raise Not_Found;
      end if; --  all DEP handled

    elsif DE.Part.V.Kind = Perfdef then --  all PERFDEF handled
      OX(1) := Add(DE.Stems(3), "i");
      OX(2) := Add(DE.Stems(3), "isse");
      OX(3) := Add(DE.Stems(4), "us");
      OX(4) := Null_OX; --  Flag for later use

    elsif DE.Part.V.Kind = Impers and then
    ((DE.Stems(1) (1 .. 3) = "zzz") and -- Recognize as PERFDEF IMPERS
    (DE.Stems(2) (1 .. 3) = "zzz")) then
      OX(1) := Add(DE.Stems(3), "it");
      OX(2) := Add(DE.Stems(3), "isse");
      OX(3) := Add(DE.Stems(4), "us est");
      --          OX(4) := ADD(NULL_OX, "PERFDEF");


    else --  Not DEP/PERFDEF/IMPERS  


      if DE.Part.V.Kind = Impers then
        if DE.Part.V.Con.Which = 1 then
          OX(1) := Add(DE.Stems(1), "at");
        elsif DE.Part.V.Con.Which = 2 then
          OX(1) := Add(DE.Stems(1), "et");
        elsif DE.Part.V.Con.Which = 3 then
          if DE.Part.V.Con.Var = 2 then
            OX(1) := Add(DE.Stems(1), "t");
          else
            if DE.Stems(1) (Trim(DE.Stems(1))'Last) = 'i' then
              OX(1) := Add(DE.Stems(1), "t");
            else
              OX(1) := Add(DE.Stems(1), "it");
            end if;
          end if;
        elsif DE.Part.V.Con.Which = 5 then
          if DE.Part.V.Con.Var = 1 then
            OX(1) := Add(DE.Stems(1), "est");
          end if;
        elsif DE.Part.V.Con.Which = 7 then
          if DE.Part.V.Con.Var = 1 or
          DE.Part.V.Con.Var = 2 then
            OX(1) := Add(DE.Stems(1), "t");
          end if;
        end if;

      else

        --  OX 1
        if DE.Part.V.Con.Which = 2 then
          OX(1) := Add(DE.Stems(1), "eo");

        elsif DE.Part.V.Con.Which = 5 then
          OX(1) := Add(DE.Stems(1), "um");
        elsif DE.Part.V.Con = (7, 2) then
          OX(1) := Add(DE.Stems(1), "am");
        else
          OX(1) := Add(DE.Stems(1), "o");
        end if; --  /= IMPERS handled
        --end if;      
        --  OX(1) handled
      end if;

      --  OX 2
      if DE.Part.V.Con.Which = 1 then
        OX(2) := Add(DE.Stems(2), "are");
      elsif DE.Part.V.Con.Which = 2 then
        OX(2) := Add(DE.Stems(2), "ere");
      elsif DE.Part.V.Con.Which = 3 then
        if DE.Part.V.Con.Var = 2 then
          OX(2) := Add(DE.Stems(2), "re");
        elsif DE.Part.V.Con.Var = 3 then
          OX(2) := Add(DE.Stems(2), "eri");
        elsif DE.Part.V.Con.Var = 4 then
          OX(2) := Add(DE.Stems(2), "ire");
        else
          OX(2) := Add(DE.Stems(2), "ere");
        end if;
        --            elsif DE.PART.V.CON.WHICH = 4  then
        --              OX(2) := ADD(DE.STEMS(2), "ire");
      elsif DE.Part.V.Con.Which = 5 then
        if DE.Part.V.Con.Var = 1 then
          OX(2) := Add(DE.Stems(2), "esse");
        elsif DE.Part.V.Con.Var = 2 then
          OX(2) := Add(DE.Stems(1), "e"); --  tricky, but it is 1
        end if;
      elsif DE.Part.V.Con.Which = 6 then
        if DE.Part.V.Con.Var = 1 then
          OX(2) := Add(DE.Stems(2), "re");
        elsif DE.Part.V.Con.Var = 2 then
          OX(2) := Add(DE.Stems(2), "le");
        end if;
      elsif DE.Part.V.Con.Which = 7 then
        if DE.Part.V.Con.Var = 3 then
          OX(2) := Add(DE.Stems(2), "se");
        end if;
      elsif DE.Part.V.Con.Which = 8 then
        if DE.Part.V.Con.Var = 1 then
          OX(2) := Add(DE.Stems(2), "are");
        elsif DE.Part.V.Con.Var = 2 then
          OX(2) := Add(DE.Stems(2), "ere");
        elsif DE.Part.V.Con.Var = 3 then
          OX(2) := Add(DE.Stems(2), "ere");
        elsif DE.Part.V.Con.Var = 4 then
          OX(2) := Add(DE.Stems(2), "ire");
        else
          OX(2) := Add(DE.Stems(2), "ere");
        end if;
      elsif DE.Part.V.Con = (9, 8) then
        OX(1) := Add(DE.Stems(1), ".");
        OX(2) := Add(Null_OX, "abb.");
      elsif DE.Part.V.Con = (9, 9) then
        OX(1) := Add(DE.Stems(1), "");
        OX(2) := Add(Null_OX, "undeclined");

      end if; --  OX(2) handled

      --  OX 3 & 4
      if DE.Part.V.Kind = Impers then
        if (OX(3) (1 .. 7) /= "PERFDEF") then
          OX(3) := Add(DE.Stems(3), "it");
        end if;
        OX(4) := Add(DE.Stems(4), "us est");
      elsif DE.Part.V.Kind = Semidep then --  Finalization correction
        OX(4) := Add(DE.Stems(4), "us sum");
      elsif DE.Part.V.Con = (5, 1) then
        OX(3) := Add(DE.Stems(3), "i");
        OX(4) := Add(DE.Stems(4), "urus");
      elsif DE.Part.V.Con.Which = 8 then
        OX(3) := Add("", "additional");
        OX(4) := Add("", "forms");
      elsif DE.Part.V.Con.Which = 9 then
        OX(3) := Add(Null_OX, "BLANK"); --  Flag for later use
        OX(4) := Add(Null_OX, "BLANK"); --  Flag for later use
      else
        OX(3) := Add(DE.Stems(3), "i");
        OX(4) := Add(DE.Stems(4), "us");
      end if; --  OX(3 & 4) handled

    end if; --  On V KIND

    if DE.Part.V.Con = (6, 1) then --  Finalization correction
      OX(3) := Add(OX(3), " (ii)");
    end if;

  elsif (DE.Part.POFS = Num) and then (DE.Part.Num.Sort = X) then
    if DE.Part.Num.DEcl.Which = 1 then
      if DE.Part.Num.Decl.Var = 1 then
        OX(1) := Add(DE.Stems(1), "us -a -um");
        OX(2) := Add(DE.Stems(2), "us -a -um");
        OX(3) := Add(DE.Stems(3), "i -ae -a");
        OX(4) := Add(DE.Stems(4), "");
      elsif DE.Part.Num.Decl.Var = 2 then
        OX(1) := Add(DE.Stems(1), "o -ae o");
        OX(2) := Add(DE.Stems(2), "us -a -um");
        OX(3) := Add(DE.Stems(3), "i -ae -a");
        OX(4) := Add(DE.Stems(4), "");
      elsif DE.Part.Num.Decl.Var = 3 then
        OX(1) := Add(DE.Stems(1), "es -es -ia");
        OX(2) := Add(DE.Stems(2), "us -a -um");
        OX(3) := Add(DE.Stems(3), "i -ae -a");
        OX(4) := Add(DE.Stems(4), "");
      elsif DE.Part.Num.Decl.Var = 4 then
        OX(1) := Add(DE.Stems(1), "i -ae -a");
        OX(2) := Add(DE.Stems(2), "us -a -um");
        OX(3) := Add(DE.Stems(3), "i -ae -a");
        OX(4) := Add(DE.Stems(4), "ie(n)s");
      end if;

    elsif DE.Part.Num.DEcl.Which = 2 then
      OX(1) := Add(DE.Stems(1), "");
      OX(2) := Add(DE.Stems(2), "us -a -um");
      OX(3) := Add(DE.Stems(3), "i -ae -a");
      OX(4) := Add(DE.Stems(4), "ie(n)s");

    end if;

  elsif (DE.Part.POFS = Num) and then (DE.Part.Num.Sort = Card) then
    if DE.Part.Num.DEcl.Which = 1 then
      if DE.Part.Num.Decl.Var = 1 then
        OX(1) := Add(DE.Stems(1), "us");
        OX(2) := Add(DE.Stems(1), "a");
        OX(3) := Add(DE.Stems(1), "um");
      elsif DE.Part.Num.Decl.Var = 2 then
        OX(1) := Add(DE.Stems(1), "o");
        OX(2) := Add(DE.Stems(1), "ae");
        OX(3) := Add(DE.Stems(1), "o");
      elsif DE.Part.Num.Decl.Var = 3 then
        OX(1) := Add(DE.Stems(1), "es");
        OX(2) := Add(DE.Stems(1), "es");
        OX(3) := Add(DE.Stems(1), "ia");
      elsif DE.Part.Num.Decl.Var = 4 then
        OX(1) := Add(DE.Stems(1), "i");
        OX(2) := Add(DE.Stems(1), "ae");
        OX(3) := Add(DE.Stems(1), "a");
      end if;

    elsif DE.Part.Num.DEcl.Which = 2 then
      OX(1) := Add(DE.Stems(1), "");

    end if;

  elsif (DE.Part.POFS = Num) and then (DE.Part.Num.Sort = Ord) then
    OX(1) := Add(DE.Stems(1), "us");
    OX(2) := Add(DE.Stems(1), "a");
    OX(3) := Add(DE.Stems(1), "um");

  elsif (DE.Part.POFS = Num) and then (DE.Part.Num.Sort = Dist) then
    OX(1) := Add(DE.Stems(1), "i");
    OX(2) := Add(DE.Stems(1), "ae");
    OX(3) := Add(DE.Stems(1), "a");

   elsif (DE.Part.POFS = PACK) then  
    if  Trim(De.Stems(1)) = "qu"  or Trim(De.Stems(1)) = "aliqu" then 
    OX(1) := Add(DE.Stems(1), "i");
    OX(2) := Add(DE.Stems(1), "ae");
    OX(3) := Add(DE.Stems(1), "od");
    end if;
  else
    OX(1) := Add(DE.Stems(1), "");
  end if; -- On PART           

  --  Now clean up for output
  --  Because ADJs can have multiple dictionary forms, formatting them with ';'
  --  requires special processing.  Sometimes they require look ahead
  --  and each OX needs at least one change, so let's branch once here
  case DE.Part.POFS is

    when Adj =>
      if OX(1) (1 .. 3) = "zzz" then
        OX(1) := Null_OX;

      elsif OX(1) /= Null_OX then
        Add_Up(Trim(OX(1)));
      end if;

      if OX(2) (1 .. 3) = "zzz" then
        Add_Up("-");
      elsif OX(2) /= Null_OX then
        Add_Up(", " & Trim(OX(2)));
      end if;

      if OX(3) (1 .. 3) = "zzz" then
        Add_Up("; - ");
      elsif OX(3) /= Null_OX then
        if OX(4) /= Null_OX then
          Add_Up("; " & Trim(OX(3)));
        else
          Add_Up(", " & Trim(OX(3)));
        end if;
      end if;

      if OX(4) (1 .. 3) = "zzz" then
        Add_Up(", - ");
      elsif OX(4) (1 .. 5) = "BLANK" then
        null;
      elsif OX(4) (1 .. 5) = "BLANK" then
        null;
      elsif OX(4) /= Null_OX then
        Add_Up("; " & Trim(OX(4)));
      end if;

    when others =>
      if OX(1) (1 .. 3) = "zzz" then
        Add_Up(" - ");
      elsif OX(1) /= Null_OX then
        Add_Up(Trim(OX(1)));
      end if;
      if OX(2) (1 .. 3) = "zzz" then
        Add_Up(", - ");
      elsif OX(2) /= Null_OX then
        Add_Up(", " & Trim(OX(2)));
      end if;
      if OX(3) (1 .. 3) = "zzz" then
        Add_Up(", - ");
      elsif OX(3) (1 .. 3) = "DEP" then
        null;
      elsif OX(3) (1 .. 7) = "PERFDEF" then
        null;
      elsif OX(3) (1 .. 5) = "BLANK" then
        null;
      elsif OX(3) /= Null_OX then
        Add_Up(", " & Trim(OX(3)));
      end if;
      if OX(4) (1 .. 3) = "zzz" then
        Add_Up(", - ");
      elsif OX(4) (1 .. 5) = "BLANK" then
        null;
      elsif OX(4) /= Null_OX then
        Add_Up(", " & Trim(OX(4)));
      end if;
  end case;


  Add_to("  " & Part_of_Speech_Type'Image(DE.Part.POFS) & "  ");

  if DE.Part.POFS = N then

    --  For DICTPAGE
    if DE.Part.N.DEcl.Which in 1 .. 5 and
    DE.Part.N.Decl.Var in 1 .. 5 then
      Add_to(" (" & Fst(DE.Part.N.DEcl.Which) & ")");
    end if;

    Add_to(" " & Gender_Type'Image(DE.Part.N.Gender) & "  ");
  end if;

  if (DE.Part.POFS = V) then

    --  For DICTPAGE
    if DE.Part.V.Con.Which in 1 .. 3 then
      if DE.Part.V.Con.Var = 1 then
        Add_to(" (" & Fst(DE.Part.V.Con.Which) & ")");
      elsif DE.Part.V.Con = (3, 4) then
        Add_to(" (" & Fst(4) & ")");
      end if;
    end if;

    if (DE.Part.V.Kind in Gen .. Perfdef) then
      Add_to(" " & Verb_Kind_Type'Image(DE.Part.V.Kind) & "  ");
    end if;

  end if;

  --TEXT_IO.PUT_LINE(">>>>" & TRIM(FORM));

  return Trim(Form);


exception
  when Not_Found =>
    return "";
  when others =>
    return "";
end Dictionary_Form;

