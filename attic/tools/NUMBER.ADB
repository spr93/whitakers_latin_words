   with TEXT_IO; 


   procedure NUMBER is 
      use TEXT_IO;
      
   
   
   
      INPUT : TEXT_IO.FILE_TYPE;
      NUMBERED : TEXT_IO.FILE_TYPE;
      
      LINE : STRING(1..300) := (others => ' ');
      LAST, N : INTEGER := 0;
      
    begin
        
   
      PUT_LINE(
              "Takes a text file and produces a NUMBERED. file with line numbers");
              
       PUT_LINE("What file to NUMBER?");
       TEXT_IO.GET_LINE(LINE, LAST);
      
       
   
      OPEN(INPUT, IN_FILE, LINE(1..LAST)); 
   
     
      CREATE(NUMBERED, OUT_FILE, "NUMBERED.");   
   
        
                                          
      while not END_OF_FILE(INPUT) loop
        N := N + 1;
         
        GET_LINE(INPUT, LINE, LAST);
        
          TEXT_IO.PUT(NUMBERED, INTEGER'IMAGE(N));
          SET_COL(NUMBERED, 10);
        TEXT_IO.PUT_LINE(NUMBERED, LINE(1..LAST));
        
      end loop;
      
     CLOSE(NUMBERED);
      
      
   end NUMBER;
