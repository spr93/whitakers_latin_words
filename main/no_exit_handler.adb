-- Simple null interrupt handler for implementing NO_EXIT option
package body no_exit_Handler is

   ---------------------
   -- No_Exit_Handler --
   ---------------------
  protected body No_Exit_Handler is

     procedure  No_Exit_Catch_Interr Is
      use Ada.Interrupts;
      begin
      null;
      end No_Exit_Catch_Interr;

  end  No_Exit_Handler;

end no_exit_handler;
