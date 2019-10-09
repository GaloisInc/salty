package MooreController with SPARK_Mode => On is

   type Controller is private;
   
   type InputVariable is (tick);
   type OutputVariable is (red, yellow, green);
   
   type InputValues is array(InputVariable) of Boolean;
   type OutputValues is array(OutputVariable) of Boolean;
   
   function Is_Initial_System_State(C: in Controller) return Boolean;
   function Is_Initial_Inputs(i: in InputValues) return Boolean is (True);
   function Is_Initial_Outputs(o: in OutputValues) return Boolean is (o(red) and not o(yellow) and not o(green)) with 
     Ghost => True;
   function Initial_Controller_Outputs(C: in Controller) return Boolean with
     Ghost => True;
  
   -- These encode the same thing, but (1) uses the inputs directly, (2) gets inputs out of C'Old and C
   function Env_Trans (C: in Controller; i: in InputValues) return Boolean;
   function Env_Trans (C_old, C: in Controller) return Boolean;
   
   -- Encode system transition properties that don't use the next state operator in this "invariant" function
   function Sys_Inv (o: in OutputValues) return Boolean is 
     ((o(red) or o(yellow) or o(green)) and 
        (not (o(red) and o(yellow)) and not (o(red) and o(green)) and not (o(yellow) and o(green))));
   -- Same as above, but over the Controller state
   function Sys_Inv (C: in Controller) return Boolean 
     with Ghost => True;
   
   -- These encode the same thing, but (1) uses inputs & outputs directly, 
   -- (2) gets inputs & outputs from C'Old and C
   function Sys_Trans (C: in Controller; i: in InputValues; o: in OutputValues) return Boolean
     with Ghost => True;
   function Sys_Trans (C_old, C: in Controller) return Boolean
     with Ghost => True;
  
   -- Version of move with properties over C'Old and C
   procedure move(C: in out Controller; i: in InputValues; o: out OutputValues) with
   Contract_Cases =>
       (Is_Initial_System_State (C) => 
          (if Is_Initial_Inputs(i) then Is_Initial_Outputs(o) and Initial_Controller_Outputs(C) and (not Is_Initial_System_State (C))),
        others => (Sys_Inv(C'Old) and (if Env_Trans(C'Old, C) then (Sys_Trans(C'Old, C)  and Sys_Inv(o) and not Is_Initial_System_State (C)))));
   
   -- Version of move with properties over C'Old, i, and o
--     procedure move(C: in out Controller; i: in InputValues; o: out OutputValues) with
--     Contract_Cases =>
--         (Is_Initial_System_State (C) => 
--            (if Is_Initial_Inputs(i) then Is_Initial_Outputs(o) and Initial_Controller_Outputs(C) and (not Is_Initial_System_State (C))),
--          others => (Sys_Inv(C'Old) and (if Env_Trans(C'Old, i) then (Sys_Trans(C'Old, i, o)  and Sys_Inv(o) and not Is_Initial_System_State (C)))));

private
   
   function State_To_Output_Mapping(C: Controller) return Boolean;
   
   type State_Number is new Integer range 0 .. 6;
   
   type Controller is record 
      State : State_Number := State_Number'Last;
      i : InputValues;
      o : OutputValues := (True, False, False);
   end record
   with Type_Invariant => State_To_Output_Mapping(Controller);

end MooreController;
