package TrafficLightEnum with SPARK_Mode is
  type Controller is private;
  
  type Color_Type is (Red, Yellow, Green);
  
  function Is_Init (C: Controller) return Boolean;
  
  function Env_Init (tick: Boolean) return Boolean
  is
    (True);
  
  function Sys_Init (color: Color_Type) return Boolean
  is
    ((color = Red))
  with
    Ghost => True;
  
  function Env_Trans (C: Controller; tick: Boolean) return Boolean
  with
    Pre => (not Is_Init (C));
  
  function Sys_Trans (C: Controller; tick: Boolean; color: Color_Type) return Boolean
  with
    Pre => (not Is_Init (C)),
    Ghost => True;
  
   function Possible_Transition (C : Controller; tick : Boolean) return Boolean;

   
   procedure Move (C: in out Controller; tick: in Boolean; color: out Color_Type)
   with
   
     --  The following precondition is not useful in this case, but it will be in
     --  other examples (when the environment is constrained by a env_init)  
     Pre            => Possible_Transition (C, tick),
     Contract_Cases =>
       (Is_Init (C) => (if Env_Init (tick) then Sys_Init (color) and (not Is_Init (C))),
        others => (if Env_Trans (C'Old, tick) then Sys_Trans (C'Old, tick, color) and (not Is_Init (C))));

private
   subtype State_Num is Integer range 1..7;  
   
   function Is_Correct (C : Controller) return Boolean;  
   type Controller is 
      record
         State: State_Num := State_Num'Last;
         tick: Boolean;
         color: Color_Type := Red;
      end record
   
   --  The Type_Invariant might have a different name, but it just states that
   --  the state number always corresponds to the correct color. It is necessary
   --  to prove the second Contract_Case of Move.
   with Type_Invariant => Is_Correct (Controller);
end TrafficLightEnum;
