package body MooreController with SPARK_Mode => On is

   type Equivalence_Array is array (State_Number) of OutputValues;
   State_Color_Equivalence : constant Equivalence_Array := (
                                                            (True, False, False), (True, False, False), (False, False, True), 
                                                            (False, False, True), (False, True, False), (False, True, False), 
                                                            (True, False, False));
   
   function State_To_Output_Mapping(C: Controller) return Boolean is (C.o = State_Color_Equivalence (C.State));
   
   function Is_Initial_System_State(C: in Controller) return Boolean is (C.State = 6);
   
   function Initial_Controller_Outputs(C: in Controller) return Boolean is
      (Is_Initial_Outputs(C.o));
   
   function Env_Trans(C_old, C: in Controller) return Boolean is (True);
   
   function Env_Trans(C: in Controller; i: in InputValues) return Boolean is (True);
   
   function Sys_Inv (C: in Controller) return Boolean is 
     ((C.o(red) or C.o(yellow) or C.o(green)) and 
        (not (C.o(red) and C.o(yellow)) and not (C.o(red) and C.o(green)) and not (C.o(yellow) and C.o(green))));
   
   function Sys_Trans (i, iTick: in InputValues; o, oTick: in OutputValues) return Boolean is (
                                                                                               (if o(red) then (if iTick(tick) then oTick(green) else oTick(red)) else True) and
                                                                                               (if o(yellow) then (if iTick(tick) then oTick(red) else oTick(yellow)) else True) and
                                                                                               (if o(green) then (if iTick(tick) then oTick(yellow) else oTick(green)) else True))
     with Ghost => True;
   
   function Sys_Trans (C: in Controller; i: in InputValues; o: in OutputValues) return Boolean is 
     (Sys_Trans(C.i, i, C.o, o));

   function Sys_Trans (C_old, C: in Controller) return Boolean is 
      (Sys_Trans(C_old.i, C.i, C_old.o, C.o));
   
   procedure move(C: in out Controller; i: in InputValues; o: out OutputValues) is
   begin
      case C.State is
         when 0 => -- Red
            case i(tick) is
               when False => C.State := 0;
               when True => C.State := 2;
            end case;
         when 1 => -- Red
            case i(tick) is
               when False => C.State := 0;
               when True => C.State := 2;
            end case;
         when 2 => -- Green
            case i(tick) is
               when False => C.State := 3;
               when True => C.State := 4;
            end case;
         when 3 => -- Green
            case i(tick) is
               when False => C.State := 3;
               when True => C.State := 4;
            end case;
         when 4 => -- Yellow
            case i(tick) is
               when False => C.State := 5;
               when True => C.State := 1;
            end case;
         when 5 => -- Yellow
            case i(tick) is
               when False => C.State := 5;
               when True => C.State := 1;
            end case;
         when 6 =>
            case i(tick) is
               when False => C.State := 0;
               when True => C.State := 1;
            end case;
      end case;
      C.i := i;
      o := C.o;
   end move;

end MooreController;
