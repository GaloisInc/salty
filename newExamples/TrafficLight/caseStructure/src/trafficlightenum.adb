with Ada.Containers; use Ada.Containers;
with Ada.Containers.Formal_Hashed_Maps;

package body TrafficLightEnum with SPARK_Mode is
  type Output is 
    record
      State: State_Num;
      color: Color_Type;
    end record;
  
  function Hash (tick: Boolean) return Ada.Containers.Hash_Type
  is
    Hash: Hash_Type := 17;
  begin
    Hash := Hash * 31 + Hash_Type (Boolean'Pos (tick));
    return Hash;
  end Hash;
  
  function Equivalent_Keys (Left, Right: Boolean) return Boolean
  is
    (Left = Right);
  
  package Transition_Maps is new Ada.Containers.Formal_Hashed_Maps
    (Key_Type        => Boolean,
     Element_Type    => Output,
     Hash            => Hash,
     Equivalent_Keys => Equivalent_Keys);
  
  Transition_Map_Max_Capacity: constant := State_Num'Last-1;
  
  subtype Transition_Map is Transition_Maps.Map
    (Capacity => Transition_Map_Max_Capacity,
     Modulus  => Transition_Maps.Default_Modulus (Transition_Map_Max_Capacity));
  
  type Transition_Lookup is array (State_Num) of Transition_Map;
  
  function Is_Init (C: Controller) return Boolean
  is
    (C.State = State_Num'Last);
  
  function Env_Trans (tick, tick_Next: Boolean; color: Color_Type) return Boolean
  is
    (True);
  
  function Env_Trans (C: Controller; tick: Boolean) return Boolean
  is
    (Env_Trans (C.tick, tick, C.color));
  
  function Sys_Trans (tick, tick_Next: Boolean; color, color_Next: Color_Type) return Boolean
  is
    ((not ((color = Red)) or ((not (tick_Next) or (color_Next = Green)) and (not (not (tick_Next)) or (color_Next = Red)))) and (not ((color = Yellow)) or ((not (tick_Next) or (color_Next = Red)) and (not (not (tick_Next)) or (color_Next = Yellow)))) and (not ((color = Green)) or ((not (tick_Next) or (color_Next = Yellow)) and (not (not (tick_Next)) or (color_Next = Green)))));
  
  function Sys_Trans (C: Controller; tick: Boolean; color: Color_Type) return Boolean
  is
    (Sys_Trans (C.tick, tick, C.color, color));
  
  function Initial_States_Valid (T: Transition_Lookup) return Boolean
  is
    (for all K of Transition_Maps.Formal_Model.Keys (T(T'Last)) =>
      Env_Init (K) and Sys_Init (Transition_Maps.Element (T(T'Last), K).color))
  with
    Ghost => True;
  
  function Transitions_Valid (T: Transition_Lookup) return Boolean
  is
    (for all I in T'Range =>
      (for all J of Transition_Maps.Formal_Model.Keys (T(I)) =>
        (for all K of Transition_Maps.Formal_Model.Keys (T(Transition_Maps.Element (T(I), J).State)) =>
          Env_Trans (J, K, Transition_Maps.Element (T(I), J).color) and
          Sys_Trans (J, K, Transition_Maps.Element (T(I), J).color, Transition_Maps.Element (T(Transition_Maps.Element (T(I), J).State), K).color))))
  with
    Ghost => True;
  
  function Init return Transition_Lookup
  with
    Post => (
      Initial_States_Valid (Init'Result)
      and Transitions_Valid (Init'Result)
      
      and Transition_Maps.Contains (Init'Result(1), False)
      and Transition_Maps.Contains (Init'Result(1), True)
      and Transition_Maps.Contains (Init'Result(2), False)
      and Transition_Maps.Contains (Init'Result(2), True)
      and Transition_Maps.Contains (Init'Result(3), False)
      and Transition_Maps.Contains (Init'Result(3), True)
      and Transition_Maps.Contains (Init'Result(4), False)
      and Transition_Maps.Contains (Init'Result(4), True)
      and Transition_Maps.Contains (Init'Result(5), False)
      and Transition_Maps.Contains (Init'Result(5), True)
      and Transition_Maps.Contains (Init'Result(6), False)
      and Transition_Maps.Contains (Init'Result(6), True)
      and Transition_Maps.Contains (Init'Result(7), False)
      and Transition_Maps.Contains (Init'Result(7), True)
      
      and (Transition_Maps.Element (Init'Result(1), Boolean'(False)) = (1, Red))
      and (Transition_Maps.Element (Init'Result(1), Boolean'(True)) = (3, Green))
      and (Transition_Maps.Element (Init'Result(2), Boolean'(False)) = (1, Red))
      and (Transition_Maps.Element (Init'Result(2), Boolean'(True)) = (3, Green))
      and (Transition_Maps.Element (Init'Result(3), Boolean'(False)) = (4, Green))
      and (Transition_Maps.Element (Init'Result(3), Boolean'(True)) = (5, Yellow))
      and (Transition_Maps.Element (Init'Result(4), Boolean'(False)) = (4, Green))
      and (Transition_Maps.Element (Init'Result(4), Boolean'(True)) = (5, Yellow))
      and (Transition_Maps.Element (Init'Result(5), Boolean'(False)) = (6, Yellow))
      and (Transition_Maps.Element (Init'Result(5), Boolean'(True)) = (2, Red))
      and (Transition_Maps.Element (Init'Result(6), Boolean'(False)) = (6, Yellow))
      and (Transition_Maps.Element (Init'Result(6), Boolean'(True)) = (2, Red))
      and (Transition_Maps.Element (Init'Result(7), Boolean'(False)) = (1, Red))
      and (Transition_Maps.Element (Init'Result(7), Boolean'(True)) = (2, Red)))
  is
    Transitions: Transition_Lookup;
  begin
    Transition_Maps.Insert (Transitions(1), False, (1, Red));
    Transition_Maps.Insert (Transitions(1), True, (3, Green));
    Transition_Maps.Insert (Transitions(2), False, (1, Red));
    Transition_Maps.Insert (Transitions(2), True, (3, Green));
    Transition_Maps.Insert (Transitions(3), False, (4, Green));
    Transition_Maps.Insert (Transitions(3), True, (5, Yellow));
    Transition_Maps.Insert (Transitions(4), False, (4, Green));
    Transition_Maps.Insert (Transitions(4), True, (5, Yellow));
    Transition_Maps.Insert (Transitions(5), False, (6, Yellow));
    Transition_Maps.Insert (Transitions(5), True, (2, Red));
    Transition_Maps.Insert (Transitions(6), False, (6, Yellow));
    Transition_Maps.Insert (Transitions(6), True, (2, Red));
    Transition_Maps.Insert (Transitions(7), False, (1, Red));
    Transition_Maps.Insert (Transitions(7), True, (2, Red));
    
    return Transitions;
  end Init;
  
   Transitions: constant Transition_Lookup := Init;
   
   -------------------------------------
   --  What I added to make it prove  --
   -------------------------------------
   
   type Equivalence_Array is array (State_Num) of Color_Type;
   State_Color_Equivalence : constant Equivalence_Array := (Red, Red, Green, Green, Yellow, Yellow, Red);
   
   function Is_Correct (C: Controller) return Boolean is (C.color = State_Color_Equivalence (C.State));
   
   function Possible_Transition (C : Controller; tick : Boolean) return Boolean is
      (Transition_Maps.Contains (Transitions (C.State), tick));
  
   -----------
   --  End  --
   -----------
   
  procedure Move (C: in out Controller; tick: in Boolean; color: out Color_Type)
  is
    O: Output;
  begin
--    if Transition_Maps.Contains (Transitions(C.State), tick) then
      O := Transition_Maps.Element (Transitions(C.State), tick);
      color := O.color;
      C := (State => O.State, tick => tick, color => O.color);
 --   end if;
  end Move;
end TrafficLightEnum;
