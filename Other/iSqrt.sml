exception Negative;

fun intrt 0 = 0
  | intrt n =
  if n < 0 then raise Negative else
  let
    fun iterate x = (x + (n div x)) div 2
    fun isq old new = 
      if old-new <= 1 then Int.min(new, iterate new)
      else isq new (iterate new)
  in 
    isq n (iterate n)
  end;
