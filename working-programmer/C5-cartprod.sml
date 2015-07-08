(* Testing the fastest for cartestian product. *)
fun fcprod xs ys = foldr (fn(x,e) => (foldr (fn(y,l)=>(x,y)::l) e ys)) [] xs;

fun lcprod xs ys = List.concat (map (fn x => map (fn y => (x,y)) ys) xs);

(** Random numbers, courtesy Stephen K. Park and Keith W. Miller, 
	CACM 31 (1988), 1192-1201.  **)
local val a = 16807.0  and  m = 2147483647.0
in  fun nextrandom seed =
          let val t = a*seed
          in  t - m * real(floor(t/m))  end

    (*truncate to integer from 1 to k*)
    and truncto k r = 1 + floor((r / m) * (real k))
end;

fun randlist (n,seed,seeds) =
    if n=0  then  (seed,seeds)  
    else  randlist(n-1, nextrandom seed, seed::seeds);

fun makexs() = #2(randlist (7200, 1.0, []));

fun time cartprod xs  =
 let
   val cPU_time   = Timer.startCPUTimer()
   and real_time  = Timer.startRealTimer()
   val cartesian  = cartprod xs xs
 in
   (Timer.checkCPUTimer cPU_time, Timer.checkRealTimer real_time)
  end;
(* > time fcprod xs_eval;
 * val it = ({sys = 1.459, usr = 37.881}, 28.890):
 *    {sys: Time.time, usr: Time.time} * Time.time
 * > time lcprod xs_eval;
 * val it = ({sys = 1.872, usr = 426.993}, 320.633):
 *    {sys: Time.time, usr: Time.time} * Time.time   *)

