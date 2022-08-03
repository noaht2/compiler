{-# LANGUAGE TupleSections #-}
module Backend (backend) where
import Control.Monad.State.Strict
import Debug.Trace
import Types
import Mangler

preamble1 :: String
preamble1
  = "tc_val\tGREG\t0\n"
  ++ "tc_val1\tGREG\t0\n"
  ++ "tc_val2\tGREG\t0\n"
  ++ "tc_env\tGREG\t0\n"
  ++ "tc_loc\tGREG\t0\n"
  ++ "tc_heap\tGREG\t0\n"
  ++ "params\tGREG\t0\n"
  ++ "var\tGREG\t0\n"
  ++ "param_ptr\tGREG\t0\n"
  ++ "arg_ptr\tGREG\t0\n"
  ++ "env_ptr\tGREG\t0\n"
  ++ "w\tGREG\t0\n"
  ++ "x\tGREG\t0\n"
  ++ "y\tGREG\t0\n"
  ++ "z\tGREG\t0\n"
  ++ "i\tGREG\t0\n"
  ++ "j\tGREG\t0\n"
  ++ "c_val\tGREG\t0\n"
  ++ "c_valn\tIS\t8\n"
  ++ "null\tBYTE\t0\n"
  ++ "nl\tBYTE\t10\n"
  ++ "n\tGREG\t0\n"
  ++ "dec\tGREG\t0\n"
  ++ "col\tGREG\t0\n"
  ++ "\n"
  ++ "\tLOC\tData_Segment\n"
  ++ "\tGREG\t@\n"
  ++ "String\tBYTE\t\"hello\",10,0\n"
  ++ "tc_val_start\tOCTA\t#2110000000000000\n"
  ++ "tc_val1_start\tOCTA\t#2190000000000000\n"
  ++ "tc_val2_start\tOCTA\t#21b0000000000000\n"
  ++ "tc_env_start\tOCTA\t#2010000000000000\n"
  ++ "tc_loc_start\tOCTA\t#21c0000000000000\n"
  ++ "tc_heap_start\tOCTA\t#21e0000000000000\n"
  ++ "c0\tOCTA\t0\n"
  ++ "c1\tOCTA\t1\n"
  ++ "c8\tOCTA\t#8\n"
  ++ "c16\tOCTA\t#10\n"
  ++ "c256\tOCTA\t#100\n"
  ++ "c264\tOCTA\t#108\n"
  ++ "params_start\tOCTA\t#21a0000000000000\n"

preamble2 :: String
preamble2
  = "\n"
  ++ "\tLOC\t#100\n"
  ++ "Main\tSWYM\n"
--  ++ "\tLDA\t$255,String\n"
-- ++ "\tTRAP\t0,Fputs,StdOut\n"
  ++ "\tLDO\ttc_val,tc_val_start\n"
  ++ "\tLDO\ttc_val1,tc_val1_start\n"
  ++ "\tLDO\ttc_val2,tc_val2_start\n"
  ++ "\tLDO\ttc_env,tc_env_start\n"
  ++ "\tLDO\ttc_loc,tc_loc_start\n"
  ++ "\tLDO\tparams,params_start\n"
  ++ "\tLDO\ttc_heap,tc_heap_start\n"
  ++ "\tLDO\tc_val,c256\n"
  ++ "c_env\tIS\t#10\n"
--  ++ "\tLDA\t$255,String\n"
--  ++ "\tTRAP\t0,Fputs,StdOut\n"

var_string :: Int -> String
var_string n = var ++ "_text" ++ "\tBYTE\t\"" ++ var ++ "\"\n"
  where var = int_to_var n

var_strings :: Int -> String
var_strings 0 = var_string 0
var_strings n = var_strings (n - 1) ++ var_string n

postscript :: String
postscript
  = "End\tSWYM\n"
--  ++ "\tTRAP\t0,Halt,0\n"
--  ++ "\tLDA\t$255,String\n"
--  ++ "\tTRAP\t0,Fputs,StdOut\n"
  ++ "\tSUB\ttc_val,tc_val,c_val\n"
  ++ "\tLDO\tn,tc_val\n"
--  ++ "\tLDA\t$255,n\n"
--  ++ "\tTRAP\t0,Fputs,StdOut\n"
  ++ "\tLDO\tdec,tc_val_start\n"
  ++ "\tSET\tcol,10000\n"
  ++ "ConversionLoop\tSWYM\n"
  ++ "\tDIV\tx,n,col\n"
  ++ "\tADD\tx,x,'0'\n"
  ++ "\tSTB\tx,dec\n"
  ++ "\tGET\tn,rR\n"
  ++ "\tADD\tdec,dec,1\n"
  ++ "\tDIV\tcol,col,10\n"
  ++ "\tPBNZ\tcol,ConversionLoop\n"
  ++ "\tLDB\tx,nl\n"
  ++ "\tSTB\tx,dec\n"
  ++ "\tLDB\tx,null\n"
  ++ "\tSTB\tx,dec,1\n"
  ++ "\tLDO\t$255,tc_val_start\n"
  ++ "\tTRAP\t0,Fputs,StdOut\n"
  ++ "\tTRAP\t0,Halt,0\n"

backend :: Asm -> State Int String
backend ir = do
  result <- sequence (map convert ir)
  let consts = concat (map fst result)
  let instructions = concat (map snd result)
  n <- get
  return (preamble1 ++ var_strings n ++ consts ++ preamble2 ++ instructions ++ postscript)

convert :: (String, AsmInstruction) -> State Int (String, String)
convert ("", PushVal n) = do
  n_label <- get_var
  return
    (n_label ++ "\tOCTA\t" ++ show n ++ "\n",
     "\tSWYM ;; PushVal\n"
     ++ "\tLDO\tx," ++ n_label ++ "\n"
     ++ "\tSTO\tx,tc_val\n"
     ++ "\tADD\ttc_val,tc_val,c_val\n")
convert ("", PopMoveVal1Val)
  = return $ ("",)
    ("\tSWYM ;; PopMoveVal1Val\n"
     ++ "\tSUB\ttc_val,tc_val,c_val\n"
     ++ "\tLDO\tx,tc_val\n"
     ++ "\tSTO\tx,tc_val1\n"
     ++ "\tADD\ttc_val1,tc_val1,c_valn\n")
convert (label, PopMoveVal2Val)
  = return $ ("",)
    ("\tSWYM ;; PopMoveVal2Val\n"
     ++ "\tSUB\ttc_val,tc_val,c_val\n"
     ++ "\tLDO\tx,tc_val\n"
     ++ "\tSTO\tx,tc_val2\n"
     ++ "\tADD\ttc_val2,tc_val2,c_valn\n")
convert (label, PopVal)
  = return $ ("",)
    (label ++ "\tSWYM ;; PopVal\n"
     ++ "\tSUB\ttc_val,tc_val,c_val\n")
convert ("", SetParams ss) = do
  let set_param p
        = "\tLDO\tx," ++ p ++ "_text\n"
          ++ "\tSTO\tx,params,i\n"
          ++ "\tADD\ti,i,#8\n"
  return $ ("",)
    ("\tSWYM ;; SetParams\n"
      ++ "\tLDO\ti,c0\n"
      ++ concatMap set_param ss
      ++ "\tLDO\tx,c0\n"
      ++ "\tSTO\tx,params,i\n")
convert ("", SetVar var) = do
  return $ ("",)
    ("\tSWYM ;; SetVar\n"
     -- ++ var_label ++ "\tOCTA\t\"" ++ var ++ "\"\n"
     -- ++ "\tGETA\tx," ++ var_label ++ "\n"
     ++ "\tLDO\tvar," ++ var ++ "_text\n")
convert (label, PushLoc loc)
  = return $ ("",)
    (label ++ "\tSWYM ;; PushLoc\n"
     ++ "\tLDA\tx," ++ loc ++ "\n"
     ++ "\tSTO\tx,tc_loc\n"
     ++ "\tADD\ttc_loc,tc_loc,#8\n")
convert ("", SubValVal1Val2)
 = return $ ("",)
   ("\tSWYM ;; SubValVal1Val2\n"
    ++ "\tSUB\ttc_val1,tc_val1,c_valn\n"
    ++ "\tLDO\tx,tc_val1\n"
    ++ "\tSUB\ttc_val2,tc_val2,c_valn\n"
    ++ "\tLDO\ty,tc_val2\n"
    ++ "\tSUB\tx,x,y\n"
    ++ "\tSTO\tx,tc_val\n"
    ++ "\tADD\ttc_val,tc_val,c_val\n")
convert ("", PushOneIfZeroValVal) = do
  zero <- get_var
  afterwards <- get_var
  return $ ("",)
    ("\tSWYM ;; PushOneIfZeroValVal\n"
     ++ "\tSUB\ttc_val,tc_val,c_val\n"
     ++ "\tLDO\tx,tc_val\n"
     ++ "\tBZ\tx," ++ zero ++ "\n"
     ++ "\tLDO\ty,c0\n"
     ++ "\tSTO\ty,tc_val\n"
     ++ "\tJMP\t" ++ afterwards ++ "\n"
     ++ zero ++ "\tLDO\ty,c1\n"
     ++ "\tSTO\ty,tc_val\n"
     ++ afterwards ++ "\tADD\ttc_val,tc_val,c_val\n")
convert ("", Jmp destination)
  = return $ ("",)
    ("\tSWYM ;; Jmp\n"
     ++ "\tJMP\t" ++ destination ++ "\n")
convert ("", BzVal loc)
  = return $ ("",)
    ("\tSWYM ;; BzVal\n"
     ++ "\tSUB\ttc_val,tc_val,c_val\n"
     ++ "\tLDO\tx,tc_val\n"
     ++ "\tBZ\tx," ++ loc ++ "\n")
convert ("", ApplyEnv) = do
  loop_label <- get_var
  loop'_label <- get_var
  return $ ("",)
    ("\tSWYM ;; ApplyEnv\n"
     ++ "\tLDO\ti,c0\n"
     ++ loop_label ++ "\tSUB\ti,i,#10\n"
     ++ "\tADD\tx,tc_env,i\n"
     ++ "\tLDO\ty,x\n"
     ++ "\tCMP\tz,var,y\n"
     ++ "\tPBNZ\tz," ++ loop_label ++ "\n"
     ++ "\tADD\tx,x,#8\n"
     ++ "\tLDO\tx,x\n"
     ++ "\tLDO\ti,c0\n"
     ++ loop'_label ++ "\tLDO\ty,x,i\n"
     ++ "\tSTO\ty,tc_val,i\n"
     ++ "\tADD\ti,i,#8\n"
     ++ "\tCMP\tz,i,c_val\n"
     ++ "\tPBNZ\tz," ++ loop'_label ++ "\n"
     ++ "\tADD\ttc_val,tc_val,c_val\n")
convert ("", ExtendEnv) = do
  loop <- get_var
  return $ ("",)
    ("\tSWYM ;; ExtendEnv\n"
     ++ "\tSTO\tvar,tc_env\n"
     ++ "\tSTO\ttc_heap,tc_env,#8\n"
     ++ "\tADD\ttc_env,tc_env,c_env\n"
     ++ "\tLDO\ti,c0\n"
     ++ "\tSUB\ttc_val,tc_val,c_val\n"
     ++ loop ++ "\tLDO\tx,tc_val,i\n"
     ++ "\tSTO\tx,tc_heap,i\n"
     ++ "\tADD\ti,i,#8\n"
     ++ "\tCMP\ty,i,c_val\n"
     ++ "\tPBNZ\ty," ++ loop ++ "\n"
     ++ "\tADD\ttc_heap,tc_heap,c_val\n")
convert ("", CreateProc) = do
  loop_label <- get_var
  loop'_label <- get_var
  return $ ("",)
    ("\tSWYM ;; CreateProc\n"
     ++ "\tSUB\ttc_loc,tc_loc,#8\n"
     ++ "\tLDO\tw,tc_loc\n"
     ++ "\tSTO\tw,tc_val\n"
     ++ "\tLDO\ti,c8\n"
     ++ "\tLDO\tj,c0\n"
     ++ loop_label ++ "\tLDO\tx,params,j\n"
     ++ "\tSTO\tx,tc_val,i\n"
     ++ "\tADD\ti,i,#8\n"
     ++ "\tADD\tj,j,#8\n"
     ++ "\tPBNZ\tx," ++ loop_label ++ "\n"
     ++ "\tSET\ti,#80\n"
     ++ "\tNEG\tj,i\n"
     ++ loop'_label ++ "\tADD\tx,tc_env,j\n"
     ++ "\tLDO\ty,x\n"
     ++ "\tSTO\ty,tc_val,i\n"
     ++ "\tADD\ti,i,#8\n"
     ++ "\tADD\tj,j,#8\n"
     ++ "\tPBNZ\tj," ++ loop'_label ++ "\n"
     ++ "\tADD\ttc_val,tc_val,c_val\n")
convert ("", CreateRecProc var) = do
  loop_label <- get_var
  loop'_label <- get_var
  return $ ("",)
    ("\tSWYM ;; CreateRecProc\n"
     ++ "\tLDO\tw," ++ var ++ "_text\n"
     ++ "\tSTO\tw,tc_env\n"
     ++ "\tSTO\ttc_heap,tc_env,#8\n"
     ++ "\tADD\ttc_env,tc_env,c_env\n"
     ++ "\tSUB\ttc_loc,tc_loc,#8\n"
     ++ "\tLDO\tz,tc_loc\n"
     ++ "\tSTO\tz,tc_val\n"
     ++ "\tSTO\tz,tc_heap\n"
     ++ "\tLDO\ti,c8\n"
     ++ "\tLDO\tj,c0\n"
     ++ loop_label ++ "\tLDO\tx,params,j\n"
     ++ "\tSTO\tx,tc_val,i\n"
     ++ "\tSTO\tx,tc_heap,i\n"
     ++ "\tADD\ti,i,#8\n"
     ++ "\tADD\tj,j,#8\n"
     ++ "\tPBNZ\tx," ++ loop_label ++ "\n"
     ++ "\tSET\ti,#80\n"
     ++ "\tNEG\tj,i\n"
     ++ loop'_label ++ "\tADD\tx,tc_env,j\n"
     ++ "\tLDO\ty,x\n"
     ++ "\tSTO\ty,tc_val,i\n"
     ++ "\tSTO\ty,tc_heap,i\n"
     ++ "\tADD\ti,i,#8\n"
     ++ "\tADD\tj,j,#8\n"
     ++ "\tPBNZ\tj," ++ loop'_label ++ "\n"
     ++ "\tADD\ttc_val,tc_val,c_val\n"
     ++ "\tADD\ttc_heap,tc_heap,c_val\n")
    --  ++ "\tADD\tx,tc_env,c_env\n"
    --  ++ "\tSTO\tx,tc_val\n"
    --  ++ "\tSUB\ttc_loc,tc_loc,#8\n"
    --  ++ "\tLDO\tw,tc_loc\n"
    --  ++ "\tSTO\tw,tc_val,#8\n"
    --  ++ "\tSET\ti,#10\n"
    --  ++ "\tSET\tj,#0\n"
    --  ++ loop_label ++ "\tLDO\tx,params,j\n"
    --  ++ "\tSTO\tx,tc_val,i\n"
    --  ++ "\tADD\ti,i,#8\n"
    --  ++ "\tADD\tj,j,#8\n"
    --  ++ "\tPBNZ\tx," ++ loop_label ++ "\n"
    --  ++ "\tADD\ttc_val,tc_val,c_val\n")
convert ("", Apply args) = do
  args' <- sequence (map sequence (map (map convert) args))
  let ns = concat (concat (map (map fst) args'))
  let args'' = map (concat . map snd) args'
  let process arg = do
        loop <- get_var
        return (arg
                ++ "\tSWYM ;; argument processing\n"
                ++ "\tLDO\tvar,param_ptr\n"
                ++ "\tSTO\tvar,env_ptr\n"
                ++ "\tSTO\ttc_heap,env_ptr,#8\n"
                ++ "\tADD\tenv_ptr,env_ptr,c_env\n"
                ++ "\tSUB\ttc_val,tc_val,c_val\n"
                ++ "\tLDO\ti,c0\n"
                ++ loop ++ "\tLDO\tx,tc_val,i\n"
                ++ "\tSTO\tx,tc_heap,i\n"
                ++ "\tADD\ti,i,#8\n"
                ++ "\tCMP\ty,i,c_val\n"
                ++ "\tPBNZ\ty," ++ loop ++ "\n"
                ++ "\tADD\ttc_heap,tc_heap,c_val\n"
                ++ "\tADD\tparam_ptr,param_ptr,#8\n")
  args''' <- fmap concat (sequence (map process args''))
  loop_label <- get_var
  return (ns,
    "\tSWYM ;; Apply\n"
    ++ "\tSUB\ttc_val,tc_val,c_val\n"
    ++ "\tSETH\ti,0\n"
    ++ "\tSETL\ti,#80\n"
    ++ "\tLDO\tj,c0\n"
    ++ loop_label ++ "\tLDO\tx,tc_val,i\n"
    ++ "\tSTO\tx,tc_env,j\n"
    ++ "\tADD\ti,i,#8\n"
    ++ "\tADD\tj,j,#8\n"
    ++ "\tCMP\ty,j,#80\n"
    ++ "\tPBNZ\ty," ++ loop_label ++ "\n"
    ++ "\tLDO\tw,tc_val\n"
    ++ "\tSTO\tw,tc_loc\n"
    ++ "\tADD\ttc_loc,tc_loc,#8\n"
    ++ "\tADD\tparam_ptr,tc_val,#8\n"
    ++ "\tADD\ttc_val,tc_val,c_val\n"
    ++ "\tADD\tenv_ptr,tc_env,#80\n"
    ++ args'''
    ++ "\tLDA\ttc_env,env_ptr\n"
    ++ "\tSUB\ttc_loc,tc_loc,#8\n"
    ++ "\tLDO\tw,tc_loc\n"
    ++ "\tGO\tx,w\n")
convert ("", Done)
  = return $ ("",)
    ("\tSWYM ;; Done\n"
     ++ "\tJMP\tEnd\n")
convert (label, Swym)
  = return $ ("",) (label ++ "\tSWYM ;; Swym\n")
