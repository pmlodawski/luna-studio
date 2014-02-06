-- extensions --
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- module --
module Main where

-- imports --
import qualified System.Exit                       as Exit    
import qualified Flowbox.Graphics.Raster.Repr.RGBA as RGBA    
import qualified Flowbox.Graphics.Raster.IO        as Image   
import qualified Flowbox.Graphics.Raster.Image     as Image   
import qualified Flowbox.Graphics.Raster.Channel   as Channel 
import qualified Flowbox.Graphics.Mockup           as Mockup  
import qualified Flowbox.Graphics.Algorithms       as Alg     
import qualified Data.Map.Lazy                     as Map     
import Data.Map.Lazy                     (Map)      
import qualified Data.Array.Accelerate             as A       
import Control.Applicative                          
import Luna.Target.HS.Core
import Flowbox.Graphics.Mockup

-- body --
data Sys = Sys deriving (Show, Eq, Ord, Generic)
$(registerCon ''Sys 'Sys 0 [])
$(mkCallInsts 'm_Sys_Sys 0 0)
$(generateAccessors ''Sys)
$(registerAccessors ''Sys)
$(mkInstsAccessors ''Sys)
m_Sys_exitSuccess = (\ _v_1204 -> do {
     Pure (Safe ());
     Exit.exitSuccess *> returnIO (Safe ());
     
})
$(registerFunc 'm_Sys_exitSuccess 1 [])
$(mkCallInsts 'm_Sys_exitSuccess 1 0)
$(mkMemInst "exitSuccess" ''Sys 'm_Sys_exitSuccess)
m_Sys_exitFailure = (\ _v_1206 -> do {
     Pure (Safe ());
     Exit.exitFailure *> returnIO (Safe ());
     
})
$(registerFunc 'm_Sys_exitFailure 1 [])
$(mkCallInsts 'm_Sys_exitFailure 1 0)
$(mkMemInst "exitFailure" ''Sys 'm_Sys_exitFailure)
data Vector a = Vector {
     p_Vector_x :: Pure (Safe a),
     p_Vector_y :: Pure (Safe a),
     p_Vector_z :: Pure (Safe a) 
} deriving (Show, Eq, Ord, Generic)
$(registerCon ''Vector 'Vector 3 [])
$(mkCallInsts 'm_Vector_Vector 3 0)
$(generateAccessors ''Vector)
$(registerAccessors ''Vector)
$(mkInstsAccessors ''Vector)
m_Vector__43 = (\ _v_1208 _v_885 -> do {
     Pure (Safe ());
     call con_Vector ((call (member (P :: P "_43") (call (member (P :: P "x") _v_1208) ())) ((call (member (P :: P "x") _v_885) ()), ())), ((call (member (P :: P "_43") (call (member (P :: P "y") _v_1208) ())) ((call (member (P :: P "y") _v_885) ()), ())), ((call (member (P :: P "_43") (call (member (P :: P "z") _v_1208) ())) ((call (member (P :: P "z") _v_885) ()), ())), ())));
     
})
$(registerFunc 'm_Vector__43 2 [])
$(mkCallInsts 'm_Vector__43 2 0)
$(mkMemInst "_43" ''Vector 'm_Vector__43)
m_Vector_length = (\ _v_1210 -> do {
     Pure (Safe ());
     call (member (P :: P "sqrt") (call (member (P :: P "_43") (call (member (P :: P "_43") (call (member (P :: P "_42") (call (member (P :: P "x") _v_1210) ())) ((call (member (P :: P "x") _v_1210) ()), ()))) ((call (member (P :: P "_42") (call (member (P :: P "y") _v_1210) ())) ((call (member (P :: P "y") _v_1210) ()), ())), ()))) ((call (member (P :: P "_42") (call (member (P :: P "z") _v_1210) ())) ((call (member (P :: P "z") _v_1210) ()), ())), ()))) ();
     
})
$(registerFunc 'm_Vector_length 1 [])
$(mkCallInsts 'm_Vector_length 1 0)
$(mkMemInst "length" ''Vector 'm_Vector_length)
m_Vector_normalize = (\ _v_1212 -> do {
     Pure (Safe ());
     call con_Vector ((call (member (P :: P "_47") (call (member (P :: P "x") _v_1212) ())) ((call (member (P :: P "length") _v_1212) ()), ())), ((call (member (P :: P "_47") (call (member (P :: P "y") _v_1212) ())) ((call (member (P :: P "length") _v_1212) ()), ())), ((call (member (P :: P "_47") (call (member (P :: P "z") _v_1212) ())) ((call (member (P :: P "length") _v_1212) ()), ())), ())));
     
})
$(registerFunc 'm_Vector_normalize 1 [])
$(mkCallInsts 'm_Vector_normalize 1 0)
$(mkMemInst "normalize" ''Vector 'm_Vector_normalize)
data Main = Main deriving (Show, Eq, Ord, Generic)
$(registerCon ''Main 'Main 0 [])
$(mkCallInsts 'm_Main_Main 0 0)
$(generateAccessors ''Main)
$(registerAccessors ''Main)
$(mkInstsAccessors ''Main)
m_Main_print = (\ _v_1214 _v_26 -> do {
     Pure (Safe ());
     print' _v_26;
     
})
$(registerFunc 'm_Main_print 2 [])
$(mkCallInsts 'm_Main_print 2 0)
$(mkMemInst "print" ''Main 'm_Main_print)
m_List_at = (\ _v_1216 _v_34 -> do {
     Pure (Safe ());
     typeMatch _v_34 (undefined :: m_35 (s_35 Int));
     (flattenCtx `dot2` liftf2 (!!)) _v_1216 _v_34;
     
})
$(registerFunc 'm_List_at 2 [])
$(mkCallInsts 'm_List_at 2 0)
$(mkMemInst "at" ''List 'm_List_at)
m_List__43 = (\ _v_1218 _v_44 -> do {
     Pure (Safe ());
     liftf2 (++) _v_1218 _v_44;
     
})
$(registerFunc 'm_List__43 2 [])
$(mkCallInsts 'm_List__43 2 0)
$(mkMemInst "_43" ''List 'm_List__43)
m_List_contains = (\ _v_1220 _v_53 -> do {
     Pure (Safe ());
     liftf2 (elem) (val _v_53) _v_1220;
     
})
$(registerFunc 'm_List_contains 2 [])
$(mkCallInsts 'm_List_contains 2 0)
$(mkMemInst "contains" ''List 'm_List_contains)
m_List_each = (\ _v_1222 _v_62 -> do {
     Pure (Safe ());
     let {
        mymap = liftf2 map
    };
     mymap (val $ call1 _v_62) _v_1222;
     
})
$(registerFunc 'm_List_each 2 [])
$(mkCallInsts 'm_List_each 2 0)
$(mkMemInst "each" ''List 'm_List_each)
m_List_foldr = (\ _v_1224 _v_73 _v_75 -> do {
     Pure (Safe ());
     flattenCtx $ (fmap.fmap) (foldr (call2 _v_73) _v_75) _v_1224;
     
})
$(registerFunc 'm_List_foldr 3 [])
$(mkCallInsts 'm_List_foldr 3 0)
$(mkMemInst "foldr" ''List 'm_List_foldr)
m_List_head = (\ _v_1226 -> do {
     Pure (Safe ());
     flattenCtx $ liftf1 head _v_1226;
     
})
$(registerFunc 'm_List_head 1 [])
$(mkCallInsts 'm_List_head 1 0)
$(mkMemInst "head" ''List 'm_List_head)
m_List_length = (\ _v_1228 -> do {
     Pure (Safe ());
     liftf1 length _v_1228;
     
})
$(registerFunc 'm_List_length 1 [])
$(mkCallInsts 'm_List_length 1 0)
$(mkMemInst "length" ''List 'm_List_length)
m_List_sort = (\ _v_1230 -> do {
     Pure (Safe ());
     (fmap.fmap.fmap) val $ liftf1 sort ((fmap.fmap.fmap) (fromSafe.fromPure) _v_1230);
     
})
$(registerFunc 'm_List_sort 1 [])
$(mkCallInsts 'm_List_sort 1 0)
$(mkMemInst "sort" ''List 'm_List_sort)
m_List_min = (\ _v_1232 -> do {
     Pure (Safe ());
     call (member (P :: P "head") (call (member (P :: P "sort") _v_1232) ())) ();
     
})
$(registerFunc 'm_List_min 1 [])
$(mkCallInsts 'm_List_min 1 0)
$(mkMemInst "min" ''List 'm_List_min)
data CF_l_111 = CF_l_111 deriving (Show)
l_111 = (\ _v_113 _v_115 -> do {
     Pure (Safe ());
     call (member (P :: P "_43") _v_113) (_v_115, ());
     
})
$(registerFunc 'l_111 2 [])
$(mkCallInsts 'l_111 2 0)
m_List_sum = (\ _v_1234 -> do {
     Pure (Safe ());
     call (member (P :: P "foldr") _v_1234) (h_l_111, ((Pure (Safe (0 :: Int))), ()));
     
})
$(registerFunc 'm_List_sum 1 [])
$(mkCallInsts 'm_List_sum 1 0)
$(mkMemInst "sum" ''List 'm_List_sum)
m_Map_each = (\ _v_1236 _v_126 -> do {
     Pure (Safe ());
     let {
        mymap = liftf2 Map.map
    };
     mymap (val $ call _v_126) _v_1236;
     
})
$(registerFunc 'm_Map_each 2 [])
$(mkCallInsts 'm_Map_each 2 0)
$(mkMemInst "each" ''Map 'm_Map_each)
m_Map_eachWithKey = (\ _v_1238 _v_137 -> do {
     Pure (Safe ());
     let {
        mymapK = liftf2 Map.mapWithKey
    };
     mymapK (val $ call1 _v_137) _v_1238;
     
})
$(registerFunc 'm_Map_eachWithKey 2 [])
$(mkCallInsts 'm_Map_eachWithKey 2 0)
$(mkMemInst "eachWithKey" ''Map 'm_Map_eachWithKey)
m_Bool_not = (\ _v_1240 -> do {
     Pure (Safe ());
     liftf1 not _v_1240;
     
})
$(registerFunc 'm_Bool_not 1 [])
$(mkCallInsts 'm_Bool_not 1 0)
$(mkMemInst "not" ''Bool 'm_Bool_not)
m_Int__43 = (\ _v_1242 _v_153 -> do {
     Pure (Safe ());
     liftf2 (+) _v_1242 _v_153;
     
})
$(registerFunc 'm_Int__43 2 [])
$(mkCallInsts 'm_Int__43 2 0)
$(mkMemInst "_43" ''Int 'm_Int__43)
m_Int__45 = (\ _v_1244 _v_162 -> do {
     Pure (Safe ());
     liftf2 (-) _v_1244 _v_162;
     
})
$(registerFunc 'm_Int__45 2 [])
$(mkCallInsts 'm_Int__45 2 0)
$(mkMemInst "_45" ''Int 'm_Int__45)
m_Int__42 = (\ _v_1246 _v_171 -> do {
     Pure (Safe ());
     liftf2 (*) _v_1246 _v_171;
     
})
$(registerFunc 'm_Int__42 2 [])
$(mkCallInsts 'm_Int__42 2 0)
$(mkMemInst "_42" ''Int 'm_Int__42)
m_Int__47 = (\ _v_1248 _v_180 -> do {
     Pure (Safe ());
     liftf2 (\a b -> toDouble a / toDouble b) _v_1248 _v_180;
     
})
$(registerFunc 'm_Int__47 2 [])
$(mkCallInsts 'm_Int__47 2 0)
$(mkMemInst "_47" ''Int 'm_Int__47)
m_Int__60 = (\ _v_1250 _v_189 -> do {
     Pure (Safe ());
     liftf2 (<) _v_1250 _v_189;
     
})
$(registerFunc 'm_Int__60 2 [])
$(mkCallInsts 'm_Int__60 2 0)
$(mkMemInst "_60" ''Int 'm_Int__60)
m_Int__62 = (\ _v_1252 _v_198 -> do {
     Pure (Safe ());
     liftf2 (>) _v_1252 _v_198;
     
})
$(registerFunc 'm_Int__62 2 [])
$(mkCallInsts 'm_Int__62 2 0)
$(mkMemInst "_62" ''Int 'm_Int__62)
m_Int_sqrt = (\ _v_1254 -> do {
     Pure (Safe ());
     liftf1 (sqrt.toDouble) _v_1254;
     
})
$(registerFunc 'm_Int_sqrt 1 [])
$(mkCallInsts 'm_Int_sqrt 1 0)
$(mkMemInst "sqrt" ''Int 'm_Int_sqrt)
m_Int_next = (\ _v_1256 -> do {
     Pure (Safe ());
     liftf1 (+1) _v_1256;
     
})
$(registerFunc 'm_Int_next 1 [])
$(mkCallInsts 'm_Int_next 1 0)
$(mkMemInst "next" ''Int 'm_Int_next)
m_Int_prev = (\ _v_1258 -> do {
     Pure (Safe ());
     liftf1 (-1) _v_1258;
     
})
$(registerFunc 'm_Int_prev 1 [])
$(mkCallInsts 'm_Int_prev 1 0)
$(mkMemInst "prev" ''Int 'm_Int_prev)
m_Int_str = (\ _v_1260 -> do {
     Pure (Safe ());
     liftf1 show _v_1260;
     
})
$(registerFunc 'm_Int_str 1 [])
$(mkCallInsts 'm_Int_str 1 0)
$(mkMemInst "str" ''Int 'm_Int_str)
m_Double__43 = (\ _v_1262 _v_227 -> do {
     Pure (Safe ());
     liftf2 (+) _v_1262 _v_227;
     
})
$(registerFunc 'm_Double__43 2 [])
$(mkCallInsts 'm_Double__43 2 0)
$(mkMemInst "_43" ''Double 'm_Double__43)
m_Double__45 = (\ _v_1264 _v_236 -> do {
     Pure (Safe ());
     liftf2 (-) _v_1264 _v_236;
     
})
$(registerFunc 'm_Double__45 2 [])
$(mkCallInsts 'm_Double__45 2 0)
$(mkMemInst "_45" ''Double 'm_Double__45)
m_Double__42 = (\ _v_1266 _v_245 -> do {
     Pure (Safe ());
     liftf2 (*) _v_1266 _v_245;
     
})
$(registerFunc 'm_Double__42 2 [])
$(mkCallInsts 'm_Double__42 2 0)
$(mkMemInst "_42" ''Double 'm_Double__42)
m_Double__47 = (\ _v_1268 _v_254 -> do {
     Pure (Safe ());
     liftf2 (/) _v_1268 _v_254;
     
})
$(registerFunc 'm_Double__47 2 [])
$(mkCallInsts 'm_Double__47 2 0)
$(mkMemInst "_47" ''Double 'm_Double__47)
m_Double__60 = (\ _v_1270 _v_263 -> do {
     Pure (Safe ());
     liftf2 (<) _v_1270 _v_263;
     
})
$(registerFunc 'm_Double__60 2 [])
$(mkCallInsts 'm_Double__60 2 0)
$(mkMemInst "_60" ''Double 'm_Double__60)
m_Double__62 = (\ _v_1272 _v_272 -> do {
     Pure (Safe ());
     liftf2 (>) _v_1272 _v_272;
     
})
$(registerFunc 'm_Double__62 2 [])
$(mkCallInsts 'm_Double__62 2 0)
$(mkMemInst "_62" ''Double 'm_Double__62)
m_Double_sqrt = (\ _v_1274 -> do {
     Pure (Safe ());
     liftf1 sqrt _v_1274;
     
})
$(registerFunc 'm_Double_sqrt 1 [])
$(mkCallInsts 'm_Double_sqrt 1 0)
$(mkMemInst "sqrt" ''Double 'm_Double_sqrt)
m_Double_next = (\ _v_1276 -> do {
     Pure (Safe ());
     liftf1 (+1) _v_1276;
     
})
$(registerFunc 'm_Double_next 1 [])
$(mkCallInsts 'm_Double_next 1 0)
$(mkMemInst "next" ''Double 'm_Double_next)
m_Double_prev = (\ _v_1278 -> do {
     Pure (Safe ());
     liftf1 (-1) _v_1278;
     
})
$(registerFunc 'm_Double_prev 1 [])
$(mkCallInsts 'm_Double_prev 1 0)
$(mkMemInst "prev" ''Double 'm_Double_prev)
m_Double_str = (\ _v_1280 -> do {
     Pure (Safe ());
     liftf1 show _v_1280;
     
})
$(registerFunc 'm_Double_str 1 [])
$(mkCallInsts 'm_Double_str 1 0)
$(mkMemInst "str" ''Double 'm_Double_str)
m_Double_invert = (\ _v_1282 -> do {
     Pure (Safe ());
     liftf1 Alg.invert _v_1282;
     
})
$(registerFunc 'm_Double_invert 1 [])
$(mkCallInsts 'm_Double_invert 1 0)
$(mkMemInst "invert" ''Double 'm_Double_invert)
m_Double_invert__ = (\ _v_1284 -> do {
     Pure (Safe ());
     liftf1 Alg.invert' _v_1284;
     
})
$(registerFunc 'm_Double_invert__ 1 [])
$(mkCallInsts 'm_Double_invert__ 1 0)
$(mkMemInst "invert__" ''Double 'm_Double_invert__)
m_Double_sign = (\ _v_1286 -> do {
     Pure (Safe ());
     liftf1 Alg.sign _v_1286;
     
})
$(registerFunc 'm_Double_sign 1 [])
$(mkCallInsts 'm_Double_sign 1 0)
$(mkMemInst "sign" ''Double 'm_Double_sign)
m_Double_parametrize = (\ _v_1288 _v_316 _v_318 -> do {
     Pure (Safe ());
     liftf3 Alg.parametrize _v_316 _v_318 _v_1288;
     
})
$(registerFunc 'm_Double_parametrize 3 [])
$(mkCallInsts 'm_Double_parametrize 3 0)
$(mkMemInst "parametrize" ''Double 'm_Double_parametrize)
m_Double_bias = (\ _v_1290 _v_329 -> do {
     Pure (Safe ());
     liftf2 Alg.bias _v_329 _v_1290;
     
})
$(registerFunc 'm_Double_bias 2 [])
$(mkCallInsts 'm_Double_bias 2 0)
$(mkMemInst "bias" ''Double 'm_Double_bias)
m_Double_gain = (\ _v_1292 _v_338 -> do {
     Pure (Safe ());
     liftf2 Alg.gain _v_338 _v_1292;
     
})
$(registerFunc 'm_Double_gain 2 [])
$(mkCallInsts 'm_Double_gain 2 0)
$(mkMemInst "gain" ''Double 'm_Double_gain)
m_Double_gamma = (\ _v_1294 _v_347 -> do {
     Pure (Safe ());
     liftf2 Alg.gamma _v_347 _v_1294;
     
})
$(registerFunc 'm_Double_gamma 2 [])
$(mkCallInsts 'm_Double_gamma 2 0)
$(mkMemInst "gamma" ''Double 'm_Double_gamma)
m_Double_compress = (\ _v_1296 _v_356 _v_358 -> do {
     Pure (Safe ());
     liftf3 Alg.compress _v_356 _v_358 _v_1296;
     
})
$(registerFunc 'm_Double_compress 3 [])
$(mkCallInsts 'm_Double_compress 3 0)
$(mkMemInst "compress" ''Double 'm_Double_compress)
m_Double_expand = (\ _v_1298 _v_369 _v_371 -> do {
     Pure (Safe ());
     liftf3 Alg.expand _v_369 _v_371 _v_1298;
     
})
$(registerFunc 'm_Double_expand 3 [])
$(mkCallInsts 'm_Double_expand 3 0)
$(mkMemInst "expand" ''Double 'm_Double_expand)
m_Double_remap = (\ _v_1300 _v_382 _v_384 _v_386 _v_388 -> do {
     Pure (Safe ());
     liftf5 Alg.remap _v_382 _v_384 _v_386 _v_388 _v_1300;
     
})
$(registerFunc 'm_Double_remap 5 [])
$(mkCallInsts 'm_Double_remap 5 0)
$(mkMemInst "remap" ''Double 'm_Double_remap)
m_Main_raise = (\ _v_1302 _v_403 _v_405 -> do {
     Pure (Safe ());
     raise _v_403 _v_405;
     
})
$(registerFunc 'm_Main_raise 3 [])
$(mkCallInsts 'm_Main_raise 3 0)
$(mkMemInst "raise" ''Main 'm_Main_raise)
m_Main_catch = (\ _v_1304 _v_414 _v_416 -> do {
     Pure (Safe ());
     catch _v_414 _v_416;
     
})
$(registerFunc 'm_Main_catch 3 [])
$(mkCallInsts 'm_Main_catch 3 0)
$(mkMemInst "catch" ''Main 'm_Main_catch)
m_Main_isError = (\ _v_1306 _v_425 -> do {
     Pure (Safe ());
     isError _v_425;
     
})
$(registerFunc 'm_Main_isError 2 [])
$(mkCallInsts 'm_Main_isError 2 0)
$(mkMemInst "isError" ''Main 'm_Main_isError)
m_Main_cuda = (\ _v_1308 -> do {
     Pure (Safe ());
     liftf0 cuda;
     
})
$(registerFunc 'm_Main_cuda 1 [])
$(mkCallInsts 'm_Main_cuda 1 0)
$(mkMemInst "cuda" ''Main 'm_Main_cuda)
m_Main_interp = (\ _v_1310 -> do {
     Pure (Safe ());
     liftf0 interp;
     
})
$(registerFunc 'm_Main_interp 1 [])
$(mkCallInsts 'm_Main_interp 1 0)
$(mkMemInst "interp" ''Main 'm_Main_interp)
m_Main_readImageWord_56 = (\ _v_1312 _v_451 -> do {
     Pure (Safe ());
     flattenCtx $ liftf1 (Image.readImageFromBMP2) _v_451;
     
})
$(registerFunc 'm_Main_readImageWord_56 2 [])
$(mkCallInsts 'm_Main_readImageWord_56 2 0)
$(mkMemInst "readImageWord_56" ''Main 'm_Main_readImageWord_56)
m_Main_readImage = (\ _v_1314 _v_458 -> do {
     Pure (Safe ());
     call (member (P :: P "reprDouble") (call (member (P :: P "decompose") (call (member (P :: P "readImageWord_56") (call con_Main ())) (_v_458, ()))) ())) ();
     
})
$(registerFunc 'm_Main_readImage 2 [])
$(mkCallInsts 'm_Main_readImage 2 0)
$(mkMemInst "readImage" ''Main 'm_Main_readImage)
m_Image_writeWord_56 = (\ _v_1316 _v_468 _v_470 -> do {
     Pure (Safe ());
     flattenCtx $ liftf3 writeImage _v_1316 _v_468 _v_470;
     _v_1316;
     
})
$(registerFunc 'm_Image_writeWord_56 3 [])
$(mkCallInsts 'm_Image_writeWord_56 3 0)
$(mkMemInst "writeWord_56" ''Image 'm_Image_writeWord_56)
m_Image_write = (\ _v_1318 _v_482 _v_484 -> do {
     Pure (Safe ());
     call (member (P :: P "writeWord_56") (call (member (P :: P "compose") (call (member (P :: P "reprWord_56") _v_1318) ())) ())) (_v_482, (_v_484, ()));
     
})
$(registerFunc 'm_Image_write 3 [])
$(mkCallInsts 'm_Image_write 3 0)
$(mkMemInst "write" ''Image 'm_Image_write)
m_Image_compose = (\ _v_1320 -> do {
     Pure (Safe ());
     flattenCtx $ liftf1 (Pure . RGBA.compose) _v_1320;
     
})
$(registerFunc 'm_Image_compose 1 [])
$(mkCallInsts 'm_Image_compose 1 0)
$(mkMemInst "compose" ''Image 'm_Image_compose)
m_Image_decompose = (\ _v_1322 -> do {
     Pure (Safe ());
     flattenCtx $ liftf1 (Pure . RGBA.decompose) _v_1322;
     
})
$(registerFunc 'm_Image_decompose 1 [])
$(mkCallInsts 'm_Image_decompose 1 0)
$(mkMemInst "decompose" ''Image 'm_Image_decompose)
m_Image_reprDouble = (\ _v_1324 -> do {
     Pure (Safe ());
     liftf1 Image.reprDouble _v_1324;
     
})
$(registerFunc 'm_Image_reprDouble 1 [])
$(mkCallInsts 'm_Image_reprDouble 1 0)
$(mkMemInst "reprDouble" ''Image 'm_Image_reprDouble)
m_Image_reprWord_56 = (\ _v_1326 -> do {
     Pure (Safe ());
     liftf1 Image.reprWord8 _v_1326;
     
})
$(registerFunc 'm_Image_reprWord_56 1 [])
$(mkCallInsts 'm_Image_reprWord_56 1 0)
$(mkMemInst "reprWord_56" ''Image 'm_Image_reprWord_56)
m_Image_getChannels = (\ _v_1328 -> do {
     Pure (Safe ());
     liftf1 getChannels _v_1328;
     
})
$(registerFunc 'm_Image_getChannels 1 [])
$(mkCallInsts 'm_Image_getChannels 1 0)
$(mkMemInst "getChannels" ''Image 'm_Image_getChannels)
m_Image_setChannels = (\ _v_1330 _v_521 -> do {
     Pure (Safe ());
     liftf1 setChannels _v_521;
     
})
$(registerFunc 'm_Image_setChannels 2 [])
$(mkCallInsts 'm_Image_setChannels 2 0)
$(mkMemInst "setChannels" ''Image 'm_Image_setChannels)
m_Image_each = (\ _v_1332 _v_528 -> do {
     Pure (Safe ());
     call (member (P :: P "setChannels") _v_1332) ((call (member (P :: P "eachWithKey") (call (member (P :: P "getChannels") _v_1332) ())) (_v_528, ())), ());
     
})
$(registerFunc 'm_Image_each 2 [])
$(mkCallInsts 'm_Image_each 2 0)
$(mkMemInst "each" ''Image 'm_Image_each)
m_Image_adjustCB = (\ _v_1334 _v_541 _v_543 -> do {
     Pure (Safe ());
     flattenCtx $ liftf3 Mockup.adjustCB _v_541 _v_543 _v_1334;
     
})
$(registerFunc 'm_Image_adjustCB 3 [])
$(mkCallInsts 'm_Image_adjustCB 3 0)
$(mkMemInst "adjustCB" ''Image 'm_Image_adjustCB)
m_Image_convolve = (\ _v_1336 _v_554 -> do {
     Pure (Safe ());
     flattenCtx $ liftf2 Mockup.convolve _v_554 _v_1336;
     
})
$(registerFunc 'm_Image_convolve 2 [])
$(mkCallInsts 'm_Image_convolve 2 0)
$(mkMemInst "convolve" ''Image 'm_Image_convolve)
m_Image_convertRGBtoHSV = (\ _v_1338 -> do {
     Pure (Safe ());
     flattenCtx $ liftf1 (Pure . Alg.convertRGBtoHSV) _v_1338;
     
})
$(registerFunc 'm_Image_convertRGBtoHSV 1 [])
$(mkCallInsts 'm_Image_convertRGBtoHSV 1 0)
$(mkMemInst "convertRGBtoHSV" ''Image 'm_Image_convertRGBtoHSV)
m_Image_convertHSVtoRGB = (\ _v_1340 -> do {
     Pure (Safe ());
     flattenCtx $ liftf1 (Pure . Alg.convertHSVtoRGB) _v_1340;
     
})
$(registerFunc 'm_Image_convertHSVtoRGB 1 [])
$(mkCallInsts 'm_Image_convertHSVtoRGB 1 0)
$(mkMemInst "convertHSVtoRGB" ''Image 'm_Image_convertHSVtoRGB)
m_Image_get = (\ _v_1342 _v_573 -> do {
     Pure (Safe ());
     flattenCtx $ liftf2 (Pure `dot2` Image.lookup) _v_573 _v_1342;
     
})
$(registerFunc 'm_Image_get 2 [])
$(mkCallInsts 'm_Image_get 2 0)
$(mkMemInst "get" ''Image 'm_Image_get)
m_Image_put = (\ _v_1344 _v_582 _v_584 -> do {
     Pure (Safe ());
     liftf3 Image.insert _v_582 _v_584 _v_1344;
     
})
$(registerFunc 'm_Image_put 3 [])
$(mkCallInsts 'm_Image_put 3 0)
$(mkMemInst "put" ''Image 'm_Image_put)
m_Image_displayP = (\ _v_1346 _v_595 -> do {
     Pure (Safe ());
     _v_598 <- call (member (P :: P "interp") (call con_Main ())) ();
     call (member (P :: P "write") _v_1346) (_v_595, (_v_598, ()));
     
})
$(registerFunc 'm_Image_displayP 2 [])
$(mkCallInsts 'm_Image_displayP 2 0)
$(mkMemInst "displayP" ''Image 'm_Image_displayP)
m_Main_extractBackground = (\ _v_1348 _v_608 -> do {
     Pure (Safe ());
     flattenCtx $ liftf1 (Pure . (Alg.extractBackground ("r", "g", "b"))) _v_608;
     
})
$(registerFunc 'm_Main_extractBackground 2 [])
$(mkCallInsts 'm_Main_extractBackground 2 0)
$(mkMemInst "extractBackground" ''Main 'm_Main_extractBackground)
m_Channel_erode = (\ _v_1350 -> do {
     Pure (Safe ());
     liftf1 Alg.erodeChannel _v_1350;
     
})
$(registerFunc 'm_Channel_erode 1 [])
$(mkCallInsts 'm_Channel_erode 1 0)
$(mkMemInst "erode" ''Channel 'm_Channel_erode)
m_Channel_map = (\ _v_1352 _v_620 -> do {
     Pure (Safe ());
     liftf2 Channel.map (val $ (\x -> (fromSafe.fromPure) (call1 _v_620(val x)))) _v_1352;
     
})
$(registerFunc 'm_Channel_map 2 [])
$(mkCallInsts 'm_Channel_map 2 0)
$(mkMemInst "map" ''Channel 'm_Channel_map)
m_Image_transform = (\ _v_1354 -> do {
     Pure (Safe ());
     val $ Image.transform _v_1354;
     
})
$(registerFunc 'm_Image_transform 1 [])
$(mkCallInsts 'm_Image_transform 1 0)
$(mkMemInst "transform" ''Image 'm_Image_transform)
m_Image_rasterize = (\ _v_1356 -> do {
     Pure (Safe ());
     _v_1356;
     
})
$(registerFunc 'm_Image_rasterize 1 [])
$(mkCallInsts 'm_Image_rasterize 1 0)
$(mkMemInst "rasterize" ''Image 'm_Image_rasterize)
m_Transformed_transform = (\ _v_1358 -> do {
     Pure (Safe ());
     _v_1358;
     
})
$(registerFunc 'm_Transformed_transform 1 [])
$(mkCallInsts 'm_Transformed_transform 1 0)
$(mkMemInst "transform" ''Transformed 'm_Transformed_transform)
m_Transformed_rasterize = (\ _v_1360 -> do {
     Pure (Safe ());
     flattenCtx $ liftf1 Mockup.rasterize' _v_1360;
     
})
$(registerFunc 'm_Transformed_rasterize 1 [])
$(mkCallInsts 'm_Transformed_rasterize 1 0)
$(mkMemInst "rasterize" ''Transformed 'm_Transformed_rasterize)
m_Transformed_translate = (\ _v_1362 _v_645 _v_647 -> do {
     Pure (Safe ());
     liftf3 Image.translate _v_645 _v_647 _v_1362;
     
})
$(registerFunc 'm_Transformed_translate 3 [])
$(mkCallInsts 'm_Transformed_translate 3 0)
$(mkMemInst "translate" ''Transformed 'm_Transformed_translate)
m_Transformed_rotate = (\ _v_1364 _v_658 -> do {
     Pure (Safe ());
     liftf2 Image.rotate _v_658 _v_1364;
     
})
$(registerFunc 'm_Transformed_rotate 2 [])
$(mkCallInsts 'm_Transformed_rotate 2 0)
$(mkMemInst "rotate" ''Transformed 'm_Transformed_rotate)
m_Transformed_rotateAt = (\ _v_1366 _v_667 _v_669 _v_671 -> do {
     Pure (Safe ());
     liftf4 Image.rotateAt _v_667 _v_669 _v_671 _v_1366;
     
})
$(registerFunc 'm_Transformed_rotateAt 4 [])
$(mkCallInsts 'm_Transformed_rotateAt 4 0)
$(mkMemInst "rotateAt" ''Transformed 'm_Transformed_rotateAt)
m_Transformed_scale = (\ _v_1368 _v_684 _v_686 -> do {
     Pure (Safe ());
     liftf3 Image.scale _v_1368 _v_684 _v_686;
     
})
$(registerFunc 'm_Transformed_scale 3 [])
$(mkCallInsts 'm_Transformed_scale 3 0)
$(mkMemInst "scale" ''Transformed 'm_Transformed_scale)
m_Transformed_scaleAt = (\ _v_1370 _v_697 _v_699 _v_701 _v_703 -> do {
     Pure (Safe ());
     liftf5 Image.scaleAt _v_1370 _v_697 _v_699 _v_701 _v_703;
     
})
$(registerFunc 'm_Transformed_scaleAt 5 [])
$(mkCallInsts 'm_Transformed_scaleAt 5 0)
$(mkMemInst "scaleAt" ''Transformed 'm_Transformed_scaleAt)
m_Transformed_adjustCB = (\ _v_1372 _v_718 _v_720 -> do {
     Pure (Safe ());
     let {
         f1 c b self = call2 (member (P :: P "adjustCB") self) c b
    };
     let {
         f2 self c b = (fmap.fmap.fmap) (f1 c b) self
    };
     f2 _v_1372 _v_718 _v_720;
     
})
$(registerFunc 'm_Transformed_adjustCB 3 [])
$(mkCallInsts 'm_Transformed_adjustCB 3 0)
$(mkMemInst "adjustCB" ''Transformed 'm_Transformed_adjustCB)
m_Transformed_convolve = (\ _v_1374 _v_735 -> do {
     Pure (Safe ());
     let {
         f1 k self = call1 (member (P :: P "convolve") self) k
    };
     let {
         f2 self k = (fmap.fmap.fmap) (f1 k) self
    };
     f2 _v_1374 _v_735;
     
})
$(registerFunc 'm_Transformed_convolve 2 [])
$(mkCallInsts 'm_Transformed_convolve 2 0)
$(mkMemInst "convolve" ''Transformed 'm_Transformed_convolve)
m_Transformed_displayP = (\ _v_1376 _v_748 -> do {
     Pure (Safe ());
     _v_751 <- call (member (P :: P "interp") (call con_Main ())) ();
     call (member (P :: P "write") (call (member (P :: P "rasterize") _v_1376) ())) (_v_748, (_v_751, ()));
     
})
$(registerFunc 'm_Transformed_displayP 2 [])
$(mkCallInsts 'm_Transformed_displayP 2 0)
$(mkMemInst "displayP" ''Transformed 'm_Transformed_displayP)
m_Image_translate = (\ _v_1378 _v_762 _v_764 -> do {
     Pure (Safe ());
     call (member (P :: P "translate") (call (member (P :: P "transform") _v_1378) ())) (_v_762, (_v_764, ()));
     
})
$(registerFunc 'm_Image_translate 3 [])
$(mkCallInsts 'm_Image_translate 3 0)
$(mkMemInst "translate" ''Image 'm_Image_translate)
m_Image_rotate = (\ _v_1380 _v_775 -> do {
     Pure (Safe ());
     call (member (P :: P "rotate") (call (member (P :: P "transform") _v_1380) ())) (_v_775, ());
     
})
$(registerFunc 'm_Image_rotate 2 [])
$(mkCallInsts 'm_Image_rotate 2 0)
$(mkMemInst "rotate" ''Image 'm_Image_rotate)
m_Image_rotateAt = (\ _v_1382 _v_784 _v_786 _v_788 -> do {
     Pure (Safe ());
     call (member (P :: P "rotateAt") (call (member (P :: P "transform") _v_1382) ())) (_v_784, (_v_786, (_v_788, ())));
     
})
$(registerFunc 'm_Image_rotateAt 4 [])
$(mkCallInsts 'm_Image_rotateAt 4 0)
$(mkMemInst "rotateAt" ''Image 'm_Image_rotateAt)
m_Image_scale = (\ _v_1384 _v_801 _v_803 -> do {
     Pure (Safe ());
     call (member (P :: P "scale") (call (member (P :: P "transform") _v_1384) ())) (_v_801, (_v_803, ()));
     
})
$(registerFunc 'm_Image_scale 3 [])
$(mkCallInsts 'm_Image_scale 3 0)
$(mkMemInst "scale" ''Image 'm_Image_scale)
m_Image_scaleAt = (\ _v_1386 _v_814 _v_816 _v_818 _v_820 -> do {
     Pure (Safe ());
     call (member (P :: P "scaleAt") (call (member (P :: P "transform") _v_1386) ())) (_v_814, (_v_816, (_v_818, (_v_820, ()))));
     
})
$(registerFunc 'm_Image_scaleAt 5 [])
$(mkCallInsts 'm_Image_scaleAt 5 0)
$(mkMemInst "scaleAt" ''Image 'm_Image_scaleAt)
m_Main_constant = (\ _v_1388 _v_835 -> do {
     Pure (Safe ());
     liftf1 A.constant _v_835;
     
})
$(registerFunc 'm_Main_constant 2 [])
$(mkCallInsts 'm_Main_constant 2 0)
$(mkMemInst "constant" ''Main 'm_Main_constant)
m_Exp__43 = (\ _v_1390 _v_842 -> do {
     Pure (Safe ());
     liftf2 (+) _v_1390 _v_842;
     
})
$(registerFunc 'm_Exp__43 2 [])
$(mkCallInsts 'm_Exp__43 2 0)
$(mkMemInst "_43" ''Exp 'm_Exp__43)
m_Exp__45 = (\ _v_1392 _v_851 -> do {
     Pure (Safe ());
     liftf2 (-) _v_1392 _v_851;
     
})
$(registerFunc 'm_Exp__45 2 [])
$(mkCallInsts 'm_Exp__45 2 0)
$(mkMemInst "_45" ''Exp 'm_Exp__45)
m_Exp__42 = (\ _v_1394 _v_860 -> do {
     Pure (Safe ());
     liftf2 (*) _v_1394 _v_860;
     
})
$(registerFunc 'm_Exp__42 2 [])
$(mkCallInsts 'm_Exp__42 2 0)
$(mkMemInst "_42" ''Exp 'm_Exp__42)
m_Exp__47 = (\ _v_1396 _v_869 -> do {
     Pure (Safe ());
     liftf2 (/) _v_1396 _v_869;
     
})
$(registerFunc 'm_Exp__47 2 [])
$(mkCallInsts 'm_Exp__47 2 0)
$(mkMemInst "_47" ''Exp 'm_Exp__47)
m_Main_fmapme = (\ _v_1398 _v_952 _v_954 -> do {
     Pure (Safe ());
     liftf1 (fmap (call1 _v_952)) _v_954;
     
})
$(registerFunc 'm_Main_fmapme 3 [])
$(mkCallInsts 'm_Main_fmapme 3 0)
$(mkMemInst "fmapme" ''Main 'm_Main_fmapme)
m_Main_evalArr = (\ _v_1400 _v_963 -> do {
     Pure (Safe ());
     flattenCtx $ (fmap.fmap) (fmap sequence . sequence) _v_963;
     
})
$(registerFunc 'm_Main_evalArr 2 [])
$(mkCallInsts 'm_Main_evalArr 2 0)
$(mkMemInst "evalArr" ''Main 'm_Main_evalArr)
m_Main_xxx = (\ _v_1402 _v_970 -> do {
     Pure (Safe ());
     call (member (P :: P "readImage") (call con_Main ())) ((call (member (P :: P "_43") (call (member (P :: P "_43") (Pure (Safe ("../video/frame-" :: String)))) ((call (member (P :: P "str") _v_970) ()), ()))) ((Pure (Safe (".bmp" :: String))), ())), ());
     
})
$(registerFunc 'm_Main_xxx 2 [])
$(mkCallInsts 'm_Main_xxx 2 0)
$(mkMemInst "xxx" ''Main 'm_Main_xxx)
m_Main_main_50 = (\ _v_1404 -> do {
     Pure (Safe ());
     call (member (P :: P "print") (call con_Main ())) ((Pure (Safe ("start!" :: String))), ());
     _v_991 <- call (member (P :: P "cuda") (call con_Main ())) ();
     _v_994 <- Pure (Safe ("luna.bmp" :: String));
     _v_998 <- call (member (P :: P "each") (Pure (Safe (concatPure [rangeFromTo (Pure (Safe (0 :: Int))) (Pure (Safe (20 :: Int)))])))) ((member (P :: P "xxx") (call con_Main ())), ());
     _v_1010 <- call (member (P :: P "evalArr") (call con_Main ())) (_v_998, ());
     _v_1015 <- call (member (P :: P "extractBackground") (call con_Main ())) (_v_1010, ());
     call (member (P :: P "write") _v_1015) (_v_994, (_v_991, ()));
     
})
$(registerFunc 'm_Main_main_50 1 [])
$(mkCallInsts 'm_Main_main_50 1 0)
$(mkMemInst "main_50" ''Main 'm_Main_main_50)
data CF_l_1121 = CF_l_1121 deriving (Show)
l_1121 = (\ _v_1123 -> do {
     Pure (Safe ());
     call (member (P :: P "_42") _v_1123) (_v_1123, ());
     
})
$(registerFunc 'l_1121 1 [])
$(mkCallInsts 'l_1121 1 0)
m_Main_main = (\ _v_1406 -> do {
     Pure (Safe ());
     call (member (P :: P "print") (call con_Main ())) ((call (member (P :: P "min") (Pure (Safe [Pure (Safe (3 :: Int)), Pure (Safe (2 :: Int)), Pure (Safe (1 :: Int))]))) ()), ());
     _v_1038 <- call con_Vector ((Pure (Safe (1 :: Int))), ((Pure (Safe (2 :: Int))), ((Pure (Safe (3 :: Int))), ())));
     call (member (P :: P "print") (call con_Main ())) (_v_1038, ());
     call (member (P :: P "print") (call con_Main ())) ((call (member (P :: P "_43") _v_1038) (_v_1038, ())), ());
     call (member (P :: P "print") (call con_Main ())) ((call (member (P :: P "length") _v_1038) ()), ());
     call (member (P :: P "print") (call con_Main ())) ((call (member (P :: P "normalize") _v_1038) ()), ());
     call (member (P :: P "print") (call con_Main ())) ((call (member (P :: P "_43") (call (member (P :: P "normalize") _v_1038) ())) ((call (member (P :: P "normalize") _v_1038) ()), ())), ());
     _v_1076 <- call (member (P :: P "interp") (call con_Main ())) ();
     _v_1079 <- Pure (Safe ("lena.bmp" :: String));
     _v_1083 <- Pure (Safe ("luna.bmp" :: String));
     _v_1087 <- call (member (P :: P "readImage") (call con_Main ())) (_v_1079, ());
     if (call (member (P :: P "isError") (call con_Main ())) (_v_1087, ())) then do {
         call (member (P :: P "print") (call con_Main ())) ((Pure (Safe ("oh no!" :: String))), ());
         call (member (P :: P "exitFailure") (call con_Sys ())) ();
         
    } else do {
         nop;
         
    };
     call (member (P :: P "print") (call con_Main ())) (_v_1079, ());
     call (member (P :: P "print") (call con_Main ())) ((Pure (Safe ("==========" :: String))), ());
     _v_1109 <- call (member (P :: P "get") _v_1087) ((Pure (Safe ("r" :: String))), ());
     _v_1116 <- call (member (P :: P "map") _v_1109) (h_l_1121, ());
     _v_1130 <- call (member (P :: P "put") _v_1087) ((Pure (Safe ("r" :: String))), (_v_1116, ()));
     call (member (P :: P "write") _v_1130) (_v_1083, (_v_1076, ()));
     call (member (P :: P "print") (call con_Main ())) ((Pure (Safe ("==========" :: String))), ());
     _v_1149 <- Pure (Safe (1.5 :: Double));
     _v_1153 <- Pure (Safe (0.2 :: Double));
     _v_1157 <- Pure (Safe (0.1 :: Double));
     _v_1161 <- call (member (P :: P "transform") _v_1130) ();
     _v_1165 <- call (member (P :: P "rotate") _v_1161) ((Pure (Safe (0.5 :: Double))), ());
     _v_1172 <- call (member (P :: P "adjustCB") _v_1165) (_v_1149, (_v_1153, ()));
     _v_1180 <- call (member (P :: P "convolve") _v_1172) (_v_1157, ());
     _v_1186 <- call (member (P :: P "rasterize") _v_1180) ();
     _v_1190 <- Pure (Safe ("luna2.bmp" :: String));
     call (member (P :: P "print") (call con_Main ())) ((Pure (Safe ("==========" :: String))), ());
     call (member (P :: P "write") _v_1186) (_v_1190, (_v_1076, ()));
     
})
$(registerFunc 'm_Main_main 1 [])
$(mkCallInsts 'm_Main_main 1 0)
$(mkMemInst "main" ''Main 'm_Main_main)
main = do {
     m <- call0 con_Main;
     getIO (call0 (member (P :: P "main") m));
     
}

