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
import Luna.Target.HS.Core
import Flowbox.Graphics.Mockup

-- body --
data Console = Console deriving (Show, Eq, Ord, Generic)
registerCon ''Console 'Console 0 []
mkCallInsts 'm_Console_Console 0 0
generateAccessors ''Console
registerAccessors ''Console
mkInstsAccessors ''Console
m_Console_print = (\ _v_7 _v_9 -> do {
     val ();
     print' _v_9;
     
})
registerFunc 'm_Console_print 2 []
mkCallInsts 'm_Console_print 2 0
mkMemInst "print" ''Console 'm_Console_print
data Vector a = Vector {
     p_Vector_x :: Pure (Safe a),
     p_Vector_y :: Pure (Safe a),
     p_Vector_z :: Pure (Safe a) 
} deriving (Show, Eq, Ord, Generic)
registerCon ''Vector 'Vector 3 []
mkCallInsts 'm_Vector_Vector 3 0
generateAccessors ''Vector
registerAccessors ''Vector
mkInstsAccessors ''Vector
m_Vector_test = (\ _v_207 _v_209 -> do {
     val ();
     _v_209;
     
})
registerFunc 'm_Vector_test 2 []
mkCallInsts 'm_Vector_test 2 0
mkMemInst "test" ''Vector 'm_Vector_test
m_Vector__43 = (\ _v_214 _v_216 -> do {
     val ();
     call con_Vector ((call (member (P :: P "_43") (call (member (P :: P "x") _v_214) ())) ((call (member (P :: P "x") _v_216) ()), ())), ((call (member (P :: P "_43") (call (member (P :: P "y") _v_214) ())) ((call (member (P :: P "y") _v_216) ()), ())), ((call (member (P :: P "_43") (call (member (P :: P "z") _v_214) ())) ((call (member (P :: P "z") _v_216) ()), ())), ())));
     
})
registerFunc 'm_Vector__43 2 []
mkCallInsts 'm_Vector__43 2 0
mkMemInst "_43" ''Vector 'm_Vector__43
data Main = Main deriving (Show, Eq, Ord, Generic)
registerCon ''Main 'Main 0 []
mkCallInsts 'm_Main_Main 0 0
generateAccessors ''Main
registerAccessors ''Main
mkInstsAccessors ''Main
m_List_at = (\ _v_16 _v_18 -> do {
     val ();
     (flattenCtx `dot2` liftf2 (!!)) _v_16 _v_18;
     
})
registerFunc 'm_List_at 2 []
mkCallInsts 'm_List_at 2 0
mkMemInst "at" ''List 'm_List_at
m_List__43 = (\ _v_27 _v_29 -> do {
     val ();
     liftf2 (++) _v_27 _v_29;
     
})
registerFunc 'm_List__43 2 []
mkCallInsts 'm_List__43 2 0
mkMemInst "_43" ''List 'm_List__43
m_List_contains = (\ _v_38 _v_40 -> do {
     val ();
     liftf2 (elem) (val _v_40) _v_38;
     
})
registerFunc 'm_List_contains 2 []
mkCallInsts 'm_List_contains 2 0
mkMemInst "contains" ''List 'm_List_contains
m_List_length = (\ _v_49 -> do {
     val ();
     liftf1 length _v_49;
     
})
registerFunc 'm_List_length 1 []
mkCallInsts 'm_List_length 1 0
mkMemInst "length" ''List 'm_List_length
m_List_each = (\ _v_56 _v_58 -> do {
     val ();
     let {
        mymap = liftf2 map
    };
     mymap (val $ call1 _v_58) _v_56;
     
})
registerFunc 'm_List_each 2 []
mkCallInsts 'm_List_each 2 0
mkMemInst "each" ''List 'm_List_each
m_Bool_not = (\ _v_69 -> do {
     val ();
     liftf1 not _v_69;
     
})
registerFunc 'm_Bool_not 1 []
mkCallInsts 'm_Bool_not 1 0
mkMemInst "not" ''Bool 'm_Bool_not
m_Int__43 = (\ _v_76 _v_78 -> do {
     val ();
     liftf2 (+) _v_76 _v_78;
     
})
registerFunc 'm_Int__43 2 []
mkCallInsts 'm_Int__43 2 0
mkMemInst "_43" ''Int 'm_Int__43
m_Int__42 = (\ _v_87 _v_89 -> do {
     val ();
     liftf2 (*) _v_87 _v_89;
     
})
registerFunc 'm_Int__42 2 []
mkCallInsts 'm_Int__42 2 0
mkMemInst "_42" ''Int 'm_Int__42
m_Int__47 = (\ _v_98 _v_100 -> do {
     val ();
     liftf2 (-) _v_98 _v_100;
     
})
registerFunc 'm_Int__47 2 []
mkCallInsts 'm_Int__47 2 0
mkMemInst "_47" ''Int 'm_Int__47
m_Float__43 = (\ _v_109 _v_111 -> do {
     val ();
     liftf2 (+) _v_109 _v_111;
     
})
registerFunc 'm_Float__43 2 []
mkCallInsts 'm_Float__43 2 0
mkMemInst "_43" ''Float 'm_Float__43
m_Float__47 = (\ _v_120 _v_122 -> do {
     val ();
     liftf2 (/) _v_120 _v_122;
     
})
registerFunc 'm_Float__47 2 []
mkCallInsts 'm_Float__47 2 0
mkMemInst "_47" ''Float 'm_Float__47
m_Float__42 = (\ _v_131 _v_133 -> do {
     val ();
     liftf2 (*) _v_131 _v_133;
     
})
registerFunc 'm_Float__42 2 []
mkCallInsts 'm_Float__42 2 0
mkMemInst "_42" ''Float 'm_Float__42
m_Float__45 = (\ _v_142 _v_144 -> do {
     val ();
     liftf2 (-) _v_142 _v_144;
     
})
registerFunc 'm_Float__45 2 []
mkCallInsts 'm_Float__45 2 0
mkMemInst "_45" ''Float 'm_Float__45
m_Main_readImage = (\ _v_153 _v_155 -> do {
     val ();
     flattenCtx $ liftf1 readImage _v_155;
     
})
registerFunc 'm_Main_readImage 2 []
mkCallInsts 'm_Main_readImage 2 0
mkMemInst "readImage" ''Main 'm_Main_readImage
m_ImgF_writeImage = (\ _v_162 _v_164 -> do {
     val ();
     flattenCtx $ liftf2 writeImage _v_164 _v_162;
     _v_162;
     
})
registerFunc 'm_ImgF_writeImage 2 []
mkCallInsts 'm_ImgF_writeImage 2 0
mkMemInst "writeImage" ''ImgF 'm_ImgF_writeImage
m_ImgF_adjustCB = (\ _v_174 _v_176 _v_178 -> do {
     val ();
     liftf3 adjustCB _v_176 _v_178 _v_174;
     
})
registerFunc 'm_ImgF_adjustCB 3 []
mkCallInsts 'm_ImgF_adjustCB 3 0
mkMemInst "adjustCB" ''ImgF 'm_ImgF_adjustCB
m_ImgF_convolve = (\ _v_189 _v_191 -> do {
     val ();
     flattenCtx $ liftf2 convolve _v_191 _v_189;
     
})
registerFunc 'm_ImgF_convolve 2 []
mkCallInsts 'm_ImgF_convolve 2 0
mkMemInst "convolve" ''ImgF 'm_ImgF_convolve
m_Main_main = (\ _v_248 -> do {
     val ();
     _v_251 <- call con_Console ();
     _v_255 <- val ("lena.bmp" :: String);
     _v_259 <- val ("luna.bmp" :: String);
     _v_263 <- call (member (P :: P "readImage") _v_248) (_v_255, ());
     _v_269 <- val (1.5 :: Double);
     _v_273 <- val (0.2 :: Double);
     _v_277 <- val (0.1 :: Double);
     _v_281 <- val [val (0.2 :: Double),
     val (0.2 :: Double),
     val (0.2 :: Double),
     val (0.2 :: Double),
     val (0.2 :: Double),
     val (0.2 :: Double),
     val (0.2 :: Double),
     val (0.2 :: Double),
     val (0.2 :: Double)];
     _v_302 <- call (member (P :: P "adjustCB") _v_263) (_v_269, (_v_273, ()));
     call (member (P :: P "writeImage") _v_302) (_v_259, ());
     
})
registerFunc 'm_Main_main 1 []
mkCallInsts 'm_Main_main 1 0
mkMemInst "main" ''Main 'm_Main_main
main = do {
     m <- call0 con_Main;
     getIO (call0 (member (P :: P "main") m));
     
}

