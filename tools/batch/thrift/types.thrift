///////////////////////////////////////////////////////////////////////////
// Copyright (C) Flowbox, Inc / All Rights Reserved
// Unauthorized copying of this file, via any medium is strictly prohibited
// Proprietary and confidential
// Flowbox Team <contact@flowbox.io>, 2013
///////////////////////////////////////////////////////////////////////////


enum TypeType {
    Undefined,
    Package,
    Function,
    Class,
    Interface,
    Named, 
    TypeVariable,
    Tuple,
    List;
}


struct Type {
    1: required TypeType  cls
    2: optional string    name
    3: optional list<i32> items
    4: optional list<i32> params
    5: optional list<i32> inputs
    6: optional list<i32> outputs
    7: optional i32 type
}


struct TypeContainer { 
    1: optional list<Type> typs
}