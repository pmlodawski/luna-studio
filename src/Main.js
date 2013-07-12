/* Thunk
   Creates a thunk representing the given closure.
   Since we want automatic memoization of as many expressions as possible, we
   use a JS object as a sort of tagged pointer, where the member x denotes the
   object actually pointed to. If a "pointer" points to a thunk, it has a
   member 't' which is set to true; if it points to a value, be it a function,
   a value of an algebraic type of a primitive value, it has no member 't'.
*/

function T(f) {
    return new Thunk(f);
}

function Thunk(f) {
    this.f = f;
}

/* Apply
   Applies the function f to the arguments args. If the application is under-
   saturated, a closure is returned, awaiting further arguments. If it is over-
   saturated, the function is fully applied, and the result (assumed to be a
   function) is then applied to the remaining arguments.
*/
function A(f, args) {
    f = f instanceof Thunk ? E(f) : f;
    // Closure does some funny stuff with functions that occasionally
    // results in non-functions getting applied, so we have to deal with
    // it.
    if(!f.apply) {
        return f;
    }

    var arity = f.arity ? f.arity : f.length;
    if(args.length === arity) {
        return f.apply(null, args);
    }
    if(args.length > arity) {
        var first = args.splice(0, arity);
        return A(f.apply(null, first), args);
    } else {
        var g = function() {
            var as = args.concat(Array.prototype.slice.call(arguments));
            return A(f, as);
        };
        g.arity = arity - args.length;
        return g;
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof Thunk) {
        if(t.f) {
            t.x = t.f();
            t.f = 0;
        }
        return t.x;
    }
    return t;
}

/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw err;
}

function quot(a, b) {
    return (a-a%b)/b;
}

function quotRemI(a, b) {
    return [1, (a-a%b)/b, a%b];
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
function imul(a, b) {
  // ignore high a * high a as the result will always be truncated
  var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
  var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
  var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
  return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
}

function addC(a, b) {
    var x = a+b;
    return [1, x & 0xffffffff, x > 0x7fffffff];
}

function subC(a, b) {
    var x = a-b;
    return [1, x & 0xffffffff, x < -2147483648];
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

// Scratch space for byte arrays.
var rts_scratchBuf = new ArrayBuffer(8);
var rts_scratchW32 = new Uint32Array(rts_scratchBuf);
var rts_scratchFloat = new Float32Array(rts_scratchBuf);
var rts_scratchDouble = new Float64Array(rts_scratchBuf);

function decodeFloat(x) {
    rts_scratchFloat[0] = x;
    var sign = x < 0 ? -1 : 1;
    var exp = ((rts_scratchW32[0] >> 23) & 0xff) - 150;
    var man = rts_scratchW32[0] & 0x7fffff;
    if(exp === 0) {
        ++exp;
    } else {
        man |= (1 << 23);
    }
    return [1, sign*man, exp];
}

function decodeDouble(x) {
    rts_scratchDouble[0] = x;
    var sign = x < 0 ? -1 : 1;
    var manHigh = rts_scratchW32[1] & 0xfffff;
    var manLow = rts_scratchW32[0];
    var exp = ((rts_scratchW32[1] >> 20) & 0x7ff) - 1075;
    if(exp === 0) {
        ++exp;
    } else {
        manHigh |= (1 << 20);
    }
    return [1, sign, manHigh, manLow, exp];
}

function err(str) {
    die(toJSStr(str)[1]);
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {return unAppCStr(str, [1]);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = A(f, [[1, str.charCodeAt(i)], acc]);
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return [2,[1,str.charCodeAt(i)],T(function() {
            return unAppCStr(str,chrs,i+1);
        })];
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str)[1]);
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str[0] == 2; str = E(str[2])) {
        s += String.fromCharCode(E(str[1])[1]);
    }
    return [1,s];
}

// newMutVar
function nMV(val) {
    return ({x: val});
}

// readMutVar
function rMV(mv) {
    return mv.x;
}

// writeMutVar
function wMV(mv, val) {
    mv.x = val;
}

function localeEncoding() {
    var le = newByteArr(5);
    le['b']['i8'] = 'U'.charCodeAt(0);
    le['b']['i8'] = 'T'.charCodeAt(0);
    le['b']['i8'] = 'F'.charCodeAt(0);
    le['b']['i8'] = '-'.charCodeAt(0);
    le['b']['i8'] = '8'.charCodeAt(0);
    return le;
}

var isFloatNaN = isDoubleNaN = isNaN;

function isDoubleInfinite(d) {
    return (d === Infinity);
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x) {
    return (x===0 && (1/x)===-Infinity);
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b) {
    return a == b;
}

function strOrd(a, b) {
    var ord;
    if(a < b) {
        ord = [1];
    } else if(a == b) {
        ord = [2];
    } else {
        ord = [3];
    }
    return ord;
}

function jsCatch(act, handler) {
    try {
        return A(act,[0]);
    } catch(e) {
        return A(handler,[e, 0]);
    }
}

function hs_eqWord64(a, b) {
    return (a[0] == b[0] && a[1] == b[1]);
}

// Word64# representation: (Word, Word)
function hs_wordToWord64(low) {
    return [0, low];
}

function hs_mkWord64(high, low) {
    return [high, low];
}

var coercionToken = undefined;

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Array) {
        return x[0]-1;
    } else {
        return x-1;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round; // Stupid GHC doesn't like periods in FFI IDs...
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function jsAlert(val) {
    if(typeof alert != 'undefined') {
        alert(val);
    } else {
        print(val);
    }
}

function jsLog(val) {
    console.log(val);
}

function jsPrompt(str) {
    var val;
    if(typeof prompt != 'undefined') {
        val = prompt(str);
    } else {
        print(str);
        val = readline();
    }
    return val == undefined ? '' : val.toString();
}

function jsEval(str) {
    var x = eval(str);
    return x == undefined ? '' : x.toString();
}

function isNull(obj) {
    return obj === null;
}

function jsRead(str) {
    return Number(str);
}

function jsShowI(val) {return val.toString();}
function jsShow(val) {
    var ret = val.toString();
    return val == Math.round(val) ? ret + '.0' : ret;
}

function jsSetCB(elem, evt, cb) {
    // Count return press in single line text box as a change event.
    if(evt == 'change' && elem.type.toLowerCase() == 'text') {
        setCB(elem, 'keyup', function(k) {
            if(k == '\n'.charCodeAt(0)) {
                A(cb,[[1,k.keyCode],0]);
            }
        });
    }

    var fun;
    switch(evt) {
    case 'click':
    case 'dblclick':
    case 'mouseup':
    case 'mousedown':
        fun = function(x) {A(cb,[[1,x.button],0]);};
        break;
    case 'keypress':
    case 'keyup':
    case 'keydown':
        fun = function(x) {A(cb,[[1,x.keyCode],0]);};
        break;        
    default:
        fun = function() {A(cb,[0]);};
        break;
    }
    return setCB(elem, evt, fun);
}

function setCB(elem, evt, cb) {
    if(elem.addEventListener) {
        elem.addEventListener(evt, cb, false);
        return true;
    } else if(elem.attachEvent) {
        elem.attachEvent('on'+evt, cb);
        return true;
    }
    return false;
}

function jsSetTimeout(msecs, cb) {
    window.setTimeout(function() {A(cb,[0]);}, msecs);
}

// Degenerate versions of u_iswspace, u_iswalnum and u_iswalpha.
function u_iswspace(c) {
    return c==9 || c==10 || c==13 || c==32;
}

function u_iswalnum(c) {
    return (c >= 48 && c <= 57) || u_iswalpha(c);
}

// [a-zA-ZåäöÅÄÖ]
function u_iswalpha(c) {
    return (c >= 65 && c <= 90) || (c >= 97 && c <= 122) ||
            c == 229 || c == 228 || c == 246 ||
            c == 197 || c == 196 || c == 214;
}

function jsGet(elem, prop) {
    return elem[prop].toString();
}

function jsSet(elem, prop, val) {
    elem[prop] = val;
}

function jsGetStyle(elem, prop) {
    return elem.style[prop].toString();
}

function jsSetStyle(elem, prop, val) {
    elem.style[prop] = val;
}

function jsKillChild(child, parent) {
    parent.removeChild(child);
}

function jsClearChildren(elem) {
    while(elem.hasChildNodes()){
        elem.removeChild(elem.lastChild);
    }
}

function jsFind(elem) {
    var e = document.getElementById(elem)
    if(e) {
        return [2,[1,e]];
    }
    return [1];
}

function jsCreateElem(tag) {
    return document.createElement(tag);
}

function jsGetChildBefore(elem) {
    elem = elem.previousSibling;
    while(elem) {
        if(typeof elem.tagName != 'undefined') {
            return [2,[1,elem]];
        }
        elem = elem.previousSibling;
    }
    return [1];
}

function jsGetLastChild(elem) {
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [2,[1,elem.childNodes[i]]];
        }
    }
    return [1];
}

function jsGetChildren(elem) {
    var children = [1];
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            children = [2, [1,elem.childNodes[i]], children];
        }
    }
    return children;
}

function jsSetChildren(elem, children) {
    children = E(children);
    jsClearChildren(elem, 0);
    while(children[0] === 2) {
        elem.appendChild(E(E(children[1])[1]));
        children = E(children[2]);
    }
}

function jsAppendChild(child, container) {
    container.appendChild(child);
}

function jsAddChildBefore(child, container, after) {
    container.insertBefore(child, after);
}

var jsRand = Math.random;

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs[0] != 1) {
        strs = E(strs);
        arr.push(E(strs[1])[1]);
        strs = E(strs[2]);
    }
    return arr.join(sep);
}

// Escape all double quotes in a string
function jsUnquote(str) {
    return str.replace(/"/, '\\"');
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return [1];
    }
    return [2,hs];
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return [1, [1, jsRead(obj)]];
    case 'string':
        return [2, [1, obj]];
        break;
    case 'boolean':
        return [3, obj]; // Booleans are special wrt constructor tags!
        break;
    case 'object':
        if(obj instanceof Array) {
            return [4, arr2lst(obj, 0)];
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [1];
            for(var i in ks) {
                xs = [2, [1, [1,ks[i]], toHS(obj[ks[i]])], xs];
            }
            return [5, xs];
        }
    }
}

function arr2lst(arr, elem) {
    if(elem >= arr.length) {
        return [1];
    }
    return [2, toHS(arr[elem]), T(function() {return arr2lst(arr,elem+1);})]
}

function ajaxReq(method, url, async, postdata, cb) {
    var xhr = new XMLHttpRequest();
    xhr.open(method, url, async);
    xhr.setRequestHeader('Cache-control', 'no-cache');
    xhr.onreadystatechange = function() {
        if(xhr.readyState == 4) {
            if(xhr.status == 200) {
                A(cb,[[2,[1,xhr.responseText]],0]);
            } else {
                A(cb,[[1],0]); // Nothing
            }
        }
    }
    xhr.send(postdata);
}

function u_towlower(charCode) {
    return String.fromCharCode(charCode).toLowerCase().charCodeAt(0);
}

function u_towupper(charCode) {
    return String.fromCharCode(charCode).toUpperCase().charCodeAt(0);
}

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return ({empty: true});
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return [1, 0, undefined];
    } else {
        mv.empty = true;
        mv.x = null;
        return [1, 1, mv.x];
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    mv.empty = true;
    mv.x = null;
    return mv.x;
}

function putMVar(mv, val) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
}

function tryPutMVar(mv, val) {
    if(!mv.empty) {
        return 0;
    } else {
        mv.empty = false;
        mv.x = val;
        return 1;
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv) {
    return mv.empty ? 1 : 0;
}

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;

function makeStableName(x) {
    if(!x.stableName) {
        x.stableName = __next_stable_name;
        __next_stable_name += 1;
    }
    return x.stableName;
}

function eqStableName(x, y) {
    return (x == y) ? 1 : 0;
}

var Integer = function(bits, sign) {
  this.bits_ = [];
  this.sign_ = sign;

  var top = true;
  for (var i = bits.length - 1; i >= 0; i--) {
    var val = bits[i] | 0;
    if (!top || val != sign) {
      this.bits_[i] = val;
      top = false;
    }
  }
};

Integer.IntCache_ = {};

var I_fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Integer.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Integer([value | 0], value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Integer.IntCache_[value] = obj;
  }
  return obj;
};

var I_fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Integer.ZERO;
  } else if (value < 0) {
    return I_negate(I_fromNumber(-value));
  } else {
    var bits = [];
    var pow = 1;
    for (var i = 0; value >= pow; i++) {
      bits[i] = (value / pow) | 0;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return new Integer(bits, 0);
  }
};

Integer.fromBits = function(bits) {
  var high = bits[bits.length - 1];
  return new Integer(bits, high & (1 << 31) ? -1 : 0);
};

I_fromString = function(str, opt_radix) {
  if (str.length == 0) {
    throw Error('number format error: empty string');
  }

  var radix = opt_radix || 10;
  if (radix < 2 || 36 < radix) {
    throw Error('radix out of range: ' + radix);
  }

  if (str.charAt(0) == '-') {
    return I_negate(I_fromString(str.substring(1), radix));
  } else if (str.indexOf('-') >= 0) {
    throw Error('number format error: interior "-" character');
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 8));

  var result = Integer.ZERO;
  for (var i = 0; i < str.length; i += 8) {
    var size = Math.min(8, str.length - i);
    var value = parseInt(str.substring(i, i + size), radix);
    if (size < 8) {
      var power = I_fromNumber(Math.pow(radix, size));
      result = I_add(I_mul(result, power), I_fromNumber(value));
    } else {
      result = I_mul(result, radixToPower);
      result = I_add(add, I_fromNumber(value));
    }
  }
  return result;
};


Integer.TWO_PWR_32_DBL_ = (1 << 16) * (1 << 16);
Integer.ZERO = I_fromInt(0);
Integer.ONE = I_fromInt(1);
Integer.TWO_PWR_24_ = I_fromInt(1 << 24);

var I_toInt = function(self) {
  return self.bits_.length > 0 ? self.bits_[0] : self.sign_;
};

var I_toWord = function(self) {
  return I_toInt(self) >>> 0;
};

var I_toNumber = function(self) {
  if (isNegative(self)) {
    return -I_toNumber(I_negate(self));
  } else {
    var val = 0;
    var pow = 1;
    for (var i = 0; i < self.bits_.length; i++) {
      val += getBitsUnsigned(self, i) * pow;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return val;
  }
};

var getBits = function(self, index) {
  if (index < 0) {
    return 0;
  } else if (index < self.bits_.length) {
    return self.bits_[index];
  } else {
    return self.sign_;
  }
};

var getBitsUnsigned = function(self, index) {
  var val = getBits(self, index);
  return val >= 0 ? val : Integer.TWO_PWR_32_DBL_ + val;
};

var getSign = function(self) {
  return self.sign_;
};

var isZero = function(self) {
  if (self.sign_ != 0) {
    return false;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    if (self.bits_[i] != 0) {
      return false;
    }
  }
  return true;
};

var isNegative = function(self) {
  return self.sign_ == -1;
};

var isOdd = function(self) {
  return (self.bits_.length == 0) && (self.sign_ == -1) ||
         (self.bits_.length > 0) && ((self.bits_[0] & 1) != 0);
};

var I_equals = function(self, other) {
  if (self.sign_ != other.sign_) {
    return false;
  }
  var len = Math.max(self.bits_.length, other.bits_.length);
  for (var i = 0; i < len; i++) {
    if (getBits(self, i) != getBits(other, i)) {
      return false;
    }
  }
  return true;
};

var I_notEquals = function(self, other) {
  return !I_equals(self, other);
};

var greaterThan = function(self, other) {
  return I_compare(self, other) > 0;
};

var greaterThanOrEqual = function(self, other) {
  return I_compare(self, other) >= 0;
};

var lessThan = function(self, other) {
  return I_compare(self, other) < 0;
};

var lessThanOrEqual = function(self, other) {
  return I_compare(self, other) <= 0;
};

var I_compare = function(self, other) {
  var diff = I_sub(self, other);
  if (isNegative(diff)) {
    return -1;
  } else if (isZero(diff)) {
    return 0;
  } else {
    return +1;
  }
};

var I_compareInt = function(self, other) {
  return I_compare(self, I_fromInt(other));
}

var shorten = function(self, numBits) {
  var arr_index = (numBits - 1) >> 5;
  var bit_index = (numBits - 1) % 32;
  var bits = [];
  for (var i = 0; i < arr_index; i++) {
    bits[i] = getBits(self, i);
  }
  var sigBits = bit_index == 31 ? 0xFFFFFFFF : (1 << (bit_index + 1)) - 1;
  var val = getBits(self, arr_index) & sigBits;
  if (val & (1 << bit_index)) {
    val |= 0xFFFFFFFF - sigBits;
    bits[arr_index] = val;
    return new Integer(bits, -1);
  } else {
    bits[arr_index] = val;
    return new Integer(bits, 0);
  }
};

var I_negate = function(self) {
  return I_add(not(self), Integer.ONE);
};

var I_add = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  var carry = 0;

  for (var i = 0; i <= len; i++) {
    var a1 = getBits(self, i) >>> 16;
    var a0 = getBits(self, i) & 0xFFFF;

    var b1 = getBits(other, i) >>> 16;
    var b0 = getBits(other, i) & 0xFFFF;

    var c0 = carry + a0 + b0;
    var c1 = (c0 >>> 16) + a1 + b1;
    carry = c1 >>> 16;
    c0 &= 0xFFFF;
    c1 &= 0xFFFF;
    arr[i] = (c1 << 16) | c0;
  }
  return Integer.fromBits(arr);
};

var I_sub = function(self, other) {
  return I_add(self, I_negate(other));
};

var I_mul = function(self, other) {
  if (isZero(self)) {
    return Integer.ZERO;
  } else if (isZero(other)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_mul(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_mul(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_mul(self, I_negate(other)));
  }

  if (lessThan(self, Integer.TWO_PWR_24_) &&
      lessThan(other, Integer.TWO_PWR_24_)) {
    return I_fromNumber(I_toNumber(self) * I_toNumber(other));
  }

  var len = self.bits_.length + other.bits_.length;
  var arr = [];
  for (var i = 0; i < 2 * len; i++) {
    arr[i] = 0;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    for (var j = 0; j < other.bits_.length; j++) {
      var a1 = getBits(self, i) >>> 16;
      var a0 = getBits(self, i) & 0xFFFF;

      var b1 = getBits(other, j) >>> 16;
      var b0 = getBits(other, j) & 0xFFFF;

      arr[2 * i + 2 * j] += a0 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j);
      arr[2 * i + 2 * j + 1] += a1 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 1] += a0 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 2] += a1 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 2);
    }
  }

  for (var i = 0; i < len; i++) {
    arr[i] = (arr[2 * i + 1] << 16) | arr[2 * i];
  }
  for (var i = len; i < 2 * len; i++) {
    arr[i] = 0;
  }
  return new Integer(arr, 0);
};

Integer.carry16_ = function(bits, index) {
  while ((bits[index] & 0xFFFF) != bits[index]) {
    bits[index + 1] += bits[index] >>> 16;
    bits[index] &= 0xFFFF;
  }
};

var I_mod = function(self, other) {
  return I_rem(I_add(other, I_rem(self, other)), other);
}

var I_div = function(self, other) {
  if(greaterThan(self, Integer.ZERO) != greaterThan(other, Integer.ZERO)) {
    if(I_rem(self, other) != Integer.ZERO) {
      return I_sub(I_quot(self, other), Integer.ONE);
    }
  }
  return I_quot(self, other);
}

var I_quotRem = function(self, other) {
  return [1, I_quot(self, other), I_rem(self, other)];
}

var I_divMod = function(self, other) {
  return [1, I_div(self, other), I_mod(self, other)];
}

var I_quot = function(self, other) {
  if (isZero(other)) {
    throw Error('division by zero');
  } else if (isZero(self)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_quot(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_quot(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_quot(self, I_negate(other)));
  }

  var res = Integer.ZERO;
  var rem = self;
  while (greaterThanOrEqual(rem, other)) {
    var approx = Math.max(1, Math.floor(I_toNumber(rem) / I_toNumber(other)));
    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);
    var approxRes = I_fromNumber(approx);
    var approxRem = I_mul(approxRes, other);
    while (isNegative(approxRem) || greaterThan(approxRem, rem)) {
      approx -= delta;
      approxRes = I_fromNumber(approx);
      approxRem = I_mul(approxRes, other);
    }

    if (isZero(approxRes)) {
      approxRes = Integer.ONE;
    }

    res = I_add(res, approxRes);
    rem = I_sub(rem, approxRem);
  }
  return res;
};

var I_rem = function(self, other) {
  return I_sub(self, I_mul(I_quot(self, other), other));
};

var not = function(self) {
  var len = self.bits_.length;
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = ~self.bits_[i];
  }
  return new Integer(arr, ~self.sign_);
};

var I_and = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = getBits(self, i) & getBits(other, i);
  }
  return new Integer(arr, self.sign_ & other.sign_);
};

var I_or = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = getBits(self, i) | getBits(other, i);
  }
  return new Integer(arr, self.sign_ | other.sign_);
};

var I_xor = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = getBits(self, i) ^ getBits(other, i);
  }
  return new Integer(arr, self.sign_ ^ other.sign_);
};

var I_shiftLeft = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length + arr_delta + (bit_delta > 0 ? 1 : 0);
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (getBits(self, i - arr_delta) << bit_delta) |
               (getBits(self, i - arr_delta - 1) >>> (32 - bit_delta));
    } else {
      arr[i] = getBits(self, i - arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_shiftRight = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length - arr_delta;
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (getBits(self, i + arr_delta) >>> bit_delta) |
               (getBits(self, i + arr_delta + 1) << (32 - bit_delta));
    } else {
      arr[i] = getBits(self, i + arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_signum = function(self) {
  var cmp = I_compare(self, Integer.ZERO);
  if(cmp > 0) {
    return Integer.ONE
  }
  if(cmp < 0) {
    return I_sub(Integer.ZERO, Integer.ONE);
  }
  return Integer.ZERO;
};

var I_abs = function(self) {
  if(I_compare(self, Integer.ZERO) < 0) {
    return I_sub(Integer.ZERO, self);
  }
  return self;
};

var I_decodeDouble = function(x) {
  var dec = decodeDouble(x);
  var mantissa = Integer.fromBits([dec[3], dec[2]]);
  if(dec[1] < 0) {
    mantissa = I_negate(mantissa);
  }
  return [1, dec[4], mantissa];
}

var I_toString = function(self) {
  var radix = 10;

  if (isZero(self)) {
    return '0';
  } else if (isNegative(self)) {
    return '-' + I_toString(I_negate(self));
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 6));

  var rem = self;
  var result = '';
  while (true) {
    var remDiv = I_div(rem, radixToPower);
    var intval = I_toInt(I_sub(rem, I_mul(remDiv, radixToPower)));
    var digits = intval.toString();

    rem = remDiv;
    if (isZero(rem)) {
      return digits + result;
    } else {
      while (digits.length < 6) {
        digits = '0' + digits;
      }
      result = '' + digits + result;
    }
  }
};

var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}

// Joseph Myers' MD5 implementation; used under the BSD license.

function md5cycle(x, k) {
var a = x[0], b = x[1], c = x[2], d = x[3];

a = ff(a, b, c, d, k[0], 7, -680876936);
d = ff(d, a, b, c, k[1], 12, -389564586);
c = ff(c, d, a, b, k[2], 17,  606105819);
b = ff(b, c, d, a, k[3], 22, -1044525330);
a = ff(a, b, c, d, k[4], 7, -176418897);
d = ff(d, a, b, c, k[5], 12,  1200080426);
c = ff(c, d, a, b, k[6], 17, -1473231341);
b = ff(b, c, d, a, k[7], 22, -45705983);
a = ff(a, b, c, d, k[8], 7,  1770035416);
d = ff(d, a, b, c, k[9], 12, -1958414417);
c = ff(c, d, a, b, k[10], 17, -42063);
b = ff(b, c, d, a, k[11], 22, -1990404162);
a = ff(a, b, c, d, k[12], 7,  1804603682);
d = ff(d, a, b, c, k[13], 12, -40341101);
c = ff(c, d, a, b, k[14], 17, -1502002290);
b = ff(b, c, d, a, k[15], 22,  1236535329);

a = gg(a, b, c, d, k[1], 5, -165796510);
d = gg(d, a, b, c, k[6], 9, -1069501632);
c = gg(c, d, a, b, k[11], 14,  643717713);
b = gg(b, c, d, a, k[0], 20, -373897302);
a = gg(a, b, c, d, k[5], 5, -701558691);
d = gg(d, a, b, c, k[10], 9,  38016083);
c = gg(c, d, a, b, k[15], 14, -660478335);
b = gg(b, c, d, a, k[4], 20, -405537848);
a = gg(a, b, c, d, k[9], 5,  568446438);
d = gg(d, a, b, c, k[14], 9, -1019803690);
c = gg(c, d, a, b, k[3], 14, -187363961);
b = gg(b, c, d, a, k[8], 20,  1163531501);
a = gg(a, b, c, d, k[13], 5, -1444681467);
d = gg(d, a, b, c, k[2], 9, -51403784);
c = gg(c, d, a, b, k[7], 14,  1735328473);
b = gg(b, c, d, a, k[12], 20, -1926607734);

a = hh(a, b, c, d, k[5], 4, -378558);
d = hh(d, a, b, c, k[8], 11, -2022574463);
c = hh(c, d, a, b, k[11], 16,  1839030562);
b = hh(b, c, d, a, k[14], 23, -35309556);
a = hh(a, b, c, d, k[1], 4, -1530992060);
d = hh(d, a, b, c, k[4], 11,  1272893353);
c = hh(c, d, a, b, k[7], 16, -155497632);
b = hh(b, c, d, a, k[10], 23, -1094730640);
a = hh(a, b, c, d, k[13], 4,  681279174);
d = hh(d, a, b, c, k[0], 11, -358537222);
c = hh(c, d, a, b, k[3], 16, -722521979);
b = hh(b, c, d, a, k[6], 23,  76029189);
a = hh(a, b, c, d, k[9], 4, -640364487);
d = hh(d, a, b, c, k[12], 11, -421815835);
c = hh(c, d, a, b, k[15], 16,  530742520);
b = hh(b, c, d, a, k[2], 23, -995338651);

a = ii(a, b, c, d, k[0], 6, -198630844);
d = ii(d, a, b, c, k[7], 10,  1126891415);
c = ii(c, d, a, b, k[14], 15, -1416354905);
b = ii(b, c, d, a, k[5], 21, -57434055);
a = ii(a, b, c, d, k[12], 6,  1700485571);
d = ii(d, a, b, c, k[3], 10, -1894986606);
c = ii(c, d, a, b, k[10], 15, -1051523);
b = ii(b, c, d, a, k[1], 21, -2054922799);
a = ii(a, b, c, d, k[8], 6,  1873313359);
d = ii(d, a, b, c, k[15], 10, -30611744);
c = ii(c, d, a, b, k[6], 15, -1560198380);
b = ii(b, c, d, a, k[13], 21,  1309151649);
a = ii(a, b, c, d, k[4], 6, -145523070);
d = ii(d, a, b, c, k[11], 10, -1120210379);
c = ii(c, d, a, b, k[2], 15,  718787259);
b = ii(b, c, d, a, k[9], 21, -343485551);

x[0] = add32(a, x[0]);
x[1] = add32(b, x[1]);
x[2] = add32(c, x[2]);
x[3] = add32(d, x[3]);

}

function cmn(q, a, b, x, s, t) {
a = add32(add32(a, q), add32(x, t));
return add32((a << s) | (a >>> (32 - s)), b);
}

function ff(a, b, c, d, x, s, t) {
return cmn((b & c) | ((~b) & d), a, b, x, s, t);
}

function gg(a, b, c, d, x, s, t) {
return cmn((b & d) | (c & (~d)), a, b, x, s, t);
}

function hh(a, b, c, d, x, s, t) {
return cmn(b ^ c ^ d, a, b, x, s, t);
}

function ii(a, b, c, d, x, s, t) {
return cmn(c ^ (b | (~d)), a, b, x, s, t);
}

function md51(s) {
txt = '';
var n = s.length,
state = [1732584193, -271733879, -1732584194, 271733878], i;
for (i=64; i<=s.length; i+=64) {
md5cycle(state, md5blk(s.substring(i-64, i)));
}
s = s.substring(i-64);
var tail = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
for (i=0; i<s.length; i++)
tail[i>>2] |= s.charCodeAt(i) << ((i%4) << 3);
tail[i>>2] |= 0x80 << ((i%4) << 3);
if (i > 55) {
md5cycle(state, tail);
for (i=0; i<16; i++) tail[i] = 0;
}
tail[14] = n*8;
md5cycle(state, tail);
return state;
}

function md5blk(s) {
var md5blks = [], i;
for (i=0; i<64; i+=4) {
md5blks[i>>2] = s.charCodeAt(i)
+ (s.charCodeAt(i+1) << 8)
+ (s.charCodeAt(i+2) << 16)
+ (s.charCodeAt(i+3) << 24);
}
return md5blks;
}

var hex_chr = '0123456789abcdef'.split('');

function rhex(n)
{
var s='', j=0;
for(; j<4; j++)
s += hex_chr[(n >> (j * 8 + 4)) & 0x0F]
+ hex_chr[(n >> (j * 8)) & 0x0F];
return s;
}

function hex(x) {
for (var i=0; i<x.length; i++)
x[i] = rhex(x[i]);
return x.join('');
}

function md5(s) {
return hex(md51(s));
}

function add32(a, b) {
return (a + b) & 0xFFFFFFFF;
}

// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = [];
    for(; n >= 0; --n) {
        arr.push(x);
    }
    return arr;
}

// Create all views at once; perhaps it's wasteful, but it's better than having
// to check for the right view at each read or write.
function newByteArr(n) {
    // Pad the thing to multiples of 8.
    var padding = 8 - n % 8;
    if(padding < 8) {
        n += padding;
    }
    var arr = {};
    var buffer = new ArrayBuffer(n);
    var views = {};
    views['i8']  = new Int8Array(buffer);
    views['i16'] = new Int16Array(buffer);
    views['i32'] = new Int32Array(buffer);
    views['w8']  = new Uint8Array(buffer);
    views['w16'] = new Uint16Array(buffer);
    views['w32'] = new Uint32Array(buffer);
    views['f32'] = new Float32Array(buffer);
    views['f64'] = new Float64Array(buffer);
    arr['b'] = buffer;
    arr['v'] = views;
    // ByteArray and Addr are the same thing, so keep an offset if we get
    // casted.
    arr['off'] = 0;
    return arr;
}

// An attempt at emulating pointers enough for ByteString and Text to be
// usable without patching the hell out of them.
// The general idea is that Addr# is a byte array with an associated offset.

function plusAddr(addr, off) {
    var newaddr = {};
    newaddr['off'] = addr['off'] + off;
    newaddr['b']   = addr['b'];
    newaddr['v']   = addr['v'];
    return newaddr;
}

function writeOffAddr(type, elemsize, addr, off, x) {
    addr['v'][type][addr.off/elemsize + off] = x;
}

function readOffAddr(type, elemsize, addr, off) {
    return addr['v'][type][addr.off/elemsize + off];
}

// Two addresses are equal if they point to the same buffer and have the same
// offset. For other comparisons, just use the offsets - nobody in their right
// mind would check if one pointer is less than another, completely unrelated,
// pointer and then act on that information anyway.
function addrEq(a, b) {
    if(a == b) {
        return true;
    }
    return a && b && a['b'] == b['b'] && a['off'] == b['off'];
}

function addrLT(a, b) {
    if(a) {
        return b && a['off'] < b['off'];
    } else {
        return (b != 0); 
    }
}

function addrGT(a, b) {
    if(b) {
        return a && a['off'] > b['off'];
    } else {
        return (a != 0);
    }
}

var _0=[1],_1=true,_2=[1],_3=[2],_4=function(_5,_6,_7,_8,_9,_a,_b,_){var _c=E(_b),_d=_c[1],_e=_c[2],_f=_c[3],_g=_c[4],_h=_c[5],_i=[1,_5,_6,_7,_8,0,0];return (function(_j,_k,_){while(1){var _l=(function(_m,_n,_){if(_n<_g){if(_m<_a){var _o=readOffAddr("w32",4,_5,_m);0;writeOffAddr("w8",1,plusAddr(_d,_n),0,_o>>>0&255);0;var _p=_m+1|0,_q=_n+1|0;_j=_p;_k=_q;return null;}else{return [1,_2,T(function(){return _m==_a==false?[1,_5,_6,_7,_8,_m,_a]:E(_i);}),[1,_d,_e,_f,_g,_h,_n]];}}else{return [1,_3,T(function(){return _m==_a==false?[1,_5,_6,_7,_8,_m,_a]:E(_i);}),[1,_d,_e,_f,_g,_h,_n]];}})(_j,_k,_);if(_l!=null){return _l;}}})(_9,_c[6],_);},_r=function(_s,_t,_u,_v,_){return (function(_w,_x,_){while(1){var _y=A(_s,[_w,_x,_]),_z=E(_y),_A=_z[2],_B=_z[3];if(E(_z[1])[0]==3){var _C=E(_A);if(E(_w)[5]!=_C[5]){return [1,_C,_B];}else{var _D=A(_t,[_C,_B,_]),_E=E(_D);_w=_E[1];_x=_E[2];continue;}}else{return [1,_A,_B];}}})(_u,_v,_);},_F=function(_G){return E(E(_G)[5]);},_H=function(_I,_J,_K,_L,_M,_N,_O,_){var _P=E(_I),_Q=_P[6],_R=rMV(_Q),_S=function(_T,_U,_){var _V=E(_U),_W=_V[6],_X=function(_){var _Y=A(T(function(){return _F(_P[2]);}),[_P[4],_V,_]),_=wMV(_Q,_Y),_Z=E(_T),_10=_Z[5],_11=_Z[6];return _10==_11==false?_H(_P,_Z[1],_Z[2],_Z[3],_Z[4],_10,_11,_):_0;};if(_V[4]!=_W){var _12=E(_T),_13=_12[1],_14=_12[2],_15=_12[3],_16=_12[4],_17=_12[5],_18=_12[6],_19=T(function(){return _17==_18;}),_1a=function(_){var _1b=E(_P[7]);switch(_1b[0]){case 1:return _X(_);case 2:var _=wMV(_Q,_V);return E(_19)==false?_H(_P,_13,_14,_15,_16,_17,_18,_):_0;default:var _1c=E(_1b[1]);if(_1c[0]==1){var _=wMV(_Q,_V);return E(_19)==false?_H(_P,_13,_14,_15,_16,_17,_18,_):_0;}else{if((_W-_V[5]|0)<E(_1c[1])[1]){var _=wMV(_Q,_V);return E(_19)==false?_H(_P,_13,_14,_15,_16,_17,_18,_):_0;}else{return _X(_);}}}};return E(_19)==false?_17==_N==false?_1a(_):_X(_):_1a(_);}else{return _X(_);}},_1d=E(_P[11]);if(_1d[0]==1){var _1e=_4(_J,_K,_L,_M,_N,_O,_R,_),_1f=E(_1e);return _S(_1f[2],_1f[3],_);}else{var _1g=E(_1d[1]),_1h=_r(_1g[1],_1g[2],[1,_J,_K,_L,_M,_N,_O],_R,_),_1i=E(_1h);return _S(_1i[1],_1i[2],_);}},_1j=T(function(){return unCStr("hPutChar");}),_1k=[2],_1l=function(_1m){return E(E(_1m)[4]);},_1n=[2],_1o=[6],_1p=[1],_1q=[1],_1r=T(function(){return unCStr("cannot flush the read buffer: underlying device is not seekable");}),_1s=[1,_1p,_1o,_1q,_1r,_1p,_1p],_1t=T(function(){return unCStr("GHC.IO.Exception");}),_1u=T(function(){return unCStr("base");}),_1v=T(function(){return unCStr("IOException");}),_1w=T(function(){var _1x=hs_wordToWord64(4053623282),_1y=hs_wordToWord64(3693590983);return [1,_1x,_1y,[1,_1x,_1y,_1u,_1t,_1v],_1q];}),_1z=function(_1A){return E(_1w);},_1B=function(_1C){return E(E(_1C)[1]);},_1D=T(function(){return unCStr("Maybe.fromJust: Nothing");}),_1E=T(function(){return err(_1D);}),_1F=function(_1G,_1H,_1I){var _1J=T(function(){var _1K=A(_1G,[_1I]),_1L=A(_1H,[T(function(){var _1M=E(_1J);return _1M[0]==1?E(_1E):E(_1M[1]);})]),_1N=hs_eqWord64(_1K[1],_1L[1]);if(!E(_1N)){return [1];}else{var _1O=hs_eqWord64(_1K[2],_1L[2]);return E(_1O)==0?[1]:[2,_1I];}});return E(_1J);},_1P=function(_1Q){var _1R=E(_1Q);return _1F(_1B(_1R[1]),_1z,_1R[2]);},_1S=T(function(){return unCStr(": ");}),_1T=[1,41],_1U=T(function(){return unCStr(" (");}),_1V=function(_1W,_1X){var _1Y=E(_1W);if(_1Y[0]==1){return E(_1X);}else{return [2,_1Y[1],T(function(){return _1V(_1Y[2],_1X);})];}},_1Z=T(function(){return unCStr("already exists");}),_20=T(function(){return unCStr("does not exist");}),_21=T(function(){return unCStr("protocol error");}),_22=T(function(){return unCStr("failed");}),_23=T(function(){return unCStr("invalid argument");}),_24=T(function(){return unCStr("inappropriate type");}),_25=T(function(){return unCStr("hardware fault");}),_26=T(function(){return unCStr("unsupported operation");}),_27=T(function(){return unCStr("timeout");}),_28=T(function(){return unCStr("resource vanished");}),_29=T(function(){return unCStr("interrupted");}),_2a=T(function(){return unCStr("resource busy");}),_2b=T(function(){return unCStr("resource exhausted");}),_2c=T(function(){return unCStr("end of file");}),_2d=T(function(){return unCStr("illegal operation");}),_2e=T(function(){return unCStr("permission denied");}),_2f=T(function(){return unCStr("user error");}),_2g=T(function(){return unCStr("unsatisified constraints");}),_2h=T(function(){return unCStr("system error");}),_2i=function(_2j,_2k){switch(E(_2j)[0]){case 1:return _1V(_1Z,_2k);case 2:return _1V(_20,_2k);case 3:return _1V(_2a,_2k);case 4:return _1V(_2b,_2k);case 5:return _1V(_2c,_2k);case 6:return _1V(_2d,_2k);case 7:return _1V(_2e,_2k);case 8:return _1V(_2f,_2k);case 9:return _1V(_2g,_2k);case 10:return _1V(_2h,_2k);case 11:return _1V(_21,_2k);case 12:return _1V(_22,_2k);case 13:return _1V(_23,_2k);case 14:return _1V(_24,_2k);case 15:return _1V(_25,_2k);case 16:return _1V(_26,_2k);case 17:return _1V(_27,_2k);case 18:return _1V(_28,_2k);default:return _1V(_29,_2k);}},_2l=[1,125],_2m=T(function(){return unCStr("{handle: ");}),_2n=function(_2o,_2p,_2q,_2r,_2s,_2t){var _2u=T(function(){var _2v=T(function(){return _2i(_2p,T(function(){var _2w=E(_2r);return _2w[0]==1?E(_2t):_1V(_1U,T(function(){return _1V(_2w,[2,_1T,_2t]);}));}));}),_2x=E(_2q);return _2x[0]==1?E(_2v):_1V(_2x,T(function(){return _1V(_1S,_2v);}));}),_2y=E(_2s);if(_2y[0]==1){var _2z=E(_2o);if(_2z[0]==1){return E(_2u);}else{var _2A=E(_2z[1]);return _2A[0]==1?_1V(_2m,T(function(){return _1V(_2A[1],[2,_2l,T(function(){return _1V(_1S,_2u);})]);})):_1V(_2m,T(function(){return _1V(_2A[1],[2,_2l,T(function(){return _1V(_1S,_2u);})]);}));}}else{return _1V(_2y[1],T(function(){return _1V(_1S,_2u);}));}},_2B=function(_2C){var _2D=E(_2C);return _2n(_2D[1],_2D[2],_2D[3],_2D[4],_2D[6],_1q);},_2E=function(_2F,_2G){var _2H=E(_2F);return _2n(_2H[1],_2H[2],_2H[3],_2H[4],_2H[6],_2G);},_2I=[1,44],_2J=[1,93],_2K=[1,91],_2L=function(_2M,_2N,_2O){var _2P=E(_2N);return _2P[0]==1?unAppCStr("[]",_2O):[2,_2K,T(function(){return A(_2M,[_2P[1],T(function(){var _2Q=function(_2R){var _2S=E(_2R);if(_2S[0]==1){return E([2,_2J,_2O]);}else{return [2,_2I,T(function(){return A(_2M,[_2S[1],T(function(){return _2Q(_2S[2]);})]);})];}};return _2Q(_2P[2]);})]);})];},_2T=function(_2U,_2V){return _2L(_2E,_2U,_2V);},_2W=function(_2X,_2Y,_2Z){var _30=E(_2Y);return _2n(_30[1],_30[2],_30[3],_30[4],_30[6],_2Z);},_31=[1,_2W,_2B,_2T],_32=T(function(){return [1,_1z,_31,_33,_1P];}),_33=function(_34){return [1,_32,_34];},_35=function(_36,_){return die(T(function(){return _33(_36);}));},_37=function(_38,_){return _35(_38,_);},_39=function(_3a){return E(E(_3a)[4]);},_3b=function(_3c){return E(E(_3c)[5]);},_3d=function(_3e){return [1,_3e];},_3f=function(_3g,_){var _3h=E(_3g),_3i=_3h[1],_3j=_3h[4],_3k=_3h[6],_3l=rMV(_3k),_3m=E(_3l),_3n=_3m[5],_3o=_3m[6];if(_3n!=_3o){var _3p=A(_39,[_3i,_3j,_]),_3q=function(_){A(_3b,[_3i,_3j,_1n,T(function(){return _3d(-(_3o-_3n|0));}),_]);wMV(_3k,[1,_3m[1],_3m[2],_3m[3],_3m[4],0,0]);return _0;};if(!E(_3p)){_37(_1s,_);return _3q(_);}else{return _3q(_);}}else{return _0;}},_3r=T(function(){return unCStr("handle is closed");}),_3s=[1,_1p,_1o,_1q,_3r,_1p,_1p],_3t=[1,_1p,_1o,_1q,_3r,_1p,_1p],_3u=T(function(){return unCStr("handle is not open for writing");}),_3v=[1,_1p,_1o,_1q,_3u,_1p,_1p],_3w=T(function(){return unCStr("AsyncException");}),_3x=T(function(){var _3y=hs_wordToWord64(2363394409),_3z=hs_wordToWord64(2156861182);return [1,_3y,_3z,[1,_3y,_3z,_1u,_1t,_3w],_1q];}),_3A=function(_3B){return E(_3x);},_3C=function(_3D,_3E,_3F){var _3G=E(_3D);return [1,[2,_3F],_3G[2],_3E,_3G[4],_3G[5],T(function(){var _3H=E(_3G[6]);if(_3H[0]==1){var _3I=E(_3F);return _3I[0]==1?[2,_3I[1]]:[2,_3I[1]];}else{return E(_3H);}})];},_3J=function(_38,_){return _35(_38,_);},_3K=function(_3L,_3M,_3N,_){return _3J(_3C(_3N,_3L,_3M),_);},_3O=function(_3P,_3Q,_3R,_3S,_){var _3T=takeMVar(_3S);return jsCatch(T(function(){return A(_3R,[_3T]);}),function(_3U,_){var _=putMVar(_3S,_3T),_3V=E(_3U),_3W=_3V[1],_3X=_3V[2],_3Y=_1F(_1B(_3W),_1z,_3X);if(_3Y[0]==1){if(_1F(_1B(_3W),_3A,_3X)[0]==1){return die(_3V);}else{die("Unsupported PrimOp: myThreadId#");var _=die("Unsupported PrimOp: killThread#");return _3O(_3P,_3Q,_3R,_3S,_);}}else{return _3K(_3P,_3Q,_3Y[1],_);}});},_3Z=function(_40,_41,_42,_43,_){var _44=0,_45=function(_){var _46=E(_42)[1],_47=_3O(_40,_41,_43,_46,_),_48=E(_47);putMVar(_46,_48[1]);return _48[2];};return E(_44)==0?_45():_45(_);},_49=function(_4a,_4b,_4c,_4d,_){return _3Z(_4a,_4b,_4c,function(_4e,_){var _4f=E(_4e),_4g=_4f[6],_4h=_4f[9];switch(E(_4f[5])[0]){case 1:var _4i=_37(_3s,_);return [1,_4f,_4i];case 2:var _4j=_37(_3t,_);return [1,_4f,_4j];case 3:var _4k=_37(_3v,_);return [1,_4f,_4k];case 6:var _4l=rMV(_4h);if(E(E(_4l)[3])[0]==1){var _4m=rMV(_4h),_4n=E(_4m),_4o=function(_){_3f(_4f,_);var _4p=rMV(_4h),_=wMV(_4h,T(function(){var _4q=E(_4p);return [1,_4q[1],_4q[2],_1k,_4q[4],_4q[5],_4q[6]];})),_4r=rMV(_4g),_4s=A(_1l,[_4f[2],_4f[4],_4r,_]),_=wMV(_4g,_4s),_4t=A(_4d,[_4f,_]);return [1,_4f,_4t];};if(E(_4n[3])[0]==1){if(_4n[5]!=_4n[6]){var _4u=rMV(_4f[8]),_4v=E(_4u),_4w=_4v[2],_4x=rMV(_4h),_=wMV(_4h,T(function(){var _4y=E(_4x);return [1,_4y[1],_4y[2],_4y[3],_4y[4],0,0];})),_4z=E(_4x),_4A=E(_4z[5]);if(!_4A){var _=wMV(_4g,_4w);return _4o(_);}else{var _4B=E(_4f[12]);if(_4B[0]==1){var _=wMV(_4g,T(function(){var _4C=E(_4w);return [1,_4C[1],_4C[2],_4C[3],_4C[4],_4C[5]+_4A|0,_4C[6]];}));return _4o(_);}else{var _4D=E(_4B[1]);A(_4D[5],[_4v[1],_]);var _4E=_r(_4D[1],_4D[2],_4w,[1,_4z[1],_4z[2],_4z[3],_4A,0,0],_),_=wMV(_4g,E(_4E)[1]);return _4o(_);}}}else{return _4o(_);}}else{return _4o(_);}}else{var _4F=A(_4d,[_4f,_]);return [1,_4f,_4F];}break;default:var _4G=A(_4d,[_4f,_]);return [1,_4f,_4G];}},_);},_4H=function(_4I,_4J,_4K,_){var _4L=E(_4J);return _4L[0]==1?_49(_4I,_4L,[1,_4L[2]],_4K,_):_49(_4I,_4L,[1,_4L[3]],_4K,_);},_4M=function(_4N,_4O,_){return _4H(_1j,_4N,function(_4P,_){var _4Q=E(_4P),_4R=_4Q[6],_4S=rMV(_4Q[9]),_4T=E(_4O);if(_4T==10){var _4U=function(_){if(E(_4Q[7])[0]==2){var _4V=rMV(_4R),_4W=E(_4V);if(_4W[5]!=_4W[6]){var _4X=A(_F,[_4Q[2],_4Q[4],_4W,_]);wMV(_4R,_4X);return _0;}else{return _0;}}else{return _0;}};if(E(_4Q[15])[0]==1){var _4Y=E(_4S),_4Z=_4Y[1],_50=_4Y[6],_=writeOffAddr("w32",4,_4Z,_50,10),_=0;_H(_4Q,_4Z,_4Y[2],_4Y[3],_4Y[4],_4Y[5],_50+1|0,_);return _4U(_);}else{var _51=E(_4S),_52=_51[1],_53=_51[6],_=writeOffAddr("w32",4,_52,_53,13),_=0,_54=_53+1|0,_=writeOffAddr("w32",4,_52,_54,10),_=0;_H(_4Q,_52,_51[2],_51[3],_51[4],_51[5],_54+1|0,_);return _4U(_);}}else{var _55=E(_4S),_56=_55[1],_57=_55[6],_=writeOffAddr("w32",4,_56,_57,_4T),_=0;_H(_4Q,_56,_55[2],_55[3],_55[4],_55[5],_57+1|0,_);return _0;}},_);},_58=T(function(){return unCStr("commitBuffer");}),_59=[1,10],_5a=[2,_59,_1q],_5b=function(_5c,_5d,_5e,_5f,_5g,_5h,_5i,_5j,_){var _5k=function(_5l,_5m,_5n,_){while(1){var _5o=(function(_5p,_5q,_5r,_){var _5s=E(_5q);if(_5s[0]==1){var _5t=E(_5r);if(_5t[0]==1){return _4H(_58,_5c,function(_5u){var _5v=E(_5u),_5w=_5v[10];return E(function(_){_H(_5v,_5g,_5h,_1k,_5i,0,_5p,_);var _5x=rMV(_5v[9]);if(_5i!=E(_5x)[4]){return _0;}else{var _5y=rMV(_5w);wMV(_5w,[2,[1,_5g,_5h],_5y]);return _0;}});},_);}else{var _5z=_5p;_5m=_5t;_5n=_1q;_5l=_5z;return null;}}else{var _5A=_5s[2];if((_5p+1|0)<_5i){var _5B=E(E(_5s[1])[1]);if(_5B==10){var _5C=function(_,_5D){if(!E(_5d)){return _5k(_5D,_5A,_5r,_);}else{_4H(_58,_5c,function(_5E){var _5F=E(_5E),_5G=_5F[6];return E(function(_){_H(_5F,_5g,_5h,_1k,_5i,0,_5D,_);var _5H=rMV(_5G),_5I=E(_5H);if(_5I[5]!=_5I[6]){var _5J=A(_F,[_5F[2],_5F[4],_5I,_]);wMV(_5G,_5J);return _0;}else{return _0;}});},_);return _5k(0,_5A,_5r,_);}};if(E(_5f)[0]==1){var _=writeOffAddr("w32",4,_5g,_5p,10),_=0;return _5C(_,_5p+1|0);}else{var _=writeOffAddr("w32",4,_5g,_5p,13),_=0,_5K=_5p+1|0,_=writeOffAddr("w32",4,_5g,_5K,10),_=0;return _5C(_,_5K+1|0);}}else{writeOffAddr("w32",4,_5g,_5p,_5B);0;var _5z=_5p+1|0;_5m=_5A;var _5L=_5r;_5l=_5z;_5n=_5L;return null;}}else{_4H(_58,_5c,function(_5M){return E(function(_){_H(E(_5M),_5g,_5h,_1k,_5i,0,_5p,_);return _0;});},_);_5l=0;_5m=_5s;var _5L=_5r;_5n=_5L;return null;}}})(_5l,_5m,_5n,_);if(_5o!=null){return _5o;}}};return _5k(0,_5j,T(function(){return E(_5e)==false?[1]:E(_5a);}),_);},_5N=false,_5O=function(_5P,_5Q,_){while(1){var _5R=E(_5Q);if(_5R[0]==1){return _0;}else{_4M(_5P,E(_5R[1])[1],_);_5Q=_5R[2];continue;}}},_5S=[1],_5T=T(function(){return unCStr("no buffer!");}),_5U=T(function(){return err(_5T);}),_5V=[1,_5S,_5U],_5W=[1],_5X=[1,_5W,_1q],_5Y=T(function(){return unCStr("mallocForeignPtrBytes: size must be >= 0");}),_5Z=T(function(){return err(_5Y);}),_60=function(_61,_){var _62=E(_61),_63=_62[10],_64=_62[15],_65=E(_62[7]);if(_65[0]==1){return [1,_5V,_64];}else{var _66=rMV(_63),_67=rMV(_62[9]),_68=E(_66);if(_68[0]==1){var _69=E(_67)[4],_6a=imul(_69,4)|0;if(_6a>=0){var _6b=nMV(_5X),_6c=newByteArr(_6a);return [1,[1,_65,[1,_6c,[2,_6c,E([1,_6b])],_1k,_69,0,0]],_64];}else{return E(_5Z);}}else{wMV(_63,_68[2]);return [1,[1,_65,T(function(){var _6d=E(_68[1]);return [1,_6d[1],_6d[2],_1k,E(_67)[4],0,0];})],_64];}}},_6e=T(function(){return unCStr("hPutStr");}),_6f=function(_6g,_6h,_6i,_){var _6j=_4H(_6e,_6g,_60,_),_6k=E(_6j),_6l=_6k[2],_6m=E(_6k[1]),_6n=_6m[2];switch(E(_6m[1])[0]){case 1:_5O(_6g,_6h,_);return E(_6i)==false?_0:_4M(_6g,10,_);case 2:var _6o=E(_6n);return _5b(_6g,_1,_6i,_6l,_6o[1],_6o[2],_6o[4],_6h,_);default:var _6p=E(_6n);return _5b(_6g,_5N,_6i,_6l,_6p[1],_6p[2],_6p[4],_6h,_);}},_6q=[1,125],_6r=T(function(){return unCStr("defs = ");}),_6s=T(function(){return unCStr(", ");}),_6t=T(function(){return unCStr("libraries = ");}),_6u=T(function(){return unCStr("DefManager {");}),_6v=function(_6w,_6x){var _6y=jsShowI(_6w);return _1V(fromJSStr([1,_6y]),_6x);},_6z=[1,41],_6A=[1,40],_6B=function(_6C,_6D,_6E){return _6D<0==false?_6v(_6D,_6E):_6C>6==false?_6v(_6D,_6E):[2,_6A,T(function(){return _6v(_6D,[2,_6z,_6E]);})];},_6F=function(_6G){return _6B(0,E(_6G)[1],_1q);},_6H=function(_6I,_6J){return _6B(0,E(_6I)[1],_6J);},_6K=function(_6L,_6M){return _2L(_6H,_6L,_6M);},_6N=function(_6O,_6P,_6Q){return _6B(E(_6O)[1],E(_6P)[1],_6Q);},_6R=[1,_6N,_6F,_6K],_6S=[1,125],_6T=T(function(){return unCStr("path = ");}),_6U=T(function(){return unCStr("Library {");}),_6V=[1,0],_6W=[1,34],_6X=T(function(){return unCStr("Node ");}),_6Y=T(function(){return unCStr("Empty");}),_6Z=function(_70){return _1V(_6Y,_70);},_71=T(function(){return unCStr("Current");}),_72=function(_70){return _1V(_71,_70);},_73=T(function(){return unCStr("Up");}),_74=function(_70){return _1V(_73,_70);},_75=T(function(){return unCStr("Root ");}),_76=T(function(){return unCStr("Prelude.(!!): negative index\n");}),_77=T(function(){return err(_76);}),_78=T(function(){return unCStr("Prelude.(!!): index too large\n");}),_79=T(function(){return err(_78);}),_7a=function(_7b,_7c){while(1){var _7d=E(_7b);if(_7d[0]==1){return E(_79);}else{var _7e=E(_7c);if(!_7e){return E(_7d[1]);}else{_7b=_7d[2];_7c=_7e-1|0;continue;}}}},_7f=T(function(){return unCStr("ACK");}),_7g=T(function(){return unCStr("BEL");}),_7h=T(function(){return unCStr("BS");}),_7i=T(function(){return unCStr("SP");}),_7j=[2,_7i,_1q],_7k=T(function(){return unCStr("US");}),_7l=[2,_7k,_7j],_7m=T(function(){return unCStr("RS");}),_7n=[2,_7m,_7l],_7o=T(function(){return unCStr("GS");}),_7p=[2,_7o,_7n],_7q=T(function(){return unCStr("FS");}),_7r=[2,_7q,_7p],_7s=T(function(){return unCStr("ESC");}),_7t=[2,_7s,_7r],_7u=T(function(){return unCStr("SUB");}),_7v=[2,_7u,_7t],_7w=T(function(){return unCStr("EM");}),_7x=[2,_7w,_7v],_7y=T(function(){return unCStr("CAN");}),_7z=[2,_7y,_7x],_7A=T(function(){return unCStr("ETB");}),_7B=[2,_7A,_7z],_7C=T(function(){return unCStr("SYN");}),_7D=[2,_7C,_7B],_7E=T(function(){return unCStr("NAK");}),_7F=[2,_7E,_7D],_7G=T(function(){return unCStr("DC4");}),_7H=[2,_7G,_7F],_7I=T(function(){return unCStr("DC3");}),_7J=[2,_7I,_7H],_7K=T(function(){return unCStr("DC2");}),_7L=[2,_7K,_7J],_7M=T(function(){return unCStr("DC1");}),_7N=[2,_7M,_7L],_7O=T(function(){return unCStr("DLE");}),_7P=[2,_7O,_7N],_7Q=T(function(){return unCStr("SI");}),_7R=[2,_7Q,_7P],_7S=T(function(){return unCStr("SO");}),_7T=[2,_7S,_7R],_7U=T(function(){return unCStr("CR");}),_7V=[2,_7U,_7T],_7W=T(function(){return unCStr("FF");}),_7X=[2,_7W,_7V],_7Y=T(function(){return unCStr("VT");}),_7Z=[2,_7Y,_7X],_80=T(function(){return unCStr("LF");}),_81=[2,_80,_7Z],_82=T(function(){return unCStr("HT");}),_83=[2,_82,_81],_84=[2,_7h,_83],_85=[2,_7g,_84],_86=[2,_7f,_85],_87=T(function(){return unCStr("ENQ");}),_88=[2,_87,_86],_89=T(function(){return unCStr("EOT");}),_8a=[2,_89,_88],_8b=T(function(){return unCStr("ETX");}),_8c=[2,_8b,_8a],_8d=T(function(){return unCStr("STX");}),_8e=[2,_8d,_8c],_8f=T(function(){return unCStr("SOH");}),_8g=[2,_8f,_8e],_8h=T(function(){return unCStr("NUL");}),_8i=[2,_8h,_8g],_8j=[1,92],_8k=T(function(){return unCStr("\\DEL");}),_8l=T(function(){return unCStr("\\a");}),_8m=T(function(){return unCStr("\\\\");}),_8n=T(function(){return unCStr("\\SO");}),_8o=T(function(){return unCStr("\\r");}),_8p=T(function(){return unCStr("\\f");}),_8q=T(function(){return unCStr("\\v");}),_8r=T(function(){return unCStr("\\n");}),_8s=T(function(){return unCStr("\\t");}),_8t=T(function(){return unCStr("\\b");}),_8u=function(_8v,_8w){if(_8v<=127){var _8x=E(_8v);switch(_8x){case 92:return _1V(_8m,_8w);case 127:return _1V(_8k,_8w);default:if(_8x<32){var _8y=E(_8x);switch(_8y){case 7:return _1V(_8l,_8w);case 8:return _1V(_8t,_8w);case 9:return _1V(_8s,_8w);case 10:return _1V(_8r,_8w);case 11:return _1V(_8q,_8w);case 12:return _1V(_8p,_8w);case 13:return _1V(_8o,_8w);case 14:return _1V(_8n,T(function(){var _8z=E(_8w);if(_8z[0]==1){return [1];}else{return E(E(_8z[1])[1])==72?unAppCStr("\\&",_8z):E(_8z);}}));default:return _1V([2,_8j,T(function(){var _8A=_8y;return _8A<0==false?_7a(_8i,_8A):E(_77);})],_8w);}}else{return [2,[1,_8x],_8w];}}}else{return [2,_8j,T(function(){var _8B=jsShowI(_8v);return _1V(fromJSStr([1,_8B]),T(function(){var _8C=E(_8w);if(_8C[0]==1){return [1];}else{var _8D=E(_8C[1])[1];return _8D>=48==false?E(_8C):_8D<=57==false?E(_8C):unAppCStr("\\&",_8C);}}));})];}},_8E=T(function(){return unCStr("\\\"");}),_8F=function(_8G,_8H){var _8I=E(_8G);if(_8I[0]==1){return E(_8H);}else{var _8J=_8I[2],_8K=E(E(_8I[1])[1]);if(_8K==34){return _1V(_8E,T(function(){return _8F(_8J,_8H);}));}else{return _8u(_8K,T(function(){return _8F(_8J,_8H);}));}}},_8L=function(_8M,_8N){var _8O=E(_8N);switch(_8O[0]){case 1:var _8P=_8O[1];if(E(_8M)[1]<11){return E(function(_8Q){return _1V(_6X,[2,_6W,T(function(){return _8F(_8P,[2,_6W,_8Q]);})]);});}else{return E(function(_8R){return [2,_6A,T(function(){return _1V(_6X,[2,_6W,T(function(){return _8F(_8P,[2,_6W,[2,_6z,_8R]]);})]);})];});}break;case 2:var _8S=_8O[1];if(E(_8M)[1]<11){return E(function(_8T){return _1V(_75,[2,_6W,T(function(){return _8F(_8S,[2,_6W,_8T]);})]);});}else{return E(function(_8U){return [2,_6A,T(function(){return _1V(_75,[2,_6W,T(function(){return _8F(_8S,[2,_6W,[2,_6z,_8U]]);})]);})];});}break;case 3:return E(_74);case 4:return E(_72);default:return E(_6Z);}},_8V=function(_8W,_8X,_8Y){var _8Z=function(_90){return _1V(_6U,T(function(){return _1V(_6T,T(function(){var _91=[2,_6S,_90],_92=E(_8X);return _92[0]==1?unAppCStr("[]",_91):[2,_2K,T(function(){return A(_8L,[_6V,_92[1],T(function(){var _93=function(_94){var _95=E(_94);if(_95[0]==1){return E([2,_2J,_91]);}else{return [2,_2I,T(function(){return A(_8L,[_6V,_95[1],T(function(){return _93(_95[2]);})]);})];}};return _93(_92[2]);})]);})];}));}));};return _8W>=11==false?_8Z(_8Y):[2,_6A,T(function(){return _8Z([2,_6z,_8Y]);})];},_96=function(_97){return _8V(0,E(_97)[1],_1q);},_98=function(_99,_9a){return _8V(0,E(_99)[1],_9a);},_9b=function(_9c,_9d){return _2L(_98,_9c,_9d);},_9e=function(_9f,_9g,_9h){return _8V(E(_9f)[1],E(_9g)[1],_9h);},_9i=[1,_9e,_96,_9b],_9j=function(_9k,_9l){while(1){var _9m=(function(_9n,_9o){var _9p=E(_9o);if(_9p[0]==1){_9k=[2,[1,_9p[2],_9p[3]],T(function(){return _9j(_9n,_9p[5]);})];_9l=_9p[4];return null;}else{return E(_9n);}})(_9k,_9l);if(_9m!=null){return _9m;}}},_9q=function(_9r,_9s,_9t){return A(_9r,[[2,_2I,T(function(){return A(_9s,[_9t]);})]]);},_9u=T(function(){return unCStr(": empty list");}),_9v=T(function(){return unCStr("Prelude.");}),_9w=function(_9x){return err(_1V(_9v,T(function(){return _1V(_9x,_9u);})));},_9y=T(function(){return unCStr("foldr1");}),_9z=T(function(){return _9w(_9y);}),_9A=function(_9B,_9C){var _9D=E(_9C);if(_9D[0]==1){return E(_9z);}else{var _9E=_9D[1],_9F=E(_9D[2]);if(_9F[0]==1){return E(_9E);}else{return A(_9B,[_9E,T(function(){return _9A(_9B,_9F);})]);}}},_9G=[1,0],_9H=function(_9I){return E(E(_9I)[1]);},_9J=function(_9K,_9L,_9M,_9N,_9O){return [2,_6A,T(function(){return A(_9A,[_9q,[2,T(function(){return A(_9H,[_9K,_9G,_9M]);}),[2,T(function(){return A(_9H,[_9L,_9G,_9N]);}),_1q]],[2,_6z,_9O]]);})];},_9P=function(_9Q,_9R,_9S,_9T){return _2L(function(_9U,_9V){var _9W=E(_9U);return _9J(_9Q,_9R,_9W[1],_9W[2],_9V);},_9S,_9T);},_9X=T(function(){return unCStr("fromList ");}),_9Y=function(_9Z,_a0,_a1,_a2){var _a3=T(function(){return _9j(_1q,_a2);});if(_a1<=10){return E(function(_a4){return _1V(_9X,T(function(){return _9P(_9Z,_a0,_a3,_a4);}));});}else{return E(function(_a5){return [2,_6A,T(function(){return _1V(_9X,T(function(){return _9P(_9Z,_a0,_a3,[2,_6z,_a5]);}));})];});}},_a6=T(function(){return unCStr("Standard");}),_a7=T(function(){return unCStr("Arrow");}),_a8=[1,125],_a9=T(function(){return unCStr("cls = ");}),_aa=T(function(){return unCStr("target = ");}),_ab=T(function(){return unCStr(", ");}),_ac=T(function(){return unCStr("source = ");}),_ad=T(function(){return unCStr("Edge {");}),_ae=function(_af,_ag,_ah,_ai,_aj){var _ak=function(_al){return _1V(_ad,T(function(){return _1V(_ac,[2,_6W,T(function(){return _8F(_ag,[2,_6W,T(function(){return _1V(_ab,T(function(){return _1V(_aa,[2,_6W,T(function(){return _8F(_ah,[2,_6W,T(function(){return _1V(_ab,T(function(){return _1V(_a9,T(function(){return E(_ai)[0]==1?_1V(_a6,[2,_a8,_al]):_1V(_a7,[2,_a8,_al]);}));}));})]);})]);}));})]);})]);}));};return _af>=11==false?_ak(_aj):[2,_6A,T(function(){return _ak([2,_6z,_aj]);})];},_am=function(_an){var _ao=E(_an);return _ae(0,_ao[1],_ao[2],_ao[3],_1q);},_ap=function(_aq,_ar){var _as=E(_aq);return _ae(0,_as[1],_as[2],_as[3],_ar);},_at=function(_au,_av){return _2L(_ap,_au,_av);},_aw=function(_ax,_ay,_az){var _aA=E(_ay);return _ae(E(_ax)[1],_aA[1],_aA[2],_aA[3],_az);},_aB=[1,_aw,_am,_at],_aC=function(_aD){return E(E(_aD)[3]);},_aE=function(_aF,_aG){return A(_aC,[_aF,_aG,_1q]);},_aH=function(_aI,_aJ,_aK){return _2L(T(function(){return _aC(_aI);}),_aJ,_aK);},_aL=function(_aM){return [1,function(_aN){return E(T(function(){return _aC(_aM);}));},function(_aO){return _aE(_aM,_aO);},function(_aP,_aO){return _aH(_aM,_aP,_aO);}];},_aQ=T(function(){return _aL(_6R);}),_aR=[1,39],_aS=[2,_aR,_1q],_aT=T(function(){return unCStr("\'\\\'\'");}),_aU=function(_aV){var _aW=E(E(_aV)[1]);return _aW==39?E(_aT):[2,_aR,T(function(){return _8u(_aW,_aS);})];},_aX=function(_aY,_aZ){return [2,_6W,T(function(){return _8F(_aY,[2,_6W,_aZ]);})];},_b0=function(_aO){return _1V(_aT,_aO);},_b1=function(_b2,_b3){var _b4=E(E(_b3)[1]);if(_b4==39){return E(_b0);}else{return E(function(_b5){return [2,_aR,T(function(){return _8u(_b4,[2,_aR,_b5]);})];});}},_b6=[1,_b1,_aU,_aX],_b7=T(function(){return _aL(_b6);}),_b8=[1,0],_b9=T(function(){return unCStr("inputs = ");}),_ba=T(function(){return unCStr("NodeDef {");}),_bb=T(function(){return unCStr("NotLoaded");}),_bc=function(_bd){return _1V(_bb,_bd);},_be=[1,125],_bf=T(function(){return unCStr("libID = ");}),_bg=T(function(){return unCStr(", ");}),_bh=T(function(){return unCStr("graph = ");}),_bi=T(function(){return unCStr("imports = ");}),_bj=T(function(){return unCStr("outputs = ");}),_bk=function(_bl,_bm){var _bn=E(_bm);if(_bn[0]==1){return E(_bc);}else{var _bo=function(_bp){return _1V(_ba,T(function(){return _1V(_b9,T(function(){var _bq=T(function(){return _1V(_bg,T(function(){return _1V(_bj,T(function(){var _br=T(function(){return _1V(_bg,T(function(){return _1V(_bi,T(function(){var _bs=T(function(){return _1V(_bg,T(function(){return _1V(_bh,T(function(){return A(T(function(){var _bt=E(_bn[4]);return _bu(0,_bt[1],_bt[2],_bt[3],_bt[4],_bt[5],_bt[6]);}),[T(function(){return _1V(_bg,T(function(){return _1V(_bf,T(function(){return _6B(0,E(_bn[5])[1],[2,_be,_bp]);}));}));})]);}));}));}),_bv=E(_bn[3]);return _bv[0]==1?unAppCStr("[]",_bs):[2,_2K,[2,_6W,T(function(){return _8F(_bv[1],[2,_6W,T(function(){var _bw=function(_bx){var _by=E(_bx);if(_by[0]==1){return E([2,_2J,_bs]);}else{return [2,_2I,[2,_6W,T(function(){return _8F(_by[1],[2,_6W,T(function(){return _bw(_by[2]);})]);})]];}};return _bw(_bv[2]);})]);})]];}));}));}),_bz=E(_bn[2]);return _bz[0]==1?unAppCStr("[]",_br):[2,_2K,[2,_6W,T(function(){return _8F(_bz[1],[2,_6W,T(function(){var _bA=function(_bB){var _bC=E(_bB);if(_bC[0]==1){return E([2,_2J,_br]);}else{return [2,_2I,[2,_6W,T(function(){return _8F(_bC[1],[2,_6W,T(function(){return _bA(_bC[2]);})]);})]];}};return _bA(_bz[2]);})]);})]];}));}));}),_bD=E(_bn[1]);return _bD[0]==1?unAppCStr("[]",_bq):[2,_2K,[2,_6W,T(function(){return _8F(_bD[1],[2,_6W,T(function(){var _bE=function(_bF){var _bG=E(_bF);if(_bG[0]==1){return E([2,_2J,_bq]);}else{return [2,_2I,[2,_6W,T(function(){return _8F(_bG[1],[2,_6W,T(function(){return _bE(_bG[2]);})]);})]];}};return _bE(_bD[2]);})]);})]];}));}));};if(E(_bl)[1]<11){return E(_bo);}else{return E(function(_bH){return [2,_6A,T(function(){return _bo([2,_6z,_bH]);})];});}}},_bI=T(function(){return unCStr("DefaultString ");}),_bJ=T(function(){return unCStr("DefaultInt ");}),_bK=function(_bL,_bM,_bN){var _bO=E(_bM);if(_bO[0]==1){var _bP=_bO[1];return _bL>=11==false?_1V(_bJ,T(function(){return _6B(11,E(_bP)[1],_bN);})):[2,_6A,T(function(){return _1V(_bJ,T(function(){return _6B(11,E(_bP)[1],[2,_6z,_bN]);}));})];}else{var _bQ=_bO[1];return _bL>=11==false?_1V(_bI,[2,_6W,T(function(){return _8F(_bQ,[2,_6W,_bN]);})]):[2,_6A,T(function(){return _1V(_bI,[2,_6W,T(function(){return _8F(_bQ,[2,_6W,[2,_6z,_bN]]);})]);})];}},_bR=T(function(){return unCStr("value = ");}),_bS=T(function(){return unCStr("DefaultNode {");}),_bT=T(function(){return unCStr("def = ");}),_bU=T(function(){return unCStr("name = ");}),_bV=T(function(){return unCStr("PackageNode {");}),_bW=T(function(){return unCStr("FunctionNode {");}),_bX=T(function(){return unCStr("ClassNode {");}),_bY=T(function(){return unCStr("CallNode {");}),_bZ=T(function(){return unCStr("TypeNode {");}),_c0=function(_c1,_c2){var _c3=E(_c2);switch(_c3[0]){case 1:var _c4=function(_c5){return _1V(_bU,[2,_6W,T(function(){return _8F(_c3[1],[2,_6W,[2,_be,_c5]]);})]);};if(_c1<11){return E(function(_c6){return _1V(_bZ,T(function(){return _c4(_c6);}));});}else{return E(function(_c7){return [2,_6A,T(function(){return _1V(_bZ,T(function(){return _c4([2,_6z,_c7]);}));})];});}break;case 2:var _c8=function(_c9){return _1V(_bU,[2,_6W,T(function(){return _8F(_c3[1],[2,_6W,[2,_be,_c9]]);})]);};if(_c1<11){return E(function(_ca){return _1V(_bY,T(function(){return _c8(_ca);}));});}else{return E(function(_cb){return [2,_6A,T(function(){return _1V(_bY,T(function(){return _c8([2,_6z,_cb]);}));})];});}break;case 3:var _cc=function(_cd){return _1V(_bX,T(function(){return _1V(_bU,[2,_6W,T(function(){return _8F(_c3[1],[2,_6W,T(function(){return _1V(_bg,T(function(){return _1V(_bT,T(function(){return A(T(function(){return _bk(_b8,_c3[2]);}),[[2,_be,_cd]]);}));}));})]);})]);}));};if(_c1<11){return E(_cc);}else{return E(function(_ce){return [2,_6A,T(function(){return _cc([2,_6z,_ce]);})];});}break;case 4:var _cf=function(_cg){return _1V(_bW,T(function(){return _1V(_bU,[2,_6W,T(function(){return _8F(_c3[1],[2,_6W,T(function(){return _1V(_bg,T(function(){return _1V(_bT,T(function(){return A(T(function(){return _bk(_b8,_c3[2]);}),[[2,_be,_cg]]);}));}));})]);})]);}));};if(_c1<11){return E(_cf);}else{return E(function(_ch){return [2,_6A,T(function(){return _cf([2,_6z,_ch]);})];});}break;case 5:var _ci=function(_cj){return _1V(_bV,T(function(){return _1V(_bU,[2,_6W,T(function(){return _8F(_c3[1],[2,_6W,T(function(){return _1V(_bg,T(function(){return _1V(_bT,T(function(){return A(T(function(){return _bk(_b8,_c3[2]);}),[[2,_be,_cj]]);}));}));})]);})]);}));};if(_c1<11){return E(_ci);}else{return E(function(_ck){return [2,_6A,T(function(){return _ci([2,_6z,_ck]);})];});}break;default:var _cl=function(_cm){return _1V(_bS,T(function(){return _1V(_bR,T(function(){return _bK(0,_c3[1],[2,_be,_cm]);}));}));};if(_c1<11){return E(_cl);}else{return E(function(_cn){return [2,_6A,T(function(){return _cl([2,_6z,_cn]);})];});}}},_co=function(_cp){return A(_c0,[0,_cp,_1q]);},_cq=function(_cr){return _c0(0,_cr);},_cs=function(_ct,_bd){return _2L(_cq,_ct,_bd);},_cu=function(_cv,_cw){return _c0(E(_cv)[1],_cw);},_cx=T(function(){return [1,_cu,_co,_cs];}),_cy=T(function(){return unCStr("packages = ");}),_cz=T(function(){return unCStr("functions = ");}),_cA=T(function(){return unCStr("classes = ");}),_cB=T(function(){return unCStr("calls = ");}),_cC=T(function(){return unCStr("types = ");}),_cD=T(function(){return unCStr("repr = ");}),_cE=T(function(){return unCStr("Graph {");}),_cF=function(_cG){return E(_cG);},_cH=T(function(){return unCStr("->");}),_cI=[1,58],_cJ=[1,10],_cK=function(_cL,_cM,_cN){var _cO=E(_cN);if(_cO[0]==1){return E(_cF);}else{var _cP=E(_cO[3]),_cQ=E(_cP[2]);return E(function(_cR){return A(T(function(){return _cK(_cL,_cM,_cO[2]);}),[[2,_cJ,T(function(){return _6B(0,E(_cP[1])[1],[2,_cI,T(function(){return A(T(function(){return A(_9H,[_cL,_9G,_cQ[2]]);}),[T(function(){return _1V(_cH,T(function(){return _9P(_cM,_6R,_cQ[3],T(function(){return A(T(function(){return _cK(_cL,_cM,_cO[4]);}),[_cR]);}));}));})]);})]);})]]);});}},_bu=function(_cS,_cT,_cU,_cV,_cW,_cX,_cY){var _cZ=function(_d0){return _1V(_cD,T(function(){return A(T(function(){return _cK(_cx,_aB,E(_cT)[1]);}),[T(function(){return _1V(_bg,T(function(){return _1V(_cC,T(function(){return A(T(function(){return _9Y(_b7,_6R,0,_cU);}),[T(function(){return _1V(_bg,T(function(){return _1V(_cB,T(function(){return A(T(function(){return _9Y(_b7,_6R,0,_cV);}),[T(function(){return _1V(_bg,T(function(){return _1V(_cA,T(function(){return A(T(function(){return _9Y(_b7,_6R,0,_cW);}),[T(function(){return _1V(_bg,T(function(){return _1V(_cz,T(function(){return _1V(T(function(){return A(_9Y,[_b7,_aQ,0,T(function(){return E(E(_cX)[3]);}),_1q]);}),T(function(){return _1V(_bg,T(function(){return _1V(_cy,T(function(){return A(T(function(){return _9Y(_b7,_6R,0,_cY);}),[[2,_be,_d0]]);}));}));}));}));}));})]);}));}));})]);}));}));})]);}));}));})]);}));};if(_cS<11){return E(function(_d1){return _1V(_cE,T(function(){return _cZ(_d1);}));});}else{return E(function(_d2){return [2,_6A,T(function(){return _1V(_cE,T(function(){return _cZ([2,_6z,_d2]);}));})];});}},_d3=function(_d4,_d5,_d6){var _d7=function(_d8){return _1V(_6u,T(function(){return _1V(_6t,T(function(){return A(T(function(){return _9Y(_6R,_9i,0,_d5);}),[T(function(){return _1V(_6s,T(function(){return _1V(_6r,T(function(){return A(T(function(){var _d9=E(_d6);return _bu(0,_d9[1],_d9[2],_d9[3],_d9[4],_d9[5],_d9[6]);}),[[2,_6q,_d8]]);}));}));})]);}));}));};if(_d4<11){return E(_d7);}else{return E(function(_da){return [2,_6A,T(function(){return _d7([2,_6z,_da]);})];});}},_db=[2],_dc=T(function(){return unCStr("Failure in Data.Map.balanceL");}),_dd=T(function(){return err(_dc);}),_de=function(_df,_dg,_dh,_di){var _dj=E(_di);if(_dj[0]==1){var _dk=_dj[1],_dl=E(_dh);if(_dl[0]==1){var _dm=_dl[1],_dn=_dl[2],_do=_dl[3];if(_dm<=(imul(3,_dk)|0)){return [1,(1+_dm|0)+_dk|0,E(E(_df)),_dg,E(_dl),E(_dj)];}else{var _dp=E(_dl[4]);if(_dp[0]==1){var _dq=_dp[1],_dr=E(_dl[5]);if(_dr[0]==1){var _ds=_dr[1],_dt=_dr[2],_du=_dr[3],_dv=_dr[4];if(_ds>=(imul(2,_dq)|0)){var _dw=function(_dx){var _dy=E(_dr[5]);if(_dy[0]==1){return [1,(1+_dm|0)+_dk|0,E(_dt),_du,E([1,(1+_dq|0)+_dx|0,E(_dn),_do,E(_dp),E(_dv)]),E([1,(1+_dk|0)+_dy[1]|0,E(E(_df)),_dg,E(_dy),E(_dj)])];}else{return [1,(1+_dm|0)+_dk|0,E(_dt),_du,E([1,(1+_dq|0)+_dx|0,E(_dn),_do,E(_dp),E(_dv)]),E([1,1+_dk|0,E(E(_df)),_dg,E(_db),E(_dj)])];}},_dz=E(_dv);return _dz[0]==1?_dw(_dz[1]):_dw(0);}else{return [1,(1+_dm|0)+_dk|0,E(_dn),_do,E(_dp),E([1,(1+_dk|0)+_ds|0,E(E(_df)),_dg,E(_dr),E(_dj)])];}}else{return E(_dd);}}else{return E(_dd);}}}else{return [1,1+_dk|0,E(E(_df)),_dg,E(_db),E(_dj)];}}else{var _dA=E(_dh);if(_dA[0]==1){var _dB=_dA[1],_dC=_dA[2],_dD=_dA[3],_dE=_dA[5],_dF=E(_dA[4]);if(_dF[0]==1){var _dG=_dF[1],_dH=E(_dE);if(_dH[0]==1){var _dI=_dH[1],_dJ=_dH[2],_dK=_dH[3],_dL=_dH[4];if(_dI>=(imul(2,_dG)|0)){var _dM=function(_dN){var _dO=E(_dH[5]);if(_dO[0]==1){return [1,1+_dB|0,E(_dJ),_dK,E([1,(1+_dG|0)+_dN|0,E(_dC),_dD,E(_dF),E(_dL)]),E([1,1+_dO[1]|0,E(E(_df)),_dg,E(_dO),E(_db)])];}else{return [1,1+_dB|0,E(_dJ),_dK,E([1,(1+_dG|0)+_dN|0,E(_dC),_dD,E(_dF),E(_dL)]),E([1,1,E(E(_df)),_dg,E(_db),E(_db)])];}},_dP=E(_dL);return _dP[0]==1?_dM(_dP[1]):_dM(0);}else{return [1,1+_dB|0,E(_dC),_dD,E(_dF),E([1,1+_dI|0,E(E(_df)),_dg,E(_dH),E(_db)])];}}else{return [1,3,E(_dC),_dD,E(_dF),E([1,1,E(E(_df)),_dg,E(_db),E(_db)])];}}else{var _dQ=E(_dE);if(_dQ[0]==1){return [1,3,E(_dQ[2]),_dQ[3],E([1,1,E(_dC),_dD,E(_db),E(_db)]),E([1,1,E(E(_df)),_dg,E(_db),E(_db)])];}else{return [1,2,E(E(_df)),_dg,E(_dA),E(_db)];}}}else{return [1,1,E(E(_df)),_dg,E(_db),E(_db)];}}},_dR=T(function(){return unCStr("Failure in Data.Map.balanceR");}),_dS=T(function(){return err(_dR);}),_dT=function(_dU,_dV,_dW,_dX){var _dY=E(_dW);if(_dY[0]==1){var _dZ=_dY[1],_e0=E(_dX);if(_e0[0]==1){var _e1=_e0[1],_e2=_e0[2],_e3=_e0[3];if(_e1<=(imul(3,_dZ)|0)){return [1,(1+_dZ|0)+_e1|0,E(E(_dU)),_dV,E(_dY),E(_e0)];}else{var _e4=E(_e0[4]);if(_e4[0]==1){var _e5=_e4[1],_e6=_e4[2],_e7=_e4[3],_e8=_e4[4],_e9=E(_e0[5]);if(_e9[0]==1){var _ea=_e9[1];if(_e5>=(imul(2,_ea)|0)){var _eb=function(_ec){var _ed=E(_dU),_ee=E(_e4[5]);if(_ee[0]==1){return [1,(1+_dZ|0)+_e1|0,E(_e6),_e7,E([1,(1+_dZ|0)+_ec|0,E(_ed),_dV,E(_dY),E(_e8)]),E([1,(1+_ea|0)+_ee[1]|0,E(_e2),_e3,E(_ee),E(_e9)])];}else{return [1,(1+_dZ|0)+_e1|0,E(_e6),_e7,E([1,(1+_dZ|0)+_ec|0,E(_ed),_dV,E(_dY),E(_e8)]),E([1,1+_ea|0,E(_e2),_e3,E(_db),E(_e9)])];}},_ef=E(_e8);return _ef[0]==1?_eb(_ef[1]):_eb(0);}else{return [1,(1+_dZ|0)+_e1|0,E(_e2),_e3,E([1,(1+_dZ|0)+_e5|0,E(E(_dU)),_dV,E(_dY),E(_e4)]),E(_e9)];}}else{return E(_dS);}}else{return E(_dS);}}}else{return [1,1+_dZ|0,E(E(_dU)),_dV,E(_dY),E(_db)];}}else{var _eg=E(_dX);if(_eg[0]==1){var _eh=_eg[1],_ei=_eg[2],_ej=_eg[3],_ek=_eg[5],_el=E(_eg[4]);if(_el[0]==1){var _em=_el[1],_en=_el[2],_eo=_el[3],_ep=_el[4],_eq=E(_ek);if(_eq[0]==1){var _er=_eq[1];if(_em>=(imul(2,_er)|0)){var _es=function(_et){var _eu=E(_dU),_ev=E(_el[5]);if(_ev[0]==1){return [1,1+_eh|0,E(_en),_eo,E([1,1+_et|0,E(_eu),_dV,E(_db),E(_ep)]),E([1,(1+_er|0)+_ev[1]|0,E(_ei),_ej,E(_ev),E(_eq)])];}else{return [1,1+_eh|0,E(_en),_eo,E([1,1+_et|0,E(_eu),_dV,E(_db),E(_ep)]),E([1,1+_er|0,E(_ei),_ej,E(_db),E(_eq)])];}},_ew=E(_ep);return _ew[0]==1?_es(_ew[1]):_es(0);}else{return [1,1+_eh|0,E(_ei),_ej,E([1,1+_em|0,E(E(_dU)),_dV,E(_db),E(_el)]),E(_eq)];}}else{return [1,3,E(_en),_eo,E([1,1,E(E(_dU)),_dV,E(_db),E(_db)]),E([1,1,E(_ei),_ej,E(_db),E(_db)])];}}else{var _ex=E(_ek);if(_ex[0]==1){return [1,3,E(_ei),_ej,E([1,1,E(E(_dU)),_dV,E(_db),E(_db)]),E(_ex)];}else{return [1,2,E(E(_dU)),_dV,E(_db),E(_eg)];}}}else{return [1,1,E(E(_dU)),_dV,E(_db),E(_db)];}}},_ey=function(_ez,_eA,_eB){var _eC=E(_ez),_eD=_eC[1],_eE=E(_eB);if(_eE[0]==1){var _eF=_eE[3],_eG=_eE[4],_eH=_eE[5],_eI=E(_eE[2]),_eJ=_eI[1];if(_eD>=_eJ){if(_eD!=_eJ){return _dT(_eI,_eF,_eG,_ey(_eC,_eA,_eH));}else{return [1,_eE[1],E(_eC),_eA,E(_eG),E(_eH)];}}else{return _de(_eI,_eF,_ey(_eC,_eA,_eG),_eH);}}else{return [1,1,E(_eC),_eA,E(_db),E(_db)];}},_eK=function(_eL,_eM){while(1){var _eN=E(_eM);if(_eN[0]==1){return E(_eL);}else{var _eO=E(_eN[1]),_eP=_ey(_eO[1],_eO[2],_eL);_eM=_eN[2];_eL=_eP;continue;}}},_eQ=function(_eR,_eS){return [1,1,E(E(_eR)),_eS,E(_db),E(_db)];},_eT=function(_eU,_eV,_eW){var _eX=E(_eW);if(_eX[0]==1){return _dT(_eX[2],_eX[3],_eX[4],_eT(_eU,_eV,_eX[5]));}else{return _eQ(_eU,_eV);}},_eY=function(_eZ,_f0,_f1){var _f2=E(_f1);if(_f2[0]==1){return _de(_f2[2],_f2[3],_eY(_eZ,_f0,_f2[4]),_f2[5]);}else{return _eQ(_eZ,_f0);}},_f3=function(_f4,_f5,_f6,_f7,_f8,_f9,_fa,_fb){var _fc=E(_f6);if(_fc[0]==1){var _fd=_fc[1],_fe=_fc[2],_ff=_fc[3],_fg=_fc[4],_fh=_fc[5];if((imul(3,_fd)|0)>=_f7){if((imul(3,_f7)|0)>=_fd){return [1,(_fd+_f7|0)+1|0,E(E(_f4)),_f5,E(_fc),E([1,_f7,E(_f8),_f9,E(_fa),E(_fb)])];}else{return _dT(_fe,_ff,_fg,_f3(_f4,_f5,_fh,_f7,_f8,_f9,_fa,_fb));}}else{return _de(_f8,_f9,_fi(_f4,_f5,_fd,_fe,_ff,_fg,_fh,_fa),_fb);}}else{return _eY(_f4,_f5,[1,_f7,E(_f8),_f9,E(_fa),E(_fb)]);}},_fi=function(_fj,_fk,_fl,_fm,_fn,_fo,_fp,_fq){var _fr=E(_fq);if(_fr[0]==1){var _fs=_fr[1],_ft=_fr[2],_fu=_fr[3],_fv=_fr[4],_fw=_fr[5];if((imul(3,_fl)|0)>=_fs){if((imul(3,_fs)|0)>=_fl){return [1,(_fl+_fs|0)+1|0,E(E(_fj)),_fk,E([1,_fl,E(_fm),_fn,E(_fo),E(_fp)]),E(_fr)];}else{return _dT(_fm,_fn,_fo,_f3(_fj,_fk,_fp,_fs,_ft,_fu,_fv,_fw));}}else{return _de(_ft,_fu,_fi(_fj,_fk,_fl,_fm,_fn,_fo,_fp,_fv),_fw);}}else{return _eT(_fj,_fk,[1,_fl,E(_fm),_fn,E(_fo),E(_fp)]);}},_fx=function(_fy,_fz,_fA,_fB){var _fC=E(_fA);if(_fC[0]==1){var _fD=_fC[1],_fE=_fC[2],_fF=_fC[3],_fG=_fC[4],_fH=_fC[5],_fI=E(_fB);if(_fI[0]==1){var _fJ=_fI[1],_fK=_fI[2],_fL=_fI[3],_fM=_fI[4],_fN=_fI[5];if((imul(3,_fD)|0)>=_fJ){if((imul(3,_fJ)|0)>=_fD){return [1,(_fD+_fJ|0)+1|0,E(E(_fy)),_fz,E(_fC),E(_fI)];}else{return _dT(_fE,_fF,_fG,_f3(_fy,_fz,_fH,_fJ,_fK,_fL,_fM,_fN));}}else{return _de(_fK,_fL,_fi(_fy,_fz,_fD,_fE,_fF,_fG,_fH,_fM),_fN);}}else{return _eT(_fy,_fz,_fC);}}else{return _eY(_fy,_fz,_fB);}},_fO=function(_fP,_fQ){var _fR=E(_fQ);if(_fR[0]==1){return [1,_db,_1q,_1q];}else{var _fS=E(_fP);if(_fS==1){var _fT=E(_fR[1]),_fU=_fT[1],_fV=_fT[2],_fW=E(_fR[2]);if(_fW[0]==1){return [1,T(function(){return [1,1,E(E(_fU)),_fV,E(_db),E(_db)];}),_1q,_1q];}else{var _fX=E(_fU);return _fX[1]>=E(E(_fW[1])[1])[1]==false?[1,[1,1,E(_fX),_fV,E(_db),E(_db)],_fW,_1q]:[1,[1,1,E(_fX),_fV,E(_db),E(_db)],_1q,_fW];}}else{var _fY=_fO(_fS>>1,_fR),_fZ=_fY[1],_g0=_fY[3],_g1=E(_fY[2]);if(_g1[0]==1){return [1,_fZ,_1q,_g0];}else{var _g2=E(_g1[1]),_g3=_g2[1],_g4=_g2[2],_g5=E(_g1[2]);if(_g5[0]==1){return [1,T(function(){return _eT(_g3,_g4,_fZ);}),_1q,_g0];}else{var _g6=E(_g3);if(_g6[1]<E(E(_g5[1])[1])[1]){var _g7=_fO(_fS>>1,_g5);return [1,T(function(){return _fx(_g6,_g4,_fZ,_g7[1]);}),_g7[2],_g7[3]];}else{return [1,_fZ,_1q,_g1];}}}}}},_g8=function(_g9,_ga,_gb){while(1){var _gc=E(_gb);if(_gc[0]==1){return E(_ga);}else{var _gd=E(_gc[1]),_ge=_gd[1],_gf=_gd[2],_gg=E(_gc[2]);if(_gg[0]==1){return _eT(_ge,_gf,_ga);}else{var _gh=E(_ge);if(_gh[1]<E(E(_gg[1])[1])[1]){var _gi=_fO(_g9,_gg),_gj=_gi[1],_gk=E(_gi[3]);if(_gk[0]==1){var _gl=_g9<<1,_gm=_fx(_gh,_gf,_ga,_gj);_gb=_gi[2];_g9=_gl;_ga=_gm;continue;}else{return _eK(_fx(_gh,_gf,_ga,_gj),_gk);}}else{return _eK(_ga,_gc);}}}}},_gn=function(_go){var _gp=E(_go);if(_gp[0]==1){return [2];}else{var _gq=E(_gp[1]),_gr=_gq[1],_gs=_gq[2],_gt=E(_gp[2]);if(_gt[0]==1){return [1,1,E(E(_gr)),_gs,E(_db),E(_db)];}else{var _gu=E(_gr);return _gu[1]>=E(E(_gt[1])[1])[1]==false?_g8(1,[1,1,E(_gu),_gs,E(_db),E(_db)],_gt):_eK([1,1,E(_gu),_gs,E(_db),E(_db)],_gt);}}},_gv=function(_gw,_gx){return E(_gw)[1]!=E(_gx)[1];},_gy=function(_gz,_gA){return E(_gz)[1]==E(_gA)[1];},_gB=[1,_gy,_gv],_gC=[1,47],_gD=[2,_gC,_1q],_gE=function(_gF){return E(E(_gF)[1]);},_gG=[3],_gH=[2,_1q],_gI=[2,_gH,_1q],_gJ=function(_gK){while(1){var _gL=(function(_gM){var _gN=E(_gM);if(_gN[0]==1){return [1];}else{var _gO=_gN[2],_gP=E(_gN[1]);if(_gP[0]==1){_gK=_gO;return null;}else{return [2,_gP,T(function(){return _gJ(_gO);})];}}})(_gK);if(_gL!=null){return _gL;}}},_gQ=function(_gR){var _gS=E(_gR);return _gS[0]==1?E(_gS[1]):E(_gS[1]);},_gT=[2],_gU=[2,_gH,_1q],_gV=function(_gW,_gX){var _gY=E(_gX);if(_gY[0]==1){return [1];}else{var _gZ=_gY[1],_h0=_gY[2],_h1=T(function(){var _h2=E(_gZ);if(_h2[0]==1){var _h3=E(_h0);return _h3[0]==1?[2,_h2,_gU]:E(_h3[1])[0]==1?[2,_h2,[2,_gH,T(function(){return _gV(_gW,_h3);})]]:[2,_h2,T(function(){return _gV(_gW,_h3);})];}else{return [2,_h2,T(function(){return _gV(_gW,_h0);})];}});if(E(_gW)[0]==2){var _h4=E(_gZ);if(_h4[0]==1){var _h5=E(_h0);if(_h5[0]==1){return E(_h1);}else{if(E(_h5[1])[0]==1){return [2,_h4,T(function(){return _gV(_gT,_h5);})];}else{return E(_h1);}}}else{return E(_h1);}}else{return E(_h1);}}},_h6=function(_h7,_h8){var _h9=E(_h8);if(_h9[0]==1){return [1];}else{return [2,T(function(){return A(_h7,[_h9[1]]);}),T(function(){return _h6(_h7,_h9[2]);})];}},_ha=function(_hb,_hc){var _hd=E(_hb);if(_hd[0]==1){return [2,[1,_1q,_hc]];}else{var _he=E(_hc);if(_he[0]==1){return [1];}else{var _hf=_he[1];if(!A(_hd[1],[_hf])){return [1];}else{var _hg=_ha(_hd[2],_he[2]);if(_hg[0]==1){return [1];}else{var _hh=E(_hg[1]);return [2,[1,[2,_hf,_hh[1]],_hh[2]]];}}}}},_hi=function(_hj,_hk){var _hl=E(_hj);if(_hl[0]==1){return [1,_1q,[2,[1,_1q,_hk]]];}else{var _hm=E(_hk);if(_hm[0]==1){return [1,_1q,_1p];}else{var _hn=_ha(_hl,_hm);if(_hn[0]==1){var _ho=T(function(){var _hp=_hi(_hl,_hm[2]);return [1,_hp[1],_hp[2]];});return [1,[2,_hm[1],T(function(){return E(E(_ho)[1]);})],T(function(){return E(E(_ho)[2]);})];}else{return [1,_1q,_hn];}}}},_hq=[1,_1q],_hr=function(_hs,_ht){var _hu=E(_ht);if(_hu[0]==1){return [1];}else{var _hv=_hi(_hs,_hu),_hw=_hv[2],_hx=function(_hy){var _hz=E(_hy);if(_hz[0]==1){return [1];}else{var _hA=E(_hz[1]),_hB=_hA[2],_hC=E(_hA[1]);if(_hC[0]==1){var _hD=E(_hB);return _hD[0]==1?E([2,_hq,T(function(){return _hr(_hs,_1q);})]):[2,_hq,[2,[2,[2,_hD[1],_1q]],T(function(){return _hr(_hs,_hD[2]);})]];}else{return [2,[1,_hC],T(function(){return _hr(_hs,_hB);})];}}},_hE=E(_hv[1]);return _hE[0]==1?_hx(_hw):[2,[2,_hE],T(function(){return _hx(_hw);})];}},_hF=function(_hG,_hH){return E(function(_hI){var _hJ=_hr(T(function(){return _h6(T(function(){return _gE(_hG);}),_hH);}),_hI);if(_hJ[0]==1){var _hK=_gJ(_gI);return _hK[0]==1?_h6(_gQ,_1q):_h6(_gQ,_hK);}else{if(E(_hJ[1])[0]==1){var _hL=_gJ([2,_gH,T(function(){return _gV(_gG,_hJ);})]);return _hL[0]==1?_h6(_gQ,_1q):_h6(_gQ,_hL);}else{var _hM=_gJ(_gV(_gG,_hJ));return _hM[0]==1?_h6(_gQ,_1q):_h6(_gQ,_hM);}}});},_hN=T(function(){return _hF(_gB,_gD);}),_hO=[2,_gC,_1q],_hP=[2,_gC,_1q],_hQ=T(function(){return _hF(_gB,_hP);}),_hR=function(_hS,_hT){while(1){var _hU=E(_hS);if(_hU[0]==1){return E(_hT)[0]==1?true:false;}else{var _hV=E(_hT);if(_hV[0]==1){return false;}else{if(E(_hU[1])[1]!=E(_hV[1])[1]){return false;}else{_hS=_hU[2];_hT=_hV[2];continue;}}}}},_hW=[2,_gC,_1q],_hX=[2,_hW],_hY=T(function(){return unCStr("..");}),_hZ=function(_i0){var _i1=T(function(){if(!_hR(_i0,_hY)){var _i2=E(_i0);if(_i2[0]==1){return [5];}else{return E(E(_i2[1])[1])==46?E(_i2[2])[0]==1?[4]:[1,_i2]:[1,_i2];}}else{return [3];}}),_i3=E(_i0);if(_i3[0]==1){return E(_i1);}else{return E(E(_i3[1])[1])==47?E(_i3[2])[0]==1?E(_hX):E(_i1):E(_i1);}},_i4=function(_i5){var _i6=E(_i5);if(_i6[0]==1){return [1];}else{return [2,T(function(){return _hZ(_i6[1]);}),T(function(){return _i4(_i6[2]);})];}},_i7=function(_i8){var _i9=E(_i8);if(_i9[0]==1){return [1];}else{return [2,T(function(){return _hZ(_i9[1]);}),T(function(){return _i7(_i9[2]);})];}},_ia=function(_ib){var _ic=E(_ib);if(_ic[0]==1){return [1];}else{if(E(E(_ic[1])[1])==47){return _i4([2,_hO,T(function(){return A(_hN,[_ic[2]]);})]);}else{return _i7(A(_hQ,[_ic]));}}},_id=T(function(){return unCStr("workspace");}),_ie=T(function(){return _ia(_id);}),_if=[1,_ie],_ig=[1,1],_ih=[1,_ig,_if],_ii=[2,_ih,_1q],_ij=T(function(){return unCStr("stdlib");}),_ik=T(function(){return _ia(_ij);}),_il=[1,_ik],_im=[1,0],_in=[1,_im,_il],_io=[2,_in,_ii],_ip=T(function(){return _gn(_io);}),_iq=[1],_ir=[1,_iq],_is=[1,0],_it=[1,_is,_is,_db],_iu=[1,_ir,_db,_db,_db,_it,_db],_iv=function(_iw,_ix){while(1){var _iy=E(_iw);if(_iy[0]==1){return E(_ix)[0]==1?[2]:[1];}else{var _iz=E(_ix);if(_iz[0]==1){return [3];}else{var _iA=E(_iy[1])[1],_iB=E(_iz[1])[1];if(_iA!=_iB){return _iA<=_iB==false?[3]:[1];}else{_iw=_iy[2];_ix=_iz[2];continue;}}}}},_iC=function(_iD,_iE,_iF){var _iG=E(_iD),_iH=E(_iF);if(_iH[0]==1){var _iI=_iH[2],_iJ=_iH[3],_iK=_iH[4],_iL=_iH[5];switch(_iv(_iG,_iI)[0]){case 1:return _de(_iI,_iJ,_iC(_iG,_iE,_iK),_iL);case 2:return [1,_iH[1],E(_iG),_iE,E(_iK),E(_iL)];default:return _dT(_iI,_iJ,_iK,_iC(_iG,_iE,_iL));}}else{return [1,1,E(_iG),_iE,E(_db),E(_db)];}},_iM=function(_iN){return err(unAppCStr("Node Exception, Node: ",T(function(){return _6B(0,_iN,_1q);})));},_iO=function(_iP,_iQ){return E(_iP)[1]==E(_iQ)[1];},_iR=function(_iS,_iT){return E(_iS)[1]!=E(_iT)[1];},_iU=[1,_iO,_iR],_iV=function(_iW,_iX){var _iY=E(_iW),_iZ=E(_iX);return _iY[1]<=_iZ[1]==false?E(_iY):E(_iZ);},_j0=function(_j1,_j2){var _j3=E(_j1),_j4=E(_j2);return _j3[1]<=_j4[1]==false?E(_j4):E(_j3);},_j5=function(_j6,_j7){return _j6<_j7==false?_j6==_j7==false?[3]:[2]:[1];},_j8=function(_j9,_ja){return _j5(E(_j9)[1],E(_ja)[1]);},_jb=function(_jc,_jd){return E(_jc)[1]>=E(_jd)[1];},_je=function(_jf,_jg){return E(_jf)[1]>E(_jg)[1];},_jh=function(_ji,_jj){return E(_ji)[1]<=E(_jj)[1];},_jk=function(_jl,_jm){return E(_jl)[1]<E(_jm)[1];},_jn=[1,_iU,_j8,_jk,_jb,_je,_jh,_iV,_j0],_jo=[1,1],_jp=function(_jq,_jr,_js){return [2,T(function(){var _jt=E(_jq);if(_jt[0]==1){var _ju=E(_js);if(_ju[0]==1){return E(_jo);}else{var _jv=E(_ju[1])[1];if(0>_jv){return E(_jo);}else{return [1,1+_jv|0];}}}else{var _jw=E(_jt[1])[1],_jx=E(_js);if(_jx[0]==1){if(_jw>0){return [1,1+_jw|0];}else{return E(_jo);}}else{var _jy=E(_jx[1])[1];if(_jw>_jy){return [1,1+_jw|0];}else{return [1,1+_jy|0];}}}}),_jq,_jr,_js];},_jz=T(function(){return unCStr("rotl on invalid FiniteMap");}),_jA=T(function(){return err(_jz);}),_jB=function(_jC){var _jD=E(_jC);if(_jD[0]==1){return [1];}else{var _jE=E(_jD[4]);return _jE[0]==1?E(_jA):_jp(T(function(){return _jp(_jD[2],_jD[3],_jE[2]);}),_jE[3],_jE[4]);}},_jF=T(function(){return unCStr("rotr on invalid FiniteMap");}),_jG=T(function(){return err(_jF);}),_jH=function(_jI){var _jJ=E(_jI);if(_jJ[0]==1){return [1];}else{var _jK=E(_jJ[2]);return _jK[0]==1?E(_jG):_jp(_jK[2],_jK[3],T(function(){return _jp(_jK[4],_jJ[3],_jJ[4]);}));}},_jL=function(_jM,_jN,_jO,_jP){var _jQ=function(_jR){var _jS=function(_jT){var _jU=T(function(){if((_jR+1|0)>=_jT){var _jV=T(function(){if((_jT+1|0)>=_jR){return _jp(_jM,[1,_jN,_jO],_jP);}else{return _jB(_jp(_jM,[1,_jN,_jO],_jP));}});if((_jT+1|0)>=_jR){return E(_jV);}else{var _jW=function(_jX){if(0>=_jX){return E(_jV);}else{return _jB(_jp(_jM,[1,_jN,_jO],T(function(){return _jH(_jP);})));}},_jY=E(_jP);if(_jY[0]==1){return _jW(0);}else{var _jZ=_jY[4],_k0=E(_jY[2]);if(_k0[0]==1){var _k1=E(_jZ);if(_k1[0]==1){return _jW(0);}else{return _jW(-E(_k1[1])[1]|0);}}else{var _k2=E(_k0[1])[1],_k3=E(_jZ);if(_k3[0]==1){return _jW(_k2);}else{return _jW(_k2-E(_k3[1])[1]|0);}}}}}else{return _jH(_jp(_jM,[1,_jN,_jO],_jP));}});if((_jR+1|0)>=_jT){return E(_jU);}else{var _k4=function(_k5){if(_k5>=0){return E(_jU);}else{return _jH(_jp(T(function(){return _jB(_jM);}),[1,_jN,_jO],_jP));}},_k6=E(_jM);if(_k6[0]==1){return _k4(0);}else{var _k7=_k6[4],_k8=E(_k6[2]);if(_k8[0]==1){var _k9=E(_k7);if(_k9[0]==1){return _k4(0);}else{return _k4(-E(_k9[1])[1]|0);}}else{var _ka=E(_k8[1])[1],_kb=E(_k7);if(_kb[0]==1){return _k4(_ka);}else{return _k4(_ka-E(_kb[1])[1]|0);}}}}},_kc=E(_jM);if(_kc[0]==1){return _jS(0);}else{return _jS(E(_kc[1])[1]);}},_kd=E(_jP);if(_kd[0]==1){return _jQ(0);}else{return _jQ(E(_kd[1])[1]);}},_ke=function(_kf){return E(E(_kf)[3]);},_kg=function(_kh){return E(E(_kh)[5]);},_ki=function(_kj,_kk,_kl,_km){var _kn=E(_kk);if(_kn[0]==1){return [2,_jo,_iq,[1,_kl,_km],_iq];}else{var _ko=_kn[2],_kp=_kn[4],_kq=E(_kn[3]),_kr=_kq[1],_ks=_kq[2];if(!A(_ke,[_kj,_kl,_kr])){if(!A(_kg,[_kj,_kl,_kr])){return [2,_kn[1],_ko,[1,_kr,_km],_kp];}else{return _jL(_ko,_kr,_ks,_ki(_kj,_kp,_kl,_km));}}else{return _jL(_ki(_kj,_ko,_kl,_km),_kr,_ks,_kp);}}},_kt=function(_ku,_kv,_kw){while(1){var _kx=E(_kv);if(_kx[0]==1){return [1];}else{var _ky=E(_kx[3]),_kz=_ky[1];if(!A(_ke,[_ku,_kw,_kz])){if(!A(_kg,[_ku,_kw,_kz])){return [2,_ky[2]];}else{_kv=_kx[4];continue;}}else{_kv=_kx[2];continue;}}}},_kA=function(_kB){return err(unAppCStr("Edge Exception, Node: ",T(function(){return _6B(0,_kB,_1q);})));},_kC=function(_kD){return _kA(E(_kD)[1]);},_kE=function(_kF,_kG,_kH,_kI){var _kJ=E(_kG);if(_kJ[0]==1){return [1];}else{var _kK=_kJ[1],_kL=_kJ[2],_kM=_kJ[4],_kN=E(_kJ[3]),_kO=_kN[1];if(!A(_ke,[_kF,_kH,_kO])){if(!A(_kg,[_kF,_kH,_kO])){return [2,_kK,_kL,[1,_kO,T(function(){return A(_kI,[_kN[2]]);})],_kM];}else{return [2,_kK,_kL,_kN,_kE(_kF,_kM,_kH,_kI)];}}else{return [2,_kK,_kE(_kF,_kL,_kH,_kI),_kN,_kM];}}},_kP=function(_kQ,_kR,_kS){while(1){var _kT=(function(_kU,_kV,_kW){var _kX=E(_kV);if(_kX[0]==1){return E(_kU);}else{var _kY=E(_kX[1]),_kZ=_kY[2];if(_kt(_jn,_kU,_kZ)[0]==1){return _kC(_kZ);}else{var _l0=_kE(_jn,_kU,_kZ,T(function(){return A(_kW,[_kY[1]]);}));_kR=_kX[2];var _l1=_kW;_kQ=_l0;_kS=_l1;return null;}}})(_kQ,_kR,_kS);if(_kT!=null){return _kT;}}},_l2=function(_l3,_l4,_l5,_l6,_l7){if(_kt(_jn,_l7,_l4)[0]==1){return T(function(){return _kP(_kP(_ki(_jn,_l7,_l4,[1,_l3,_l5,_l6]),_l3,function(_l8,_l9){var _la=E(_l9);return [1,_la[1],_la[2],[2,[1,_l8,_l4],_la[3]]];}),_l6,function(_lb,_lc){var _ld=E(_lc);return [1,[2,[1,_lb,_l4],_ld[1]],_ld[2],_ld[3]];});});}else{return _iM(E(_l4)[1]);}},_le=T(function(){return unCStr("Word32");}),_lf=T(function(){return unCStr("}: tried to take `succ\' of maxBound");}),_lg=function(_lh){return err(unAppCStr("Enum.succ{",T(function(){return _1V(_lh,_lf);})));},_li=function(_lj){return _lg(_lj);},_lk=T(function(){return _li(_le);}),_ll=function(_lm){var _ln=E(_lm)[1];if(_ln!=4294967295){return [1,_ln+1>>>0];}else{return E(_lk);}},_lo=function(_lp){return E(E(_lp)[2]);},_lq=T(function(){return unCStr("Map.!: given key is not an element in the map");}),_lr=T(function(){return err(_lq);}),_ls=function(_lt,_lu,_lv){return (function(_lw,_lx){while(1){var _ly=E(_lw),_lz=E(_lx);if(_lz[0]==1){switch(A(T(function(){return _lo(_lt);}),[_ly,_lz[2]])[0]){case 1:_lw=_ly;_lx=_lz[4];continue;case 2:return E(_lz[3]);default:_lw=_ly;_lx=_lz[5];continue;}}else{return E(_lr);}}})(_lu,_lv);},_lA=function(_lB,_lC,_lD,_lE){var _lF=E(_lC),_lG=E(_lE);if(_lG[0]==1){var _lH=_lG[2],_lI=_lG[3],_lJ=_lG[4],_lK=_lG[5];switch(A(_lo,[_lB,_lF,_lH])[0]){case 1:return _de(_lH,_lI,_lA(_lB,_lF,_lD,_lJ),_lK);case 2:return [1,_lG[1],E(_lF),_lD,E(_lJ),E(_lK)];default:return _dT(_lH,_lI,_lJ,_lA(_lB,_lF,_lD,_lK));}}else{return [1,1,E(_lF),_lD,E(_db),E(_db)];}},_lL=function(_lM,_lN,_lO,_lP){return _lA(_lM,_lN,_lO,_lP);},_lQ=function(_lR,_lS,_lT){return (function(_lU,_lV){while(1){var _lW=E(_lU),_lX=E(_lV);if(_lX[0]==1){switch(A(T(function(){return _lo(_lR);}),[_lW,_lX[2]])[0]){case 1:_lU=_lW;_lV=_lX[4];continue;case 2:return true;default:_lU=_lW;_lV=_lX[5];continue;}}else{return false;}}})(_lS,_lT);},_lY=function(_lZ,_m0,_m1,_m2,_m3,_m4){return _lQ(_lZ,_m0,_m4)==false?[1,T(function(){return _ll(_m2);}),T(function(){return _ll(_m3);}),T(function(){return _lL(_lZ,_m0,[2,_m1,_1q],_m4);})]:[1,_m2,T(function(){return _ll(_m3);}),T(function(){return _lL(_lZ,_m0,[2,_m1,T(function(){return _ls(_lZ,_m0,_m4);})],_m4);})];},_m5=function(_m6,_m7){return E(_m6)[1]<E(_m7)[1];},_m8=function(_m9,_ma){return E(_m9)[1]<=E(_ma)[1];},_mb=function(_mc,_md){return E(_mc)[1]>E(_md)[1];},_me=function(_mf,_mg){return E(_mf)[1]>=E(_mg)[1];},_mh=function(_mi,_mj){var _mk=E(_mi)[1],_ml=E(_mj)[1];return _mk==_ml==false?_mk<=_ml==false?[3]:[1]:[2];},_mm=function(_mn,_mo){var _mp=E(_mn),_mq=E(_mo);return _mp[1]<=_mq[1]==false?E(_mp):E(_mq);},_mr=function(_ms,_mt){var _mu=E(_ms),_mv=E(_mt);return _mu[1]<=_mv[1]==false?E(_mv):E(_mu);},_mw=[1,_gB,_mh,_m5,_me,_mb,_m8,_mm,_mr],_mx=function(_my,_mz,_mA){while(1){var _mB=E(_mz);if(_mB[0]==1){return E(_mA)[0]==1?[2]:[1];}else{var _mC=E(_mA);if(_mC[0]==1){return [3];}else{var _mD=A(_lo,[_my,_mB[1],_mC[1]]);if(_mD[0]==2){_mz=_mB[2];_mA=_mC[2];continue;}else{return E(_mD);}}}}},_mE=function(_mF,_mG,_mH,_mI){return _mx(_mG,_mH,_mI)[0]==1?true:false;},_mJ=function(_mK,_mL,_mM,_mN){return _mx(_mL,_mM,_mN)[0]==3?false:true;},_mO=function(_mP,_mQ,_mR,_mS){return _mx(_mQ,_mR,_mS)[0]==3?true:false;},_mT=function(_mU,_mV,_mW,_mX){return _mx(_mV,_mW,_mX)[0]==1?false:true;},_mY=function(_mZ,_n0,_n1,_n2){return _mx(_n0,_n1,_n2)[0]==3?E(_n1):E(_n2);},_n3=function(_n4,_n5,_n6,_n7){return _mx(_n5,_n6,_n7)[0]==3?E(_n7):E(_n6);},_n8=function(_n9,_na){return [1,_n9,function(_nb,_nc){return _mx(_na,_nb,_nc);},function(_nd,_ne){return _mE(_n9,_na,_nd,_ne);},function(_nd,_ne){return _mT(_n9,_na,_nd,_ne);},function(_nd,_ne){return _mO(_n9,_na,_nd,_ne);},function(_nd,_ne){return _mJ(_n9,_na,_nd,_ne);},function(_nd,_ne){return _mY(_n9,_na,_nd,_ne);},function(_nd,_ne){return _n3(_n9,_na,_nd,_ne);}];},_nf=function(_ng,_nh,_ni){while(1){var _nj=E(_nh);if(_nj[0]==1){return E(_ni)[0]==1?true:false;}else{var _nk=E(_ni);if(_nk[0]==1){return false;}else{if(!A(_gE,[_ng,_nj[1],_nk[1]])){return false;}else{_nh=_nj[2];_ni=_nk[2];continue;}}}}},_nl=function(_nm,_nn,_no){return _nf(_nm,_nn,_no)==false?true:false;},_np=function(_nq){return [1,function(_nd,_ne){return _nf(_nq,_nd,_ne);},function(_nd,_ne){return _nl(_nq,_nd,_ne);}];},_nr=T(function(){return _np(_gB);}),_ns=T(function(){return _n8(_nr,_mw);}),_nt=function(_nu,_nv,_nw,_nx,_ny,_nz,_nA,_nB){var _nC=E(_nv);switch(_nC[0]){case 1:return [1,T(function(){return [1,_l2(_1q,_nu,_nC,_1q,E(_nw)[1])];}),T(function(){return _iC(_nC[1],_nu,_nx);}),_ny,_nz,_nA,_nB];case 2:return [1,T(function(){return [1,_l2(_1q,_nu,_nC,_1q,E(_nw)[1])];}),_nx,T(function(){return _iC(_nC[1],_nu,_ny);}),_nz,_nA,_nB];case 3:return [1,T(function(){return [1,_l2(_1q,_nu,_nC,_1q,E(_nw)[1])];}),_nx,_ny,T(function(){return _iC(_nC[1],_nu,_nz);}),_nA,_nB];case 4:return [1,T(function(){return [1,_l2(_1q,_nu,_nC,_1q,E(_nw)[1])];}),_nx,_ny,_nz,T(function(){var _nD=E(_nA),_nE=_lY(_ns,_nC[1],_nu,_nD[1],_nD[2],_nD[3]);return [1,_nE[1],_nE[2],_nE[3]];}),_nB];case 5:return [1,T(function(){return [1,_l2(_1q,_nu,_nC,_1q,E(_nw)[1])];}),_nx,_ny,_nz,_nA,T(function(){return _iC(_nC[1],_nu,_nB);})];default:return [1,T(function(){return [1,_l2(_1q,_nu,_nC,_1q,E(_nw)[1])];}),_nx,_ny,_nz,_nA,_nB];}},_nF=function(_nG,_nH){var _nI=function(_nJ){var _nK=E(_nJ);if(_nK[0]==1){return E(_nH);}else{var _nL=E(_nK[1]),_nM=_nI(_nK[2]),_nN=_nt(_nL[1],_nL[2],_nM[1],_nM[2],_nM[3],_nM[4],_nM[5],_nM[6]);return [1,_nN[1],_nN[2],_nN[3],_nN[4],_nN[5],_nN[6]];}};return _nI(_nG);},_nO=function(_nP,_nQ){while(1){var _nR=(function(_nS,_nT){var _nU=E(_nT);if(_nU[0]==1){return [1];}else{var _nV=_nU[1],_nW=_nU[2];if(!A(_nS,[_nV])){var _nX=_nS;_nQ=_nW;_nP=_nX;return null;}else{return [2,_nV,T(function(){return _nO(_nS,_nW);})];}}})(_nP,_nQ);if(_nR!=null){return _nR;}}},_nY=T(function(){return unCStr("splitMax on empty FiniteMap");}),_nZ=T(function(){return err(_nY);}),_o0=function(_o1){var _o2=E(_o1);if(_o2[0]==1){return E(_nZ);}else{var _o3=_o2[2],_o4=_o2[3],_o5=E(_o2[4]);if(_o5[0]==1){return [1,_o3,_o4];}else{var _o6=T(function(){var _o7=_o0(_o5);return [1,_o7[1],_o7[2]];});return [1,T(function(){var _o8=E(_o4);return _jL(_o3,_o8[1],_o8[2],E(_o6)[1]);}),T(function(){return E(E(_o6)[2]);})];}}},_o9=function(_oa,_ob){var _oc=E(_ob);if(_oc[0]==1){return E(_oa);}else{var _od=E(_oa);if(_od[0]==1){return E(_oc);}else{var _oe=_o0(_od),_of=E(_oe[2]);return _jL(_oe[1],_of[1],_of[2],_oc);}}},_og=function(_oh,_oi,_oj){var _ok=E(_oi);if(_ok[0]==1){return [1];}else{var _ol=_ok[2],_om=_ok[4],_on=E(_ok[3]),_oo=_on[1],_op=_on[2];if(!A(_ke,[_oh,_oj,_oo])){if(!A(_kg,[_oh,_oj,_oo])){return [2,[1,T(function(){return _o9(_ol,_om);}),_on]];}else{var _oq=_og(_oh,_om,_oj);if(_oq[0]==1){return [1];}else{var _or=E(_oq[1]);return [2,[1,T(function(){return _jL(_ol,_oo,_op,_or[1]);}),_or[2]]];}}}else{var _os=_og(_oh,_ol,_oj);if(_os[0]==1){return [1];}else{var _ot=E(_os[1]);return [2,[1,T(function(){return _jL(_ot[1],_oo,_op,_om);}),_ot[2]]];}}}},_ou=function(_ov,_ow){var _ox=_og(_jn,_ow,_ov);if(_ox[0]==1){return [1,_1p,[1,_ow]];}else{var _oy=E(_ox[1]),_oz=E(E(_oy[2])[2]),_oA=_oz[3],_oB=T(function(){return _nO(function(_oC){return E(E(_oC)[2])[1]!=E(_ov)[1];},_oz[1]);});return [1,[2,[1,_oB,_ov,_oz[2],_oA]],[1,T(function(){var _oD=function(_oE){return E(E(_oE)[2])[1]!=E(_ov)[1];};return _kP(_kP(_oy[1],_nO(_oD,_oA),function(_oF,_oG){var _oH=E(_oG);return [1,T(function(){return _nO(function(_oI){return _oD(_oI);},_oH[1]);}),_oH[2],_oH[3]];}),_oB,function(_oJ,_oK){var _oL=E(_oK);return [1,_oL[1],_oL[2],T(function(){return _nO(_oD,_oL[3]);})];});})]];}},_oM=T(function(){return unCStr("Control.Exception.Base");}),_oN=T(function(){return unCStr("base");}),_oO=T(function(){return unCStr("PatternMatchFail");}),_oP=T(function(){var _oQ=hs_wordToWord64(18445595),_oR=hs_wordToWord64(52003073);return [1,_oQ,_oR,[1,_oQ,_oR,_oN,_oM,_oO],_1q];}),_oS=function(_oT){return E(_oP);},_oU=function(_oV){var _oW=E(_oV);return _1F(_1B(_oW[1]),_oS,_oW[2]);},_oX=function(_oY){return E(E(_oY)[1]);},_oZ=function(_p0,_p1){return _1V(E(_p0)[1],_p1);},_p2=function(_p3,_p4){return _2L(_oZ,_p3,_p4);},_p5=function(_p6,_p7,_p8){return _1V(E(_p7)[1],_p8);},_p9=[1,_p5,_oX,_p2],_pa=T(function(){return [1,_oS,_p9,_pb,_oU];}),_pb=function(_pc){return [1,_pa,_pc];},_pd=T(function(){return unCStr("Irrefutable pattern failed for pattern");}),_pe=function(_pf,_pg){return die(T(function(){return A(_pg,[_pf]);}));},_ph=function(_pi,_pj){var _pk=E(_pj);if(_pk[0]==1){return [1,_1q,_1q];}else{var _pl=_pk[1];if(!A(_pi,[_pl])){return [1,_1q,_pk];}else{var _pm=T(function(){var _pn=_ph(_pi,_pk[2]);return [1,_pn[1],_pn[2]];});return [1,[2,_pl,T(function(){return E(E(_pm)[1]);})],T(function(){return E(E(_pm)[2]);})];}}},_po=[1,32],_pp=[1,10],_pq=[2,_pp,_1q],_pr=function(_ps){return E(E(_ps)[1])==124?false:true;},_pt=function(_pu,_pv){var _pw=_ph(_pr,unCStr(_pu)),_px=_pw[1],_py=function(_pz,_pA){return _1V(_pz,T(function(){return unAppCStr(": ",T(function(){return _1V(_pv,T(function(){return _1V(_pA,_pq);}));}));}));},_pB=E(_pw[2]);if(_pB[0]==1){return _py(_px,_1q);}else{return E(E(_pB[1])[1])==124?_py(_px,[2,_po,_pB[2]]):_py(_px,_1q);}},_pC=function(_pD){return _pe([1,T(function(){return _pt(_pD,_pd);})],_pb);},_pE=T(function(){return _pC("Data/Graph/Inductive/Graph.hs:250:27-60|(Data.Maybe.Just (pr,\n                                                          _,\n                                                          la,\n                                                          su),\n                                         g\')");}),_pF=function(_pG,_pH){var _pI=function(_pJ){var _pK=E(_pJ);if(_pK[0]==1){return E(_pH);}else{var _pL=_pI(_pK[2]);return [1,T(function(){var _pM=E(_pK[1]),_pN=_pM[1],_pO=_ou(_pN,E(_pL[1])[1]),_pP=E(_pO[1]);if(_pP[0]==1){return E(_pE);}else{var _pQ=E(_pP[1]);return [1,_l2(_pQ[1],_pN,_pQ[3],[2,[1,_pM[3],_pM[2]],_pQ[4]],E(_pO[2])[1])];}}),_pL[2],_pL[3],_pL[4],_pL[5],_pL[6]];}};return _pI(_pG);},_pR=[1,9],_pS=[1,8],_pT=[1],_pU=T(function(){return unCStr("value");}),_pV=T(function(){return unCStr("instance");}),_pW=[1,_pV,_pU,_pT],_pX=[1,_pS,_pR,_pW],_pY=[2,_pX,_1q],_pZ=[1,3],_q0=T(function(){return unCStr("self");}),_q1=[1,_pV,_q0,_pT],_q2=[1,_pZ,_pR,_q1],_q3=[2,_q2,_pY],_q4=[1,7],_q5=[1,_pU,_pU,_pT],_q6=[1,_q4,_pS,_q5],_q7=[2,_q6,_q3],_q8=[1,6],_q9=[1,_pV,_q0,_pT],_qa=[1,_q8,_pS,_q9],_qb=[2,_qa,_q7],_qc=[1,5],_qd=T(function(){return unCStr("type");}),_qe=[1,_qd,_qd,_pT],_qf=[1,_qc,_q8,_qe],_qg=[2,_qf,_qb],_qh=[1,4],_qi=T(function(){return unCStr("name");}),_qj=[1,_pU,_qi,_pT],_qk=[1,_qh,_qc,_qj],_ql=[2,_qk,_qg],_qm=[1,2],_qn=[1,_pV,_q0,_pT],_qo=[1,_qm,_pZ,_qn],_qp=[2,_qo,_ql],_qq=[1,_qd,_qd,_pT],_qr=[1,_ig,_qm,_qq],_qs=[2,_qr,_qp],_qt=[1,_pU,_qi,_pT],_qu=[1,_im,_ig,_qt],_qv=[2,_qu,_qs],_qw=T(function(){return unCStr("std.types.Console");}),_qx=[2,_qw],_qy=[6,_qx],_qz=[1,_im,_qy],_qA=T(function(){return unCStr("std.types.Console.print");}),_qB=[2,_qA],_qC=[1,_pR,_qB],_qD=[2,_qC,_1q],_qE=T(function(){return unCStr("std.types.String.init");}),_qF=[2,_qE],_qG=[1,_pS,_qF],_qH=[2,_qG,_qD],_qI=T(function(){return unCStr("hello world!");}),_qJ=[2,_qI],_qK=[6,_qJ],_qL=[1,_q4,_qK],_qM=[2,_qL,_qH],_qN=T(function(){return unCStr("std.types.new");}),_qO=[2,_qN],_qP=[1,_q8,_qO],_qQ=[2,_qP,_qM],_qR=T(function(){return unCStr("std.types.type");}),_qS=[1,_qR],_qT=[1,_qc,_qS],_qU=[2,_qT,_qQ],_qV=T(function(){return unCStr("std.types.String");}),_qW=[2,_qV],_qX=[6,_qW],_qY=[1,_qh,_qX],_qZ=[2,_qY,_qU],_r0=T(function(){return unCStr("std.types.Console.init");}),_r1=[2,_r0],_r2=[1,_pZ,_r1],_r3=[2,_r2,_qZ],_r4=[2,_qN],_r5=[1,_qm,_r4],_r6=[2,_r5,_r3],_r7=[1,_qR],_r8=[1,_ig,_r7],_r9=[2,_r8,_r6],_ra=[2,_qz,_r9],_rb=T(function(){return _nF(_ra,_iu);}),_rc=T(function(){return _pF(_qv,_rb);}),_rd=[2,_1q,_1q,_1q,_rc,_ig],_re=T(function(){return unCStr("my");}),_rf=[5,_re,_rd],_rg=[1,_ig,_rf],_rh=[2,_rg,_1q],_ri=T(function(){return unCStr("std");}),_rj=[2,_qd,_1q],_rk=[2,_qi,_1q],_rl=[2,_rk,_rj,_1q,_iu,_im],_rm=[4,_qd,_rl],_rn=[1,_qm,_rm],_ro=[2,_rn,_1q],_rp=[2,_pV,_1q],_rq=[2,_qd,_1q],_rr=[2,_rq,_rp,_1q,_iu,_im],_rs=T(function(){return unCStr("new");}),_rt=[4,_rs,_rr],_ru=[1,_ig,_rt],_rv=[2,_ru,_ro],_rw=T(function(){return unCStr("init");}),_rx=T(function(){var _ry=_lY(_ns,_rw,_im,_is,_is,_db);return [1,_ry[1],_ry[2],_ry[3]];}),_rz=[2,_pV,_1q],_rA=[2,_pU,_1q],_rB=[2,_q0,_rA],_rC=[2,_rB,_rz,_1q,_iu,_im],_rD=[4,_rw,_rC],_rE=T(function(){return [1,_l2(_1q,_im,_rD,_1q,_iq)];}),_rF=[1,_rE,_db,_db,_db,_rx,_db],_rG=[2,_1q,_1q,_1q,_rF,_im],_rH=T(function(){return unCStr("String");}),_rI=[3,_rH,_rG],_rJ=[1,_im,_rI],_rK=[2,_rJ,_rv],_rL=T(function(){return _nF(_rK,_iu);}),_rM=[2,_1q,_1q,_1q,_rL,_im],_rN=T(function(){return unCStr("types");}),_rO=[5,_rN,_rM],_rP=[1,_im,_rO],_rQ=T(function(){return unCStr("io");}),_rR=T(function(){return unCStr("Console");}),_rS=T(function(){return _iC(_rR,_im,_db);}),_rT=[2,_pV,_1q],_rU=[2,_q0,_1q],_rV=[2,_rU,_rT,_1q,_iu,_im],_rW=[4,_rw,_rV],_rX=[1,_ig,_rW],_rY=[2,_rX,_1q],_rZ=T(function(){return unCStr("console");}),_s0=[2,_rZ,_1q],_s1=[2,_pU,_1q],_s2=[2,_q0,_s1],_s3=[2,_s2,_s0,_1q,_iu,_im],_s4=T(function(){return unCStr("print");}),_s5=[4,_s4,_s3],_s6=[1,_im,_s5],_s7=[2,_s6,_rY],_s8=T(function(){return _nF(_s7,_iu);}),_s9=[2,_1q,_1q,_1q,_s8,_im],_sa=[3,_rR,_s9],_sb=T(function(){return [1,_l2(_1q,_im,_sa,_1q,_iq)];}),_sc=[1,_sb,_db,_db,_rS,_it,_db],_sd=[2,_1q,_1q,_1q,_sc,_im],_se=[5,_rQ,_sd],_sf=[1,_ig,_se],_sg=[2,_sf,_1q],_sh=[2,_rP,_sg],_si=T(function(){return _nF(_sh,_iu);}),_sj=[2,_1q,_1q,_1q,_si,_im],_sk=[5,_ri,_sj],_sl=[1,_im,_sk],_sm=[2,_sl,_rh],_sn=T(function(){return _nF(_sm,_iu);}),_so=T(function(){return _d3(0,_ip,_sn);}),_sp=T(function(){return A(_so,[_1q]);}),_sq=T(function(){var _sr=E(_rc);return A(_bu,[0,_sr[1],_sr[2],_sr[3],_sr[4],_sr[5],_sr[6],_1q]);}),_ss=[1,0],_st=function(_){return _ss;},_su=function(_){return _ss;},_sv=[1,0],_sw=[1,-1],_sx=function(_){return _sw;},_sy=function(_){return _ss;},_sz=function(_sA,_sB,_){writeOffAddr("w32",4,E(_sA)[1],0,E(_sB)[1]);return _0;},_sC=function(_sD,_){var _sE=readOffAddr("w32",4,E(_sD)[1],0);return [1,_sE];},_sF=function(_sG,_sH,_sI,_){writeOffAddr("w32",4,plusAddr(E(_sG)[1],E(_sH)[1]),0,E(_sI)[1]);return _0;},_sJ=function(_sK,_sL,_){var _sM=readOffAddr("w32",4,plusAddr(E(_sK)[1],E(_sL)[1]),0);return [1,_sM];},_sN=[1,4],_sO=function(_sP){return E(_sN);},_sQ=function(_sR){return E(_sN);},_sS=function(_sT,_sU,_){var _sV=readOffAddr("w32",4,E(_sT)[1],E(_sU)[1]);return [1,_sV];},_sW=function(_sX,_sY,_sZ,_){writeOffAddr("w32",4,E(_sX)[1],E(_sY)[1],E(_sZ)[1]);return _0;},_t0=[1,_sQ,_sO,_sS,_sW,_sJ,_sF,_sC,_sz],_t1=[1,0],_t2=function(_t3){return E(E(_t3)[3]);},_t4=function(_t5,_t6,_t7,_){if(_t6>0){return (function(_t8,_t9,_){while(1){var _ta=E(_t8);if(!_ta){var _tb=A(T(function(){return A(_t2,[_t5,_t7,_t1]);}),[_]);return [2,_tb,_t9];}else{var _tc=A(T(function(){return _t2(_t5);}),[_t7,[1,_ta],_]);_t8=_ta-1|0;var _td=[2,_tc,_t9];_t9=_td;continue;}}})(_t6-1|0,_1q,_);}else{return _1q;}},_te=[1],_tf=[1,0],_tg=function(_th,_ti,_){A(_th,[_]);return die(_ti);},_tj=function(_tk){return E(E(_tk)[3]);},_tl=function(_tm,_tn,_){return _tg(T(function(){return _tj(_tm);}),_tn,_);},_to=function(_tp,_tq,_){var _tr=(function(_ts,_){while(1){var _tt=readOffAddr("i8",1,_tq,_ts);if(!E(_tt)){return [1,_ts];}else{var _tu=_ts+1|0;_ts=_tu;continue;}}})(0,_),_tv=E(_tp)[2],_tw=0,_tx=function(_ty,_){var _tz=nMV(_5X),_tA=E(_tr)[1],_tB=function(_tC){var _tD=imul(_tC,4)|0;if(_tD>=0){var _tE=nMV(_5X),_tF=newByteArr(_tD),_tG=function(_tH,_tI,_){var _tJ=E(_ty),_tK=A(_tJ[1],[_tI,[1,_tF,[2,_tF,E([1,_tE])],_1k,_tC,0,0],_]),_tL=E(_tK),_tM=_tL[3],_tN=E(_tL[2]);if(_tN[5]!=_tN[6]){var _tO=function(_tP,_tQ,_tR,_tS,_tT,_){var _tU=_t4(_t0,_tT-_tS|0,[1,_tQ],_),_=0,_tV=_tG(T(function(){return [1,E(_tH)[1]+1|0];}),_tP,_);return T(function(){return _1V(_tU,_tV);});};if(E(_tL[1])[0]==2){var _tW=E(_tM);return _tO(_tN,_tW[1],_tW[2],_tW[5],_tW[6],_);}else{var _tX=A(_tJ[2],[_tN,_tM,_]),_tY=E(_tX),_tZ=E(_tY[2]);return _tO(_tY[1],_tZ[1],_tZ[2],_tZ[5],_tZ[6],_);}}else{var _u0=E(_tM);_u0[2];var _u1=_t4(_t0,_u0[6]-_u0[5]|0,[1,_u0[1]],_);0;return _u1;}};return _tG(_tf,[1,_tq,[1,E([1,_tz])],_te,_tA,0,_tA],_);}else{return E(_5Z);}};return _tA<=1==false?_tB(_tA):_tB(1);};if(!E(_tw)){return (function(_){var _u2=A(_tv,[_]),_u3=jsCatch(function(_){return (function(_){return _tx(_u2,_);})();},function(_u4,_){return _tl(_u2,_u4,_);});A(E(_u2)[3],[_]);return _u3;})();}else{var _u5=A(_tv,[_]),_u6=jsCatch(function(_){return _tx(_u5,_);},function(_u4,_){return _tl(_u5,_u4,_);});A(E(_u5)[3],[_]);return _u6;}},_u7=[2],_u8=T(function(){return unCStr("UTF16BE");}),_u9=T(function(){return unCStr("UTF16");}),_ua=T(function(){return unCStr("UTF8");}),_ub=T(function(){return unCStr("UTF32LE");}),_uc=T(function(){return unCStr("UTF32BE");}),_ud=T(function(){return unCStr("UTF32");}),_ue=T(function(){return unCStr("UTF16LE");}),_uf=function(_ug){return err(unAppCStr("Prelude.chr: bad argument: ",T(function(){return _6B(9,_ug,_1q);})));},_uh=function(_ui){var _uj=u_towupper(_ui);if(_uj>>>0>1114111){return _uf(_uj);}else{return [1,_uj];}},_uk=function(_ul){while(1){var _um=(function(_un){var _uo=E(_un);if(_uo[0]==1){return [1];}else{var _up=_uo[2],_uq=E(E(_uo[1])[1]);if(_uq==45){_ul=_up;return null;}else{return [2,T(function(){return _uh(_uq);}),T(function(){return _uk(_up);})];}}})(_ul);if(_um!=null){return _um;}}},_ur=T(function(){return unCStr("UTF-32LE");}),_us=function(_ut){return E(E(_ut)[1])==47?false:true;},_uu=[1,2],_uv=[1,0],_uw=[1,-1],_ux=T(function(){return unCStr("iconvRecoder");}),_uy=function(_uz,_uA,_uB,_uC,_uD,_){var _uE=E(_uA),_uF=_uE[1],_uG=_uE[2],_uH=_uE[3],_uI=_uE[4],_uJ=_uE[5],_uK=_uE[6],_uL=E(_uC),_uM=_uL[1],_uN=_uL[2],_uO=_uL[3],_uP=_uL[4],_uQ=_uL[5],_uR=_uL[6],_uS=newByteArr(4),_uT=_uS,_uU=E(_uB)[1],_uV=function(_uW){var _=die("Unsupported PrimOp: writeAddrOffAddr#"),_uX=newByteArr(4),_uY=_uX,_uZ=E(_uD)[1],_v0=function(_v1){var _=die("Unsupported PrimOp: writeAddrOffAddr#"),_v2=newByteArr(4),_v3=_v2,_v4=function(_v5){var _v6=_v3,_=writeOffAddr("w32",4,_v6,0,_v5),_v7=newByteArr(4),_v8=_v7,_v9=function(_va){var _vb=_v8,_=writeOffAddr("w32",4,_vb,0,_va),_vc=hs_iconv(E(_uz)[1],_uT,_v6,_uY,_vb),_vd=readOffAddr("w32",4,_v6,0),_ve=readOffAddr("w32",4,_vb,0),_vf=T(function(){if(_uZ<32){return [1,(_ve&4294967295)>>_uZ];}else{return (_ve&4294967295)<0==false?E(_uv):E(_uw);}}),_vg=T(function(){if(_vd!=0){if(_uU<32){return [1,_uF,_uG,_uH,_uI,_uK-((_vd&4294967295)>>_uU)|0,_uK];}else{if((_vd&4294967295)>=0){return [1,_uF,_uG,_uH,_uI,_uK,_uK];}else{return [1,_uF,_uG,_uH,_uI,_uK+1|0,_uK];}}}else{return [1,_uF,_uG,_uH,_uI,0,0];}});if(_vc!=4294967295){0;0;0;0;0;0;return [1,_2,_vg,T(function(){return [1,_uM,_uN,_uO,_uP,_uQ,_uP-E(_vf)[1]|0];})];}else{var _vh=__hscore_get_errno();switch(E(_vh)){case 7:0;0;0;0;0;0;return [1,_3,_vg,T(function(){return [1,_uM,_uN,_uO,_uP,_uQ,_uP-E(_vf)[1]|0];})];case 22:0;0;0;0;0;0;return [1,_2,_vg,T(function(){return [1,_uM,_uN,_uO,_uP,_uQ,_uP-E(_vf)[1]|0];})];case 84:0;0;0;0;0;0;return [1,T(function(){return E(E(_vf)[1])==0?[2]:[3];}),_vg,T(function(){return [1,_uM,_uN,_uO,_uP,_uQ,_uP-E(_vf)[1]|0];})];default:var _vi=__hscore_get_errno(),_vj=_3J(_vk(_ux,[1,_vi],_1p,_1p),_);0;0;0;0;0;0;return _vj;}}};if(_uZ<32){return _v9((_uP-_uR|0)<<_uZ>>>0);}else{return _v9(0);}};if(_uU<32){return _v4((_uK-_uJ|0)<<_uU>>>0);}else{return _v4(0);}};if(_uZ<32){return _v0(plusAddr(_uM,_uR<<_uZ));}else{return _v0(plusAddr(_uM,0));}};if(_uU<32){return _uV(plusAddr(_uF,_uJ<<_uU));}else{return _uV(plusAddr(_uF,0));}},_vl=function(_vm,_vn,_vo,_){return _uy(_vm,_vn,_uu,_vo,_uv,_);},_vp=function(_vq,_vr,_vs,_){return _uy(_vq,_vr,_uv,_vs,_uu,_);},_vt=function(_vu,_){var _vv=hs_iconv_close(E(_vu)[1]);return [1,_vv];},_vw=function(_vx,_){return _vt(_vx,_);},_vy=function(_vz,_){return _0;},_vA=function(_){return _0;},_vB=T(function(){return unCStr("Iconv.close");}),_vC=function(_vD,_vE){return E(_vD)[1]==E(_vE)[1]==false?true:false;},_vF=function(_vG,_vH){return E(_vG)[1]==E(_vH)[1];},_vI=[1,_vF,_vC],_vJ=function(_vK,_vL){return [1,imul(E(_vK)[1],E(_vL)[1])|0];},_vM=function(_vN,_vO){return [1,E(_vN)[1]+E(_vO)[1]|0];},_vP=function(_vQ,_vR){return [1,E(_vQ)[1]-E(_vR)[1]|0];},_vS=function(_vT){var _vU=E(_vT),_vV=_vU[1];if(_vV<0){return [1,-_vV];}else{return E(_vU);}},_vW=function(_vX){var _vY=E(_vX);return _vY[0]==1?E(_vY[1]):I_toInt(_vY[1]);},_vZ=function(_w0){return [1,_vW(_w0)];},_w1=function(_w2){return [1,-E(_w2)[1]];},_w3=[1,-1],_w4=[1,1],_w5=[1,0],_w6=function(_w7){var _w8=E(_w7)[1];return _w8>0==false?E(_w8)==0?E(_w5):E(_w3):E(_w4);},_w9=[1,_vM,_vJ,_vP,_w1,_vS,_w6,_vZ],_wa=function(_wb){return E(E(_wb)[7]);},_wc=function(_wd){return E(E(_wd)[4]);},_we=[1,1],_wf=function(_wg,_wh){return E(function(_wi,_wj,_){var _wk=A(_wj,[_]);if(!A(T(function(){return _gE(_wg);}),[_wk,T(function(){return A(_wc,[_wh,T(function(){return A(_wa,[_wh,_we]);})]);})])){return _0;}else{var _wl=__hscore_get_errno();_3J(_vk(_wi,[1,_wl],_1p,_1p),_);return _0;}});},_wm=T(function(){return _wf(_vI,_w9);}),_wn=T(function(){return A(_wm,[_vB]);}),_wo=T(function(){return unCStr("mkTextEncoding");}),_wp=function(_wq,_wr,_){writeOffAddr("i8",1,E(_wq)[1],0,E(_wr)[1]);return _0;},_ws=function(_wt,_){var _wu=readOffAddr("i8",1,E(_wt)[1],0);return [1,_wu];},_wv=function(_ww,_wx,_wy,_){writeOffAddr("i8",1,plusAddr(E(_ww)[1],E(_wx)[1]),0,E(_wy)[1]);return _0;},_wz=function(_wA,_wB,_){var _wC=readOffAddr("i8",1,plusAddr(E(_wA)[1],E(_wB)[1]),0);return [1,_wC];},_wD=[1,1],_wE=function(_wF){return E(_wD);},_wG=function(_wH){return E(_wD);},_wI=function(_wJ,_wK,_){var _wL=readOffAddr("i8",1,E(_wJ)[1],E(_wK)[1]);return [1,_wL];},_wM=function(_wN,_wO,_wP,_){writeOffAddr("i8",1,E(_wN)[1],E(_wO)[1],E(_wP)[1]);return _0;},_wQ=[1,_wG,_wE,_wI,_wM,_wz,_wv,_ws,_wp],_wR=function(_wS,_wT){while(1){var _wU=E(_wS);if(_wU[0]==1){return E(_wT);}else{_wS=_wU[2];var _wV=_wT+1|0;_wT=_wV;continue;}}},_wW=function(_wX){return E(E(_wX)[2]);},_wY=function(_wZ){return E(E(_wZ)[1]);},_x0=T(function(){return unCStr("Prelude.undefined");}),_x1=T(function(){return err(_x0);}),_x2=function(_x3,_x4){return E(function(_x5,_){E(T(function(){return A(_wW,[_x3,_x1]);}))[1];var _x6=newByteArr(E(T(function(){return [1,imul(E(_x4)[1],A(_wY,[_x3,_x1])[1])|0];}))[1]),_x7=_x6,_x8=A(_x5,[[1,_x7],_]);0;return _x8;});},_x9=function(_xa,_xb){return A(_x2,[_wQ,T(function(){return [1,_wR(_xa,0)+1|0];}),function(_xc,_){(function(_xd,_xe,_){while(1){var _xf=E(_xd);if(_xf[0]==1){writeOffAddr("i8",1,E(_xc)[1],_xe,0);return _0;}else{writeOffAddr("i8",1,E(_xc)[1],_xe,E(_xf[1])[1]&255);_xd=_xf[2];var _xg=_xe+1|0;_xe=_xg;continue;}}})(_xa,0,_);return A(_xb,[_xc,_]);}]);},_xh=function(_xi,_xj,_xk,_xl){return _x9(_xi,function(_xm){return _x9(_xj,function(_xn,_){var _xo=hs_iconv_open(E(_xn)[1],E(_xm)[1]),_xp=E(_xo);if(_xp==(-1)){var _xq=__hscore_get_errno(),_xr=_3J(_vk(_wo,[1,_xq],_1p,_1p),_);return [1,T(function(){return A(_xl,[_xr]);}),_xk,T(function(){return A(_wn,[function(_){return _vw(_xr,_);}]);}),_vA,_vy];}else{return [1,T(function(){return A(_xl,[[1,_xp]]);}),_xk,T(function(){return A(_wn,[function(_){var _xs=hs_iconv_close(_xp);return [1,_xs];}]);}),_vA,_vy];}});});},_xt=function(_xu){return _uf(_xu&4294967295);},_xv=[13],_xw=T(function(){return unCStr("invalid byte sequence");}),_xx=T(function(){return unCStr("recoverDecode");}),_xy=[1,_1p,_xv,_xx,_xw,_1p,_1p],_xz=function(_xA,_xB,_xC,_xD,_xE,_xF,_xG,_xH,_){var _xI=E(_xH),_xJ=_xI[1],_xK=_xI[2],_xL=_xI[3],_xM=_xI[4],_xN=_xI[5],_xO=_xI[6];switch(E(_xA)[0]){case 1:return _37(_xy,_);case 2:return [1,[1,_xB,_xC,_xD,_xE,_xF+1|0,_xG],_xI];case 3:writeOffAddr("w32",4,_xJ,_xO,65533);0;return [1,[1,_xB,_xC,_xD,_xE,_xF+1|0,_xG],[1,_xJ,_xK,_xL,_xM,_xN,_xO+1|0]];default:var _xP=readOffAddr("w8",1,plusAddr(_xB,_xF),0);0;if(_xP>=128){var _xQ=56320+(_xP&4294967295)|0;if(_xQ>>>0>1114111){return _uf(_xQ);}else{writeOffAddr("w32",4,_xJ,_xO,_xQ);0;return [1,[1,_xB,_xC,_xD,_xE,_xF+1|0,_xG],[1,_xJ,_xK,_xL,_xM,_xN,_xO+1|0]];}}else{if(_xP>1114111){return _xt(_xP);}else{writeOffAddr("w32",4,_xJ,_xO,_xP&4294967295);0;return [1,[1,_xB,_xC,_xD,_xE,_xF+1|0,_xG],[1,_xJ,_xK,_xL,_xM,_xN,_xO+1|0]];}}}},_xR=function(_xS,_xT,_xU,_){var _xV=E(_xT);return _xz(_xS,_xV[1],_xV[2],_xV[3],_xV[4],_xV[5],_xV[6],_xU,_);},_xW=T(function(){return unCStr("recoverEncode");}),_xX=T(function(){return unCStr("invalid character");}),_xY=[1,_1p,_xv,_xW,_xX,_1p,_1p],_xZ=function(_){return _37(_xY,_);},_y0=function(_y1,_y2,_y3,_y4,_y5,_y6,_y7,_y8,_){var _y9=E(_y8),_ya=_y9[1],_yb=_y9[6],_yc=readOffAddr("w32",4,_y2,_y6),_=0;switch(E(_y1)[0]){case 1:return _xZ(_);case 2:return [1,[1,_y2,_y3,_y4,_y5,_y6+1|0,_y7],_y9];case 3:if(E(_yc)==63){return [1,[1,_y2,_y3,_y4,_y5,_y6+1|0,_y7],_y9];}else{writeOffAddr("w32",4,_y2,_y6,63);0;return [1,[1,_y2,_y3,_y4,_y5,_y6,_y7],_y9];}break;default:var _yd=_yc;if(56448>_yd){return _xZ(_);}else{if(_yd>=56576){return _xZ(_);}else{writeOffAddr("w8",1,plusAddr(_ya,_yb),0,_yd>>>0&255);0;return [1,[1,_y2,_y3,_y4,_y5,_y6+1|0,_y7],[1,_ya,_y9[2],_y9[3],_y9[4],_y9[5],_yb+1|0]];}}}},_ye=function(_yf,_yg,_yh,_){var _yi=E(_yg);return _y0(_yf,_yi[1],_yi[2],_yi[3],_yi[4],_yi[5],_yi[6],_yh,_);},_yj=function(_yk,_yl,_){return [1,_yl,T(function(){var _ym=T(function(){var _yn=_ph(_us,_yl);return [1,_yn[1],_yn[2]];});return _xh(T(function(){return E(E(_ym)[1]);}),T(function(){return _1V(_ur,T(function(){return E(E(_ym)[2]);}));}),function(_yo,_vx,_){return _xR(_yk,_yo,_vx,_);},_vp);}),T(function(){return _xh(_ur,_yl,function(_yo,_vx,_){return _ye(_yk,_yo,_vx,_);},_vl);})];},_yp=[3],_yq=function(_yr,_ys,_yt,_yu,_yv,_yw,_yx,_){var _yy=E(_yx),_yz=_yy[1],_yA=_yy[2],_yB=_yy[3],_yC=_yy[4],_yD=_yy[5],_yE=[1,_yr,_ys,_yt,_yu,0,0],_yF=function(_yG,_yH,_){while(1){var _yI=(function(_yJ,_yK,_){if(_yJ<_yw){if((_yC-_yK|0)>=2){var _yL=readOffAddr("w32",4,_yr,_yJ),_=0,_yM=_yL;if(_yM>=65536){if((_yC-_yK|0)>=4){var _yN=_yM-65536|0;writeOffAddr("w8",1,plusAddr(_yz,_yK),0,((_yN>>18)+216|0)>>>0&255);0;writeOffAddr("w8",1,plusAddr(_yz,_yK+1|0),0,_yN>>10>>>0&255);0;writeOffAddr("w8",1,plusAddr(_yz,_yK+2|0),0,((((_yN>>>0&1023)>>>0&4294967295)>>8)+220|0)>>>0&255);0;writeOffAddr("w8",1,plusAddr(_yz,_yK+3|0),0,(_yN>>>0&1023)>>>0&255);0;var _yO=_yJ+1|0,_yP=_yK+4|0;_yG=_yO;_yH=_yP;return null;}else{return [1,_3,T(function(){return _yJ==_yw==false?[1,_yr,_ys,_yt,_yu,_yJ,_yw]:E(_yE);}),[1,_yz,_yA,_yB,_yC,_yD,_yK]];}}else{var _yQ=[1,_yp,T(function(){return _yJ==_yw==false?[1,_yr,_ys,_yt,_yu,_yJ,_yw]:E(_yE);}),[1,_yz,_yA,_yB,_yC,_yD,_yK]],_yR=function(_){if(56320>_yM){var _=writeOffAddr("w8",1,plusAddr(_yz,_yK),0,_yM>>8>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_yz,_yK+1|0),0,_yM>>>0&255),_=0;return _yF(_yJ+1|0,_yK+2|0,_);}else{if(_yM>57343){var _=writeOffAddr("w8",1,plusAddr(_yz,_yK),0,_yM>>8>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_yz,_yK+1|0),0,_yM>>>0&255),_=0;return _yF(_yJ+1|0,_yK+2|0,_);}else{return _yQ;}}};return 55296<=_yM==false?_yR(_):_yM<=56319==false?_yR(_):_yQ;}}else{return [1,_3,T(function(){return _yJ==_yw==false?[1,_yr,_ys,_yt,_yu,_yJ,_yw]:E(_yE);}),[1,_yz,_yA,_yB,_yC,_yD,_yK]];}}else{return [1,_2,T(function(){return _yJ==_yw==false?[1,_yr,_ys,_yt,_yu,_yJ,_yw]:E(_yE);}),[1,_yz,_yA,_yB,_yC,_yD,_yK]];}})(_yG,_yH,_);if(_yI!=null){return _yI;}}};return _yF(_yv,_yy[6],_);},_yS=function(_yT,_yU,_yV,_yW,_yX,_yY,_yZ,_z0,_){var _z1=rMV(_yT);if(!E(_z1)){if((_yY-_z0|0)>=2){var _=wMV(_yT,_1),_=writeOffAddr("w8",1,plusAddr(_yV,_z0),0,254),_=0,_=writeOffAddr("w8",1,plusAddr(_yV,_z0+1|0),0,255),_=0,_z2=E(_yU);return _yq(_z2[1],_z2[2],_z2[3],_z2[4],_z2[5],_z2[6],[1,_yV,_yW,_yX,_yY,_yZ,_z0+2|0],_);}else{return [1,_3,_yU,[1,_yV,_yW,_yX,_yY,_yZ,_z0]];}}else{var _z3=E(_yU);return _yq(_z3[1],_z3[2],_z3[3],_z3[4],_z3[5],_z3[6],[1,_yV,_yW,_yX,_yY,_yZ,_z0],_);}},_z4=function(_z5,_z6,_z7,_z8,_z9,_za,_zb,_){var _zc=E(_zb),_zd=_zc[1],_ze=_zc[2],_zf=_zc[3],_zg=_zc[4],_zh=_zc[5],_zi=[1,_z5,_z6,_z7,_z8,0,0];return (function(_zj,_zk,_){while(1){var _zl=(function(_zm,_zn,_){if(_zn<_zg){if(_zm<_za){if((_zm+1|0)!=_za){var _zo=readOffAddr("w8",1,plusAddr(_z5,_zm),0);0;var _zp=readOffAddr("w8",1,plusAddr(_z5,_zm+1|0),0);0;var _zq=(_zp<<8>>>0&65535)+_zo>>>0&65535;if(_zq>=55296){if(_zq<=57343){if((_za-_zm|0)>=4){var _zr=readOffAddr("w8",1,plusAddr(_z5,_zm+2|0),0);0;var _zs=readOffAddr("w8",1,plusAddr(_z5,_zm+3|0),0);0;if(_zq<55296){return [1,_yp,T(function(){return _zm==_za==false?[1,_z5,_z6,_z7,_z8,_zm,_za]:E(_zi);}),[1,_zd,_ze,_zf,_zg,_zh,_zn]];}else{if(_zq>56319){return [1,_yp,T(function(){return _zm==_za==false?[1,_z5,_z6,_z7,_z8,_zm,_za]:E(_zi);}),[1,_zd,_ze,_zf,_zg,_zh,_zn]];}else{var _zt=(_zs<<8>>>0&65535)+_zr>>>0&65535;if(_zt<56320){return [1,_yp,T(function(){return _zm==_za==false?[1,_z5,_z6,_z7,_z8,_zm,_za]:E(_zi);}),[1,_zd,_ze,_zf,_zg,_zh,_zn]];}else{if(_zt>57343){return [1,_yp,T(function(){return _zm==_za==false?[1,_z5,_z6,_z7,_z8,_zm,_za]:E(_zi);}),[1,_zd,_ze,_zf,_zg,_zh,_zn]];}else{writeOffAddr("w32",4,_zd,_zn,((((_zq&4294967295)-55296|0)<<10)+((_zt&4294967295)-56320|0)|0)+65536|0);0;var _zu=_zm+4|0,_zv=_zn+1|0;_zj=_zu;_zk=_zv;return null;}}}}}else{return [1,_2,T(function(){return _zm==_za==false?[1,_z5,_z6,_z7,_z8,_zm,_za]:E(_zi);}),[1,_zd,_ze,_zf,_zg,_zh,_zn]];}}else{writeOffAddr("w32",4,_zd,_zn,_zq&4294967295);0;var _zu=_zm+2|0,_zv=_zn+1|0;_zj=_zu;_zk=_zv;return null;}}else{writeOffAddr("w32",4,_zd,_zn,_zq&4294967295);0;var _zu=_zm+2|0,_zv=_zn+1|0;_zj=_zu;_zk=_zv;return null;}}else{return [1,_2,T(function(){return _zm==_za==false?[1,_z5,_z6,_z7,_z8,_zm,_za]:E(_zi);}),[1,_zd,_ze,_zf,_zg,_zh,_zn]];}}else{return [1,_2,T(function(){return _zm==_za==false?[1,_z5,_z6,_z7,_z8,_zm,_za]:E(_zi);}),[1,_zd,_ze,_zf,_zg,_zh,_zn]];}}else{return [1,_3,T(function(){return _zm==_za==false?[1,_z5,_z6,_z7,_z8,_zm,_za]:E(_zi);}),[1,_zd,_ze,_zf,_zg,_zh,_zn]];}})(_zj,_zk,_);if(_zl!=null){return _zl;}}})(_z9,_zc[6],_);},_zw=function(_zx,_zy,_zz,_zA,_zB,_zC,_zD,_){var _zE=E(_zD),_zF=_zE[1],_zG=_zE[2],_zH=_zE[3],_zI=_zE[4],_zJ=_zE[5],_zK=[1,_zx,_zy,_zz,_zA,0,0];return (function(_zL,_zM,_){while(1){var _zN=(function(_zO,_zP,_){if(_zP<_zI){if(_zO<_zC){if((_zO+1|0)!=_zC){var _zQ=readOffAddr("w8",1,plusAddr(_zx,_zO),0);0;var _zR=readOffAddr("w8",1,plusAddr(_zx,_zO+1|0),0);0;var _zS=(_zQ<<8>>>0&65535)+_zR>>>0&65535;if(_zS>=55296){if(_zS<=57343){if((_zC-_zO|0)>=4){var _zT=readOffAddr("w8",1,plusAddr(_zx,_zO+2|0),0);0;var _zU=readOffAddr("w8",1,plusAddr(_zx,_zO+3|0),0);0;if(_zS<55296){return [1,_yp,T(function(){return _zO==_zC==false?[1,_zx,_zy,_zz,_zA,_zO,_zC]:E(_zK);}),[1,_zF,_zG,_zH,_zI,_zJ,_zP]];}else{if(_zS>56319){return [1,_yp,T(function(){return _zO==_zC==false?[1,_zx,_zy,_zz,_zA,_zO,_zC]:E(_zK);}),[1,_zF,_zG,_zH,_zI,_zJ,_zP]];}else{var _zV=(_zT<<8>>>0&65535)+_zU>>>0&65535;if(_zV<56320){return [1,_yp,T(function(){return _zO==_zC==false?[1,_zx,_zy,_zz,_zA,_zO,_zC]:E(_zK);}),[1,_zF,_zG,_zH,_zI,_zJ,_zP]];}else{if(_zV>57343){return [1,_yp,T(function(){return _zO==_zC==false?[1,_zx,_zy,_zz,_zA,_zO,_zC]:E(_zK);}),[1,_zF,_zG,_zH,_zI,_zJ,_zP]];}else{writeOffAddr("w32",4,_zF,_zP,((((_zS&4294967295)-55296|0)<<10)+((_zV&4294967295)-56320|0)|0)+65536|0);0;var _zW=_zO+4|0,_zX=_zP+1|0;_zL=_zW;_zM=_zX;return null;}}}}}else{return [1,_2,T(function(){return _zO==_zC==false?[1,_zx,_zy,_zz,_zA,_zO,_zC]:E(_zK);}),[1,_zF,_zG,_zH,_zI,_zJ,_zP]];}}else{writeOffAddr("w32",4,_zF,_zP,_zS&4294967295);0;var _zW=_zO+2|0,_zX=_zP+1|0;_zL=_zW;_zM=_zX;return null;}}else{writeOffAddr("w32",4,_zF,_zP,_zS&4294967295);0;var _zW=_zO+2|0,_zX=_zP+1|0;_zL=_zW;_zM=_zX;return null;}}else{return [1,_2,T(function(){return _zO==_zC==false?[1,_zx,_zy,_zz,_zA,_zO,_zC]:E(_zK);}),[1,_zF,_zG,_zH,_zI,_zJ,_zP]];}}else{return [1,_2,T(function(){return _zO==_zC==false?[1,_zx,_zy,_zz,_zA,_zO,_zC]:E(_zK);}),[1,_zF,_zG,_zH,_zI,_zJ,_zP]];}}else{return [1,_3,T(function(){return _zO==_zC==false?[1,_zx,_zy,_zz,_zA,_zO,_zC]:E(_zK);}),[1,_zF,_zG,_zH,_zI,_zJ,_zP]];}})(_zL,_zM,_);if(_zN!=null){return _zN;}}})(_zB,_zE[6],_);},_zY=function(_zZ,_A0,_){var _A1=E(_zZ);return _z4(_A1[1],_A1[2],_A1[3],_A1[4],_A1[5],_A1[6],_A0,_);},_A2=[2,_zY],_A3=function(_A4,_A5,_){var _A6=E(_A4);return _zw(_A6[1],_A6[2],_A6[3],_A6[4],_A6[5],_A6[6],_A5,_);},_A7=[2,_A3],_A8=function(_A9,_Aa,_Ab,_Ac,_Ad,_Ae,_Af,_Ag,_){var _Ah=rMV(_A9),_Ai=E(_Ah);if(_Ai[0]==1){if((_Af-_Ae|0)>=2){var _Aj=readOffAddr("w8",1,plusAddr(_Aa,_Ae),0),_=0,_Ak=readOffAddr("w8",1,plusAddr(_Aa,_Ae+1|0),0),_=0,_Al=function(_){if(_Aj!=255){var _=wMV(_A9,_A7);return _zw(_Aa,_Ab,_Ac,_Ad,_Ae,_Af,_Ag,_);}else{if(_Ak!=254){var _=wMV(_A9,_A7);return _zw(_Aa,_Ab,_Ac,_Ad,_Ae,_Af,_Ag,_);}else{var _=wMV(_A9,_A2);return _z4(_Aa,_Ab,_Ac,_Ad,_Ae+2|0,_Af,_Ag,_);}}};if(_Aj!=254){return _Al(_);}else{if(_Ak!=255){return _Al(_);}else{var _=wMV(_A9,_A7);return _zw(_Aa,_Ab,_Ac,_Ad,_Ae+2|0,_Af,_Ag,_);}}}else{return [1,_2,[1,_Aa,_Ab,_Ac,_Ad,_Ae,_Af],_Ag];}}else{return A(_Ai[1],[[1,_Aa,_Ab,_Ac,_Ad,_Ae,_Af],_Ag,_]);}},_Am=function(_){return _0;},_An=T(function(){return unCStr("UTF-16");}),_Ao=function(_Ap){return [1,_An,function(_){var _Aq=nMV(_1p);return [1,function(_Ar,_As,_){var _At=E(_Ar);return _A8(_Aq,_At[1],_At[2],_At[3],_At[4],_At[5],_At[6],_As,_);},function(_Au,_Av,_){return _xR(_Ap,_Au,_Av,_);},_Am,function(_){return rMV(_Aq);},function(_Aw,_){wMV(_Aq,_Aw);return _0;}];},function(_){var _Ax=nMV(_5N);return [1,function(_Ay,_Az,_){var _AA=E(_Az);return _yS(_Ax,_Ay,_AA[1],_AA[2],_AA[3],_AA[4],_AA[5],_AA[6],_);},function(_Au,_Av,_){return _ye(_Ap,_Au,_Av,_);},_Am,function(_){return rMV(_Ax);},function(_AB,_){wMV(_Ax,_AB);return _0;}];}];},_AC=function(_AD,_){return _0;},_AE=function(_Av,_){return _AC(_Av,_);},_AF=function(_AG,_AH,_){var _AI=E(_AG);return _yq(_AI[1],_AI[2],_AI[3],_AI[4],_AI[5],_AI[6],_AH,_);},_AJ=T(function(){return unCStr("UTF-16BE");}),_AK=function(_AL){return [1,_AJ,function(_){return [1,_A3,function(_Au,_Av,_){return _xR(_AL,_Au,_Av,_);},_Am,_Am,_AC];},function(_){return [1,_AF,function(_Au,_Av,_){return _ye(_AL,_Au,_Av,_);},_Am,_Am,_AE];}];},_AM=function(_Av,_){return _AC(_Av,_);},_AN=function(_AO,_AP,_AQ,_AR,_AS,_AT,_AU,_){var _AV=E(_AU),_AW=_AV[1],_AX=_AV[2],_AY=_AV[3],_AZ=_AV[4],_B0=_AV[5],_B1=[1,_AO,_AP,_AQ,_AR,0,0],_B2=function(_B3,_B4,_){while(1){var _B5=(function(_B6,_B7,_){if(_B6<_AT){if((_AZ-_B7|0)>=2){var _B8=readOffAddr("w32",4,_AO,_B6),_=0,_B9=_B8;if(_B9>=65536){if((_AZ-_B7|0)>=4){var _Ba=_B9-65536|0;writeOffAddr("w8",1,plusAddr(_AW,_B7),0,_Ba>>10>>>0&255);0;writeOffAddr("w8",1,plusAddr(_AW,_B7+1|0),0,((_Ba>>18)+216|0)>>>0&255);0;writeOffAddr("w8",1,plusAddr(_AW,_B7+2|0),0,(_Ba>>>0&1023)>>>0&255);0;writeOffAddr("w8",1,plusAddr(_AW,_B7+3|0),0,((((_Ba>>>0&1023)>>>0&4294967295)>>8)+220|0)>>>0&255);0;var _Bb=_B6+1|0,_Bc=_B7+4|0;_B3=_Bb;_B4=_Bc;return null;}else{return [1,_3,T(function(){return _B6==_AT==false?[1,_AO,_AP,_AQ,_AR,_B6,_AT]:E(_B1);}),[1,_AW,_AX,_AY,_AZ,_B0,_B7]];}}else{var _Bd=[1,_yp,T(function(){return _B6==_AT==false?[1,_AO,_AP,_AQ,_AR,_B6,_AT]:E(_B1);}),[1,_AW,_AX,_AY,_AZ,_B0,_B7]],_Be=function(_){if(56320>_B9){var _=writeOffAddr("w8",1,plusAddr(_AW,_B7),0,_B9>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_AW,_B7+1|0),0,_B9>>8>>>0&255),_=0;return _B2(_B6+1|0,_B7+2|0,_);}else{if(_B9>57343){var _=writeOffAddr("w8",1,plusAddr(_AW,_B7),0,_B9>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_AW,_B7+1|0),0,_B9>>8>>>0&255),_=0;return _B2(_B6+1|0,_B7+2|0,_);}else{return _Bd;}}};return 55296<=_B9==false?_Be(_):_B9<=56319==false?_Be(_):_Bd;}}else{return [1,_3,T(function(){return _B6==_AT==false?[1,_AO,_AP,_AQ,_AR,_B6,_AT]:E(_B1);}),[1,_AW,_AX,_AY,_AZ,_B0,_B7]];}}else{return [1,_2,T(function(){return _B6==_AT==false?[1,_AO,_AP,_AQ,_AR,_B6,_AT]:E(_B1);}),[1,_AW,_AX,_AY,_AZ,_B0,_B7]];}})(_B3,_B4,_);if(_B5!=null){return _B5;}}};return _B2(_AS,_AV[6],_);},_Bf=function(_Bg,_Bh,_){var _Bi=E(_Bg);return _AN(_Bi[1],_Bi[2],_Bi[3],_Bi[4],_Bi[5],_Bi[6],_Bh,_);},_Bj=function(_Av,_){return _AC(_Av,_);},_Bk=T(function(){return unCStr("UTF16-LE");}),_Bl=function(_Bm){return [1,_Bk,function(_){return [1,_zY,function(_Au,_Av,_){return _xR(_Bm,_Au,_Av,_);},_Am,_Am,_Bj];},function(_){return [1,_Bf,function(_Au,_Av,_){return _ye(_Bm,_Au,_Av,_);},_Am,_Am,_AM];}];},_Bn=function(_Bo,_Bp,_Bq,_Br,_Bs,_Bt,_Bu,_){var _Bv=E(_Bu),_Bw=_Bv[1],_Bx=_Bv[2],_By=_Bv[3],_Bz=_Bv[4],_BA=_Bv[5],_BB=[1,_Bo,_Bp,_Bq,_Br,0,0],_BC=function(_BD,_BE,_){if(_BD<_Bt){if((_Bz-_BE|0)>=4){var _BF=readOffAddr("w32",4,_Bo,_BD),_=0,_BG=_BF,_BH=[1,_yp,T(function(){return _BD==_Bt==false?[1,_Bo,_Bp,_Bq,_Br,_BD,_Bt]:E(_BB);}),[1,_Bw,_Bx,_By,_Bz,_BA,_BE]],_BI=function(_){if(56320>_BG){var _=writeOffAddr("w8",1,plusAddr(_Bw,_BE),0,_BG>>24>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_Bw,_BE+1|0),0,_BG>>16>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_Bw,_BE+2|0),0,_BG>>8>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_Bw,_BE+3|0),0,_BG>>>0&255),_=0;return _BC(_BD+1|0,_BE+4|0,_);}else{if(_BG>57343){var _=writeOffAddr("w8",1,plusAddr(_Bw,_BE),0,_BG>>24>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_Bw,_BE+1|0),0,_BG>>16>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_Bw,_BE+2|0),0,_BG>>8>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_Bw,_BE+3|0),0,_BG>>>0&255),_=0;return _BC(_BD+1|0,_BE+4|0,_);}else{return _BH;}}};return 55296<=_BG==false?_BI(_):_BG<=56319==false?_BI(_):_BH;}else{return [1,_3,T(function(){return _BD==_Bt==false?[1,_Bo,_Bp,_Bq,_Br,_BD,_Bt]:E(_BB);}),[1,_Bw,_Bx,_By,_Bz,_BA,_BE]];}}else{return [1,_2,T(function(){return _BD==_Bt==false?[1,_Bo,_Bp,_Bq,_Br,_BD,_Bt]:E(_BB);}),[1,_Bw,_Bx,_By,_Bz,_BA,_BE]];}};return _BC(_Bs,_Bv[6],_);},_BJ=function(_BK,_BL,_BM,_BN,_BO,_BP,_BQ,_BR,_){var _BS=rMV(_BK);if(!E(_BS)){if((_BP-_BR|0)>=4){var _=wMV(_BK,_1),_=writeOffAddr("w8",1,plusAddr(_BM,_BR),0,0),_=0,_=writeOffAddr("w8",1,plusAddr(_BM,_BR+1|0),0,0),_=0,_=writeOffAddr("w8",1,plusAddr(_BM,_BR+2|0),0,254),_=0,_=writeOffAddr("w8",1,plusAddr(_BM,_BR+3|0),0,255),_=0,_BT=E(_BL);return _Bn(_BT[1],_BT[2],_BT[3],_BT[4],_BT[5],_BT[6],[1,_BM,_BN,_BO,_BP,_BQ,_BR+4|0],_);}else{return [1,_3,_BL,[1,_BM,_BN,_BO,_BP,_BQ,_BR]];}}else{var _BU=E(_BL);return _Bn(_BU[1],_BU[2],_BU[3],_BU[4],_BU[5],_BU[6],[1,_BM,_BN,_BO,_BP,_BQ,_BR],_);}},_BV=function(_BW,_BX,_BY,_BZ,_C0,_C1,_C2,_){var _C3=E(_C2),_C4=_C3[1],_C5=_C3[2],_C6=_C3[3],_C7=_C3[4],_C8=_C3[5],_C9=[1,_BW,_BX,_BY,_BZ,0,0],_Ca=function(_Cb,_Cc,_){while(1){var _Cd=(function(_Ce,_Cf,_){if(_Cf<_C7){if((_C1-_Ce|0)>=4){var _Cg=readOffAddr("w8",1,plusAddr(_BW,_Ce),0),_=0,_Ch=readOffAddr("w8",1,plusAddr(_BW,_Ce+1|0),0),_=0,_Ci=readOffAddr("w8",1,plusAddr(_BW,_Ce+2|0),0),_=0,_Cj=readOffAddr("w8",1,plusAddr(_BW,_Ce+3|0),0),_=0,_Ck=((((_Cg&4294967295)<<24)+((_Ch&4294967295)<<16)|0)+((_Ci&4294967295)<<8)|0)+(_Cj&4294967295)|0,_Cl=_Ck,_Cm=function(_){if(_Cl<=57343){return [1,_yp,T(function(){return _Ce==_C1==false?[1,_BW,_BX,_BY,_BZ,_Ce,_C1]:E(_C9);}),[1,_C4,_C5,_C6,_C7,_C8,_Cf]];}else{if(_Cl>1114111){return [1,_yp,T(function(){return _Ce==_C1==false?[1,_BW,_BX,_BY,_BZ,_Ce,_C1]:E(_C9);}),[1,_C4,_C5,_C6,_C7,_C8,_Cf]];}else{var _=writeOffAddr("w32",4,_C4,_Cf,_Ck),_=0;return _Ca(_Ce+4|0,_Cf+1|0,_);}}};if(_Cl<0){return _Cm(_);}else{if(_Cl>=55296){return _Cm(_);}else{writeOffAddr("w32",4,_C4,_Cf,_Ck);0;var _Cn=_Ce+4|0,_Co=_Cf+1|0;_Cb=_Cn;_Cc=_Co;return null;}}}else{return [1,_2,T(function(){return _Ce==_C1==false?[1,_BW,_BX,_BY,_BZ,_Ce,_C1]:E(_C9);}),[1,_C4,_C5,_C6,_C7,_C8,_Cf]];}}else{return [1,_3,T(function(){return _Ce==_C1==false?[1,_BW,_BX,_BY,_BZ,_Ce,_C1]:E(_C9);}),[1,_C4,_C5,_C6,_C7,_C8,_Cf]];}})(_Cb,_Cc,_);if(_Cd!=null){return _Cd;}}};return _Ca(_C0,_C3[6],_);},_Cp=function(_Cq,_Cr,_Cs,_Ct,_Cu,_Cv,_Cw,_){var _Cx=E(_Cw),_Cy=_Cx[1],_Cz=_Cx[2],_CA=_Cx[3],_CB=_Cx[4],_CC=_Cx[5],_CD=[1,_Cq,_Cr,_Cs,_Ct,0,0],_CE=function(_CF,_CG,_){while(1){var _CH=(function(_CI,_CJ,_){if(_CJ<_CB){if((_Cv-_CI|0)>=4){var _CK=readOffAddr("w8",1,plusAddr(_Cq,_CI),0),_=0,_CL=readOffAddr("w8",1,plusAddr(_Cq,_CI+1|0),0),_=0,_CM=readOffAddr("w8",1,plusAddr(_Cq,_CI+2|0),0),_=0,_CN=readOffAddr("w8",1,plusAddr(_Cq,_CI+3|0),0),_=0,_CO=((((_CN&4294967295)<<24)+((_CM&4294967295)<<16)|0)+((_CL&4294967295)<<8)|0)+(_CK&4294967295)|0,_CP=_CO,_CQ=function(_){if(_CP<=57343){return [1,_yp,T(function(){return _CI==_Cv==false?[1,_Cq,_Cr,_Cs,_Ct,_CI,_Cv]:E(_CD);}),[1,_Cy,_Cz,_CA,_CB,_CC,_CJ]];}else{if(_CP>1114111){return [1,_yp,T(function(){return _CI==_Cv==false?[1,_Cq,_Cr,_Cs,_Ct,_CI,_Cv]:E(_CD);}),[1,_Cy,_Cz,_CA,_CB,_CC,_CJ]];}else{var _=writeOffAddr("w32",4,_Cy,_CJ,_CO),_=0;return _CE(_CI+4|0,_CJ+1|0,_);}}};if(_CP<0){return _CQ(_);}else{if(_CP>=55296){return _CQ(_);}else{writeOffAddr("w32",4,_Cy,_CJ,_CO);0;var _CR=_CI+4|0,_CS=_CJ+1|0;_CF=_CR;_CG=_CS;return null;}}}else{return [1,_2,T(function(){return _CI==_Cv==false?[1,_Cq,_Cr,_Cs,_Ct,_CI,_Cv]:E(_CD);}),[1,_Cy,_Cz,_CA,_CB,_CC,_CJ]];}}else{return [1,_3,T(function(){return _CI==_Cv==false?[1,_Cq,_Cr,_Cs,_Ct,_CI,_Cv]:E(_CD);}),[1,_Cy,_Cz,_CA,_CB,_CC,_CJ]];}})(_CF,_CG,_);if(_CH!=null){return _CH;}}};return _CE(_Cu,_Cx[6],_);},_CT=function(_CU,_CV,_){var _CW=E(_CU);return _BV(_CW[1],_CW[2],_CW[3],_CW[4],_CW[5],_CW[6],_CV,_);},_CX=[2,_CT],_CY=function(_CZ,_D0,_){var _D1=E(_CZ);return _Cp(_D1[1],_D1[2],_D1[3],_D1[4],_D1[5],_D1[6],_D0,_);},_D2=[2,_CY],_D3=function(_D4,_D5,_D6,_D7,_D8,_D9,_Da,_Db,_){var _Dc=rMV(_D4),_Dd=E(_Dc);if(_Dd[0]==1){if((_Da-_D9|0)>=4){var _De=readOffAddr("w8",1,plusAddr(_D5,_D9),0),_=0,_Df=readOffAddr("w8",1,plusAddr(_D5,_D9+1|0),0),_=0,_Dg=readOffAddr("w8",1,plusAddr(_D5,_D9+2|0),0),_=0,_Dh=readOffAddr("w8",1,plusAddr(_D5,_D9+3|0),0),_=0,_Di=function(_){if(_De!=255){var _=wMV(_D4,_CX);return _BV(_D5,_D6,_D7,_D8,_D9,_Da,_Db,_);}else{if(_Df!=254){var _=wMV(_D4,_CX);return _BV(_D5,_D6,_D7,_D8,_D9,_Da,_Db,_);}else{if(_Dg!=0){var _=wMV(_D4,_CX);return _BV(_D5,_D6,_D7,_D8,_D9,_Da,_Db,_);}else{if(_Dh!=0){var _=wMV(_D4,_CX);return _BV(_D5,_D6,_D7,_D8,_D9,_Da,_Db,_);}else{var _=wMV(_D4,_D2);return _Cp(_D5,_D6,_D7,_D8,_D9+4|0,_Da,_Db,_);}}}}};if(_De!=0){return _Di(_);}else{if(_Df!=0){return _Di(_);}else{if(_Dg!=254){return _Di(_);}else{if(_Dh!=255){return _Di(_);}else{var _=wMV(_D4,_CX);return _BV(_D5,_D6,_D7,_D8,_D9+4|0,_Da,_Db,_);}}}}}else{return [1,_2,[1,_D5,_D6,_D7,_D8,_D9,_Da],_Db];}}else{return A(_Dd[1],[[1,_D5,_D6,_D7,_D8,_D9,_Da],_Db,_]);}},_Dj=function(_){return _0;},_Dk=T(function(){return unCStr("UTF-32");}),_Dl=function(_Dm){return [1,_Dk,function(_){var _Dn=nMV(_1p);return [1,function(_Do,_Dp,_){var _Dq=E(_Do);return _D3(_Dn,_Dq[1],_Dq[2],_Dq[3],_Dq[4],_Dq[5],_Dq[6],_Dp,_);},function(_Dr,_Ds,_){return _xR(_Dm,_Dr,_Ds,_);},_Dj,function(_){return rMV(_Dn);},function(_Dt,_){wMV(_Dn,_Dt);return _0;}];},function(_){var _Du=nMV(_5N);return [1,function(_Dv,_Dw,_){var _Dx=E(_Dw);return _BJ(_Du,_Dv,_Dx[1],_Dx[2],_Dx[3],_Dx[4],_Dx[5],_Dx[6],_);},function(_Dr,_Ds,_){return _ye(_Dm,_Dr,_Ds,_);},_Dj,function(_){return rMV(_Du);},function(_Dy,_){wMV(_Du,_Dy);return _0;}];}];},_Dz=function(_DA,_){return _0;},_DB=function(_Ds,_){return _Dz(_Ds,_);},_DC=function(_DD,_DE,_){var _DF=E(_DD);return _Bn(_DF[1],_DF[2],_DF[3],_DF[4],_DF[5],_DF[6],_DE,_);},_DG=T(function(){return unCStr("UTF-32BE");}),_DH=function(_DI){return [1,_DG,function(_){return [1,_CT,function(_Dr,_Ds,_){return _xR(_DI,_Dr,_Ds,_);},_Dj,_Dj,_Dz];},function(_){return [1,_DC,function(_Dr,_Ds,_){return _ye(_DI,_Dr,_Ds,_);},_Dj,_Dj,_DB];}];},_DJ=function(_Ds,_){return _Dz(_Ds,_);},_DK=function(_DL,_DM,_DN,_DO,_DP,_DQ,_DR,_){var _DS=E(_DR),_DT=_DS[1],_DU=_DS[2],_DV=_DS[3],_DW=_DS[4],_DX=_DS[5],_DY=[1,_DL,_DM,_DN,_DO,0,0],_DZ=function(_E0,_E1,_){if(_E0<_DQ){if((_DW-_E1|0)>=4){var _E2=readOffAddr("w32",4,_DL,_E0),_=0,_E3=_E2,_E4=[1,_yp,T(function(){return _E0==_DQ==false?[1,_DL,_DM,_DN,_DO,_E0,_DQ]:E(_DY);}),[1,_DT,_DU,_DV,_DW,_DX,_E1]],_E5=function(_){if(56320>_E3){var _=writeOffAddr("w8",1,plusAddr(_DT,_E1),0,_E3>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_DT,_E1+1|0),0,_E3>>8>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_DT,_E1+2|0),0,_E3>>16>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_DT,_E1+3|0),0,_E3>>24>>>0&255),_=0;return _DZ(_E0+1|0,_E1+4|0,_);}else{if(_E3>57343){var _=writeOffAddr("w8",1,plusAddr(_DT,_E1),0,_E3>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_DT,_E1+1|0),0,_E3>>8>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_DT,_E1+2|0),0,_E3>>16>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_DT,_E1+3|0),0,_E3>>24>>>0&255),_=0;return _DZ(_E0+1|0,_E1+4|0,_);}else{return _E4;}}};return 55296<=_E3==false?_E5(_):_E3<=56319==false?_E5(_):_E4;}else{return [1,_3,T(function(){return _E0==_DQ==false?[1,_DL,_DM,_DN,_DO,_E0,_DQ]:E(_DY);}),[1,_DT,_DU,_DV,_DW,_DX,_E1]];}}else{return [1,_2,T(function(){return _E0==_DQ==false?[1,_DL,_DM,_DN,_DO,_E0,_DQ]:E(_DY);}),[1,_DT,_DU,_DV,_DW,_DX,_E1]];}};return _DZ(_DP,_DS[6],_);},_E6=function(_E7,_E8,_){var _E9=E(_E7);return _DK(_E9[1],_E9[2],_E9[3],_E9[4],_E9[5],_E9[6],_E8,_);},_Ea=function(_Ds,_){return _Dz(_Ds,_);},_Eb=T(function(){return unCStr("UTF-32LE");}),_Ec=function(_Ed){return [1,_Eb,function(_){return [1,_CY,function(_Dr,_Ds,_){return _xR(_Ed,_Dr,_Ds,_);},_Dj,_Dj,_Ea];},function(_){return [1,_E6,function(_Dr,_Ds,_){return _ye(_Ed,_Dr,_Ds,_);},_Dj,_Dj,_DJ];}];},_Ee=function(_Ef,_){return _0;},_Eg=function(_Eh,_){return _Ee(_Eh,_);},_Ei=function(_Ej,_Ek,_El,_Em,_En,_Eo,_Ep,_){var _Eq=E(_Ep),_Er=_Eq[1],_Es=_Eq[2],_Et=_Eq[3],_Eu=_Eq[4],_Ev=_Eq[5],_Ew=[1,_Ej,_Ek,_El,_Em,0,0],_Ex=function(_Ey,_Ez,_){while(1){var _EA=(function(_EB,_EC,_){if(_EC<_Eu){if(_EB<_Eo){var _ED=readOffAddr("w32",4,_Ej,_EB),_=0,_EE=_ED;if(_EE>127){if(_EE>2047){if(_EE>65535){if((_Eu-_EC|0)>=4){writeOffAddr("w8",1,plusAddr(_Er,_EC),0,((_EE>>18)+240|0)>>>0&255);0;writeOffAddr("w8",1,plusAddr(_Er,_EC+1|0),0,(((_EE>>12>>>0&63)>>>0&4294967295)+128|0)>>>0&255);0;writeOffAddr("w8",1,plusAddr(_Er,_EC+2|0),0,(((_EE>>6>>>0&63)>>>0&4294967295)+128|0)>>>0&255);0;writeOffAddr("w8",1,plusAddr(_Er,_EC+3|0),0,(((_EE>>>0&63)>>>0&4294967295)+128|0)>>>0&255);0;var _EF=_EB+1|0,_EG=_EC+4|0;_Ey=_EF;_Ez=_EG;return null;}else{return [1,_3,T(function(){return _EB==_Eo==false?[1,_Ej,_Ek,_El,_Em,_EB,_Eo]:E(_Ew);}),[1,_Er,_Es,_Et,_Eu,_Ev,_EC]];}}else{var _EH=[1,_yp,T(function(){return _EB==_Eo==false?[1,_Ej,_Ek,_El,_Em,_EB,_Eo]:E(_Ew);}),[1,_Er,_Es,_Et,_Eu,_Ev,_EC]],_EI=function(_){var _EJ=function(_){if((_Eu-_EC|0)>=3){var _=writeOffAddr("w8",1,plusAddr(_Er,_EC),0,((_EE>>12)+224|0)>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_Er,_EC+1|0),0,(((_EE>>6>>>0&63)>>>0&4294967295)+128|0)>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_Er,_EC+2|0),0,(((_EE>>>0&63)>>>0&4294967295)+128|0)>>>0&255),_=0;return _Ex(_EB+1|0,_EC+3|0,_);}else{return [1,_3,T(function(){return _EB==_Eo==false?[1,_Ej,_Ek,_El,_Em,_EB,_Eo]:E(_Ew);}),[1,_Er,_Es,_Et,_Eu,_Ev,_EC]];}};return 56320<=_EE==false?_EJ(_):_EE<=57343==false?_EJ(_):_EH;};return 55296<=_EE==false?_EI(_):_EE<=56319==false?_EI(_):_EH;}}else{if((_Eu-_EC|0)>=2){writeOffAddr("w8",1,plusAddr(_Er,_EC),0,((_EE>>6)+192|0)>>>0&255);0;writeOffAddr("w8",1,plusAddr(_Er,_EC+1|0),0,(((_EE>>>0&63)>>>0&4294967295)+128|0)>>>0&255);0;var _EF=_EB+1|0,_EG=_EC+2|0;_Ey=_EF;_Ez=_EG;return null;}else{return [1,_3,T(function(){return _EB==_Eo==false?[1,_Ej,_Ek,_El,_Em,_EB,_Eo]:E(_Ew);}),[1,_Er,_Es,_Et,_Eu,_Ev,_EC]];}}}else{writeOffAddr("w8",1,plusAddr(_Er,_EC),0,_EE>>>0&255);0;var _EF=_EB+1|0,_EG=_EC+1|0;_Ey=_EF;_Ez=_EG;return null;}}else{return [1,_2,T(function(){return _EB==_Eo==false?[1,_Ej,_Ek,_El,_Em,_EB,_Eo]:E(_Ew);}),[1,_Er,_Es,_Et,_Eu,_Ev,_EC]];}}else{return [1,_3,T(function(){return _EB==_Eo==false?[1,_Ej,_Ek,_El,_Em,_EB,_Eo]:E(_Ew);}),[1,_Er,_Es,_Et,_Eu,_Ev,_EC]];}})(_Ey,_Ez,_);if(_EA!=null){return _EA;}}};return _Ex(_En,_Eq[6],_);},_EK=function(_EL,_EM,_){var _EN=E(_EL);return _Ei(_EN[1],_EN[2],_EN[3],_EN[4],_EN[5],_EN[6],_EM,_);},_EO=function(_){return _0;},_EP=function(_EQ,_ER,_ES,_ET,_EU,_EV,_EW,_){var _EX=E(_EW),_EY=_EX[1],_EZ=_EX[2],_F0=_EX[3],_F1=_EX[4],_F2=_EX[5],_F3=[1,_EQ,_ER,_ES,_ET,0,0],_F4=function(_F5,_F6,_){while(1){var _F7=(function(_F8,_F9,_){if(_F9<_F1){var _Fa=[1,_2,T(function(){return _F8==_EV==false?[1,_EQ,_ER,_ES,_ET,_F8,_EV]:E(_F3);}),[1,_EY,_EZ,_F0,_F1,_F2,_F9]];if(_F8<_EV){var _Fb=readOffAddr("w8",1,plusAddr(_EQ,_F8),0),_=0;if(_Fb>127){var _Fc=[1,_yp,T(function(){return _F8==_EV==false?[1,_EQ,_ER,_ES,_ET,_F8,_EV]:E(_F3);}),[1,_EY,_EZ,_F0,_F1,_F2,_F9]],_Fd=function(_){var _Fe=function(_){if(_Fb<240){return _Fc;}else{switch(_EV-_F8|0){case 1:return _Fa;case 2:var _Ff=readOffAddr("w8",1,plusAddr(_EQ,_F8+1|0),0),_=0,_Fg=T(function(){return _Ff<=191;}),_Fh=function(_){return _Fb>=241==false?_Fb==244==false?_Fc:_Ff>=128==false?_Fc:_Ff<=143==false?_Fc:_Fa:_Fb<=243==false?_Fb==244==false?_Fc:_Ff>=128==false?_Fc:_Ff<=143==false?_Fc:_Fa:_Ff>=128==false?_Fc:E(_Fg)==false?_Fb==244==false?_Fc:_Ff<=143==false?_Fc:_Fa:_Fa;};return _Fb==240==false?_Fh(_):_Ff>=144==false?_Fh(_):E(_Fg)==false?_Fh(_):_Fa;case 3:var _Fi=readOffAddr("w8",1,plusAddr(_EQ,_F8+1|0),0),_=0,_Fj=readOffAddr("w8",1,plusAddr(_EQ,_F8+2|0),0),_=0,_Fk=T(function(){return _Fj>=128;}),_Fl=T(function(){return _Fj<=191;}),_Fm=T(function(){return _Fi<=191;}),_Fn=function(_){var _Fo=T(function(){return _Fi>=128;}),_Fp=function(_){return _Fb==244==false?_Fc:E(_Fo)==false?_Fc:_Fi<=143==false?_Fc:E(_Fk)==false?_Fc:E(_Fl)==false?_Fc:_Fa;};return _Fb>=241==false?_Fp(_):_Fb<=243==false?_Fp(_):E(_Fo)==false?_Fp(_):E(_Fm)==false?_Fp(_):E(_Fk)==false?_Fp(_):E(_Fl)==false?_Fp(_):_Fa;};return _Fb==240==false?_Fn(_):_Fi>=144==false?_Fn(_):E(_Fm)==false?_Fn(_):E(_Fk)==false?_Fn(_):E(_Fl)==false?_Fn(_):_Fa;default:var _Fq=readOffAddr("w8",1,plusAddr(_EQ,_F8+1|0),0),_=0,_Fr=readOffAddr("w8",1,plusAddr(_EQ,_F8+2|0),0),_=0,_Fs=readOffAddr("w8",1,plusAddr(_EQ,_F8+3|0),0),_=0,_Ft=T(function(){return _Fs>=128;}),_Fu=T(function(){return _Fs<=191;}),_Fv=T(function(){return _Fr>=128;}),_Fw=T(function(){return _Fr<=191;}),_Fx=T(function(){return _Fq<=191;}),_Fy=function(_){var _Fz=T(function(){return _Fq>=128;}),_FA=function(_){if(_Fb!=244){return _Fc;}else{if(!E(_Fz)){return _Fc;}else{if(_Fq>143){return _Fc;}else{if(!E(_Fv)){return _Fc;}else{if(!E(_Fw)){return _Fc;}else{if(!E(_Ft)){return _Fc;}else{if(!E(_Fu)){return _Fc;}else{var _=writeOffAddr("w32",4,_EY,_F9,(((((_Fb&4294967295)-240|0)<<18)+(((_Fq&4294967295)-128|0)<<12)|0)+(((_Fr&4294967295)-128|0)<<6)|0)+((_Fs&4294967295)-128|0)|0),_=0;return _F4(_F8+4|0,_F9+1|0,_);}}}}}}}};if(_Fb<241){return _FA(_);}else{if(_Fb>243){return _FA(_);}else{if(!E(_Fz)){return _FA(_);}else{if(!E(_Fx)){return _FA(_);}else{if(!E(_Fv)){return _FA(_);}else{if(!E(_Fw)){return _FA(_);}else{if(!E(_Ft)){return _FA(_);}else{if(!E(_Fu)){return _FA(_);}else{var _=writeOffAddr("w32",4,_EY,_F9,(((((_Fb&4294967295)-240|0)<<18)+(((_Fq&4294967295)-128|0)<<12)|0)+(((_Fr&4294967295)-128|0)<<6)|0)+((_Fs&4294967295)-128|0)|0),_=0;return _F4(_F8+4|0,_F9+1|0,_);}}}}}}}}};if(_Fb!=240){return _Fy(_);}else{if(_Fq<144){return _Fy(_);}else{if(!E(_Fx)){return _Fy(_);}else{if(!E(_Fv)){return _Fy(_);}else{if(!E(_Fw)){return _Fy(_);}else{if(!E(_Ft)){return _Fy(_);}else{if(!E(_Fu)){return _Fy(_);}else{var _=writeOffAddr("w32",4,_EY,_F9,(((((_Fb&4294967295)-240|0)<<18)+(((_Fq&4294967295)-128|0)<<12)|0)+(((_Fr&4294967295)-128|0)<<6)|0)+((_Fs&4294967295)-128|0)|0),_=0;return _F4(_F8+4|0,_F9+1|0,_);}}}}}}}}}};if(_Fb<224){return _Fe(_);}else{if(_Fb>239){return _Fe(_);}else{switch(_EV-_F8|0){case 1:return _Fa;case 2:var _FB=readOffAddr("w8",1,plusAddr(_EQ,_F8+1|0),0),_=0,_FC=T(function(){return _FB<=191;}),_FD=function(_){var _FE=T(function(){return _FB>=128;}),_FF=function(_){return _Fb==237==false?_Fb>=238==false?_Fc:E(_FE)==false?_Fc:E(_FC)==false?_Fc:_Fa:E(_FE)==false?_Fc:_FB<=159==false?_Fb>=238==false?_Fc:E(_FC)==false?_Fc:_Fa:_Fa;};return _Fb>=225==false?_FF(_):_Fb<=236==false?_FF(_):E(_FE)==false?_FF(_):E(_FC)==false?_FF(_):_Fa;};return _Fb==224==false?_FD(_):_FB>=160==false?_FD(_):E(_FC)==false?_FD(_):_Fa;default:var _FG=readOffAddr("w8",1,plusAddr(_EQ,_F8+1|0),0),_=0,_FH=readOffAddr("w8",1,plusAddr(_EQ,_F8+2|0),0),_=0,_FI=T(function(){return _FH>=128;}),_FJ=T(function(){return _FH<=191;}),_FK=T(function(){return _FG<=191;}),_FL=function(_){var _FM=T(function(){return _FG>=128;}),_FN=function(_){var _FO=function(_){if(_Fb<238){return _Fc;}else{if(!E(_FM)){return _Fc;}else{if(!E(_FK)){return _Fc;}else{if(!E(_FI)){return _Fc;}else{if(!E(_FJ)){return _Fc;}else{var _=writeOffAddr("w32",4,_EY,_F9,((((_Fb&4294967295)-224|0)<<12)+(((_FG&4294967295)-128|0)<<6)|0)+((_FH&4294967295)-128|0)|0),_=0;return _F4(_F8+3|0,_F9+1|0,_);}}}}}};if(_Fb!=237){return _FO(_);}else{if(!E(_FM)){return _FO(_);}else{if(_FG>159){return _FO(_);}else{if(!E(_FI)){return _FO(_);}else{if(!E(_FJ)){return _FO(_);}else{var _=writeOffAddr("w32",4,_EY,_F9,((((_Fb&4294967295)-224|0)<<12)+(((_FG&4294967295)-128|0)<<6)|0)+((_FH&4294967295)-128|0)|0),_=0;return _F4(_F8+3|0,_F9+1|0,_);}}}}}};if(_Fb<225){return _FN(_);}else{if(_Fb>236){return _FN(_);}else{if(!E(_FM)){return _FN(_);}else{if(!E(_FK)){return _FN(_);}else{if(!E(_FI)){return _FN(_);}else{if(!E(_FJ)){return _FN(_);}else{var _=writeOffAddr("w32",4,_EY,_F9,((((_Fb&4294967295)-224|0)<<12)+(((_FG&4294967295)-128|0)<<6)|0)+((_FH&4294967295)-128|0)|0),_=0;return _F4(_F8+3|0,_F9+1|0,_);}}}}}}};if(_Fb!=224){return _FL(_);}else{if(_FG<160){return _FL(_);}else{if(!E(_FK)){return _FL(_);}else{if(!E(_FI)){return _FL(_);}else{if(!E(_FJ)){return _FL(_);}else{var _=writeOffAddr("w32",4,_EY,_F9,((((_Fb&4294967295)-224|0)<<12)+(((_FG&4294967295)-128|0)<<6)|0)+((_FH&4294967295)-128|0)|0),_=0;return _F4(_F8+3|0,_F9+1|0,_);}}}}}}}}};if(_Fb<192){return _Fd(_);}else{if(_Fb>223){return _Fd(_);}else{if((_EV-_F8|0)>=2){var _FP=readOffAddr("w8",1,plusAddr(_EQ,_F8+1|0),0);0;if(_FP>=128){if(_FP<192){writeOffAddr("w32",4,_EY,_F9,(((_Fb&4294967295)-192|0)<<6)+((_FP&4294967295)-128|0)|0);0;var _FQ=_F8+2|0,_FR=_F9+1|0;_F5=_FQ;_F6=_FR;return null;}else{return _Fc;}}else{return _Fc;}}else{return _Fa;}}}}else{writeOffAddr("w32",4,_EY,_F9,_Fb&4294967295);0;var _FQ=_F8+1|0,_FR=_F9+1|0;_F5=_FQ;_F6=_FR;return null;}}else{return _Fa;}}else{return [1,_3,T(function(){return _F8==_EV==false?[1,_EQ,_ER,_ES,_ET,_F8,_EV]:E(_F3);}),[1,_EY,_EZ,_F0,_F1,_F2,_F9]];}})(_F5,_F6,_);if(_F7!=null){return _F7;}}};return _F4(_EU,_EX[6],_);},_FS=function(_FT,_FU,_){var _FV=E(_FT);return _EP(_FV[1],_FV[2],_FV[3],_FV[4],_FV[5],_FV[6],_FU,_);},_FW=T(function(){return unCStr("UTF-8");}),_FX=function(_FY){return [1,_FW,function(_){return [1,_FS,function(_FZ,_Eh,_){return _xR(_FY,_FZ,_Eh,_);},_EO,_EO,_Ee];},function(_){return [1,_EK,function(_FZ,_Eh,_){return _ye(_FY,_FZ,_Eh,_);},_EO,_EO,_Eg];}];},_G0=function(_G1,_G2,_){var _G3=_uk(_G2);return _hR(_G3,_u9)==false?_hR(_G3,_u8)==false?_hR(_G3,_ue)==false?_hR(_G3,_ud)==false?_hR(_G3,_uc)==false?_hR(_G3,_ub)==false?_hR(_G3,_ua)==false?_yj(_G1,_G2,_):T(function(){return _FX(_G1);}):T(function(){return _Ec(_G1);}):T(function(){return _DH(_G1);}):T(function(){return _Dl(_G1);}):T(function(){return _Bl(_G1);}):T(function(){return _AK(_G1);}):T(function(){return _Ao(_G1);});},_G4=function(_G5,_){var _G6=(function(_G7,_){while(1){var _G8=readOffAddr("i8",1,_G5,_G7);if(!E(_G8)){return [1,_G7];}else{var _G9=_G7+1|0;_G7=_G9;continue;}}})(0,_),_Ga=E(_G6)[1];if(_Ga>0){return (function(_Gb,_Gc,_){while(1){var _Gd=readOffAddr("i8",1,_G5,_Gc);if(_Gc>0){var _Ge=[2,[1,_Gd>>>0&255&4294967295],_Gb],_Gf=_Gc-1|0;_Gb=_Ge;_Gc=_Gf;continue;}else{return [2,[1,_Gd>>>0&255&4294967295],_Gb];}}})(_1q,_Ga-1|0,_);}else{return _1q;}},_Gg=function(_){var _=0,_Gh=localeEncoding();return _G4(_Gh,_);},_Gi=function(_Gj){var _Gk=A(_Gj,[_]);return E(_Gk);},_Gl=T(function(){return _Gi(_Gg);}),_Gm=function(_){var _=0;return _G0(_u7,_Gl,_);},_Gn=T(function(){return _Gi(_Gm);}),_Go=function(_Gp){return _Gi(function(_){0;var _Gq=nMV(_Gp);return [1,function(_){return rMV(_Gq);},function(_Gr,_){wMV(_Gq,_Gr);return _0;}];});},_Gs=T(function(){return _Go(_Gn);}),_vk=function(_Gt,_Gu,_Gv,_Gw){return _Gi(function(_){var _=0,_Gx=E(_Gu),_Gy=_Gx[1],_Gz=strerror(_Gy),_GA=A(E(_Gs)[1],[_]),_GB=_to(_GA,_Gz,_);return [1,_Gv,T(function(){switch(E(_Gy)){case 1:return [7];case 2:return [2];case 3:return [2];case 4:return [19];case 5:return [15];case 6:return [2];case 7:return [4];case 8:return [13];case 9:return [13];case 10:return [2];case 11:return [4];case 12:return [4];case 13:return [7];case 15:return [13];case 16:return [3];case 17:return [1];case 18:return [16];case 19:return [16];case 20:return [14];case 21:return [14];case 22:return [13];case 23:return [4];case 24:return [4];case 25:return [6];case 26:return [3];case 27:return [7];case 28:return [4];case 29:return [16];case 30:return [7];case 31:return [4];case 32:return [18];case 33:return [13];case 34:return [16];case 35:return [3];case 36:return [13];case 37:return [4];case 38:return [16];case 39:return [9];case 40:return [13];case 42:return [2];case 43:return [18];case 60:return [13];case 61:return [2];case 62:return [17];case 63:return [4];case 64:return [2];case 66:return [6];case 67:return [18];case 69:return [9];case 70:return [18];case 71:return [11];case 72:return [16];case 74:return [14];case 78:return [18];case 84:return [13];case 87:return [4];case 88:return [13];case 89:return [13];case 90:return [4];case 91:return [11];case 92:return [16];case 93:return [11];case 94:return [16];case 95:return [16];case 96:return [16];case 97:return [16];case 98:return [3];case 99:return [16];case 100:return [18];case 101:return [2];case 102:return [18];case 104:return [18];case 105:return [4];case 106:return [1];case 107:return [13];case 108:return [6];case 109:return [4];case 110:return [17];case 111:return [2];case 112:return [2];case 113:return [2];case 114:return [1];case 115:return [1];case 116:return [18];case 122:return [7];default:return [12];}}),_Gt,_GB,[2,_Gx],_Gw];});},_GC=function(_GD,_){var _GE=__hscore_get_errno();return _3J(_vk(_GD,[1,_GE],_1p,_1p),_);},_GF=function(_GG,_GH,_GI,_){while(1){var _GJ=A(_GH,[_]),_GK=E(_GJ);if(E(_GK[1])==(-1)){var _GL=__hscore_get_errno();switch(E(_GL)){case 4:continue;case 11:return A(_GI,[_]);default:return _GC(_GG,_);}}else{return _GK;}}},_GM=T(function(){return unCStr("GHC.IO.FD.fdWriteNonBlocking");}),_GN=function(_GO,_GP,_){if(!E(_GP)){var _GQ=fdReady(_GO,1,0,0);if(!E(_GQ)){return _sv;}else{var _GR=rtsSupportsBoundThreads();if(!E(_GR)){var _GS=_GF(_GM,_st,_sx,_),_GT=E(E(_GS)[1]);return _GT==(-1)?_sv:[1,_GT];}else{var _GU=_GF(_GM,_su,_sx,_),_GV=E(E(_GU)[1]);return _GV==(-1)?_sv:[1,_GV];}}}else{var _GW=_GF(_GM,_sy,_sx,_),_GX=E(E(_GW)[1]);return _GX==(-1)?_sv:[1,_GX];}},_GY=function(_GZ,_H0,_H1,_H2,_H3,_H4,_H5,_){var _H6=_GN(_GZ,_H0,_);0;return [1,_H6,T(function(){var _H7=E(_H6)[1];return _H7==_H5==false?[1,_H1,_H2,_H3,_H4,_H7,_H5]:[1,_H1,_H2,_H3,_H4,0,0];})];},_H8=function(_H9,_Ha,_){var _Hb=E(_H9),_Hc=E(_Ha);return _GY(_Hb[1],_Hb[2],_Hc[1],_Hc[2],_Hc[3],_Hc[4],_Hc[6],_);},_Hd=[1,2],_He=T(function(){return E(_Hd);}),_Hf=function(_Hg,_Hh){while(1){var _Hi=E(_Hh);if(_Hi[0]==1){return E(_Hg);}else{var _Hj=(_Hg>>>0|E(_Hi[1])[1]>>>0)>>>0&4294967295;_Hh=_Hi[2];_Hg=_Hj;continue;}}},_Hk=[1],_Hl=function(_Hm,_Hn,_Ho){var _Hp=E(_Ho);switch(_Hp[0]){case 1:return [1,_1p,_Hk];case 2:var _Hq=_Hp[1],_Hr=_Hp[2];if(E(_Hn)[1]!=_Hq){return [1,_1p,_Hp];}else{var _Hs=A(_Hm,[_Hr]);return _Hs[0]==1?[1,[2,_Hr],_Hk]:[1,[2,_Hr],T(function(){return [2,_Hq,E(E(_Hs[1]))];})];}break;default:var _Ht=_Hp[1],_Hu=_Hp[2],_Hv=_Hp[3],_Hw=_Hp[4],_Hx=E(_Hn),_Hy=_Hx[1],_Hz=_Hu>>>0;if(((_Hy>>>0&((_Hz-1>>>0^4294967295)>>>0^_Hz)>>>0)>>>0&4294967295)==_Ht){if((_Hy>>>0&_Hz)>>>0!=0){var _HA=T(function(){var _HB=_Hl(_Hm,_Hx,_Hw);return [1,_HB[1],_HB[2]];});return [1,T(function(){return E(E(_HA)[1]);}),T(function(){var _HC=E(E(_HA)[2]);if(_HC[0]==1){return E(_Hv);}else{var _HD=E(_Hv);return _HD[0]==1?E(_HC):[3,_Ht,_Hu,E(_HD),E(_HC)];}})];}else{var _HE=T(function(){var _HF=_Hl(_Hm,_Hx,_Hv);return [1,_HF[1],_HF[2]];});return [1,T(function(){return E(E(_HE)[1]);}),T(function(){var _HG=E(_Hw);if(_HG[0]==1){return E(E(_HE)[2]);}else{var _HH=E(E(_HE)[2]);return _HH[0]==1?E(_HG):[3,_Ht,_Hu,E(_HH),E(_HG)];}})];}}else{return [1,_1p,_Hp];}}},_HI=function(_HJ){return [1,E(_HJ)[3]];},_HK=function(_HL,_HM){while(1){var _HN=E(_HM);switch(_HN[0]){case 1:return [1];case 2:return E(_HL)[1]==_HN[1]>>>0==false?[1]:[2,_HN[2]];default:var _HO=E(_HL);if((_HO[1]&_HN[2]>>>0)>>>0!=0){_HL=_HO;_HM=_HN[4];continue;}else{_HL=_HO;_HM=_HN[3];continue;}}}},_HP=function(_HQ,_HR,_){A(_HQ,[_]);return die(_HR);},_HS=function(_HT,_HU,_HV,_){return _HP(function(_){putMVar(_HU,_HT);return _0;},_HV,_);},_HW=function(_HX,_HY){var _HZ=hs_neInt64(_HX,_HY);return E(_HZ)==0?false:true;},_I0=function(_I1,_I2,_I3,_I4,_I5,_I6,_I7,_){var _I8=[1,_I6],_I9=0,_Ia=function(_Ib,_){var _Ic=_Hl(function(_Id){var _Ie=_nO(function(_If){return _HW(E(_If)[2],_I7);},_Id);return _Ie[0]==1?[1]:[2,_Ie];},_I8,_Ib),_Ig=_Ic[2],_Ih=function(_Ii,_Ij,_Ik){if(_Ij==_Ik){return [1,_Ii,_5N];}else{var _Il=E(_I1);A(_Il[3],[_Il[1],_I8,[1,_Ij],[1,_Ik],_]);return [1,_Ii,_1];}},_Im=E(_Ic[1]);if(_Im[0]==1){return _Ih(E(_Ib),0,0);}else{var _In=_Hf(0,_h6(_HI,_Im[1])),_Io=_HK([1,_I8[1]>>>0],_Ig);if(_Io[0]==1){return _Ih(_Ig,_In,0);}else{return _Ih(_Ig,_In,_Hf(0,_h6(_HI,_Io[1])));}}};if(!E(_I9)){return (function(_){var _Ip=takeMVar(_I2),_Iq=jsCatch(function(_){return (function(_){return _Ia(_Ip,_);})();},function(_Ir,_){return _HS(_Ip,_I2,_Ir,_);}),_Is=E(_Iq);putMVar(_I2,_Is[1]);return _Is[2];})();}else{var _It=takeMVar(_I2),_Iu=jsCatch(function(_){return _Ia(_It,_);},function(_Ir,_){return _HS(_It,_I2,_Ir,_);}),_Iv=E(_Iu);putMVar(_I2,_Iv[1]);return _Iv[2];}},_Iw=function(_Ix,_Iy){return [2,E(_Ix)[1],E(E(_Iy))];},_Iz=function(_IA,_IB,_IC,_ID){var _IE=E(_ID);switch(_IE[0]){case 1:return [1,_1p,T(function(){return _Iw(_IB,_IC);})];case 2:var _IF=_IE[1],_IG=_IE[2],_IH=E(_IB)[1];return _IH==_IF==false?[1,_1p,T(function(){var _II=(_IH>>>0^_IF>>>0)>>>0,_IJ=(_II|_II>>>1)>>>0,_IK=(_IJ|_IJ>>>2)>>>0,_IL=(_IK|_IK>>>4)>>>0,_IM=(_IL|_IL>>>8)>>>0,_IN=((_IM|_IM>>>16)>>>0|0)>>>0;if((_IH>>>0&(_IN^_IN>>>1)>>>0)>>>0!=0){var _IO=(_IN^_IN>>>1)>>>0;return [3,(_IH>>>0&((_IO-1>>>0^4294967295)>>>0^_IO)>>>0)>>>0&4294967295,_IO&4294967295,E(_IE),E([2,_IH,E(E(_IC))])];}else{var _IP=(_IN^_IN>>>1)>>>0;return [3,(_IH>>>0&((_IP-1>>>0^4294967295)>>>0^_IP)>>>0)>>>0&4294967295,_IP&4294967295,E([2,_IH,E(E(_IC))]),E(_IE)];}})]:[1,[2,_IG],T(function(){return [2,_IH,E(A(_IA,[_IC,_IG]))];})];default:var _IQ=_IE[1],_IR=_IE[2],_IS=_IE[3],_IT=_IE[4],_IU=E(_IB),_IV=_IU[1],_IW=_IR>>>0;if(((_IV>>>0&((_IW-1>>>0^4294967295)>>>0^_IW)>>>0)>>>0&4294967295)==_IQ){if((_IV>>>0&_IW)>>>0!=0){var _IX=T(function(){var _IY=_Iz(_IA,_IU,_IC,_IT);return [1,_IY[1],_IY[2]];});return [1,T(function(){return E(E(_IX)[1]);}),T(function(){return [3,_IQ,_IR,E(_IS),E(E(E(_IX)[2]))];})];}else{var _IZ=T(function(){var _J0=_Iz(_IA,_IU,_IC,_IS);return [1,_J0[1],_J0[2]];});return [1,T(function(){return E(E(_IZ)[1]);}),T(function(){return [3,_IQ,_IR,E(E(E(_IZ)[2])),E(_IT)];})];}}else{return [1,_1p,T(function(){var _J1=(_IV>>>0^_IQ>>>0)>>>0,_J2=(_J1|_J1>>>1)>>>0,_J3=(_J2|_J2>>>2)>>>0,_J4=(_J3|_J3>>>4)>>>0,_J5=(_J4|_J4>>>8)>>>0,_J6=((_J5|_J5>>>16)>>>0|0)>>>0;if((_IV>>>0&(_J6^_J6>>>1)>>>0)>>>0!=0){var _J7=(_J6^_J6>>>1)>>>0;return [3,(_IV>>>0&((_J7-1>>>0^4294967295)>>>0^_J7)>>>0)>>>0&4294967295,_J7&4294967295,E(_IE),E([2,_IV,E(E(_IC))])];}else{var _J8=(_J6^_J6>>>1)>>>0;return [3,(_IV>>>0&((_J8-1>>>0^4294967295)>>>0^_J8)>>>0)>>>0&4294967295,_J8&4294967295,E([2,_IV,E(E(_IC))]),E(_IE)];}})];}}},_J9=T(function(){return unCStr("Pattern match failure in do expression at GHC/Event/Thread.hs:90:3-10");}),_Ja=[1,9],_Jb=T(function(){return unCStr("threadWait");}),_Jc=T(function(){return _vk(_Jb,_Ja,_1p,_1p);}),_Jd=function(_Je,_){var _Jf=getOrSetSystemEventThreadEventManagerStore(E(_Je)[1]);return [1,_Jf];},_Jg=function(_Jh,_){return _Jd(_Jh,_);},_Ji=function(_Jj,_Jk,_){var _Jl=0,_Jm=function(_){var _Jn=die("Unsupported PrimOp: makeStablePtr#"),_Jo=A(_Jk,[[1,_Jn],_]);if(!addrEq(_Jn,E(_Jo)[1])){hs_free_stable_ptr(_Jn);return die("Unsupported PrimOp: deRefStablePtr#");}else{return _Jj;}};return E(_Jl)==0?_Jm():_Jm(_);},_Jp=function(_){var _=0,_Jq=nMV(_1p);return _Ji([1,_Jq],_Jg,_);},_Jr=T(function(){return _Gi(_Jp);}),_Js=function(_Jt){return [1,_Hf(0,_h6(_HI,_Jt))];},_Ju=[1,0],_Jv=T(function(){return E(_Ju);}),_Jw=[8],_Jx=function(_Jy){return [1,_1p,_Jw,_1q,_Jy,_1p,_1p];},_Jz=function(_JA,_){return die(T(function(){return _33(T(function(){return _Jx(_JA);}));}));},_JB=function(_JC,_){return _Jz(_JC,_);},_JD=function(_JE,_JF,_JG,_){return _HP(function(_){putMVar(_JF,_JE);return _0;},_JG,_);},_JH=function(_JI,_JJ,_){A(_JI,[_]);return die(_JJ);},_JK=function(_JL,_JM,_JN,_JO,_JP,_JQ,_JR,_){return _JH(function(_){var _JS=E(_JQ);return _I0(_JL,_JM,_JN,_JO,_JP,_JS[1],_JS[2],_);},_JR,_);},_JT=[1,0],_JU=T(function(){return unCStr("sendWakeup");}),_JV=function(_JW,_JX,_){var _JY=0,_JZ=function(_){var _K0=newMVar(),_K1=rMV(E(_Jr)[1]),_K2=E(_K1);if(_K2[0]==1){return _JB(_J9,_);}else{var _K3=E(_K2[1]),_K4=_K3[2],_K5=_K3[3],_K6=_K3[4],_K7=_K3[5],_K8=E(_K3[1]),_K9=(function(_){var _Ka=die("Unsupported PrimOp: readTVar#"),_Kb=hs_plusInt64(E(_Ka)[1],1);die("Unsupported PrimOp: writeTVar#");return [1,_Kb];})(),_Kc=0,_Kd=function(_,_Ke,_Kf){var _Kg=function(_){var _Kh=jsCatch(function(_){return takeMVar(_K0);},function(_Jh,_){return _JK(_K8,_K4,_K5,_K6,_K7,_Ke,_Jh,_);});return ((E(_Kh)[1]>>>0&4)>>>0&4294967295)==0?_0:_3J(_Jc,_);};if(!E(_Kf)){return _Kg(_);}else{var _Ki=newByteArr(1),_Kj=_Ki,_=writeOffAddr("w8",1,_Kj,0,255),_=0;if(E(E(_JT)[1])==(-1)){var _Kk=__hscore_get_errno();if(E(_Kk)==11){return _Kg(_);}else{var _Kl=__hscore_get_errno();_3J(_vk(_JU,[1,_Kl],_1p,_1p),_);return _Kg(_);}}else{return _Kg(_);}}},_Km=function(_Kn,_){var _Ko=takeMVar(_K4),_Kp=jsCatch(T(function(){return A(_Kn,[T(function(){var _Kq=E(_JX),_Kr=_Kq[1],_Ks=E(_Kq),_Kt=E(_K9)[1],_Ku=E(_JW);return E(function(_){var _Kv=T(function(){return E(E(_Kw)[1]);}),_Kw=T(function(){var _Kx=_Iz(_1V,_Ks,[2,[1,_Kr,_Kt,_Ku[1],E(function(_Ky,_Kz,_){var _KA=E(_Ky);return (function(_KB,_KC,_KD,_){_I0(_K8,_K4,_K5,_K6,_K7,_KB,_KC,_);putMVar(_K0,_KD);return _0;})(_KA[1],_KA[2],_Kz,_);})],_1q],_Ko),_KE=_Kx[2],_KF=E(_Kx[1]);if(_KF[0]==1){return [1,E(_KE),_Jv,_Ku];}else{return [1,E(_KE),T(function(){return _Js(_KF[1]);}),T(function(){var _KG=_HK([1,_Ks[1]>>>0],_Kv);return _KG[0]==1?E(_Jv):_Js(_KG[1]);})];}}),_KH=E(_Kw),_KI=E(_KH[2]),_KJ=E(_KH[3]);if(_KI[1]==_KJ[1]){return [1,_Kv,[1,[1,_Kr,_Kt],_5N]];}else{A(_K8[3],[_K8[1],_Ks,_KI,_KJ,_]);return [1,_Kv,[1,[1,_Kr,_Kt],_1]];}});})]);}),function(_Jh,_){return _JD(_Ko,_K4,_Jh,_);}),_KK=E(_Kp);putMVar(_K4,_KK[1]);return _KK[2];};if(!E(_Kc)){var _KL=(function(_){return _Km(function(_Jh,_){return _Jh();},_);})(),_KM=E(_KL);return _Kd(_,_KM[1],_KM[2]);}else{var _KN=_Km(_cF,_),_KO=E(_KN);return _Kd(_,_KO[1],_KO[2]);}}};return E(_JY)==0?_JZ():_JZ(_);},_KP=function(_KQ,_KR,_KS,_KT,_){while(1){var _KU=A(_KS,[_]);if(!A(_KQ,[_KU])){return E(_KV);}else{var _KW=__hscore_get_errno();switch(E(_KW)){case 4:continue;case 11:A(_KT,[_]);continue;default:return _GC(_KR,_);}}}},_KX=[1,0],_KY=function(_){return _KX;},_KZ=function(_){return _KX;},_L0=[1,-1],_L1=function(_L2,_L3){var _L4=E(_L2);if(_L4[0]==1){var _L5=_L4[1],_L6=E(_L3);return _L6[0]==1?_L5==_L6[1]:I_compareInt(_L6[1],_L5)==0?true:false;}else{var _L7=_L4[1],_L8=E(_L3);return _L8[0]==1?I_compareInt(_L7,_L8[1])==0?true:false:I_compare(_L7,_L8[1])==0?true:false;}},_L9=function(_La){return _L1(_La,_L0);},_Lb=function(_Lc,_Ld,_Le,_){var _Lf=function(_){var _Lg=rtsSupportsBoundThreads();if(!E(_Lg)){die("Unsupported PrimOp: waitWrite#");return _0;}else{return _JV(_He,[1,_Ld],_);}};if(!E(_Le)){var _Lh=fdReady(_Ld,1,0,0),_Li=function(_){var _Lj=rtsSupportsBoundThreads();if(!E(_Lj)){return _KP(_L9,_Lc,_KZ,_Lf,_);}else{return _KP(_L9,_Lc,_KY,_Lf,_);}};if(!E(_Lh)){var _Lk=rtsSupportsBoundThreads();if(!E(_Lk)){var _=die("Unsupported PrimOp: waitWrite#");return _Li(_);}else{_JV(_He,[1,_Ld],_);return _Li(_);}}else{return _Li(_);}}else{return _KP(_L9,_Lc,_KZ,_Lf,_);}},_Ll=T(function(){return unCStr("GHC.IO.FD.fdWrite");}),_Lm=function(_Ln,_Lo,_Lp,_Lq,_){while(1){var _Lr=(function(_Ls,_Lt,_Lu,_Lv,_){var _Lw=_Lb(_Ll,_Ls,_Lt,_),_Lx=E(_Lw)[1],_Ly=E(_Lv)[1];if(_Lx>=_Ly){return _0;}else{var _Lz=_Ls,_LA=_Lt;_Lp=T(function(){return [1,plusAddr(E(_Lu)[1],_Lx)];});_Lq=[1,_Ly-_Lx|0];_Ln=_Lz;_Lo=_LA;return null;}})(_Ln,_Lo,_Lp,_Lq,_);if(_Lr!=null){return _Lr;}}},_LB=function(_LC,_LD,_LE,_LF,_LG,_LH,_LI,_LJ,_){_Lm(_LC,_LD,[1,plusAddr(_LE,_LI)],[1,_LJ-_LI|0],_);0;return [1,_LE,_LF,_LG,_LH,0,0];},_LK=function(_LL,_LM,_){var _LN=E(_LL),_LO=E(_LM);return _LB(_LN[1],_LN[2],_LO[1],_LO[2],_LO[3],_LO[4],_LO[5],_LO[6],_);},_LP=function(_LQ,_LR,_){return T(function(){var _LS=E(_LR);return [1,_LS[1],_LS[2],_1k,_LS[4],0,0];});},_LT=[2,_sv],_LU=[2,_sv],_LV=function(_){return _sw;},_LW=function(_){return _ss;},_LX=[2,_sv],_LY=function(_){return _sw;},_LZ=function(_){return _ss;},_M0=T(function(){return unCStr("GHC.IO.FD.fdReadNonBlocking");}),_M1=function(_M2,_M3,_M4,_M5,_M6,_M7,_M8,_M9,_){if(!E(_M3)){var _Ma=fdReady(_M2,0,0,0);if(!E(_Ma)){0;return [1,_LT,[1,_M4,_M5,_M6,_M7,_M8,_M9]];}else{var _Mb=_GF(_M0,_LW,_LV,_),_Mc=E(_Mb),_Md=E(_Mc);switch(E(_Mc[1])){case -1:0;return [1,_LU,[1,_M4,_M5,_M6,_M7,_M8,_M9]];case 0:0;return [1,_1p,[1,_M4,_M5,_M6,_M7,_M8,_M9]];default:var _Me=E(_Md[1]);if(_Me==(-1)){0;return [1,_1p,[1,_M4,_M5,_M6,_M7,_M8,_M9]];}else{0;return [1,[2,_Md],[1,_M4,_M5,_M6,_M7,_M8,_M9+_Me|0]];}}}}else{var _Mf=_GF(_M0,_LZ,_LY,_),_Mg=E(_Mf),_Mh=E(_Mg);switch(E(_Mg[1])){case -1:0;return [1,_LX,[1,_M4,_M5,_M6,_M7,_M8,_M9]];case 0:0;return [1,_1p,[1,_M4,_M5,_M6,_M7,_M8,_M9]];default:var _Mi=E(_Mh[1]);if(_Mi==(-1)){0;return [1,_1p,[1,_M4,_M5,_M6,_M7,_M8,_M9]];}else{0;return [1,[2,_Mh],[1,_M4,_M5,_M6,_M7,_M8,_M9+_Mi|0]];}}}},_Mj=function(_Mk,_Ml,_){var _Mm=E(_Mk),_Mn=E(_Ml);return _M1(_Mm[1],_Mm[2],_Mn[1],_Mn[2],_Mn[3],_Mn[4],_Mn[5],_Mn[6],_);},_Mo=T(function(){return unCStr("GHC.IO.FD.fdRead");}),_Mp=[1,1],_Mq=T(function(){return E(_Mp);}),_Mr=function(_Ms){return _L1(_Ms,_L0);},_Mt=function(_){return _KX;},_Mu=function(_){return _KX;},_Mv=function(_Mw,_Mx,_My,_){var _Mz=[1,_Mx],_MA=function(_){var _MB=rtsSupportsBoundThreads();if(!E(_MB)){die("Unsupported PrimOp: waitRead#");return _0;}else{return _JV(_Mq,_Mz,_);}};if(!E(_My)){var _MC=fdReady(_Mx,0,0,0),_MD=function(_){var _ME=rtsSupportsBoundThreads();if(!E(_ME)){return _KP(_Mr,_Mw,_Mt,_MA,_);}else{return _KP(_Mr,_Mw,_Mu,_MA,_);}},_MF=function(_){var _MG=rtsSupportsBoundThreads();if(!E(_MG)){var _=die("Unsupported PrimOp: waitRead#");return _MD(_);}else{_JV(_Mq,_Mz,_);return _MD(_);}};switch(E(_MC)){case -1:var _MH=__hscore_get_errno(),_MI=_3J(_vk(_Mw,[1,_MH],_1p,_1p),_);return E(E(_MI)[1])==0?_MF(_):_MD(_);case 0:return _MF(_);default:return _MD(_);}}else{return _KP(_Mr,_Mw,_Mt,_MA,_);}},_MJ=function(_MK,_ML,_){var _MM=E(_MK),_MN=E(_ML),_MO=_Mv(_Mo,_MM[1],_MM[2],_);0;return [1,_MO,T(function(){return [1,_MN[1],_MN[2],_MN[3],_MN[4],_MN[5],_MN[6]+E(_MO)[1]|0];})];},_MP=function(_MQ,_MR,_){var _MS=nMV(_5X),_MT=newByteArr(8096);return [1,_MT,[2,_MT,E([1,_MS])],_MR,8096,0,0];},_MU=[1,_MP,_MJ,_Mj,_LP,_LK,_H8],_MV=function(_){return _KX;},_MW=T(function(){return unCStr("GHC.IO.FD.dup2");}),_MX=function(_MY,_MZ){var _N0=E(_MY);if(_N0[0]==1){var _N1=_N0[1],_N2=E(_MZ);return _N2[0]==1?_N1!=_N2[1]:I_compareInt(_N2[1],_N1)==0?false:true;}else{var _N3=_N0[1],_N4=E(_MZ);return _N4[0]==1?I_compareInt(_N3,_N4[1])==0?false:true:I_compare(_N3,_N4[1])==0?false:true;}},_N5=[1,_L1,_MX],_N6=function(_N7){return E(_N7);},_N8=[1,1],_N9=[1,2147483647],_Na=function(_Nb,_Nc){while(1){var _Nd=E(_Nb);if(_Nd[0]==1){var _Ne=_Nd[1],_Nf=E(_Nc);if(_Nf[0]==1){var _Ng=_Nf[1],_Nh=addC(_Ne,_Ng);if(!E(_Nh[2])){return [1,_Nh[1]];}else{_Nb=[2,I_fromInt(_Ne)];_Nc=[2,I_fromInt(_Ng)];continue;}}else{_Nb=[2,I_fromInt(_Ne)];_Nc=_Nf;continue;}}else{var _Ni=E(_Nc);if(_Ni[0]==1){_Nb=_Nd;_Nc=[2,I_fromInt(_Ni[1])];continue;}else{return [2,I_add(_Nd[1],_Ni[1])];}}}},_Nj=T(function(){return _Na(_N9,_N8);}),_Nk=function(_Nl){var _Nm=E(_Nl);if(_Nm[0]==1){var _Nn=E(_Nm[1]);if(_Nn==(-2147483648)){return E(_Nj);}else{if(_Nn<0){return [1,-_Nn];}else{return E(_Nm);}}}else{var _No=_Nm[1];if(I_compareInt(_No,0)>=0){return E(_Nm);}else{return [2,I_negate(_No)];}}},_Np=function(_Nq,_Nr){while(1){var _Ns=E(_Nq);if(_Ns[0]==1){var _Nt=_Ns[1],_Nu=E(_Nr);if(_Nu[0]==1){var _Nv=_Nu[1],_Nw=subC(_Nt,_Nv);if(!E(_Nw[2])){return [1,_Nw[1]];}else{_Nq=[2,I_fromInt(_Nt)];_Nr=[2,I_fromInt(_Nv)];continue;}}else{_Nq=[2,I_fromInt(_Nt)];_Nr=_Nu;continue;}}else{var _Nx=E(_Nr);if(_Nx[0]==1){_Nq=_Ns;_Nr=[2,I_fromInt(_Nx[1])];continue;}else{return [2,I_sub(_Ns[1],_Nx[1])];}}}},_Ny=T(function(){return _Na(_N9,_N8);}),_Nz=function(_NA){var _NB=E(_NA);if(_NB[0]==1){var _NC=E(_NB[1]);if(_NC==(-2147483648)){return E(_Ny);}else{return [1,-_NC];}}else{return [2,I_negate(_NB[1])];}},_ND=[1,0],_NE=[1,-1],_NF=function(_NG){var _NH=E(_NG);if(_NH[0]==1){var _NI=_NH[1];return _NI<0==false?E(_NI)==0?E(_ND):E(_N8):E(_NE);}else{var _NJ=I_compareInt(_NH[1],0);return _NJ>0==false?E(_NJ)==0?E(_ND):E(_NE):E(_N8);}},_NK=function(_NL,_NM){while(1){var _NN=E(_NL);if(_NN[0]==1){var _NO=_NN[1],_NP=E(_NM);if(_NP[0]==1){var _NQ=_NP[1];if(!(imul(_NO,_NQ)|0)){return [1,imul(_NO,_NQ)|0];}else{_NL=[2,I_fromInt(_NO)];_NM=[2,I_fromInt(_NQ)];continue;}}else{_NL=[2,I_fromInt(_NO)];_NM=_NP;continue;}}else{var _NR=E(_NM);if(_NR[0]==1){_NL=_NN;_NM=[2,I_fromInt(_NR[1])];continue;}else{return [2,I_mul(_NN[1],_NR[1])];}}}},_NS=[1,_Na,_NK,_Np,_Nz,_Nk,_NF,_N6],_NT=T(function(){return _wf(_N5,_NS);}),_NU=T(function(){return A(_NT,[_MW,_MV]);}),_NV=function(_NW,_NX,_){A(_NU,[_]);return T(function(){return [1,E(_NX)[1],E(_NW)[2]];});},_NY=function(_NZ,_){return _1;},_O0=function(_O1,_O2,_){return _0;},_O3=function(_O4,_O5,_){return _O0(_O4,_O5,_);},_O6=function(_O7,_O8,_){return _0;},_O9=function(_Oa,_){return _KX;},_Ob=T(function(){return unCStr("hGetPosn");}),_Oc=function(_Od){return _L1(_Od,_L0);},_Oe=function(_){return _KX;},_Of=function(_Og,_Oh,_Oi,_){while(1){var _Oj=A(_Oi,[_]);if(!A(_Og,[_Oj])){return E(_Ok);}else{var _Ol=__hscore_get_errno();if(E(_Ol)==4){continue;}else{return _GC(_Oh,_);}}}},_Om=function(_On,_){return _Of(_Oc,_Ob,_Oe,_);},_Oo=T(function(){return unCStr("seek");}),_Op=function(_Oq){return _L1(_Oq,_L0);},_Or=function(_Os,_Ot,_Ou,_){_Of(_Op,_Oo,_Oe,_);return _0;},_Ov=function(_O5,_){return _NY(_O5,_);},_Ow=function(_Ox,_){return _5N;},_Oy=function(_Oz,_){unlockFile(E(_Oz)[1]);return _0;},_OA=function(_){return _KX;},_OB=T(function(){return unCStr("GHC.IO.FD.close");}),_OC=function(_OD){return _L1(_OD,_L0);},_OE=function(_OF,_){_Of(_OC,_OB,_OA,_);return _0;},_OG=function(_OH,_OI){var _OJ=E(_OI);switch(_OJ[0]){case 1:return [1,_1p,_Hk];case 2:return E(_OH)[1]==_OJ[1]==false?[1,_1p,_OJ]:[1,[2,_OJ[2]],_Hk];default:var _OK=_OJ[1],_OL=_OJ[2],_OM=_OJ[3],_ON=_OJ[4],_OO=E(_OH),_OP=_OO[1],_OQ=_OL>>>0;if(((_OP>>>0&((_OQ-1>>>0^4294967295)>>>0^_OQ)>>>0)>>>0&4294967295)==_OK){if((_OP>>>0&_OQ)>>>0!=0){var _OR=T(function(){var _OS=_OG(_OO,_ON);return [1,_OS[1],_OS[2]];});return [1,T(function(){return E(E(_OR)[1]);}),T(function(){var _OT=E(E(_OR)[2]);if(_OT[0]==1){return E(_OM);}else{var _OU=E(_OM);return _OU[0]==1?E(_OT):[3,_OK,_OL,E(_OU),E(_OT)];}})];}else{var _OV=T(function(){var _OW=_OG(_OO,_OM);return [1,_OW[1],_OW[2]];});return [1,T(function(){return E(E(_OV)[1]);}),T(function(){var _OX=E(_ON);if(_OX[0]==1){return E(E(_OV)[2]);}else{var _OY=E(E(_OV)[2]);return _OY[0]==1?E(_OX):[3,_OK,_OL,E(_OY),E(_OX)];}})];}}else{return [1,_1p,_OJ];}}},_OZ=function(_P0,_){while(1){var _P1=E(_P0);if(_P1[0]==1){return _0;}else{var _P2=E(_P1[1]);A(_P2[4],[[1,_P2[1],_P2[2]],[1,(_P2[3]>>>0|4)>>>0&4294967295],_]);_P0=_P1[2];continue;}}},_P3=function(_P4,_){while(1){var _P5=E(_P4);if(_P5[0]==1){return _0;}else{var _P6=E(_P5[1]);A(_P6[4],[[1,_P6[1],_P6[2]],[1,(_P6[3]>>>0|4)>>>0&4294967295],_]);_P4=_P5[2];continue;}}},_P7=function(_P8,_P9,_Pa,_){return _HP(function(_){putMVar(_P8,_P9);return _0;},_Pa,_);},_Pb=function(_Pc,_Pd,_Pe,_){var _Pf=0,_Pg=function(_Ph,_){A(T(function(){return A(_Pd,[_Pe]);}),[_]);var _Pi=_OG(_Pe,_Ph),_Pj=E(_Pi[1]);if(_Pj[0]==1){return [1,_Ph,_1q];}else{var _Pk=_Pj[1],_Pl=E(_Pi[2]);if(!_Hf(0,_h6(_HI,_Pk))){return [1,_Pl,_Pk];}else{var _Pm=newByteArr(1),_Pn=_Pm,_=writeOffAddr("w8",1,_Pn,0,255),_=0;if(E(E(_JT)[1])==(-1)){var _Po=__hscore_get_errno();if(E(_Po)==11){return [1,_Pl,_Pk];}else{var _Pp=__hscore_get_errno();_3J(_vk(_JU,[1,_Pp],_1p,_1p),_);return [1,_Pl,_Pk];}}else{return [1,_Pl,_Pk];}}}};if(!E(_Pf)){var _Pq=(function(_){var _Pr=E(_Pc)[2],_Ps=takeMVar(_Pr),_Pt=jsCatch(function(_){return (function(_){return _Pg(_Ps,_);})();},function(_Ir,_){return _P7(_Pr,_Ps,_Ir,_);}),_Pu=E(_Pt);putMVar(_Pr,_Pu[1]);return _Pu[2];})();return _P3(_Pq,_);}else{var _Pv=E(_Pc)[2],_Pw=takeMVar(_Pv),_Px=jsCatch(function(_){return _Pg(_Pw,_);},function(_Ir,_){return _P7(_Pv,_Pw,_Ir,_);}),_Py=E(_Px),_=putMVar(_Pv,_Py[1]);return _OZ(_Py[2],_);}},_Pz=T(function(){return unCStr("Pattern match failure in do expression at GHC/Event/Thread.hs:84:3-10");}),_PA=function(_PB,_PC,_){var _PD=0;if(!E(_PD)){return (function(_){var _PE=jsCatch(function(_){return _PB();},function(_JC,_){return _JH(_PC,_JC,_);});A(_PC,[_]);return _PE;})();}else{var _PF=jsCatch(_PB,function(_JC,_){return _JH(_PC,_JC,_);});A(_PC,[_]);return _PF;}},_PG=function(_PH,_){return _PA(function(_){var _PI=rtsSupportsBoundThreads();if(!E(_PI)){_Of(_OC,_OB,_OA,_);return _0;}else{var _PJ=rMV(E(_Jr)[1]),_PK=E(_PJ);return _PK[0]==1?_JB(_Pz,_):_Pb(_PK[1],_OE,T(function(){return [1,E(_PH)[1]];}),_);}},function(_){return _Oy(_PH,_);},_);},_PL=T(function(){return unCStr("Prelude.Enum.Bool.toEnum: bad argument");}),_PM=T(function(){return err(_PL);}),_PN=T(function(){return unCStr("GHC.IO.FD.ready");}),_PO=function(_PP){return E(E(_PP)[1])==(-1)?true:false;},_PQ=function(_PR,_PS,_PT,_){var _PU=function(_PV){var _PW=_Of(_PO,_PN,function(_){var _PX=fdReady(_PR,_PV,_PT,0);return [1,_PX];},_);return T(function(){switch(E(E(_PW)[1])){case 0:return false;case 1:return true;default:return E(_PM);}});};return E(_PS)==false?_PU(0):_PU(1);},_PY=function(_PZ,_Q0,_Q1,_){return _PQ(E(_PZ)[1],_Q0,E(_Q1)[1],_);},_Q2=function(_Q3,_){return T(function(){return [1,0,E(_Q3)[2]];});},_Q4=[3],_Q5=function(_Q6,_){return _Q4;},_Q7=[1,_PY,_PG,_Ow,_Ov,_Or,_Om,_O9,_O6,_O3,_NY,_O0,_Q5,_Q2,_NV],_Q8=T(function(){return unCStr("FD");}),_Q9=T(function(){return unCStr("GHC.IO.FD");}),_Qa=T(function(){return unCStr("base");}),_Qb=T(function(){var _Qc=hs_wordToWord64(2302221327),_Qd=hs_wordToWord64(2077833458);return [1,_Qc,_Qd,[1,_Qc,_Qd,_Qa,_Q9,_Q8],_1q];}),_Qe=function(_Qf){return E(_Qb);},_Qg=[4],_Qh=[1],_Qi=function(_){var _=0;return _G0(_Qh,_Gl,_);},_Qj=T(function(){return _Gi(_Qi);}),_Qk=T(function(){return _Go(_Qj);}),_Ql=T(function(){return unCStr("<stdout>");}),_Qm=T(function(){return unCStr("handle is finalized");}),_Qn=function(_Qo){return _pe([1,_1p,_1o,_1q,_Qm,_1p,[2,_Qo]],_33);},_Qp=function(_Qq,_Qr,_){var _Qs=takeMVar(_Qr),_Qt=E(_Qs),_Qu=_Qt[6],_Qv=_Qt[11],_Qw=rMV(_Qu),_Qx=function(_){if(E(_Qt[5])[0]==1){putMVar(_Qr,T(function(){return _Qn(_Qq);}));return _0;}else{var _Qy=E(_Qt[12]);if(_Qy[0]==1){var _Qz=E(_Qv);if(_Qz[0]==1){putMVar(_Qr,T(function(){return _Qn(_Qq);}));return _0;}else{A(E(_Qz[1])[3],[_]);putMVar(_Qr,T(function(){return _Qn(_Qq);}));return _0;}}else{A(E(_Qy[1])[3],[_]);var _QA=E(_Qv);if(_QA[0]==1){putMVar(_Qr,T(function(){return _Qn(_Qq);}));return _0;}else{A(E(_QA[1])[3],[_]);putMVar(_Qr,T(function(){return _Qn(_Qq);}));return _0;}}}};if(E(E(_Qw)[3])[0]==1){return _Qx(_);}else{var _QB=rMV(_Qu),_QC=E(_QB);if(_QC[5]!=_QC[6]){var _QD=A(_F,[_Qt[2],_Qt[4],_QC,_]),_=wMV(_Qu,_QD);return _Qx(_);}else{return _Qx(_);}}},_QE=function(_QF,_QG,_){return _Qp(_QF,E(_QG)[1],_);},_QH=[2,_QE],_QI=[1],_QJ=function(_QK){return E(E(_QK)[3]);},_QL=T(function(){return unCStr("codec_state");}),_QM=T(function(){return err(_QL);}),_QN=[3,_1p],_QO=function(_QP){return E(E(_QP)[1]);},_QQ=function(_QR,_QS,_QT,_QU,_QV,_QW,_QX,_QY,_QZ,_R0,_R1,_){var _R2=T(function(){return E(_QW)[0]==3?[1]:[2];}),_R3=function(_R4,_R5,_){var _R6=A(T(function(){return A(_QO,[_QS,_QU,_R2]);}),[_]),_R7=nMV(_R6),_R8=nMV([1,_QM,_R6]),_R9=function(_,_Ra,_Rb){var _Rc=nMV(_QI),_Rd=newMVar();putMVar(_Rd,T(function(){return [1,_QR,_QS,_QT,E(E(_QU)),_QW,_R7,_Rb,_R8,E(_Ra)[1],_Rc,_R4,_R5,_QY,T(function(){return E(E(_QZ)[1]);}),T(function(){return E(E(_QZ)[2]);}),_R1];}));var _Re=E(_R0);if(_Re[0]==1){return [1,_QV,_Rd];}else{T(function(){return A(_Re[1],[_QV,[1,_Rd]]);});die("Unsupported PrimOp: mkWeak#");return [1,_QV,_Rd];}};if(!E(_QX)){var _Rf=nMV(_5X),_Rg=newByteArr(8192),_Rh=nMV([1,_Rg,[2,_Rg,E([1,_Rf])],_R2,2048,0,0]);return _R9(_,[1,_Rh],_5S);}else{var _Ri=nMV(_5X),_Rj=newByteArr(8192),_Rk=nMV([1,_Rj,[2,_Rj,E([1,_Ri])],_R2,2048,0,0]),_Rl=A(T(function(){return _QJ(_QR);}),[_QU,_]);return _R9(_,[1,_Rk],T(function(){return E(_Rl)==false?E(_QN):[2];}));}},_Rm=E(_QY);if(_Rm[0]==1){return _R3(_1p,_1p,_);}else{var _Rn=E(_Rm[1]),_Ro=_Rn[2],_Rp=_Rn[3];switch(E(_QW)[0]){case 3:var _Rq=A(_Ro,[_]);return _R3(_1p,[2,_Rq],_);case 4:var _Rr=A(_Rp,[_]);return _R3([2,_Rr],_1p,_);case 5:var _Rs=A(_Rp,[_]);return _R3([2,_Rs],_1p,_);case 6:var _Rt=A(_Ro,[_]),_Ru=A(_Rp,[_]);return _R3([2,_Ru],[2,_Rt],_);default:return _R3(_1p,_1p,_);}}},_Rv=[1],_Rw=[1,_Rv,_Rv],_Rx=[1,1,0],_Ry=function(_){var _=0,_Rz=A(E(_Qk)[1],[_]);return _QQ(_Q7,_MU,_Qe,_Rx,_Ql,_Qg,_1,[2,_Rz],_Rw,_QH,_1p,_);},_RA=T(function(){return _Gi(_Ry);}),_RB=function(_){_6f(_RA,_sq,_1,_);_6f(_RA,_sp,_1,_);return _0;},_RC=function(_){return _RB(_);};window.onload = function() {A(_RC, [0]);};