// This object will hold all exports.
var Haste = {};

/* Thunk
   Creates a thunk representing the given closure.
   Since we want automatic memoization of as many expressions as possible, we
   use a JS object as a sort of tagged pointer, where the member x denotes the
   object actually pointed to. If a "pointer" points to a thunk, it has a
   member 't' which is set to true; if it points to a value, be it a function,
   a value of an algebraic type of a primitive value, it has no member 't'.
*/

function T(f) {
    this.f = f;
}

function F(f) {
    this.f = f;
}

// Special object used for blackholing.
var __blackhole = {};

/* Apply
   Applies the function f to the arguments args. If the application is under-
   saturated, a closure is returned, awaiting further arguments. If it is over-
   saturated, the function is fully applied, and the result (assumed to be a
   function) is then applied to the remaining arguments.
*/
function A(f, args) {
    if(f instanceof T) {
        f = E(f);
    }
    // Closure does some funny stuff with functions that occasionally
    // results in non-functions getting applied, so we have to deal with
    // it.
    if(!(f instanceof Function)) {
        f = B(f);
        if(!(f instanceof Function)) {
            return f;
        }
    }

    if(f.arity === undefined) {
        f.arity = f.length;
    }
    if(args.length === f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return f(args[0]);
            default: return f.apply(null, args);
        }
    } else if(args.length > f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return A(f(args.shift()), args);
            default: return A(f.apply(null, args.splice(0, f.arity)), args);
        }
    } else {
        var g = function() {
            return A(f, args.concat(Array.prototype.slice.call(arguments)));
        };
        g.arity = f.arity - args.length;
        return g;
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof T) {
        if(t.f != __blackhole) {
            var f = t.f;
            t.f = __blackhole;
            t.x = f();
        }
        return t.x;
    } else {
        return t;
    }
}

/* Bounce
   Bounce on a trampoline for as long as we get a function back.
*/
function B(f) {
    while(f instanceof F) {
        var fun = f.f;
        f = __blackhole;
        f = fun();
    }
    return f;
}

// Export Haste, A, B and E. Haste because we need to preserve exports, A, B
// and E because they're handy for Haste.Foreign.
if(!window) {
    var window = {};
}
window['Haste'] = Haste;
window['A'] = A;
window['E'] = E;
window['B'] = B;


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
    return [0, (a-a%b)/b, a%b];
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
if(Math.imul) {
    var imul = Math.imul;
} else {
    var imul = function(a, b) {
        // ignore high a * high a as the result will always be truncated
        var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
        var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
        var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
        return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
    }
}

function addC(a, b) {
    var x = a+b;
    return [0, x & 0xffffffff, x > 0x7fffffff];
}

function subC(a, b) {
    var x = a-b;
    return [0, x & 0xffffffff, x < -2147483648];
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
    return [0, sign*man, exp];
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
    return [0, sign, manHigh, manLow, exp];
}

function isFloatFinite(x) {
    return isFinite(x);
}

function isDoubleFinite(x) {
    return isFinite(x);
}

function err(str) {
    die(toJSStr(str));
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {return unAppCStr(str, [0]);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = B(A(f, [[0, str.charCodeAt(i)], acc]));
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return [1,[0,str.charCodeAt(i)],new T(function() {
            return unAppCStr(str,chrs,i+1);
        })];
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str));
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str[0] == 1; str = E(str[2])) {
        s += String.fromCharCode(E(str[1])[1]);
    }
    return s;
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

// atomicModifyMutVar
function mMV(mv, f) {
    var x = B(A(f, [mv.x]));
    mv.x = x[1];
    return x[2];
}

function localeEncoding() {
    var le = newByteArr(5);
    le['v']['i8'][0] = 'U'.charCodeAt(0);
    le['v']['i8'][1] = 'T'.charCodeAt(0);
    le['v']['i8'][2] = 'F'.charCodeAt(0);
    le['v']['i8'][3] = '-'.charCodeAt(0);
    le['v']['i8'][4] = '8'.charCodeAt(0);
    return le;
}

var isDoubleNaN = isNaN;
var isFloatNaN = isNaN;

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
    if(a < b) {
        return [0];
    } else if(a == b) {
        return [1];
    }
    return [2];
}

function jsCatch(act, handler) {
    try {
        return B(A(act,[0]));
    } catch(e) {
        return B(A(handler,[e, 0]));
    }
}

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Array) {
        return x[0];
    } else {
        return x;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round; // Stupid GHC doesn't like periods in FFI IDs...
var realWorld = undefined;
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function popCnt(i) {
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
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

function jsGetMouseCoords(e) {
    var posx = 0;
    var posy = 0;
    if (!e) var e = window.event;
    if (e.pageX || e.pageY) 	{
	posx = e.pageX;
	posy = e.pageY;
    }
    else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft
	    + document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop
	    + document.documentElement.scrollTop;
    }
    return [posx - (e.currentTarget.offsetLeft || 0),
	    posy - (e.currentTarget.offsetTop || 0)];
}

function jsSetCB(elem, evt, cb) {
    // Count return press in single line text box as a change event.
    if(evt == 'change' && elem.type.toLowerCase() == 'text') {
        setCB(elem, 'keyup', function(k) {
            if(k == '\n'.charCodeAt(0)) {
                B(A(cb,[[0,k.keyCode],0]));
            }
        });
    }

    var fun;
    switch(evt) {
    case 'click':
    case 'dblclick':
    case 'mouseup':
    case 'mousedown':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,x.button],[0,mx,my],0]));
        };
        break;
    case 'mousemove':
    case 'mouseover':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,mx,my],0]));
        };
        break;
    case 'keypress':
    case 'keyup':
    case 'keydown':
        fun = function(x) {B(A(cb,[[0,x.keyCode],0]));};
        break;        
    default:
        fun = function() {B(A(cb,[0]));};
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
    window.setTimeout(function() {B(A(cb,[0]));}, msecs);
}

function jsGet(elem, prop) {
    return elem[prop].toString();
}

function jsSet(elem, prop, val) {
    elem[prop] = val;
}

function jsGetAttr(elem, prop) {
    if(elem.hasAttribute(prop)) {
        return elem.getAttribute(prop).toString();
    } else {
        return "";
    }
}

function jsSetAttr(elem, prop, val) {
    elem.setAttribute(prop, val);
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
        return [1,[0,e]];
    }
    return [0];
}

function jsQuerySelectorAll(elem, query) {
  var els = [0],
      len, nl, i;

  if (!elem || typeof elem.querySelectorAll !== 'function') {
    return els;
  }

  nl = elem.querySelectorAll(query);
  len = nl.length;

  for (i=len-1; i >= 0; --i) {
    els = [1, [0, nl[i]], els];
  }

  return els;
}

function jsCreateElem(tag) {
    return document.createElement(tag);
}

function jsCreateTextNode(str) {
    return document.createTextNode(str);
}

function jsGetChildBefore(elem) {
    elem = elem.previousSibling;
    while(elem) {
        if(typeof elem.tagName != 'undefined') {
            return [1,[0,elem]];
        }
        elem = elem.previousSibling;
    }
    return [0];
}

function jsGetLastChild(elem) {
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetFirstChild(elem) {
    var len = elem.childNodes.length;
    for(var i = 0; i < len; i++) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetChildren(elem) {
    var children = [0];
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            children = [1, [0,elem.childNodes[i]], children];
        }
    }
    return children;
}

function jsSetChildren(elem, children) {
    children = E(children);
    jsClearChildren(elem, 0);
    while(children[0] === 1) {
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
    while(strs[0]) {
        strs = E(strs);
        arr.push(E(strs[1])[1]);
        strs = E(strs[2]);
    }
    return arr.join(sep);
}

var jsJSONParse = JSON.parse;

// JSON stringify a string
function jsStringify(str) {
    return JSON.stringify(str);
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
        return [0];
    }
    return [1,hs];
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return [0, [0, jsRead(obj)]];
    case 'string':
        return [1, [0, obj]];
        break;
    case 'boolean':
        return [2, obj]; // Booleans are special wrt constructor tags!
        break;
    case 'object':
        if(obj instanceof Array) {
            return [3, arr2lst_json(obj, 0)];
        } else if (obj == null) {
            return [5];
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
            var xs = [0];
            for(var i = 0; i < ks.length; i++) {
                xs = [1, [0, [0,ks[i]], toHS(obj[ks[i]])], xs];
            }
            return [4, xs];
        }
    }
}

function arr2lst_json(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, toHS(arr[elem]), new T(function() {return arr2lst_json(arr,elem+1);})]
}

function arr2lst(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, arr[elem], new T(function() {return arr2lst(arr,elem+1);})]
}

function lst2arr(xs) {
    var arr = [];
    for(; xs[0]; xs = E(xs[2])) {
        arr.push(E(xs[1]));
    }
    return arr;
}

function ajaxReq(method, url, async, postdata, cb) {
    var xhr = new XMLHttpRequest();
    xhr.open(method, url, async);

    if(method == "POST") {
        xhr.setRequestHeader("Content-type",
                             "application/x-www-form-urlencoded");
    }
    xhr.onreadystatechange = function() {
        if(xhr.readyState == 4) {
            if(xhr.status == 200) {
                B(A(cb,[[1,[0,xhr.responseText]],0]));
            } else {
                B(A(cb,[[0],0])); // Nothing
            }
        }
    }
    xhr.send(postdata);
}

// Create a little endian ArrayBuffer representation of something.
function toABHost(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    return a;
}

function toABSwap(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    var bs = new Uint8Array(a);
    for(var i = 0, j = n-1; i < j; ++i, --j) {
        var tmp = bs[i];
        bs[i] = bs[j];
        bs[j] = tmp;
    }
    return a;
}

window['toABle'] = toABHost;
window['toABbe'] = toABSwap;

// Swap byte order if host is not little endian.
var buffer = new ArrayBuffer(2);
new DataView(buffer).setInt16(0, 256, true);
if(new Int16Array(buffer)[0] !== 256) {
    window['toABle'] = toABSwap;
    window['toABbe'] = toABHost;
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
        return [0, 0, undefined];
    } else {
        var val = mv.x;
        mv.empty = true;
        mv.x = null;
        return [0, 1, val];
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    var val = mv.x;
    mv.empty = true;
    mv.x = null;
    return val;
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

var I_fromBits = function(bits) {
  var high = bits[bits.length - 1];
  return new Integer(bits, high & (1 << 31) ? -1 : 0);
};

var I_fromString = function(str, opt_radix) {
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
      result = I_add(result, I_fromNumber(value));
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
      val += I_getBitsUnsigned(self, i) * pow;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return val;
  }
};

var I_getBits = function(self, index) {
  if (index < 0) {
    return 0;
  } else if (index < self.bits_.length) {
    return self.bits_[index];
  } else {
    return self.sign_;
  }
};

var I_getBitsUnsigned = function(self, index) {
  var val = I_getBits(self, index);
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
    if (I_getBits(self, i) != I_getBits(other, i)) {
      return false;
    }
  }
  return true;
};

var I_notEquals = function(self, other) {
  return !I_equals(self, other);
};

var I_greaterThan = function(self, other) {
  return I_compare(self, other) > 0;
};

var I_greaterThanOrEqual = function(self, other) {
  return I_compare(self, other) >= 0;
};

var I_lessThan = function(self, other) {
  return I_compare(self, other) < 0;
};

var I_lessThanOrEqual = function(self, other) {
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
    bits[i] = I_getBits(self, i);
  }
  var sigBits = bit_index == 31 ? 0xFFFFFFFF : (1 << (bit_index + 1)) - 1;
  var val = I_getBits(self, arr_index) & sigBits;
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
    var a1 = I_getBits(self, i) >>> 16;
    var a0 = I_getBits(self, i) & 0xFFFF;

    var b1 = I_getBits(other, i) >>> 16;
    var b0 = I_getBits(other, i) & 0xFFFF;

    var c0 = carry + a0 + b0;
    var c1 = (c0 >>> 16) + a1 + b1;
    carry = c1 >>> 16;
    c0 &= 0xFFFF;
    c1 &= 0xFFFF;
    arr[i] = (c1 << 16) | c0;
  }
  return I_fromBits(arr);
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

  if (I_lessThan(self, Integer.TWO_PWR_24_) &&
      I_lessThan(other, Integer.TWO_PWR_24_)) {
    return I_fromNumber(I_toNumber(self) * I_toNumber(other));
  }

  var len = self.bits_.length + other.bits_.length;
  var arr = [];
  for (var i = 0; i < 2 * len; i++) {
    arr[i] = 0;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    for (var j = 0; j < other.bits_.length; j++) {
      var a1 = I_getBits(self, i) >>> 16;
      var a0 = I_getBits(self, i) & 0xFFFF;

      var b1 = I_getBits(other, j) >>> 16;
      var b0 = I_getBits(other, j) & 0xFFFF;

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
  if(I_greaterThan(self, Integer.ZERO) != I_greaterThan(other, Integer.ZERO)) {
    if(I_rem(self, other) != Integer.ZERO) {
      return I_sub(I_quot(self, other), Integer.ONE);
    }
  }
  return I_quot(self, other);
}

var I_quotRem = function(self, other) {
  return [0, I_quot(self, other), I_rem(self, other)];
}

var I_divMod = function(self, other) {
  return [0, I_div(self, other), I_mod(self, other)];
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
  while (I_greaterThanOrEqual(rem, other)) {
    var approx = Math.max(1, Math.floor(I_toNumber(rem) / I_toNumber(other)));
    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);
    var approxRes = I_fromNumber(approx);
    var approxRem = I_mul(approxRes, other);
    while (isNegative(approxRem) || I_greaterThan(approxRem, rem)) {
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
    arr[i] = I_getBits(self, i) & I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ & other.sign_);
};

var I_or = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) | I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ | other.sign_);
};

var I_xor = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) ^ I_getBits(other, i);
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
      arr[i] = (I_getBits(self, i - arr_delta) << bit_delta) |
               (I_getBits(self, i - arr_delta - 1) >>> (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i - arr_delta);
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
      arr[i] = (I_getBits(self, i + arr_delta) >>> bit_delta) |
               (I_getBits(self, i + arr_delta + 1) << (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i + arr_delta);
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
  var mantissa = I_fromBits([dec[3], dec[2]]);
  if(dec[1] < 0) {
    mantissa = I_negate(mantissa);
  }
  return [0, dec[4], mantissa];
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

function I_fromInt64(x) {
    return I_fromBits([x.getLowBits(), x.getHighBits()]);
}

function I_toInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

function I_fromWord64(x) {
    return x;
}

function I_toWord64(x) {
    return I_rem(I_add(__w64_max, x), __w64_max);
}

// Copyright 2009 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

var Long = function(low, high) {
  this.low_ = low | 0;
  this.high_ = high | 0;
};

Long.IntCache_ = {};

Long.fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Long.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Long(value | 0, value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Long.IntCache_[value] = obj;
  }
  return obj;
};

Long.fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Long.ZERO;
  } else if (value <= -Long.TWO_PWR_63_DBL_) {
    return Long.MIN_VALUE;
  } else if (value + 1 >= Long.TWO_PWR_63_DBL_) {
    return Long.MAX_VALUE;
  } else if (value < 0) {
    return Long.fromNumber(-value).negate();
  } else {
    return new Long(
        (value % Long.TWO_PWR_32_DBL_) | 0,
        (value / Long.TWO_PWR_32_DBL_) | 0);
  }
};

Long.fromBits = function(lowBits, highBits) {
  return new Long(lowBits, highBits);
};

Long.TWO_PWR_16_DBL_ = 1 << 16;
Long.TWO_PWR_24_DBL_ = 1 << 24;
Long.TWO_PWR_32_DBL_ =
    Long.TWO_PWR_16_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_31_DBL_ =
    Long.TWO_PWR_32_DBL_ / 2;
Long.TWO_PWR_48_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_64_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_32_DBL_;
Long.TWO_PWR_63_DBL_ =
    Long.TWO_PWR_64_DBL_ / 2;
Long.ZERO = Long.fromInt(0);
Long.ONE = Long.fromInt(1);
Long.NEG_ONE = Long.fromInt(-1);
Long.MAX_VALUE =
    Long.fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0);
Long.MIN_VALUE = Long.fromBits(0, 0x80000000 | 0);
Long.TWO_PWR_24_ = Long.fromInt(1 << 24);

Long.prototype.toInt = function() {
  return this.low_;
};

Long.prototype.toNumber = function() {
  return this.high_ * Long.TWO_PWR_32_DBL_ +
         this.getLowBitsUnsigned();
};

Long.prototype.getHighBits = function() {
  return this.high_;
};

Long.prototype.getLowBits = function() {
  return this.low_;
};

Long.prototype.getLowBitsUnsigned = function() {
  return (this.low_ >= 0) ?
      this.low_ : Long.TWO_PWR_32_DBL_ + this.low_;
};

Long.prototype.isZero = function() {
  return this.high_ == 0 && this.low_ == 0;
};

Long.prototype.isNegative = function() {
  return this.high_ < 0;
};

Long.prototype.isOdd = function() {
  return (this.low_ & 1) == 1;
};

Long.prototype.equals = function(other) {
  return (this.high_ == other.high_) && (this.low_ == other.low_);
};

Long.prototype.notEquals = function(other) {
  return (this.high_ != other.high_) || (this.low_ != other.low_);
};

Long.prototype.lessThan = function(other) {
  return this.compare(other) < 0;
};

Long.prototype.lessThanOrEqual = function(other) {
  return this.compare(other) <= 0;
};

Long.prototype.greaterThan = function(other) {
  return this.compare(other) > 0;
};

Long.prototype.greaterThanOrEqual = function(other) {
  return this.compare(other) >= 0;
};

Long.prototype.compare = function(other) {
  if (this.equals(other)) {
    return 0;
  }

  var thisNeg = this.isNegative();
  var otherNeg = other.isNegative();
  if (thisNeg && !otherNeg) {
    return -1;
  }
  if (!thisNeg && otherNeg) {
    return 1;
  }

  if (this.subtract(other).isNegative()) {
    return -1;
  } else {
    return 1;
  }
};

Long.prototype.negate = function() {
  if (this.equals(Long.MIN_VALUE)) {
    return Long.MIN_VALUE;
  } else {
    return this.not().add(Long.ONE);
  }
};

Long.prototype.add = function(other) {
  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 + b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 + b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 + b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 + b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.subtract = function(other) {
  return this.add(other.negate());
};

Long.prototype.multiply = function(other) {
  if (this.isZero()) {
    return Long.ZERO;
  } else if (other.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    return other.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  } else if (other.equals(Long.MIN_VALUE)) {
    return this.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().multiply(other.negate());
    } else {
      return this.negate().multiply(other).negate();
    }
  } else if (other.isNegative()) {
    return this.multiply(other.negate()).negate();
  }

  if (this.lessThan(Long.TWO_PWR_24_) &&
      other.lessThan(Long.TWO_PWR_24_)) {
    return Long.fromNumber(this.toNumber() * other.toNumber());
  }

  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 * b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 * b00;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c16 += a00 * b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 * b00;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a16 * b16;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a00 * b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.div = function(other) {
  if (other.isZero()) {
    throw Error('division by zero');
  } else if (this.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    if (other.equals(Long.ONE) ||
        other.equals(Long.NEG_ONE)) {
      return Long.MIN_VALUE;
    } else if (other.equals(Long.MIN_VALUE)) {
      return Long.ONE;
    } else {
      var halfThis = this.shiftRight(1);
      var approx = halfThis.div(other).shiftLeft(1);
      if (approx.equals(Long.ZERO)) {
        return other.isNegative() ? Long.ONE : Long.NEG_ONE;
      } else {
        var rem = this.subtract(other.multiply(approx));
        var result = approx.add(rem.div(other));
        return result;
      }
    }
  } else if (other.equals(Long.MIN_VALUE)) {
    return Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().div(other.negate());
    } else {
      return this.negate().div(other).negate();
    }
  } else if (other.isNegative()) {
    return this.div(other.negate()).negate();
  }

  var res = Long.ZERO;
  var rem = this;
  while (rem.greaterThanOrEqual(other)) {
    var approx = Math.max(1, Math.floor(rem.toNumber() / other.toNumber()));

    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);

    var approxRes = Long.fromNumber(approx);
    var approxRem = approxRes.multiply(other);
    while (approxRem.isNegative() || approxRem.greaterThan(rem)) {
      approx -= delta;
      approxRes = Long.fromNumber(approx);
      approxRem = approxRes.multiply(other);
    }

    if (approxRes.isZero()) {
      approxRes = Long.ONE;
    }

    res = res.add(approxRes);
    rem = rem.subtract(approxRem);
  }
  return res;
};

Long.prototype.modulo = function(other) {
  return this.subtract(this.div(other).multiply(other));
};

Long.prototype.not = function() {
  return Long.fromBits(~this.low_, ~this.high_);
};

Long.prototype.and = function(other) {
  return Long.fromBits(this.low_ & other.low_,
                                 this.high_ & other.high_);
};

Long.prototype.or = function(other) {
  return Long.fromBits(this.low_ | other.low_,
                                 this.high_ | other.high_);
};

Long.prototype.xor = function(other) {
  return Long.fromBits(this.low_ ^ other.low_,
                                 this.high_ ^ other.high_);
};

Long.prototype.shiftLeft = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var low = this.low_;
    if (numBits < 32) {
      var high = this.high_;
      return Long.fromBits(
          low << numBits,
          (high << numBits) | (low >>> (32 - numBits)));
    } else {
      return Long.fromBits(0, low << (numBits - 32));
    }
  }
};

Long.prototype.shiftRight = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >> numBits);
    } else {
      return Long.fromBits(
          high >> (numBits - 32),
          high >= 0 ? 0 : -1);
    }
  }
};

Long.prototype.shiftRightUnsigned = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >>> numBits);
    } else if (numBits == 32) {
      return Long.fromBits(high, 0);
    } else {
      return Long.fromBits(high >>> (numBits - 32), 0);
    }
  }
};



// Int64
function hs_eqInt64(x, y) {return x.equals(y);}
function hs_neInt64(x, y) {return !x.equals(y);}
function hs_ltInt64(x, y) {return x.compare(y) < 0;}
function hs_leInt64(x, y) {return x.compare(y) <= 0;}
function hs_gtInt64(x, y) {return x.compare(y) > 0;}
function hs_geInt64(x, y) {return x.compare(y) >= 0;}
function hs_quotInt64(x, y) {return x.div(y);}
function hs_remInt64(x, y) {return x.modulo(y);}
function hs_plusInt64(x, y) {return x.add(y);}
function hs_minusInt64(x, y) {return x.subtract(y);}
function hs_timesInt64(x, y) {return x.multiply(y);}
function hs_negateInt64(x) {return x.negate();}
function hs_uncheckedIShiftL64(x, bits) {return x.shiftLeft(bits);}
function hs_uncheckedIShiftRA64(x, bits) {return x.shiftRight(bits);}
function hs_uncheckedIShiftRL64(x, bits) {return x.shiftRightUnsigned(bits);}
function hs_intToInt64(x) {return new Long(x, 0);}
function hs_int64ToInt(x) {return x.toInt();}



// Word64
function hs_wordToWord64(x) {
    return I_fromInt(x);
}
function hs_word64ToWord(x) {
    return I_toInt(x);
}
function hs_mkWord64(low, high) {
    return I_fromBits([low, high]);
}

var hs_and64 = I_and;
var hs_or64 = I_or;
var hs_xor64 = I_xor;
var __i64_all_ones = I_fromBits([0xffffffff, 0xffffffff]);
function hs_not64(x) {
    return I_xor(x, __i64_all_ones);
}
var hs_eqWord64 = I_equals;
var hs_neWord64 = I_notEquals;
var hs_ltWord64 = I_lessThan;
var hs_leWord64 = I_lessThanOrEqual;
var hs_gtWord64 = I_greaterThan;
var hs_geWord64 = I_greaterThanOrEqual;
var hs_quotWord64 = I_quot;
var hs_remWord64 = I_rem;
var __w64_max = I_fromBits([0,0,1]);
function hs_uncheckedShiftL64(x, bits) {
    return I_rem(I_shiftLeft(x, bits), __w64_max);
}
var hs_uncheckedShiftRL64 = I_shiftRight;
function hs_int64ToWord64(x) {
    var tmp = I_add(__w64_max, I_fromBits([x.getLowBits(), x.getHighBits()]));
    return I_rem(tmp, __w64_max);
}
function hs_word64ToInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
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

function withChar(f, charCode) {
    return f(String.fromCharCode(charCode)).charCodeAt(0);
}

function u_towlower(charCode) {
    return withChar(function(c) {return c.toLowerCase()}, charCode);
}

function u_towupper(charCode) {
    return withChar(function(c) {return c.toUpperCase()}, charCode);
}

var u_towtitle = u_towupper;

function u_iswupper(charCode) {
    var c = String.fromCharCode(charCode);
    return c == c.toUpperCase() && c != c.toLowerCase();
}

function u_iswlower(charCode) {
    var c = String.fromCharCode(charCode);
    return  c == c.toLowerCase() && c != c.toUpperCase();
}

function u_iswdigit(charCode) {
    return charCode >= 48 && charCode <= 57;
}

function u_iswcntrl(charCode) {
    return charCode <= 0x1f || charCode == 0x7f;
}

function u_iswspace(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(/\s/g,'') != c;
}

function u_iswalpha(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(__hs_alphare, '') != c;
}

function u_iswalnum(charCode) {
    return u_iswdigit(charCode) || u_iswalpha(charCode);
}

function u_iswprint(charCode) {
    return !u_iswcntrl(charCode);
}

function u_gencat(c) {
    throw 'u_gencat is only supported with --full-unicode.';
}

// Regex that matches any alphabetic character in any language. Horrible thing.
var __hs_alphare = /[\u0041-\u005A\u0061-\u007A\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05D0-\u05EA\u05F0-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u08A0\u08A2-\u08AC\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097F\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C3D\u0C58\u0C59\u0C60\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D60\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E87\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA\u0EAB\u0EAD-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F4\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1877\u1880-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191C\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19C1-\u19C7\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1CE9-\u1CEC\u1CEE-\u1CF1\u1CF5\u1CF6\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2C2E\u2C30-\u2C5E\u2C60-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312D\u3131-\u318E\u31A0-\u31BA\u31F0-\u31FF\u3400-\u4DB5\u4E00-\u9FCC\uA000-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA697\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA78E\uA790-\uA793\uA7A0-\uA7AA\uA7F8-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA80-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uABC0-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]/g;

// 2D Canvas drawing primitives.
function jsHasCtx2D(elem) {return !!elem.getContext;}
function jsGetCtx2D(elem) {return elem.getContext('2d');}
function jsBeginPath(ctx) {ctx.beginPath();}
function jsMoveTo(ctx, x, y) {ctx.moveTo(x, y);}
function jsLineTo(ctx, x, y) {ctx.lineTo(x, y);}
function jsStroke(ctx) {ctx.stroke();}
function jsFill(ctx) {ctx.fill();}
function jsRotate(ctx, radians) {ctx.rotate(radians);}
function jsTranslate(ctx, x, y) {ctx.translate(x, y);}
function jsScale(ctx, x, y) {ctx.scale(x, y);}
function jsPushState(ctx) {ctx.save();}
function jsPopState(ctx) {ctx.restore();}
function jsResetCanvas(el) {el.width = el.width;}
function jsDrawImage(ctx, img, x, y) {ctx.drawImage(img, x, y);}
function jsDrawImageClipped(ctx, img, x, y, cx, cy, cw, ch) {
    ctx.drawImage(img, cx, cy, cw, ch, x, y, cw, ch);
}
function jsDrawText(ctx, str, x, y) {ctx.fillText(str, x, y);}
function jsClip(ctx) {ctx.clip();}
function jsArc(ctx, x, y, radius, fromAngle, toAngle) {
    ctx.arc(x, y, radius, fromAngle, toAngle);
}
function jsCanvasToDataURL(el) {return el.toDataURL('image/png');}

// Simulate handles.
// When implementing new handles, remember that passed strings may be thunks,
// and so need to be evaluated before use.

function jsNewHandle(init, read, write, flush, close, seek, tell) {
    var h = {
        read: read || function() {},
        write: write || function() {},
        seek: seek || function() {},
        tell: tell || function() {},
        close: close || function() {},
        flush: flush || function() {}
    };
    init.call(h);
    return h;
}

function jsReadHandle(h, len) {return h.read(len);}
function jsWriteHandle(h, str) {return h.write(str);}
function jsFlushHandle(h) {return h.flush();}
function jsCloseHandle(h) {return h.close();}

function jsMkConWriter(op) {
    return function(str) {
        str = E(str);
        var lines = (this.buf + str).split('\n');
        for(var i = 0; i < lines.length-1; ++i) {
            op.call(console, lines[i]);
        }
        this.buf = lines[lines.length-1];
    }
}

function jsMkStdout() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.log),
        function() {console.log(this.buf); this.buf = '';}
    );
}

function jsMkStderr() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.warn),
        function() {console.warn(this.buf); this.buf = '';}
    );
}

function jsMkStdin() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(len) {
            while(this.buf.length < len) {
                this.buf += prompt('[stdin]') + '\n';
            }
            var ret = this.buf.substr(0, len);
            this.buf = this.buf.substr(len);
            return ret;
        }
    );
}

var _0=[0],_1=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_2=new T(function(){return B(err(_1));}),_3=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_4=new T(function(){return B(err(_3));}),_5=function(_6,_7){while(1){var _8=E(_6);if(!_8[0]){return E(_4);}else{var _9=E(_7);if(!_9){return E(_8[1]);}else{_6=_8[2];_7=_9-1|0;continue;}}}},_a=new T(function(){return B(unCStr("GHC.IO.Exception"));}),_b=new T(function(){return B(unCStr("base"));}),_c=new T(function(){return B(unCStr("IOException"));}),_d=[0],_e=new T(function(){var _f=hs_wordToWord64(4053623282),_g=_f,_h=hs_wordToWord64(3693590983),_i=_h;return [0,_g,_i,[0,_g,_i,_b,_a,_c],_d];}),_j=function(_k){return E(_e);},_l=function(_m){return E(E(_m)[1]);},_n=function(_o,_p,_q){var _r=B(A(_o,[_])),_s=B(A(_p,[_])),_t=hs_eqWord64(_r[1],_s[1]),_u=_t;if(!E(_u)){return [0];}else{var _v=hs_eqWord64(_r[2],_s[2]),_w=_v;return E(_w)==0?[0]:[1,_q];}},_x=function(_y){var _z=E(_y);return new F(function(){return _n(B(_l(_z[1])),_j,_z[2]);});},_A=new T(function(){return B(unCStr(": "));}),_B=[0,41],_C=new T(function(){return B(unCStr(" ("));}),_D=function(_E,_F){var _G=E(_E);return _G[0]==0?E(_F):[1,_G[1],new T(function(){return B(_D(_G[2],_F));})];},_H=new T(function(){return B(unCStr("already exists"));}),_I=new T(function(){return B(unCStr("does not exist"));}),_J=new T(function(){return B(unCStr("protocol error"));}),_K=new T(function(){return B(unCStr("failed"));}),_L=new T(function(){return B(unCStr("invalid argument"));}),_M=new T(function(){return B(unCStr("inappropriate type"));}),_N=new T(function(){return B(unCStr("hardware fault"));}),_O=new T(function(){return B(unCStr("unsupported operation"));}),_P=new T(function(){return B(unCStr("timeout"));}),_Q=new T(function(){return B(unCStr("resource vanished"));}),_R=new T(function(){return B(unCStr("interrupted"));}),_S=new T(function(){return B(unCStr("resource busy"));}),_T=new T(function(){return B(unCStr("resource exhausted"));}),_U=new T(function(){return B(unCStr("end of file"));}),_V=new T(function(){return B(unCStr("illegal operation"));}),_W=new T(function(){return B(unCStr("permission denied"));}),_X=new T(function(){return B(unCStr("user error"));}),_Y=new T(function(){return B(unCStr("unsatisified constraints"));}),_Z=new T(function(){return B(unCStr("system error"));}),_10=function(_11,_12){switch(E(_11)){case 0:return new F(function(){return _D(_H,_12);});break;case 1:return new F(function(){return _D(_I,_12);});break;case 2:return new F(function(){return _D(_S,_12);});break;case 3:return new F(function(){return _D(_T,_12);});break;case 4:return new F(function(){return _D(_U,_12);});break;case 5:return new F(function(){return _D(_V,_12);});break;case 6:return new F(function(){return _D(_W,_12);});break;case 7:return new F(function(){return _D(_X,_12);});break;case 8:return new F(function(){return _D(_Y,_12);});break;case 9:return new F(function(){return _D(_Z,_12);});break;case 10:return new F(function(){return _D(_J,_12);});break;case 11:return new F(function(){return _D(_K,_12);});break;case 12:return new F(function(){return _D(_L,_12);});break;case 13:return new F(function(){return _D(_M,_12);});break;case 14:return new F(function(){return _D(_N,_12);});break;case 15:return new F(function(){return _D(_O,_12);});break;case 16:return new F(function(){return _D(_P,_12);});break;case 17:return new F(function(){return _D(_Q,_12);});break;default:return new F(function(){return _D(_R,_12);});}},_13=[0,125],_14=new T(function(){return B(unCStr("{handle: "));}),_15=function(_16,_17,_18,_19,_1a,_1b){var _1c=new T(function(){var _1d=new T(function(){return B(_10(_17,new T(function(){var _1e=E(_19);return _1e[0]==0?E(_1b):B(_D(_C,new T(function(){return B(_D(_1e,[1,_B,_1b]));})));})));}),_1f=E(_18);return _1f[0]==0?E(_1d):B(_D(_1f,new T(function(){return B(_D(_A,_1d));})));}),_1g=E(_1a);if(!_1g[0]){var _1h=E(_16);if(!_1h[0]){return E(_1c);}else{var _1i=E(_1h[1]);return _1i[0]==0?B(_D(_14,new T(function(){return B(_D(_1i[1],[1,_13,new T(function(){return B(_D(_A,_1c));})]));}))):B(_D(_14,new T(function(){return B(_D(_1i[1],[1,_13,new T(function(){return B(_D(_A,_1c));})]));})));}}else{return new F(function(){return _D(_1g[1],new T(function(){return B(_D(_A,_1c));}));});}},_1j=function(_1k){var _1l=E(_1k);return new F(function(){return _15(_1l[1],_1l[2],_1l[3],_1l[4],_1l[6],_d);});},_1m=function(_1n,_1o){var _1p=E(_1n);return new F(function(){return _15(_1p[1],_1p[2],_1p[3],_1p[4],_1p[6],_1o);});},_1q=[0,44],_1r=[0,93],_1s=[0,91],_1t=function(_1u,_1v,_1w){var _1x=E(_1v);return _1x[0]==0?B(unAppCStr("[]",_1w)):[1,_1s,new T(function(){return B(A(_1u,[_1x[1],new T(function(){var _1y=function(_1z){var _1A=E(_1z);return _1A[0]==0?E([1,_1r,_1w]):[1,_1q,new T(function(){return B(A(_1u,[_1A[1],new T(function(){return B(_1y(_1A[2]));})]));})];};return B(_1y(_1x[2]));})]));})];},_1B=function(_1C,_1D){return new F(function(){return _1t(_1m,_1C,_1D);});},_1E=function(_1F,_1G,_1H){var _1I=E(_1G);return new F(function(){return _15(_1I[1],_1I[2],_1I[3],_1I[4],_1I[6],_1H);});},_1J=[0,_1E,_1j,_1B],_1K=new T(function(){return [0,_j,_1J,_1L,_x];}),_1L=function(_1M){return [0,_1K,_1M];},_1N=7,_1O=function(_1P){return [0,_0,_1N,_d,_1P,_0,_0];},_1Q=function(_1R,_){return new F(function(){return die(new T(function(){return B(_1L(new T(function(){return B(_1O(_1R));})));}));});},_1S=function(_1T,_){return new F(function(){return _1Q(_1T,_);});},_1U=function(_1V,_){return new F(function(){return _1S(_1V,_);});},_1W=function(_1X,_){return new F(function(){return _1U(_1X,_);});},_1Y=[0,87],_1Z=[0,66],_20=[0,125],_21=new T(function(){return B(unCStr(", "));}),_22=new T(function(){return B(unCStr("pointsW = "));}),_23=new T(function(){return B(unCStr("pointsB = "));}),_24=new T(function(){return B(unCStr("board = "));}),_25=new T(function(){return B(unCStr("turnMode = "));}),_26=new T(function(){return B(unCStr("activePlayer = "));}),_27=new T(function(){return B(unCStr("GameState {"));}),_28=[0,0],_29=function(_2a,_2b,_2c){return new F(function(){return A(_2a,[[1,_1q,new T(function(){return B(A(_2b,[_2c]));})]]);});},_2d=new T(function(){return B(unCStr("WaitRemoveRun "));}),_2e=new T(function(){return B(unCStr("RemoveRing "));}),_2f=new T(function(){return B(unCStr("RemoveRun "));}),_2g=new T(function(){return B(unCStr("MoveRing "));}),_2h=new T(function(){return B(unCStr("AddMarker"));}),_2i=function(_2j){return new F(function(){return _D(_2h,_2j);});},_2k=new T(function(){return B(unCStr("AddRing"));}),_2l=function(_2j){return new F(function(){return _D(_2k,_2j);});},_2m=new T(function(){return B(unCStr("WaitAddMarker"));}),_2n=function(_2j){return new F(function(){return _D(_2m,_2j);});},_2o=function(_2p,_2q){var _2r=jsShowI(_2p),_2s=_2r;return new F(function(){return _D(fromJSStr(_2s),_2q);});},_2t=[0,41],_2u=[0,40],_2v=function(_2w,_2x,_2y){if(_2x>=0){return new F(function(){return _2o(_2x,_2y);});}else{return _2w<=6?B(_2o(_2x,_2y)):[1,_2u,new T(function(){var _2z=jsShowI(_2x),_2A=_2z;return B(_D(fromJSStr(_2A),[1,_2t,_2y]));})];}},_2B=new T(function(){return B(unCStr(": empty list"));}),_2C=new T(function(){return B(unCStr("Prelude."));}),_2D=function(_2E){return new F(function(){return err(B(_D(_2C,new T(function(){return B(_D(_2E,_2B));}))));});},_2F=new T(function(){return B(unCStr("foldr1"));}),_2G=new T(function(){return B(_2D(_2F));}),_2H=function(_2I,_2J){var _2K=E(_2J);if(!_2K[0]){return E(_2G);}else{var _2L=_2K[1],_2M=E(_2K[2]);if(!_2M[0]){return E(_2L);}else{return new F(function(){return A(_2I,[_2L,new T(function(){return B(_2H(_2I,_2M));})]);});}}},_2N=function(_2O,_2P){var _2Q=E(_2P);switch(_2Q[0]){case 0:return E(_2l);case 1:return E(_2i);case 2:var _2R=function(_2S){return new F(function(){return _D(_2g,new T(function(){var _2T=E(_2Q[1]);return [1,_2u,new T(function(){return B(A(_2H,[_29,[1,function(_2U){return new F(function(){return _2v(0,E(_2T[1])[1],_2U);});},[1,function(_2V){return new F(function(){return _2v(0,E(_2T[2])[1],_2V);});},_d]],[1,_2t,_2S]]));})];}));});};return E(_2O)[1]<11?E(_2R):function(_2W){return [1,_2u,new T(function(){return B(_2R([1,_2t,_2W]));})];};case 3:var _2X=_2Q[1];return E(_2O)[1]<11?function(_2Y){return new F(function(){return _D(_2f,new T(function(){return E(_2X)==0?[1,_1Z,_2Y]:[1,_1Y,_2Y];}));});}:function(_2Z){return [1,_2u,new T(function(){return B(_D(_2f,new T(function(){return E(_2X)==0?[1,_1Z,[1,_2t,_2Z]]:[1,_1Y,[1,_2t,_2Z]];})));})];};case 4:var _30=_2Q[1];return E(_2O)[1]<11?function(_31){return new F(function(){return _D(_2e,new T(function(){return E(_30)==0?[1,_1Z,_31]:[1,_1Y,_31];}));});}:function(_32){return [1,_2u,new T(function(){return B(_D(_2e,new T(function(){return E(_30)==0?[1,_1Z,[1,_2t,_32]]:[1,_1Y,[1,_2t,_32]];})));})];};case 5:var _33=_2Q[1];return E(_2O)[1]<11?function(_34){return new F(function(){return _D(_2d,new T(function(){return E(_33)==0?[1,_1Z,_34]:[1,_1Y,_34];}));});}:function(_35){return [1,_2u,new T(function(){return B(_D(_2d,new T(function(){return E(_33)==0?[1,_1Z,[1,_2t,_35]]:[1,_1Y,[1,_2t,_35]];})));})];};default:return E(_2n);}},_36=[0,0],_37=function(_38){return E(E(_38)[1]);},_39=function(_3a,_3b,_3c,_3d){return new F(function(){return _1t(function(_3e,_3f){var _3g=E(_3e);return [1,_2u,new T(function(){return B(A(_2H,[_29,[1,new T(function(){return B(A(new T(function(){return B(_37(_3a));}),[_36,_3g[1]]));}),[1,new T(function(){return B(A(new T(function(){return B(_37(_3b));}),[_36,_3g[2]]));}),_d]],[1,_2t,_3f]]));})];},_3c,_3d);});},_3h=[1,_2t,_d],_3i=function(_3j,_3k,_3l){var _3m=E(_3l);return [1,_2u,new T(function(){return B(A(_2H,[_29,[1,new T(function(){return B(A(_37,[_3j,_36,_3m[1]]));}),[1,new T(function(){return B(A(_37,[_3k,_36,_3m[2]]));}),_d]],_3h]));})];},_3n=function(_3o,_3p,_3q,_3r,_3s){var _3t=E(_3r);return [1,_2u,new T(function(){return B(A(_2H,[_29,[1,new T(function(){return B(A(_37,[_3o,_36,_3t[1]]));}),[1,new T(function(){return B(A(_37,[_3p,_36,_3t[2]]));}),_d]],[1,_2t,_3s]]));})];},_3u=function(_3v,_3w){return [0,function(_3x,_3y,_3z){return new F(function(){return _3n(_3v,_3w,_3x,_3y,_3z);});},function(_3z){return new F(function(){return _3i(_3v,_3w,_3z);});},function(_3y,_3z){return new F(function(){return _39(_3v,_3w,_3y,_3z);});}];},_3A=function(_3B){return new F(function(){return _2v(0,E(_3B)[1],_d);});},_3C=function(_3D,_3E){return new F(function(){return _2v(0,E(_3D)[1],_3E);});},_3F=function(_3G,_3H){return new F(function(){return _1t(_3C,_3G,_3H);});},_3I=function(_3J,_3K,_3L){return new F(function(){return _2v(E(_3J)[1],E(_3K)[1],_3L);});},_3M=[0,_3I,_3A,_3F],_3N=new T(function(){return B(_3u(_3M,_3M));}),_3O=new T(function(){return B(unCStr("markersW = "));}),_3P=new T(function(){return B(unCStr("markersB = "));}),_3Q=new T(function(){return B(unCStr("ringsW = "));}),_3R=new T(function(){return B(unCStr("ringsB = "));}),_3S=new T(function(){return B(unCStr("bmap = "));}),_3T=new T(function(){return B(unCStr("Board {"));}),_3U=new T(function(){return B(unCStr("Marker "));}),_3V=new T(function(){return B(unCStr("Ring "));}),_3W=function(_3X,_3Y,_3Z){var _40=E(_3Y);if(!_40[0]){var _41=_40[1];return _3X<11?B(_D(_3V,new T(function(){return E(_41)==0?[1,_1Z,_3Z]:[1,_1Y,_3Z];}))):[1,_2u,new T(function(){return B(_D(_3V,new T(function(){return E(_41)==0?[1,_1Z,[1,_2t,_3Z]]:[1,_1Y,[1,_2t,_3Z]];})));})];}else{var _42=_40[1];return _3X<11?B(_D(_3U,new T(function(){return E(_42)==0?[1,_1Z,_3Z]:[1,_1Y,_3Z];}))):[1,_2u,new T(function(){return B(_D(_3U,new T(function(){return E(_42)==0?[1,_1Z,[1,_2t,_3Z]]:[1,_1Y,[1,_2t,_3Z]];})));})];}},_43=function(_44){return new F(function(){return _3W(0,_44,_d);});},_45=function(_46,_47){return new F(function(){return _3W(0,_46,_47);});},_48=function(_49,_2j){return new F(function(){return _1t(_45,_49,_2j);});},_4a=function(_4b,_4c,_4d){return new F(function(){return _3W(E(_4b)[1],_4c,_4d);});},_4e=[0,_4a,_43,_48],_4f=new T(function(){return B(unCStr("fromList "));}),_4g=function(_4h,_4i){while(1){var _4j=(function(_4k,_4l){var _4m=E(_4l);if(!_4m[0]){_4h=[1,[0,_4m[2],_4m[3]],new T(function(){return B(_4g(_4k,_4m[5]));})];_4i=_4m[4];return null;}else{return E(_4k);}})(_4h,_4i);if(_4j!=null){return _4j;}}},_4n=function(_4o,_4p,_4q,_4r){var _4s=new T(function(){return B(_4g(_d,_4r));});return _4q<=10?function(_4t){return new F(function(){return _D(_4f,new T(function(){return B(_39(_4o,_4p,_4s,_4t));}));});}:function(_4u){return [1,_2u,new T(function(){return B(_D(_4f,new T(function(){return B(_39(_4o,_4p,_4s,[1,_2t,_4u]));})));})];};},_4v=function(_4w,_4x,_4y,_4z,_4A,_4B){var _4C=function(_4D){return new F(function(){return _D(_3T,new T(function(){return B(_D(_3S,new T(function(){return B(A(new T(function(){return B(_4n(_3N,_4e,0,_4x));}),[new T(function(){return B(_D(_21,new T(function(){return B(_D(_3R,new T(function(){return B(_39(_3M,_3M,_4y,new T(function(){return B(_D(_21,new T(function(){return B(_D(_3Q,new T(function(){return B(_39(_3M,_3M,_4z,new T(function(){return B(_D(_21,new T(function(){return B(_D(_3P,new T(function(){return B(_39(_3M,_3M,_4A,new T(function(){return B(_D(_21,new T(function(){return B(_D(_3O,new T(function(){return B(_39(_3M,_3M,_4B,[1,_20,_4D]));})));})));})));})));})));})));})));})));})));})));})));})]));})));}));});};return _4w<11?E(_4C):function(_4E){return [1,_2u,new T(function(){return B(_4C([1,_2t,_4E]));})];};},_4F=function(_4G,_4H,_4I,_4J,_4K,_4L){var _4M=function(_4N){return new F(function(){return _D(_27,new T(function(){return B(_D(_26,new T(function(){var _4O=new T(function(){return B(_D(_21,new T(function(){return B(_D(_25,new T(function(){return B(A(new T(function(){return B(_2N(_28,_4I));}),[new T(function(){return B(_D(_21,new T(function(){return B(_D(_24,new T(function(){return B(A(new T(function(){var _4P=E(_4J);return B(_4v(0,_4P[1],_4P[2],_4P[3],_4P[4],_4P[5]));}),[new T(function(){return B(_D(_21,new T(function(){return B(_D(_23,new T(function(){return B(_2v(0,E(_4K)[1],new T(function(){return B(_D(_21,new T(function(){return B(_D(_22,new T(function(){return B(_2v(0,E(_4L)[1],[1,_20,_4N]));})));})));})));})));})));})]));})));})));})]));})));})));});return E(_4H)==0?[1,_1Z,_4O]:[1,_1Y,_4O];})));}));});};return _4G<11?E(_4M):function(_4Q){return [1,_2u,new T(function(){return B(_4M([1,_2t,_4Q]));})];};},_4R=function(_4S,_4T){if(_4S<=_4T){var _4U=function(_4V){return [1,[0,_4V],new T(function(){if(_4V!=_4T){var _4W=B(_4U(_4V+1|0));}else{var _4W=[0];}var _4X=_4W;return _4X;})];};return new F(function(){return _4U(_4S);});}else{return [0];}},_4Y=new T(function(){return B(_4R(0,2147483647));}),_4Z=function(_50,_51,_52){while(1){var _53=E(_52);if(!_53[0]){return [0,[0,_50],_51];}else{var _54=_53[2],_55=E(_53[1]),_56=E(_55[1])[1];if(_50>=_56){if(_50!=_56){_50=_56;_51=_55[2];_52=_54;continue;}else{_52=_54;continue;}}else{_52=_54;continue;}}}},_57=new T(function(){return B(_4R(-5,5));}),_58=function(_59){var _5a=_59,_5b=function(_5c){while(1){var _5d=(function(_5e){var _5f=E(_5e);if(!_5f[0]){return E(new T(function(){var _5g=E(_59);if(_5g==5){var _5h=[0];}else{var _5h=B(_58(_5g+1|0));}return _5h;}));}else{var _5i=_5f[2],_5j=E(_5f[1]);if(Math.pow(0.5*Math.sqrt(3)*_5a,2)+Math.pow(0.5*_5a-_5j[1],2)>Math.pow(4.6,2)){_5c=_5i;return null;}else{return [1,[0,[0,_59],_5j],new T(function(){return B(_5b(_5i));})];}}})(_5c);if(_5d!=null){return _5d;}}};return new F(function(){return _5b(_57);});},_5k=new T(function(){return B(_58(-5));}),_5l=function(_5m,_5n){var _5o=E(_5m),_5p=E(_5o[1])[1],_5q=E(_5o[2])[1],_5r=E(_5n),_5s=E(_5r[1])[1],_5t=E(_5r[2])[1],_5u=(imul(_5s,_5s)|0)+(imul(_5t,_5t)|0)|0,_5v=(imul(_5p,_5p)|0)+(imul(_5q,_5q)|0)|0;return _5v>=_5u?_5v!=_5u?2:1:0;},_5w=[1,_d,_d],_5x=function(_5y,_5z){var _5A=function(_5B,_5C){var _5D=E(_5B);if(!_5D[0]){return E(_5C);}else{var _5E=_5D[1],_5F=E(_5C);if(!_5F[0]){return E(_5D);}else{var _5G=_5F[1];return B(A(_5y,[_5E,_5G]))==2?[1,_5G,new T(function(){return B(_5A(_5D,_5F[2]));})]:[1,_5E,new T(function(){return B(_5A(_5D[2],_5F));})];}}},_5H=function(_5I){var _5J=E(_5I);if(!_5J[0]){return [0];}else{var _5K=E(_5J[2]);return _5K[0]==0?E(_5J):[1,new T(function(){return B(_5A(_5J[1],_5K[1]));}),new T(function(){return B(_5H(_5K[2]));})];}},_5L=function(_5M){while(1){var _5N=E(_5M);if(!_5N[0]){return E(new T(function(){return B(_5L(B(_5H(_d))));}));}else{if(!E(_5N[2])[0]){return E(_5N[1]);}else{_5M=B(_5H(_5N));continue;}}}},_5O=new T(function(){return B(_5P(_d));}),_5P=function(_5Q){var _5R=E(_5Q);if(!_5R[0]){return E(_5w);}else{var _5S=_5R[1],_5T=E(_5R[2]);if(!_5T[0]){return [1,_5R,_d];}else{var _5U=_5T[1],_5V=_5T[2];if(B(A(_5y,[_5S,_5U]))==2){return new F(function(){return (function(_5W,_5X,_5Y){while(1){var _5Z=(function(_60,_61,_62){var _63=E(_62);if(!_63[0]){return [1,[1,_60,_61],_5O];}else{var _64=_63[1];if(B(A(_5y,[_60,_64]))==2){_5W=_64;var _65=[1,_60,_61];_5Y=_63[2];_5X=_65;return null;}else{return [1,[1,_60,_61],new T(function(){return B(_5P(_63));})];}}})(_5W,_5X,_5Y);if(_5Z!=null){return _5Z;}}})(_5U,[1,_5S,_d],_5V);});}else{return new F(function(){return (function(_66,_67,_68){while(1){var _69=(function(_6a,_6b,_6c){var _6d=E(_6c);if(!_6d[0]){return [1,new T(function(){return B(A(_6b,[[1,_6a,_d]]));}),_5O];}else{var _6e=_6d[1],_6f=_6d[2];switch(B(A(_5y,[_6a,_6e]))){case 0:_66=_6e;_67=function(_6g){return new F(function(){return A(_6b,[[1,_6a,_6g]]);});};_68=_6f;return null;case 1:_66=_6e;_67=function(_6h){return new F(function(){return A(_6b,[[1,_6a,_6h]]);});};_68=_6f;return null;default:return [1,new T(function(){return B(A(_6b,[[1,_6a,_d]]));}),new T(function(){return B(_5P(_6d));})];}}})(_66,_67,_68);if(_69!=null){return _69;}}})(_5U,function(_6i){return [1,_5S,_6i];},_5V);});}}}};return new F(function(){return _5L(B(_5P(_5z)));});},_6j=new T(function(){return B(_5x(_5l,_5k));}),_6k=new T(function(){return B(unCStr("List.minimumBy: empty list"));}),_6l=new T(function(){return B(err(_6k));}),_6m=function(_6n,_6o){var _6p=E(_6o);return _6p[0]==0?[0]:[1,new T(function(){return B(A(_6n,[_6p[1]]));}),new T(function(){return B(_6m(_6n,_6p[2]));})];},_6q=function(_6r){var _6s=E(_6r),_6t=new T(function(){return [0,60*E(_6s[1])[1]];});return [0,new T(function(){return [0,0.5*Math.sqrt(3)*E(_6t)[1]+300];}),new T(function(){return [0, -(60*E(_6s[2])[1])+0.5*E(_6t)[1]+315];})];},_6u=new T(function(){return B(_6m(_6q,_6j));}),_6v=function(_6w,_6x,_6y){var _6z=E(_6x);if(!_6z[0]){return [0];}else{var _6A=E(_6y);return _6A[0]==0?[0]:[1,new T(function(){return B(A(_6w,[_6z[1],_6A[1]]));}),new T(function(){return B(_6v(_6w,_6z[2],_6A[2]));})];}},_6B=function(_6C,_6D){var _6E=B(_6v(function(_6F,_6G){return [0,new T(function(){var _6H=E(_6F),_6I=E(new T(function(){return [0,E(_6D)[1]];}))[1]-E(_6H[2])[1],_6J=E(new T(function(){return [0,E(_6C)[1]];}))[1]-E(_6H[1])[1];return [0,_6J*_6J+_6I*_6I];}),_6G];},_6u,_4Y));if(!_6E[0]){return E(_6l);}else{var _6K=E(_6E[1]),_6L=_6K[2],_6M=E(_6E[2]);if(!_6M[0]){var _6N=E(_6L)[1];return _6N>=0?B(_5(_6j,_6N)):E(_2);}else{var _6O=_6M[2],_6P=E(_6K[1])[1],_6Q=E(_6M[1]),_6R=E(_6Q[1])[1];if(_6P>=_6R){if(_6P!=_6R){var _6S=E(B(_4Z(_6R,_6Q[2],_6O))[2])[1];return _6S>=0?B(_5(_6j,_6S)):E(_2);}else{var _6T=E(B(_4Z(_6P,_6L,_6O))[2])[1];return _6T>=0?B(_5(_6j,_6T)):E(_2);}}else{var _6U=E(B(_4Z(_6P,_6L,_6O))[2])[1];return _6U>=0?B(_5(_6j,_6U)):E(_2);}}}},_6V=function(_6W,_6X){while(1){var _6Y=E(_6W);if(!_6Y[0]){return E(_6X);}else{_6W=_6Y[2];var _6Z=_6X+1|0;_6X=_6Z;continue;}}},_70=function(_71){return E(E(_71)[1]);},_72=function(_73,_74,_75,_76,_77,_78){return !B(A(_73,[_75,_77]))?true:!B(A(_70,[_74,_76,_78]))?true:false;},_79=function(_7a,_7b,_7c,_7d){var _7e=E(_7c),_7f=E(_7d);return new F(function(){return _72(E(_7a)[1],_7b,_7e[1],_7e[2],_7f[1],_7f[2]);});},_7g=function(_7h,_7i,_7j,_7k,_7l,_7m){return !B(A(_7h,[_7j,_7l]))?false:B(A(_70,[_7i,_7k,_7m]));},_7n=function(_7o,_7p,_7q,_7r){var _7s=E(_7q),_7t=E(_7r);return new F(function(){return _7g(E(_7o)[1],_7p,_7s[1],_7s[2],_7t[1],_7t[2]);});},_7u=function(_7v,_7w){return [0,function(_7x,_7y){return new F(function(){return _7n(_7v,_7w,_7x,_7y);});},function(_7x,_7y){return new F(function(){return _79(_7v,_7w,_7x,_7y);});}];},_7z=function(_7A,_7B){return E(_7A)[1]==E(_7B)[1];},_7C=function(_7D,_7E){return E(_7D)[1]!=E(_7E)[1];},_7F=[0,_7z,_7C],_7G=new T(function(){return B(_7u(_7F,_7F));}),_7H=[1],_7I=new T(function(){return B(unCStr("Failure in Data.Map.balanceL"));}),_7J=function(_7K){return new F(function(){return err(_7I);});},_7L=new T(function(){return B(_7J(_));}),_7M=function(_7N,_7O,_7P,_7Q){var _7R=E(_7Q);if(!_7R[0]){var _7S=_7R[1],_7T=E(_7P);if(!_7T[0]){var _7U=_7T[1],_7V=_7T[2],_7W=_7T[3];if(_7U<=(imul(3,_7S)|0)){return [0,(1+_7U|0)+_7S|0,E(E(_7N)),_7O,E(_7T),E(_7R)];}else{var _7X=E(_7T[4]);if(!_7X[0]){var _7Y=_7X[1],_7Z=E(_7T[5]);if(!_7Z[0]){var _80=_7Z[1],_81=_7Z[2],_82=_7Z[3],_83=_7Z[4];if(_80>=(imul(2,_7Y)|0)){var _84=function(_85){var _86=E(_7Z[5]);return _86[0]==0?[0,(1+_7U|0)+_7S|0,E(_81),_82,E([0,(1+_7Y|0)+_85|0,E(_7V),_7W,E(_7X),E(_83)]),E([0,(1+_7S|0)+_86[1]|0,E(E(_7N)),_7O,E(_86),E(_7R)])]:[0,(1+_7U|0)+_7S|0,E(_81),_82,E([0,(1+_7Y|0)+_85|0,E(_7V),_7W,E(_7X),E(_83)]),E([0,1+_7S|0,E(E(_7N)),_7O,E(_7H),E(_7R)])];},_87=E(_83);return _87[0]==0?B(_84(_87[1])):B(_84(0));}else{return [0,(1+_7U|0)+_7S|0,E(_7V),_7W,E(_7X),E([0,(1+_7S|0)+_80|0,E(E(_7N)),_7O,E(_7Z),E(_7R)])];}}else{return E(_7L);}}else{return E(_7L);}}}else{return [0,1+_7S|0,E(E(_7N)),_7O,E(_7H),E(_7R)];}}else{var _88=E(_7P);if(!_88[0]){var _89=_88[1],_8a=_88[2],_8b=_88[3],_8c=_88[5],_8d=E(_88[4]);if(!_8d[0]){var _8e=_8d[1],_8f=E(_8c);if(!_8f[0]){var _8g=_8f[1],_8h=_8f[2],_8i=_8f[3],_8j=_8f[4];if(_8g>=(imul(2,_8e)|0)){var _8k=function(_8l){var _8m=E(_8f[5]);return _8m[0]==0?[0,1+_89|0,E(_8h),_8i,E([0,(1+_8e|0)+_8l|0,E(_8a),_8b,E(_8d),E(_8j)]),E([0,1+_8m[1]|0,E(E(_7N)),_7O,E(_8m),E(_7H)])]:[0,1+_89|0,E(_8h),_8i,E([0,(1+_8e|0)+_8l|0,E(_8a),_8b,E(_8d),E(_8j)]),E([0,1,E(E(_7N)),_7O,E(_7H),E(_7H)])];},_8n=E(_8j);return _8n[0]==0?B(_8k(_8n[1])):B(_8k(0));}else{return [0,1+_89|0,E(_8a),_8b,E(_8d),E([0,1+_8g|0,E(E(_7N)),_7O,E(_8f),E(_7H)])];}}else{return [0,3,E(_8a),_8b,E(_8d),E([0,1,E(E(_7N)),_7O,E(_7H),E(_7H)])];}}else{var _8o=E(_8c);return _8o[0]==0?[0,3,E(_8o[2]),_8o[3],E([0,1,E(_8a),_8b,E(_7H),E(_7H)]),E([0,1,E(E(_7N)),_7O,E(_7H),E(_7H)])]:[0,2,E(E(_7N)),_7O,E(_88),E(_7H)];}}else{return [0,1,E(E(_7N)),_7O,E(_7H),E(_7H)];}}},_8p=new T(function(){return B(unCStr("Failure in Data.Map.balanceR"));}),_8q=function(_8r){return new F(function(){return err(_8p);});},_8s=new T(function(){return B(_8q(_));}),_8t=function(_8u,_8v,_8w,_8x){var _8y=E(_8w);if(!_8y[0]){var _8z=_8y[1],_8A=E(_8x);if(!_8A[0]){var _8B=_8A[1],_8C=_8A[2],_8D=_8A[3];if(_8B<=(imul(3,_8z)|0)){return [0,(1+_8z|0)+_8B|0,E(E(_8u)),_8v,E(_8y),E(_8A)];}else{var _8E=E(_8A[4]);if(!_8E[0]){var _8F=_8E[1],_8G=_8E[2],_8H=_8E[3],_8I=_8E[4],_8J=E(_8A[5]);if(!_8J[0]){var _8K=_8J[1];if(_8F>=(imul(2,_8K)|0)){var _8L=function(_8M){var _8N=E(_8u),_8O=E(_8E[5]);return _8O[0]==0?[0,(1+_8z|0)+_8B|0,E(_8G),_8H,E([0,(1+_8z|0)+_8M|0,E(_8N),_8v,E(_8y),E(_8I)]),E([0,(1+_8K|0)+_8O[1]|0,E(_8C),_8D,E(_8O),E(_8J)])]:[0,(1+_8z|0)+_8B|0,E(_8G),_8H,E([0,(1+_8z|0)+_8M|0,E(_8N),_8v,E(_8y),E(_8I)]),E([0,1+_8K|0,E(_8C),_8D,E(_7H),E(_8J)])];},_8P=E(_8I);return _8P[0]==0?B(_8L(_8P[1])):B(_8L(0));}else{return [0,(1+_8z|0)+_8B|0,E(_8C),_8D,E([0,(1+_8z|0)+_8F|0,E(E(_8u)),_8v,E(_8y),E(_8E)]),E(_8J)];}}else{return E(_8s);}}else{return E(_8s);}}}else{return [0,1+_8z|0,E(E(_8u)),_8v,E(_8y),E(_7H)];}}else{var _8Q=E(_8x);if(!_8Q[0]){var _8R=_8Q[1],_8S=_8Q[2],_8T=_8Q[3],_8U=_8Q[5],_8V=E(_8Q[4]);if(!_8V[0]){var _8W=_8V[1],_8X=_8V[2],_8Y=_8V[3],_8Z=_8V[4],_90=E(_8U);if(!_90[0]){var _91=_90[1];if(_8W>=(imul(2,_91)|0)){var _92=function(_93){var _94=E(_8u),_95=E(_8V[5]);return _95[0]==0?[0,1+_8R|0,E(_8X),_8Y,E([0,1+_93|0,E(_94),_8v,E(_7H),E(_8Z)]),E([0,(1+_91|0)+_95[1]|0,E(_8S),_8T,E(_95),E(_90)])]:[0,1+_8R|0,E(_8X),_8Y,E([0,1+_93|0,E(_94),_8v,E(_7H),E(_8Z)]),E([0,1+_91|0,E(_8S),_8T,E(_7H),E(_90)])];},_96=E(_8Z);return _96[0]==0?B(_92(_96[1])):B(_92(0));}else{return [0,1+_8R|0,E(_8S),_8T,E([0,1+_8W|0,E(E(_8u)),_8v,E(_7H),E(_8V)]),E(_90)];}}else{return [0,3,E(_8X),_8Y,E([0,1,E(E(_8u)),_8v,E(_7H),E(_7H)]),E([0,1,E(_8S),_8T,E(_7H),E(_7H)])];}}else{var _97=E(_8U);return _97[0]==0?[0,3,E(_8S),_8T,E([0,1,E(E(_8u)),_8v,E(_7H),E(_7H)]),E(_97)]:[0,2,E(E(_8u)),_8v,E(_7H),E(_8Q)];}}else{return [0,1,E(E(_8u)),_8v,E(_7H),E(_7H)];}}},_98=function(_99,_9a,_9b,_9c,_9d){var _9e=E(_9d);if(!_9e[0]){var _9f=new T(function(){var _9g=B(_98(_9e[1],_9e[2],_9e[3],_9e[4],_9e[5]));return [0,_9g[1],_9g[2]];});return [0,new T(function(){return E(E(_9f)[1]);}),new T(function(){return B(_7M(_9a,_9b,_9c,E(_9f)[2]));})];}else{return [0,[0,_9a,_9b],_9c];}},_9h=function(_9i,_9j,_9k,_9l,_9m){var _9n=E(_9l);if(!_9n[0]){var _9o=new T(function(){var _9p=B(_9h(_9n[1],_9n[2],_9n[3],_9n[4],_9n[5]));return [0,_9p[1],_9p[2]];});return [0,new T(function(){return E(E(_9o)[1]);}),new T(function(){return B(_8t(_9j,_9k,E(_9o)[2],_9m));})];}else{return [0,[0,_9j,_9k],_9m];}},_9q=function(_9r,_9s){var _9t=E(_9r);if(!_9t[0]){var _9u=_9t[1],_9v=E(_9s);if(!_9v[0]){var _9w=_9v[1];if(_9u<=_9w){var _9x=B(_9h(_9w,_9v[2],_9v[3],_9v[4],_9v[5])),_9y=E(_9x[1]);return new F(function(){return _7M(_9y[1],_9y[2],_9t,_9x[2]);});}else{var _9z=B(_98(_9u,_9t[2],_9t[3],_9t[4],_9t[5])),_9A=E(_9z[1]);return new F(function(){return _8t(_9A[1],_9A[2],_9z[2],_9v);});}}else{return E(_9t);}}else{return E(_9s);}},_9B=function(_9C,_9D,_9E){var _9F=E(_9E);if(!_9F[0]){var _9G=_9F[3],_9H=_9F[4],_9I=_9F[5],_9J=E(_9F[2]),_9K=E(_9J[1])[1];if(_9C>=_9K){if(_9C!=_9K){return new F(function(){return _7M(_9J,_9G,_9H,B(_9B(_9C,_9D,_9I)));});}else{var _9L=E(_9J[2])[1];if(_9D>=_9L){if(_9D!=_9L){return new F(function(){return _7M(_9J,_9G,_9H,B(_9B(_9C,_9D,_9I)));});}else{return new F(function(){return _9q(_9H,_9I);});}}else{return new F(function(){return _8t(_9J,_9G,B(_9B(_9C,_9D,_9H)),_9I);});}}}else{return new F(function(){return _8t(_9J,_9G,B(_9B(_9C,_9D,_9H)),_9I);});}}else{return [1];}},_9M=function(_9N,_9O,_9P){var _9Q=E(_9P);if(!_9Q[0]){var _9R=_9Q[3],_9S=_9Q[4],_9T=_9Q[5],_9U=E(_9Q[2]),_9V=E(_9U[1])[1];if(_9N>=_9V){if(_9N!=_9V){return new F(function(){return _7M(_9U,_9R,_9S,B(_9M(_9N,_9O,_9T)));});}else{var _9W=E(_9O)[1],_9X=E(_9U[2])[1];if(_9W>=_9X){if(_9W!=_9X){return new F(function(){return _7M(_9U,_9R,_9S,B(_9B(_9N,_9W,_9T)));});}else{return new F(function(){return _9q(_9S,_9T);});}}else{return new F(function(){return _8t(_9U,_9R,B(_9B(_9N,_9W,_9S)),_9T);});}}}else{return new F(function(){return _8t(_9U,_9R,B(_9M(_9N,_9O,_9S)),_9T);});}}else{return [1];}},_9Y=function(_9Z,_a0,_a1,_a2){var _a3=E(_a2);if(!_a3[0]){var _a4=_a3[3],_a5=_a3[4],_a6=_a3[5],_a7=E(_a3[2]),_a8=E(_a7[1])[1];if(_9Z>=_a8){if(_9Z!=_a8){return new F(function(){return _8t(_a7,_a4,_a5,B(_9Y(_9Z,_a0,_a1,_a6)));});}else{var _a9=E(_a7[2])[1];if(_a0>=_a9){if(_a0!=_a9){return new F(function(){return _8t(_a7,_a4,_a5,B(_9Y(_9Z,_a0,_a1,_a6)));});}else{return [0,_a3[1],E([0,[0,_9Z],[0,_a0]]),_a1,E(_a5),E(_a6)];}}else{return new F(function(){return _7M(_a7,_a4,B(_9Y(_9Z,_a0,_a1,_a5)),_a6);});}}}else{return new F(function(){return _7M(_a7,_a4,B(_9Y(_9Z,_a0,_a1,_a5)),_a6);});}}else{return [0,1,E([0,[0,_9Z],[0,_a0]]),_a1,E(_7H),E(_7H)];}},_aa=function(_ab,_ac,_ad,_ae){var _af=E(_ae);if(!_af[0]){var _ag=_af[3],_ah=_af[4],_ai=_af[5],_aj=E(_af[2]),_ak=E(_aj[1])[1];if(_ab>=_ak){if(_ab!=_ak){return new F(function(){return _8t(_aj,_ag,_ah,B(_aa(_ab,_ac,_ad,_ai)));});}else{var _al=E(_ac),_am=_al[1],_an=E(_aj[2])[1];if(_am>=_an){if(_am!=_an){return new F(function(){return _8t(_aj,_ag,_ah,B(_9Y(_ab,_am,_ad,_ai)));});}else{return [0,_af[1],E([0,[0,_ab],_al]),_ad,E(_ah),E(_ai)];}}else{return new F(function(){return _7M(_aj,_ag,B(_9Y(_ab,_am,_ad,_ah)),_ai);});}}}else{return new F(function(){return _7M(_aj,_ag,B(_aa(_ab,_ac,_ad,_ah)),_ai);});}}else{return [0,1,E([0,[0,_ab],_ac]),_ad,E(_7H),E(_7H)];}},_ao=function(_ap,_aq,_ar,_as){var _at=E(_as);if(!_at[0]){var _au=_at[3],_av=_at[4],_aw=_at[5],_ax=E(_at[2]),_ay=E(_ap),_az=_ay[1],_aA=E(_ax[1])[1];if(_az>=_aA){if(_az!=_aA){return new F(function(){return _8t(_ax,_au,_av,B(_ao(_ay,_aq,_ar,_aw)));});}else{var _aB=E(_aq),_aC=_aB[1],_aD=E(_ax[2])[1];if(_aC>=_aD){if(_aC!=_aD){return new F(function(){return _8t(_ax,_au,_av,B(_ao(_ay,_aB,_ar,_aw)));});}else{return [0,_at[1],E([0,_ay,_aB]),_ar,E(_av),E(_aw)];}}else{return new F(function(){return _7M(_ax,_au,B(_ao(_ay,_aB,_ar,_av)),_aw);});}}}else{return new F(function(){return _7M(_ax,_au,B(_ao(_ay,_aq,_ar,_av)),_aw);});}}else{return [0,1,E([0,_ap,_aq]),_ar,E(_7H),E(_7H)];}},_aE=function(_aF,_aG){if(_aF<=0){if(_aF>=0){return new F(function(){return quot(_aF,_aG);});}else{if(_aG<=0){return new F(function(){return quot(_aF,_aG);});}else{return quot(_aF+1|0,_aG)-1|0;}}}else{if(_aG>=0){if(_aF>=0){return new F(function(){return quot(_aF,_aG);});}else{if(_aG<=0){return new F(function(){return quot(_aF,_aG);});}else{return quot(_aF+1|0,_aG)-1|0;}}}else{return quot(_aF-1|0,_aG)-1|0;}}},_aH=new T(function(){return B(unCStr("ArithException"));}),_aI=new T(function(){return B(unCStr("GHC.Exception"));}),_aJ=new T(function(){return B(unCStr("base"));}),_aK=new T(function(){var _aL=hs_wordToWord64(4194982440),_aM=_aL,_aN=hs_wordToWord64(3110813675),_aO=_aN;return [0,_aM,_aO,[0,_aM,_aO,_aJ,_aI,_aH],_d];}),_aP=function(_aQ){return E(_aK);},_aR=function(_aS){var _aT=E(_aS);return new F(function(){return _n(B(_l(_aT[1])),_aP,_aT[2]);});},_aU=new T(function(){return B(unCStr("arithmetic underflow"));}),_aV=new T(function(){return B(unCStr("arithmetic overflow"));}),_aW=new T(function(){return B(unCStr("Ratio has zero denominator"));}),_aX=new T(function(){return B(unCStr("denormal"));}),_aY=new T(function(){return B(unCStr("divide by zero"));}),_aZ=new T(function(){return B(unCStr("loss of precision"));}),_b0=function(_b1){switch(E(_b1)){case 0:return E(_aV);case 1:return E(_aU);case 2:return E(_aZ);case 3:return E(_aY);case 4:return E(_aX);default:return E(_aW);}},_b2=function(_b3){return new F(function(){return _D(_aU,_b3);});},_b4=function(_b3){return new F(function(){return _D(_aV,_b3);});},_b5=function(_b3){return new F(function(){return _D(_aW,_b3);});},_b6=function(_b3){return new F(function(){return _D(_aX,_b3);});},_b7=function(_b3){return new F(function(){return _D(_aY,_b3);});},_b8=function(_b3){return new F(function(){return _D(_aZ,_b3);});},_b9=function(_ba){switch(E(_ba)){case 0:return E(_b4);case 1:return E(_b2);case 2:return E(_b8);case 3:return E(_b7);case 4:return E(_b6);default:return E(_b5);}},_bb=function(_bc,_bd){return new F(function(){return _1t(_b9,_bc,_bd);});},_be=function(_bf,_bg){switch(E(_bg)){case 0:return E(_b4);case 1:return E(_b2);case 2:return E(_b8);case 3:return E(_b7);case 4:return E(_b6);default:return E(_b5);}},_bh=[0,_be,_b0,_bb],_bi=new T(function(){return [0,_aP,_bh,_bj,_aR];}),_bj=function(_b3){return [0,_bi,_b3];},_bk=3,_bl=new T(function(){return B(_bj(_bk));}),_bm=new T(function(){return die(_bl);}),_bn=0,_bo=new T(function(){return B(_bj(_bn));}),_bp=new T(function(){return die(_bo);}),_bq=function(_br,_bs){var _bt=E(_bs);switch(_bt){case -1:var _bu=E(_br);return _bu==(-2147483648)?E(_bp):B(_aE(_bu,-1));case 0:return E(_bm);default:return new F(function(){return _aE(_br,_bt);});}},_bv=function(_bw,_bx){return [0,_bx,new T(function(){var _by=B(_bv(_bw,new T(function(){return B(A(_bw,[_bx]));})));return [1,_by[1],_by[2]];})];},_bz=function(_bA,_bB){var _bC=E(_bA);if(!_bC){return [0];}else{var _bD=E(_bB);return _bD[0]==0?[0]:[1,_bD[1],new T(function(){return B(_bz(_bC-1|0,_bD[2]));})];}},_bE=function(_bF,_bG){return _bF<0?[0]:B(_bz(_bF,_bG));},_bH=function(_bI,_bJ,_bK,_bL){var _bM=_bK-_bI|0,_bN=_bL-_bJ|0,_bO=function(_bP){var _bQ=_bP-1|0;if(_bQ>0){return new F(function(){return _bE(_bQ,B(_bv(function(_bR){var _bS=E(_bR);return [0,[0,E(_bS[1])[1]+E(new T(function(){return [0,B(_bq(_bM,_bP))];}))[1]|0],[0,E(_bS[2])[1]+E(new T(function(){return [0,B(_bq(_bN,_bP))];}))[1]|0]];},[0,[0,_bI],[0,_bJ]]))[2]);});}else{return [0];}};if(_bM<0){var _bT= -_bM;if(_bN<0){var _bU= -_bN;return _bT>_bU?B(_bO(_bT)):B(_bO(_bU));}else{return _bT>_bN?B(_bO(_bT)):B(_bO(_bN));}}else{if(_bN<0){var _bV= -_bN;return _bM>_bV?B(_bO(_bM)):B(_bO(_bV));}else{return _bM>_bN?B(_bO(_bM)):B(_bO(_bN));}}},_bW=function(_bX,_bY,_bZ){while(1){var _c0=E(_bZ);if(!_c0[0]){var _c1=_c0[4],_c2=_c0[5],_c3=E(_c0[2]),_c4=E(_c3[1])[1];if(_bX>=_c4){if(_bX!=_c4){_bZ=_c2;continue;}else{var _c5=E(_c3[2])[1];if(_bY>=_c5){if(_bY!=_c5){_bZ=_c2;continue;}else{return [1,_c0[3]];}}else{_bZ=_c1;continue;}}}else{_bZ=_c1;continue;}}else{return [0];}}},_c6=function(_c7,_c8,_c9){while(1){var _ca=E(_c9);if(!_ca[0]){var _cb=_ca[4],_cc=_ca[5],_cd=E(_ca[2]),_ce=E(_cd[1])[1];if(_c7>=_ce){if(_c7!=_ce){_c9=_cc;continue;}else{var _cf=E(_c8)[1],_cg=E(_cd[2])[1];if(_cf>=_cg){return _cf!=_cg?B(_bW(_c7,_cf,_cc)):[1,_ca[3]];}else{return new F(function(){return _bW(_c7,_cf,_cb);});}}}else{_c9=_cb;continue;}}else{return [0];}}},_ch=function(_ci,_cj,_ck){var _cl=E(_ck);if(!_cl[0]){var _cm=_cl[4],_cn=_cl[5],_co=E(_cl[2]),_cp=E(_ci)[1],_cq=E(_co[1])[1];if(_cp>=_cq){if(_cp!=_cq){return new F(function(){return _c6(_cp,_cj,_cn);});}else{var _cr=E(_cj)[1],_cs=E(_co[2])[1];if(_cr>=_cs){return _cr!=_cs?B(_bW(_cp,_cr,_cn)):[1,_cl[3]];}else{return new F(function(){return _bW(_cp,_cr,_cm);});}}}else{return new F(function(){return _c6(_cp,_cj,_cm);});}}else{return [0];}},_ct=new T(function(){return B(unCStr("Map.!: given key is not an element in the map"));}),_cu=new T(function(){return B(err(_ct));}),_cv=function(_cw,_cx,_cy){while(1){var _cz=E(_cy);if(!_cz[0]){var _cA=_cz[4],_cB=_cz[5],_cC=E(_cz[2]),_cD=E(_cC[1])[1];if(_cw>=_cD){if(_cw!=_cD){_cy=_cB;continue;}else{var _cE=E(_cC[2])[1];if(_cx>=_cE){if(_cx!=_cE){_cy=_cB;continue;}else{return E(_cz[3]);}}else{_cy=_cA;continue;}}}else{_cy=_cA;continue;}}else{return E(_cu);}}},_cF=function(_cG,_cH,_cI){while(1){var _cJ=E(_cI);if(!_cJ[0]){var _cK=_cJ[4],_cL=_cJ[5],_cM=E(_cJ[2]),_cN=E(_cM[1])[1];if(_cG>=_cN){if(_cG!=_cN){_cI=_cL;continue;}else{var _cO=E(_cH)[1],_cP=E(_cM[2])[1];if(_cO>=_cP){return _cO!=_cP?B(_cv(_cG,_cO,_cL)):E(_cJ[3]);}else{return new F(function(){return _cv(_cG,_cO,_cK);});}}}else{_cI=_cK;continue;}}else{return E(_cu);}}},_cQ=function(_cR,_cS,_cT){var _cU=E(_cT);if(!_cU[0]){return [0];}else{var _cV=_cU[1],_cW=_cU[2];return !B(A(_cR,[_cS,_cV]))?[1,_cV,new T(function(){return B(_cQ(_cR,_cS,_cW));})]:E(_cW);}},_cX=new T(function(){return B(unCStr("trying to flip something that is not a marker (invalid ring move?)"));}),_cY=new T(function(){return B(err(_cX));}),_cZ=function(_d0,_d1){var _d2=E(_d0),_d3=E(_d1);return E(_d2[1])[1]!=E(_d3[1])[1]?false:B(_7z(_d2[2],_d3[2]));},_d4=0,_d5=[1,_d4],_d6=1,_d7=[1,_d6],_d8=function(_d9,_da,_db,_dc,_dd,_de){while(1){var _df=(function(_dg,_dh,_di,_dj,_dk,_dl){var _dm=E(_dl);if(!_dm[0]){return [0,_dg,_dh,_di,_dj,_dk];}else{var _dn=_dm[2],_do=E(_dm[1]),_dp=_do[1],_dq=_do[2],_dr=B(_ch(_dp,_dq,_dg));if(!_dr[0]){var _ds=_dg,_dt=_dh,_du=_di,_dv=_dj,_dw=_dk;_de=_dn;_d9=_ds;_da=_dt;_db=_du;_dc=_dv;_dd=_dw;return null;}else{var _dx=E(_dr[1]);if(!_dx[0]){return E(_cY);}else{if(!E(_dx[1])){var _dy=E(_dp)[1],_dz=B(_cF(_dy,_dq,_dg));if(!_dz[0]){if(!E(_dz[1])){_d9=new T(function(){return B(_aa(_dy,_dq,_d7,B(_9M(_dy,_dq,_dg))));});_da=new T(function(){return B(_cQ(_cZ,[0,[0,_dy],_dq],_dh));});var _du=_di,_dv=_dj,_dw=[1,_do,_dk];_de=_dn;_db=_du;_dc=_dv;_dd=_dw;return null;}else{_d9=new T(function(){return B(_aa(_dy,_dq,_d7,B(_9M(_dy,_dq,_dg))));});var _dt=_dh;_db=new T(function(){return B(_cQ(_cZ,[0,[0,_dy],_dq],_di));});var _dv=_dj,_dw=[1,_do,_dk];_de=_dn;_da=_dt;_dc=_dv;_dd=_dw;return null;}}else{if(!E(_dz[1])){_d9=new T(function(){return B(_aa(_dy,_dq,_d7,B(_9M(_dy,_dq,_dg))));});var _dt=_dh,_du=_di;_dc=new T(function(){return B(_cQ(_cZ,[0,[0,_dy],_dq],_dj));});var _dw=[1,_do,_dk];_de=_dn;_da=_dt;_db=_du;_dd=_dw;return null;}else{_d9=new T(function(){return B(_aa(_dy,_dq,_d7,B(_9M(_dy,_dq,_dg))));});var _dt=_dh,_du=_di,_dv=_dj;_dd=[1,_do,new T(function(){return B(_cQ(_cZ,[0,[0,_dy],_dq],_dk));})];_de=_dn;_da=_dt;_db=_du;_dc=_dv;return null;}}}else{var _dA=E(_dp)[1],_dB=B(_cF(_dA,_dq,_dg));if(!_dB[0]){if(!E(_dB[1])){_d9=new T(function(){return B(_aa(_dA,_dq,_d5,B(_9M(_dA,_dq,_dg))));});_da=new T(function(){return B(_cQ(_cZ,[0,[0,_dA],_dq],_dh));});var _du=_di,_dv=[1,_do,_dj],_dw=_dk;_de=_dn;_db=_du;_dc=_dv;_dd=_dw;return null;}else{_d9=new T(function(){return B(_aa(_dA,_dq,_d5,B(_9M(_dA,_dq,_dg))));});var _dt=_dh;_db=new T(function(){return B(_cQ(_cZ,[0,[0,_dA],_dq],_di));});var _dv=[1,_do,_dj],_dw=_dk;_de=_dn;_da=_dt;_dc=_dv;_dd=_dw;return null;}}else{if(!E(_dB[1])){_d9=new T(function(){return B(_aa(_dA,_dq,_d5,B(_9M(_dA,_dq,_dg))));});var _dt=_dh,_du=_di;_dc=[1,_do,new T(function(){return B(_cQ(_cZ,[0,[0,_dA],_dq],_dj));})];var _dw=_dk;_de=_dn;_da=_dt;_db=_du;_dd=_dw;return null;}else{_d9=new T(function(){return B(_aa(_dA,_dq,_d5,B(_9M(_dA,_dq,_dg))));});var _dt=_dh,_du=_di,_dv=[1,_do,_dj];_dd=new T(function(){return B(_cQ(_cZ,[0,[0,_dA],_dq],_dk));});_de=_dn;_da=_dt;_db=_du;_dc=_dv;return null;}}}}}}})(_d9,_da,_db,_dc,_dd,_de);if(_df!=null){return _df;}}},_dC=function(_dD,_dE,_dF,_dG,_dH,_dI){while(1){var _dJ=(function(_dK,_dL,_dM,_dN,_dO,_dP){var _dQ=E(_dP);if(!_dQ[0]){return [0,_dK,_dL,_dM,_dN,_dO];}else{var _dR=_dQ[2],_dS=E(_dQ[1]),_dT=_dS[2],_dU=E(_dS[1])[1],_dV=B(_cF(_dU,_dT,_dK));if(!_dV[0]){if(!E(_dV[1])){_dD=new T(function(){return B(_9M(_dU,_dT,_dK));});_dE=new T(function(){return B(_cQ(_cZ,[0,[0,_dU],_dT],_dL));});var _dW=_dM,_dX=_dN,_dY=_dO;_dI=_dR;_dF=_dW;_dG=_dX;_dH=_dY;return null;}else{_dD=new T(function(){return B(_9M(_dU,_dT,_dK));});var _dZ=_dL;_dF=new T(function(){return B(_cQ(_cZ,[0,[0,_dU],_dT],_dM));});var _dX=_dN,_dY=_dO;_dI=_dR;_dE=_dZ;_dG=_dX;_dH=_dY;return null;}}else{if(!E(_dV[1])){_dD=new T(function(){return B(_9M(_dU,_dT,_dK));});var _dZ=_dL,_dW=_dM;_dG=new T(function(){return B(_cQ(_cZ,[0,[0,_dU],_dT],_dN));});var _dY=_dO;_dI=_dR;_dE=_dZ;_dF=_dW;_dH=_dY;return null;}else{_dD=new T(function(){return B(_9M(_dU,_dT,_dK));});var _dZ=_dL,_dW=_dM,_dX=_dN;_dH=new T(function(){return B(_cQ(_cZ,[0,[0,_dU],_dT],_dO));});_dI=_dR;_dE=_dZ;_dF=_dW;_dG=_dX;return null;}}}})(_dD,_dE,_dF,_dG,_dH,_dI);if(_dJ!=null){return _dJ;}}},_e0=function(_e1,_e2,_e3){while(1){var _e4=E(_e3);if(!_e4[0]){var _e5=_e4[4],_e6=_e4[5],_e7=E(_e4[2]),_e8=E(_e7[1])[1];if(_e1>=_e8){if(_e1!=_e8){_e3=_e6;continue;}else{var _e9=E(_e7[2])[1];if(_e2>=_e9){if(_e2!=_e9){_e3=_e6;continue;}else{return true;}}else{_e3=_e5;continue;}}}else{_e3=_e5;continue;}}else{return false;}}},_ea=function(_eb,_ec,_ed){while(1){var _ee=E(_ed);if(!_ee[0]){var _ef=_ee[4],_eg=_ee[5],_eh=E(_ee[2]),_ei=E(_eh[1])[1];if(_eb>=_ei){if(_eb!=_ei){_ed=_eg;continue;}else{var _ej=E(_ec)[1],_ek=E(_eh[2])[1];if(_ej>=_ek){return _ej!=_ek?B(_e0(_eb,_ej,_eg)):true;}else{return new F(function(){return _e0(_eb,_ej,_ef);});}}}else{_ed=_ef;continue;}}else{return false;}}},_el=function(_em,_en,_eo){var _ep=E(_eo);if(!_ep[0]){var _eq=_ep[4],_er=_ep[5],_es=E(_ep[2]),_et=E(_em)[1],_eu=E(_es[1])[1];if(_et>=_eu){if(_et!=_eu){return new F(function(){return _ea(_et,_en,_er);});}else{var _ev=E(_en)[1],_ew=E(_es[2])[1];if(_ev>=_ew){return _ev!=_ew?B(_e0(_et,_ev,_er)):true;}else{return new F(function(){return _e0(_et,_ev,_eq);});}}}else{return new F(function(){return _ea(_et,_en,_eq);});}}else{return false;}},_ex=[1],_ey=function(_ez,_eA){while(1){var _eB=E(_eA);if(!_eB[0]){return false;}else{if(!B(A(_ez,[_eB[1]]))){_eA=_eB[2];continue;}else{return true;}}}},_eC=function(_eD,_eE,_eF){while(1){var _eG=E(_eF);if(!_eG[0]){return false;}else{if(!B(A(_70,[_eD,_eE,_eG[1]]))){_eF=_eG[2];continue;}else{return true;}}}},_eH=5,_eI=0,_eJ=1,_eK=[1,_eJ,_d],_eL=[1,_eI,_eK],_eM=[1,_eH,_eL],_eN=function(_eO,_eP){return new F(function(){return _bv(function(_eQ){switch(E(_eP)){case 0:var _eR=E(_eQ);return [0,E(_eR[1]),[0,1+E(_eR[2])[1]|0]];case 1:var _eS=E(_eQ);return [0,[0,1+E(_eS[1])[1]|0],[0,1+E(_eS[2])[1]|0]];case 2:var _eT=E(_eQ);return [0,[0,1+E(_eT[1])[1]|0],E(_eT[2])];case 3:var _eU=E(_eQ);return [0,E(_eU[1]),[0,-1+E(_eU[2])[1]|0]];case 4:var _eV=E(_eQ);return [0,[0,-1+E(_eV[1])[1]|0],[0,-1+E(_eV[2])[1]|0]];default:var _eW=E(_eQ);return [0,[0,-1+E(_eW[1])[1]|0],E(_eW[2])];}},_eO);});},_eX=true,_eY=function(_eZ,_f0,_f1,_f2,_f3){var _f4=function(_f5){return function(_f6){return !E(new T(function(){var _f7=E(_f5),_f8=B(_ch(_f7[1],_f7[2],_eZ));if(!_f8[0]){var _f9=false;}else{var _fa=E(_f8[1]),_f9=_fa[0]==0?false:E(_fa[1])==0?E(_f2)==0?true:false:E(_f2)==0?false:true;}var _fb=_f9;return _fb;}))?false:E(_f6);};},_fc=function(_fd,_fe){var _ff=E(_fd);if(!_ff[0]){return true;}else{var _fg=_ff[1];if(_fe>1){return new F(function(){return A(_f4,[_fg,new T(function(){return B(_fc(_ff[2],_fe-1|0));})]);});}else{return new F(function(){return A(_f4,[_fg,_eX]);});}}},_fh=function(_fi){var _fj=new T(function(){return B(_fc(B(_eN(_fi,new T(function(){switch(E(_f3)){case 0:var _fk=3;break;case 1:var _fk=4;break;case 2:var _fk=5;break;case 3:var _fk=0;break;case 4:var _fk=1;break;default:var _fk=2;}return _fk;})))[2],2));}),_fl=function(_fm,_fn){var _fo=E(_fm);if(!_fo[0]){return E(_fj);}else{var _fp=_fo[1];if(_fn>1){return new F(function(){return A(_f4,[_fp,new T(function(){return B(_fl(_fo[2],_fn-1|0));})]);});}else{return new F(function(){return A(_f4,[_fp,_fj]);});}}};return new F(function(){return _fl(B(_eN(_fi,_f3))[2],2);});};return E(_f2)==0?B(_ey(_fh,_f0)):B(_ey(_fh,_f1));},_fq=function(_fr,_fs,_ft){var _fu=E(_fr);return new F(function(){return _eY(_fu[1],_fu[4],_fu[5],_fs,_ft);});},_fv=function(_fw,_fx){var _fy=B(_eN(_fw,_fx));return [1,_fy[1],_fy[2]];},_fz=new T(function(){return B(unCStr("tail"));}),_fA=new T(function(){return B(_2D(_fz));}),_fB=function(_fC,_fD){var _fE=E(_fD);if(!_fE[0]){return [0];}else{var _fF=_fE[1];return !B(A(_fC,[_fF]))?[0]:[1,_fF,new T(function(){return B(_fB(_fC,_fE[2]));})];}},_fG=function(_fH,_fI){var _fJ=E(_fH);return _fJ[0]==0?E(_fI):[1,_fJ[1],new T(function(){return B(_fG(_fI,_fJ[2]));})];},_fK=function(_fL,_fM,_fN){return !B(_eC(_7G,_fM,_fL))?[0]:B(_bz(5,new T(function(){var _fO=function(_fP){return new F(function(){return _eC(_7G,_fP,_fL);});};return B(_fG(B(_fB(_fO,B(_fv(_fM,_fN)))),new T(function(){var _fQ=B(_eN(_fM,new T(function(){switch(E(_fN)){case 0:var _fR=3;break;case 1:var _fR=4;break;case 2:var _fR=5;break;case 3:var _fR=0;break;case 4:var _fR=1;break;default:var _fR=2;}return _fR;}))),_fS=B(_fB(_fO,[1,_fQ[1],_fQ[2]]));return _fS[0]==0?E(_fA):E(_fS[2]);})));})));},_fT=function(_fU,_fV){return new F(function(){return _ey(function(_fW){return B(_6V(B(_fK(_fU,_fV,_fW)),0))==5?true:false;},_eM);});},_fX=[0,41],_fY=[1,_fX,_d],_fZ=new T(function(){return B(_2v(0,5,_fY));}),_g0=new T(function(){return B(unAppCStr(") is outside of enumeration\'s range (0,",_fZ));}),_g1=function(_g2){return new F(function(){return err(B(unAppCStr("toEnum{Direction}: tag (",new T(function(){return B(_2v(0,_g2,_g0));}))));});},_g3=function(_g4){return [0,new T(function(){if(_g4<0){var _g5=B(_g1(_g4));}else{var _g5=_g4>5?B(_g1(_g4)):_g4;}var _g6=_g5;return _g6;}),new T(function(){var _g7=E(_g4);if(_g7==5){var _g8=[0];}else{var _g9=B(_g3(_g7+1|0)),_g8=[1,_g9[1],_g9[2]];}return _g8;})];},_ga=new T(function(){var _gb=B(_g3(0));return [1,_gb[1],_gb[2]];}),_gc=function(_gd,_ge){var _gf=E(_ge);if(!_gf[0]){return [0,_d,_d];}else{var _gg=_gf[1];if(!B(A(_gd,[_gg]))){return [0,_d,_gf];}else{var _gh=new T(function(){var _gi=B(_gc(_gd,_gf[2]));return [0,_gi[1],_gi[2]];});return [0,[1,_gg,new T(function(){return E(E(_gh)[1]);})],new T(function(){return E(E(_gh)[2]);})];}}},_gj=function(_gk,_gl){var _gm=E(_gl);return !B(_el(_gm[1],_gm[2],E(_gk)[1]))?true:false;},_gn=function(_go){var _gp=E(_go),_gq=E(_gp[1])[1];return Math.pow(0.5*Math.sqrt(3)*_gq,2)+Math.pow(0.5*_gq-E(_gp[2])[1],2)<=Math.pow(4.6,2);},_gr=function(_gs,_gt,_gu){var _gv=B(_fB(_gn,B(_fv(_gt,_gu))));if(!_gv[0]){return E(_fA);}else{var _gw=B(_gc(function(_2j){return new F(function(){return _gj(_gs,_2j);});},_gv[2]));return new F(function(){return _D(_gw[1],new T(function(){var _gx=E(_gw[2]);if(!_gx[0]){var _gy=[0];}else{var _gz=E(_gs)[1],_gA=E(_gx[1]),_gB=B(_ch(_gA[1],_gA[2],_gz));if(!_gB[0]){var _gC=[1,_gA,_d];}else{if(!E(_gB[1])[0]){var _gD=[0];}else{var _gD=B((function(_gE){while(1){var _gF=E(_gE);if(!_gF[0]){return [0];}else{var _gG=E(_gF[1]),_gH=B(_ch(_gG[1],_gG[2],_gz));if(!_gH[0]){return [1,_gG,_d];}else{if(!E(_gH[1])[0]){return [0];}else{_gE=_gF[2];continue;}}}}})(_gx[2]));}var _gC=_gD;}var _gI=_gC,_gJ=_gI,_gy=_gJ;}return _gy;}));});}},_gK=function(_gL,_gM){var _gN=function(_gO){while(1){var _gP=(function(_gQ){var _gR=E(_gQ);if(!_gR[0]){return [0];}else{var _gS=_gR[2],_gT=B(_gr(_gL,_gM,_gR[1]));if(!_gT[0]){_gO=_gS;return null;}else{return [1,_gT[1],new T(function(){return B(_D(_gT[2],new T(function(){return B(_gN(_gS));})));})];}}})(_gO);if(_gP!=null){return _gP;}}};return new F(function(){return _gN(_ga);});},_gU=function(_gV,_gW){return E(_gV)==0?E(E(_gW)[2]):E(E(_gW)[3]);},_gX=function(_gY,_gZ){var _h0=function(_h1){while(1){var _h2=(function(_h3){var _h4=E(_h3);if(!_h4[0]){return [0];}else{var _h5=_h4[2],_h6=B(_fK(_gY,_gZ,_h4[1]));if(B(_6V(_h6,0))==5){return [1,_h6,new T(function(){return B(_h0(_h5));})];}else{_h1=_h5;return null;}}})(_h1);if(_h2!=null){return _h2;}}},_h7=B((function(_h8,_h9){var _ha=B(_fK(_gY,_gZ,_h8));return B(_6V(_ha,0))==5?[1,_ha,new T(function(){return B(_h0(_h9));})]:B(_h0(_h9));})(_eH,_eL));return _h7[0]==0?[0]:E(_h7[1]);},_hb=[0,_d6],_hc=[0,_d4],_hd=function(_he,_hf,_hg,_hh,_hi,_hj){var _hk=E(_hf);switch(_hk[0]){case 0:var _hl=E(_hg),_hm=_hl[1],_hn=_hl[2],_ho=_hl[3],_hp=_hl[4],_hq=_hl[5],_hr=E(_hj),_hs=_hr[1],_ht=_hr[2];return !B(_el(_hs,_ht,_hm))?[1,[0,new T(function(){return E(_he)==0?1:0;}),new T(function(){return (B(_6V(_hn,0))+B(_6V(_ho,0))|0)>=9?[1]:[0];}),new T(function(){return E(_he)==0?[0,new T(function(){return B(_ao(_hs,_ht,_hc,_hm));}),[1,_hr,_hn],_ho,_hp,_hq]:[0,new T(function(){return B(_ao(_hs,_ht,_hb,_hm));}),_hn,[1,_hr,_ho],_hp,_hq];}),_hh,_hi]]:[0];case 1:return !B(_eC(_7G,_hj,B(_gU(_he,_hg))))?[0]:[1,[0,_he,[2,_hj],new T(function(){if(!E(_he)){var _hu=E(_hg),_hv=_hu[1],_hw=_hu[2],_hx=_hu[3],_hy=_hu[4],_hz=_hu[5],_hA=E(_hj),_hB=_hA[2],_hC=E(_hA[1])[1],_hD=B(_cF(_hC,_hB,_hv)),_hE=_hD[0]==0?E(_hD[1])==0?[0,new T(function(){return B(_aa(_hC,_hB,_d5,B(_9M(_hC,_hB,_hv))));}),new T(function(){return B(_cQ(_cZ,[0,[0,_hC],_hB],_hw));}),_hx,[1,_hA,_hy],_hz]:[0,new T(function(){return B(_aa(_hC,_hB,_d5,B(_9M(_hC,_hB,_hv))));}),_hw,new T(function(){return B(_cQ(_cZ,[0,[0,_hC],_hB],_hx));}),[1,_hA,_hy],_hz]:E(_hD[1])==0?[0,new T(function(){return B(_aa(_hC,_hB,_d5,B(_9M(_hC,_hB,_hv))));}),_hw,_hx,[1,_hA,new T(function(){return B(_cQ(_cZ,[0,[0,_hC],_hB],_hy));})],_hz]:[0,new T(function(){return B(_aa(_hC,_hB,_d5,B(_9M(_hC,_hB,_hv))));}),_hw,_hx,[1,_hA,_hy],new T(function(){return B(_cQ(_cZ,[0,[0,_hC],_hB],_hz));})];}else{var _hF=E(_hg),_hG=_hF[1],_hH=_hF[2],_hI=_hF[3],_hJ=_hF[4],_hK=_hF[5],_hL=E(_hj),_hM=_hL[2],_hN=E(_hL[1])[1],_hO=B(_cF(_hN,_hM,_hG)),_hE=_hO[0]==0?E(_hO[1])==0?[0,new T(function(){return B(_aa(_hN,_hM,_d7,B(_9M(_hN,_hM,_hG))));}),new T(function(){return B(_cQ(_cZ,[0,[0,_hN],_hM],_hH));}),_hI,_hJ,[1,_hL,_hK]]:[0,new T(function(){return B(_aa(_hN,_hM,_d7,B(_9M(_hN,_hM,_hG))));}),_hH,new T(function(){return B(_cQ(_cZ,[0,[0,_hN],_hM],_hI));}),_hJ,[1,_hL,_hK]]:E(_hO[1])==0?[0,new T(function(){return B(_aa(_hN,_hM,_d7,B(_9M(_hN,_hM,_hG))));}),_hH,_hI,new T(function(){return B(_cQ(_cZ,[0,[0,_hN],_hM],_hJ));}),[1,_hL,_hK]]:[0,new T(function(){return B(_aa(_hN,_hM,_d7,B(_9M(_hN,_hM,_hG))));}),_hH,_hI,_hJ,[1,_hL,new T(function(){return B(_cQ(_cZ,[0,[0,_hN],_hM],_hK));})]];}return _hE;}),_hh,_hi]];case 2:var _hP=_hk[1];if(!B(_eC(_7G,_hj,B(_gK(_hg,_hP))))){return [0];}else{var _hQ=new T(function(){var _hR=E(_hg),_hS=E(_hP),_hT=E(_hj),_hU=B(_d8(_hR[1],_hR[2],_hR[3],_hR[4],_hR[5],B(_bH(E(_hS[1])[1],E(_hS[2])[1],E(_hT[1])[1],E(_hT[2])[1]))));return [0,_hU[1],_hU[2],_hU[3],_hU[4],_hU[5]];}),_hV=new T(function(){return E(_he)==0?1:0;});return [1,[0,_hV,new T(function(){if(!B(_ey(function(_2j){return new F(function(){return _fq(_hQ,_he,_2j);});},_eM))){var _hW=!B(_ey(function(_2j){return new F(function(){return _fq(_hQ,_hV,_2j);});},_eM))?[1]:[3,_he];}else{var _hW=[5,_he];}return _hW;}),new T(function(){if(!E(_he)){var _hX=E(_hQ),_hY=[0,new T(function(){var _hZ=E(_hj);return B(_ao(_hZ[1],_hZ[2],_hc,_hX[1]));}),[1,_hj,_hX[2]],_hX[3],_hX[4],_hX[5]];}else{var _i0=E(_hQ),_hY=[0,new T(function(){var _i1=E(_hj);return B(_ao(_i1[1],_i1[2],_hb,_i0[1]));}),_i0[2],[1,_hj,_i0[3]],_i0[4],_i0[5]];}return _hY;}),_hh,_hi]];}break;case 3:var _i2=new T(function(){if(!E(_he)){var _i3=E(E(_hg)[4]);}else{var _i3=E(E(_hg)[5]);}return _i3;});return !B(_fT(_i2,_hj))?[0]:[1,[0,_he,[4,_hk[1]],new T(function(){var _i4=E(_hg),_i5=B(_dC(_i4[1],_i4[2],_i4[3],_i4[4],_i4[5],B(_gX(_i2,_hj))));return [0,_i5[1],_i5[2],_i5[3],_i5[4],_i5[5]];}),_hh,_hi]];case 4:var _i6=_hk[1];if(!B(_eC(_7G,_hj,B(_gU(_he,_hg))))){return [0];}else{var _i7=new T(function(){var _i8=E(_hg),_i9=_i8[1],_ia=_i8[2],_ib=_i8[3],_ic=_i8[4],_id=_i8[5],_ie=E(_hj),_if=_ie[2],_ig=E(_ie[1])[1],_ih=B(_cF(_ig,_if,_i9));return _ih[0]==0?E(_ih[1])==0?[0,new T(function(){return B(_9M(_ig,_if,_i9));}),new T(function(){return B(_cQ(_cZ,[0,[0,_ig],_if],_ia));}),_ib,_ic,_id]:[0,new T(function(){return B(_9M(_ig,_if,_i9));}),_ia,new T(function(){return B(_cQ(_cZ,[0,[0,_ig],_if],_ib));}),_ic,_id]:E(_ih[1])==0?[0,new T(function(){return B(_9M(_ig,_if,_i9));}),_ia,_ib,new T(function(){return B(_cQ(_cZ,[0,[0,_ig],_if],_ic));}),_id]:[0,new T(function(){return B(_9M(_ig,_if,_i9));}),_ia,_ib,_ic,new T(function(){return B(_cQ(_cZ,[0,[0,_ig],_if],_id));})];}),_ii=new T(function(){return E(_he)==0?1:0;});return [1,[0,_ii,new T(function(){if(!B(_ey(function(_2j){return new F(function(){return _fq(_i7,_he,_2j);});},_eM))){var _ij=!B(_ey(function(_2j){return new F(function(){return _fq(_i7,_ii,_2j);});},_eM))?E(_i6)==0?E(_he)==0?[1]:[6]:E(_he)==0?[6]:[1]:[3,_i6];}else{var _ij=[5,_i6];}return _ij;}),_i7,new T(function(){if(!E(_he)){var _ik=[0,E(_hh)[1]+1|0];}else{var _ik=E(_hh);}return _ik;}),new T(function(){if(!E(_he)){var _il=E(_hi);}else{var _il=[0,E(_hi)[1]+1|0];}return _il;})]];}break;case 5:return [1,[0,new T(function(){return E(_he)==0?1:0;}),[3,_hk[1]],_hg,_hh,_hi]];default:return [1,[0,new T(function(){return E(_he)==0?1:0;}),_ex,_hg,_hh,_hi]];}},_im=new T(function(){return B(unCStr("POST"));}),_in=new T(function(){return B(unCStr("GET"));}),_io=[0,38],_ip=[1,_io,_d],_iq=new T(function(){return [0,toJSStr(_ip)];}),_ir=[0,61],_is=[1,_ir,_d],_it=new T(function(){return [0,toJSStr(_is)];}),_iu=function(_iv,_iw){var _ix=jsCat([1,_iv,[1,_iw,_d]],E(_it)[1]),_iy=_ix;return E(_iy);},_iz=function(_iA){var _iB=E(_iA);return [0,B(_iu(_iB[1],_iB[2]))];},_iC=function(_iD){var _iE=jsCat(new T(function(){return B(_6m(_iz,_iD));}),E(_iq)[1]),_iF=_iE;return E(_iF);},_iG=0,_iH=function(_iI){return new F(function(){return fromJSStr(E(_iI)[1]);});},_iJ=function(_iK){return [0,toJSStr(E(_iK))];},_iL=function(_iM){var _iN=E(_iM);return [0,new T(function(){return B(_iJ(_iN[1]));}),new T(function(){return B(_iJ(_iN[2]));})];},_iO=[0,63],_iP=[1,_iO,_d],_iQ=new T(function(){return [0,toJSStr(_iP)];}),_iR=new T(function(){return [0,toJSStr(_d)];}),_iS=function(_iT,_iU,_iV,_iW,_iX,_iY,_iZ,_j0,_j1){return new F(function(){return A(_iT,[new T(function(){return B(A(_iX,[function(_){var _j2=function(_j3){var _j4=function(_j5){var _j6=ajaxReq(toJSStr(_j3),_j5,1,E(_iR)[1],function(_j7){return new F(function(){return A(_j1,[new T(function(){var _j8=E(_j7);return _j8[0]==0?[0]:[1,new T(function(){return B(_iH(_j8[1]));})];})]);});});return _iG;};if(!E(_j0)[0]){return new F(function(){return _j4(toJSStr(E(_iZ)));});}else{var _j9=jsCat([1,new T(function(){return B(_iJ(_iZ));}),[1,new T(function(){return [0,B(_iC(B(_6m(_iL,_j0))))];}),_d]],E(_iQ)[1]),_ja=_j9;return new F(function(){return _j4(_ja);});}};if(!E(_iY)){return new F(function(){return _j2(E(_in));});}else{return new F(function(){return _j2(E(_im));});}}]));}),function(_jb){return E(new T(function(){return B(A(_iV,[_iG]));}));}]);});},_jc=0,_jd=[2],_je=[1],_jf=new T(function(){return B(unCStr("Pattern match failure in do expression at src/frontend.hs:257:9-18"));}),_jg=new T(function(){return B(unCStr("Pattern match failure in do expression at src/frontend.hs:290:9-26"));}),_jh=function(_ji,_jj,_){var _jk=B(A(_ji,[_])),_jl=_jk;return new F(function(){return A(_jj,[_jl,_]);});},_jm=function(_jn){var _jo=E(_jn);return new F(function(){return _6B(_jo[1],_jo[2]);});},_jp=[0,_7H,_d,_d,_d,_d],_jq=new T(function(){return [0,"keydown"];}),_jr=new T(function(){return [0,"mousemove"];}),_js=new T(function(){return [0,"click"];}),_jt=[0,10],_ju=[1,_jt,_d],_jv=function(_jw,_jx,_){var _jy=jsWriteHandle(E(_jw)[1],toJSStr(E(_jx)));return _iG;},_jz=function(_jA,_jB,_){var _jC=E(_jA),_jD=jsWriteHandle(_jC[1],toJSStr(E(_jB)));return new F(function(){return _jv(_jC,_ju,_);});},_jE=function(_jF){return E(_jF);},_jG=function(_jH){return new F(function(){return _jE(_jH);});},_jI=new T(function(){return B(unCStr("https://david-peter.de:8000/"));}),_jJ=new T(function(){return B(unCStr("gamestate"));}),_jK=new T(function(){return B(unCStr("Pattern match failure in do expression at src/frontend.hs:241:5-12"));}),_jL=new T(function(){return B(unCStr("Pattern match failure in do expression at src/frontend.hs:242:5-11"));}),_jM=[0,1],_jN=[3,_jM],_jO=new T(function(){return B(unCStr("Prelude.read: no parse"));}),_jP=new T(function(){return B(err(_jO));}),_jQ=new T(function(){return B(unCStr("Prelude.read: ambiguous parse"));}),_jR=new T(function(){return B(err(_jQ));}),_jS=function(_jT){return E(E(_jT)[3]);},_jU=function(_jV,_jW,_jX,_jY,_jZ,_k0){switch(B(A(_jV,[_jX,_jZ]))){case 0:return true;case 1:return new F(function(){return A(_jS,[_jW,_jY,_k0]);});break;default:return false;}},_k1=function(_k2,_k3,_k4,_k5,_k6){var _k7=E(_k5),_k8=E(_k6);return new F(function(){return _jU(E(_k3)[2],_k4,_k7[1],_k7[2],_k8[1],_k8[2]);});},_k9=function(_ka){return E(E(_ka)[6]);},_kb=function(_kc,_kd,_ke,_kf,_kg,_kh){switch(B(A(_kc,[_ke,_kg]))){case 0:return true;case 1:return new F(function(){return A(_k9,[_kd,_kf,_kh]);});break;default:return false;}},_ki=function(_kj,_kk,_kl,_km,_kn){var _ko=E(_km),_kp=E(_kn);return new F(function(){return _kb(E(_kk)[2],_kl,_ko[1],_ko[2],_kp[1],_kp[2]);});},_kq=function(_kr){return E(E(_kr)[5]);},_ks=function(_kt,_ku,_kv,_kw,_kx,_ky){switch(B(A(_kt,[_kv,_kx]))){case 0:return false;case 1:return new F(function(){return A(_kq,[_ku,_kw,_ky]);});break;default:return true;}},_kz=function(_kA,_kB,_kC,_kD,_kE){var _kF=E(_kD),_kG=E(_kE);return new F(function(){return _ks(E(_kB)[2],_kC,_kF[1],_kF[2],_kG[1],_kG[2]);});},_kH=function(_kI){return E(E(_kI)[4]);},_kJ=function(_kK,_kL,_kM,_kN,_kO,_kP){switch(B(A(_kK,[_kM,_kO]))){case 0:return false;case 1:return new F(function(){return A(_kH,[_kL,_kN,_kP]);});break;default:return true;}},_kQ=function(_kR,_kS,_kT,_kU,_kV){var _kW=E(_kU),_kX=E(_kV);return new F(function(){return _kJ(E(_kS)[2],_kT,_kW[1],_kW[2],_kX[1],_kX[2]);});},_kY=function(_kZ){return E(E(_kZ)[2]);},_l0=function(_l1,_l2,_l3,_l4,_l5,_l6){switch(B(A(_l1,[_l3,_l5]))){case 0:return 0;case 1:return new F(function(){return A(_kY,[_l2,_l4,_l6]);});break;default:return 2;}},_l7=function(_l8,_l9,_la,_lb,_lc){var _ld=E(_lb),_le=E(_lc);return new F(function(){return _l0(E(_l9)[2],_la,_ld[1],_ld[2],_le[1],_le[2]);});},_lf=function(_lg,_lh,_li,_lj,_lk){var _ll=E(_lj),_lm=_ll[1],_ln=_ll[2],_lo=E(_lk),_lp=_lo[1],_lq=_lo[2];switch(B(A(E(_lh)[2],[_lm,_lp]))){case 0:return [0,_lp,_lq];case 1:return !B(A(_k9,[_li,_ln,_lq]))?[0,_lm,_ln]:[0,_lp,_lq];default:return [0,_lm,_ln];}},_lr=function(_ls,_lt,_lu,_lv,_lw){var _lx=E(_lv),_ly=_lx[1],_lz=_lx[2],_lA=E(_lw),_lB=_lA[1],_lC=_lA[2];switch(B(A(E(_lt)[2],[_ly,_lB]))){case 0:return [0,_ly,_lz];case 1:return !B(A(_k9,[_lu,_lz,_lC]))?[0,_lB,_lC]:[0,_ly,_lz];default:return [0,_lB,_lC];}},_lD=function(_lE,_lF,_lG){return [0,_lE,function(_7x,_7y){return new F(function(){return _l7(_lE,_lF,_lG,_7x,_7y);});},function(_7x,_7y){return new F(function(){return _k1(_lE,_lF,_lG,_7x,_7y);});},function(_7x,_7y){return new F(function(){return _kQ(_lE,_lF,_lG,_7x,_7y);});},function(_7x,_7y){return new F(function(){return _kz(_lE,_lF,_lG,_7x,_7y);});},function(_7x,_7y){return new F(function(){return _ki(_lE,_lF,_lG,_7x,_7y);});},function(_7x,_7y){return new F(function(){return _lf(_lE,_lF,_lG,_7x,_7y);});},function(_7x,_7y){return new F(function(){return _lr(_lE,_lF,_lG,_7x,_7y);});}];},_lH=function(_lI,_lJ){var _lK=E(_lI),_lL=E(_lJ);return _lK[1]>_lL[1]?E(_lK):E(_lL);},_lM=function(_lN,_lO){var _lP=E(_lN),_lQ=E(_lO);return _lP[1]>_lQ[1]?E(_lQ):E(_lP);},_lR=function(_lS,_lT){return _lS>=_lT?_lS!=_lT?2:1:0;},_lU=function(_lV,_lW){return new F(function(){return _lR(E(_lV)[1],E(_lW)[1]);});},_lX=function(_lY,_lZ){return E(_lY)[1]>=E(_lZ)[1];},_m0=function(_m1,_m2){return E(_m1)[1]>E(_m2)[1];},_m3=function(_m4,_m5){return E(_m4)[1]<=E(_m5)[1];},_m6=function(_m7,_m8){return E(_m7)[1]<E(_m8)[1];},_m9=[0,_7F,_lU,_m6,_lX,_m0,_m3,_lH,_lM],_ma=new T(function(){return B(_lD(_7G,_m9,_m9));}),_mb=[0,44],_mc=[1,_mb,_d],_md=function(_me,_mf){while(1){var _mg=E(_me);if(!_mg[0]){return E(_mf)[0]==0?true:false;}else{var _mh=E(_mf);if(!_mh[0]){return false;}else{if(E(_mg[1])[1]!=E(_mh[1])[1]){return false;}else{_me=_mg[2];_mf=_mh[2];continue;}}}}},_mi=function(_mj,_mk){return E(_mj)[1]!=E(_mk)[1];},_ml=function(_mm,_mn){return E(_mm)[1]==E(_mn)[1];},_mo=[0,_ml,_mi],_mp=function(_mq,_mr){while(1){var _ms=E(_mq);if(!_ms[0]){return E(_mr)[0]==0?true:false;}else{var _mt=E(_mr);if(!_mt[0]){return false;}else{if(E(_ms[1])[1]!=E(_mt[1])[1]){return false;}else{_mq=_ms[2];_mr=_mt[2];continue;}}}}},_mu=function(_mv,_mw){return !B(_mp(_mv,_mw))?true:false;},_mx=[0,_mp,_mu],_my=new T(function(){return B(unCStr("Control.Exception.Base"));}),_mz=new T(function(){return B(unCStr("base"));}),_mA=new T(function(){return B(unCStr("PatternMatchFail"));}),_mB=new T(function(){var _mC=hs_wordToWord64(18445595),_mD=_mC,_mE=hs_wordToWord64(52003073),_mF=_mE;return [0,_mD,_mF,[0,_mD,_mF,_mz,_my,_mA],_d];}),_mG=function(_mH){return E(_mB);},_mI=function(_mJ){var _mK=E(_mJ);return new F(function(){return _n(B(_l(_mK[1])),_mG,_mK[2]);});},_mL=function(_mM){return E(E(_mM)[1]);},_mN=function(_mO,_mP){return new F(function(){return _D(E(_mO)[1],_mP);});},_mQ=function(_mR,_mS){return new F(function(){return _1t(_mN,_mR,_mS);});},_mT=function(_mU,_mV,_mW){return new F(function(){return _D(E(_mV)[1],_mW);});},_mX=[0,_mT,_mL,_mQ],_mY=new T(function(){return [0,_mG,_mX,_mZ,_mI];}),_mZ=function(_n0){return [0,_mY,_n0];},_n1=new T(function(){return B(unCStr("Non-exhaustive patterns in"));}),_n2=function(_n3,_n4){return new F(function(){return die(new T(function(){return B(A(_n4,[_n3]));}));});},_n5=[0,32],_n6=[0,10],_n7=[1,_n6,_d],_n8=function(_n9){return E(E(_n9)[1])==124?false:true;},_na=function(_nb,_nc){var _nd=B(_gc(_n8,B(unCStr(_nb)))),_ne=_nd[1],_nf=function(_ng,_nh){return new F(function(){return _D(_ng,new T(function(){return B(unAppCStr(": ",new T(function(){return B(_D(_nc,new T(function(){return B(_D(_nh,_n7));})));})));}));});},_ni=E(_nd[2]);if(!_ni[0]){return new F(function(){return _nf(_ne,_d);});}else{return E(E(_ni[1])[1])==124?B(_nf(_ne,[1,_n5,_ni[2]])):B(_nf(_ne,_d));}},_nj=function(_nk){return new F(function(){return _n2([0,new T(function(){return B(_na(_nk,_n1));})],_mZ);});},_nl=new T(function(){return B(_nj("Text/ParserCombinators/ReadP.hs:(134,3)-(157,60)|function mplus"));}),_nm=function(_nn,_no){while(1){var _np=(function(_nq,_nr){var _ns=E(_nq);switch(_ns[0]){case 0:var _nt=E(_nr);if(!_nt[0]){return [0];}else{_nn=B(A(_ns[1],[_nt[1]]));_no=_nt[2];return null;}break;case 1:var _nu=B(A(_ns[1],[_nr])),_nv=_nr;_nn=_nu;_no=_nv;return null;case 2:return [0];case 3:return [1,[0,_ns[1],_nr],new T(function(){return B(_nm(_ns[2],_nr));})];default:return E(_ns[1]);}})(_nn,_no);if(_np!=null){return _np;}}},_nw=function(_nx,_ny){var _nz=function(_nA){var _nB=E(_ny);if(_nB[0]==3){return [3,_nB[1],new T(function(){return B(_nw(_nx,_nB[2]));})];}else{var _nC=E(_nx);if(_nC[0]==2){return E(_nB);}else{var _nD=E(_nB);if(_nD[0]==2){return E(_nC);}else{var _nE=function(_nF){var _nG=E(_nD);if(_nG[0]==4){return [1,function(_nH){return [4,new T(function(){return B(_D(B(_nm(_nC,_nH)),_nG[1]));})];}];}else{var _nI=E(_nC);if(_nI[0]==1){var _nJ=_nI[1],_nK=E(_nG);return _nK[0]==0?[1,function(_nL){return new F(function(){return _nw(B(A(_nJ,[_nL])),_nK);});}]:[1,function(_nM){return new F(function(){return _nw(B(A(_nJ,[_nM])),new T(function(){return B(A(_nK[1],[_nM]));}));});}];}else{var _nN=E(_nG);return _nN[0]==0?E(_nl):[1,function(_nO){return new F(function(){return _nw(_nI,new T(function(){return B(A(_nN[1],[_nO]));}));});}];}}},_nP=E(_nC);switch(_nP[0]){case 1:var _nQ=E(_nD);if(_nQ[0]==4){return [1,function(_nR){return [4,new T(function(){return B(_D(B(_nm(B(A(_nP[1],[_nR])),_nR)),_nQ[1]));})];}];}else{return new F(function(){return _nE(_);});}break;case 4:var _nS=_nP[1],_nT=E(_nD);switch(_nT[0]){case 0:return [1,function(_nU){return [4,new T(function(){return B(_D(_nS,new T(function(){return B(_nm(_nT,_nU));})));})];}];case 1:return [1,function(_nV){return [4,new T(function(){return B(_D(_nS,new T(function(){return B(_nm(B(A(_nT[1],[_nV])),_nV));})));})];}];default:return [4,new T(function(){return B(_D(_nS,_nT[1]));})];}break;default:return new F(function(){return _nE(_);});}}}}},_nW=E(_nx);switch(_nW[0]){case 0:var _nX=E(_ny);if(!_nX[0]){return [0,function(_nY){return new F(function(){return _nw(B(A(_nW[1],[_nY])),new T(function(){return B(A(_nX[1],[_nY]));}));});}];}else{return new F(function(){return _nz(_);});}break;case 3:return [3,_nW[1],new T(function(){return B(_nw(_nW[2],_ny));})];default:return new F(function(){return _nz(_);});}},_nZ=function(_o0,_o1){var _o2=E(_o0);switch(_o2[0]){case 0:return [0,function(_o3){return new F(function(){return _nZ(B(A(_o2[1],[_o3])),_o1);});}];case 1:return [1,function(_o4){return new F(function(){return _nZ(B(A(_o2[1],[_o4])),_o1);});}];case 2:return [2];case 3:return new F(function(){return _nw(B(A(_o1,[_o2[1]])),new T(function(){return B(_nZ(_o2[2],_o1));}));});break;default:var _o5=function(_o6){var _o7=E(_o6);if(!_o7[0]){return [0];}else{var _o8=E(_o7[1]);return new F(function(){return _D(B(_nm(B(A(_o1,[_o8[1]])),_o8[2])),new T(function(){return B(_o5(_o7[2]));}));});}},_o9=B(_o5(_o2[1]));return _o9[0]==0?[2]:[4,_o9];}},_oa=[2],_ob=function(_oc){return [3,_oc,_oa];},_od=function(_oe,_of){var _og=E(_oe);if(!_og){return new F(function(){return A(_of,[_iG]);});}else{return [0,function(_oh){return E(new T(function(){return B(_od(_og-1|0,_of));}));}];}},_oi=function(_oj,_ok,_ol){return function(_om){return new F(function(){return A(function(_on,_oo,_op){while(1){var _oq=(function(_or,_os,_ot){var _ou=E(_or);switch(_ou[0]){case 0:var _ov=E(_os);if(!_ov[0]){return E(_ok);}else{_on=B(A(_ou[1],[_ov[1]]));_oo=_ov[2];var _ow=_ot+1|0;_op=_ow;return null;}break;case 1:var _ox=B(A(_ou[1],[_os])),_oy=_os,_ow=_ot;_on=_ox;_oo=_oy;_op=_ow;return null;case 2:return E(_ok);case 3:return function(_oz){return new F(function(){return _od(_ot,function(_oA){return E(new T(function(){return B(_nZ(_ou,_oz));}));});});};default:return function(_oB){return new F(function(){return _nZ(_ou,_oB);});};}})(_on,_oo,_op);if(_oq!=null){return _oq;}}},[new T(function(){return B(A(_oj,[_ob]));}),_om,0,_ol]);});};},_oC=function(_oD){return new F(function(){return A(_oD,[_d]);});},_oE=function(_oF,_oG){var _oH=function(_oI){var _oJ=E(_oI);if(!_oJ[0]){return E(_oC);}else{var _oK=_oJ[1];return !B(A(_oF,[_oK]))?E(_oC):function(_oL){return [0,function(_oM){return E(new T(function(){return B(A(new T(function(){return B(_oH(_oJ[2]));}),[function(_oN){return new F(function(){return A(_oL,[[1,_oK,_oN]]);});}]));}));}];};}};return function(_oO){return new F(function(){return A(_oH,[_oO,_oG]);});};},_oP=[6],_oQ=new T(function(){return B(unCStr("valDig: Bad base"));}),_oR=new T(function(){return B(err(_oQ));}),_oS=function(_oT,_oU){var _oV=function(_oW,_oX){var _oY=E(_oW);if(!_oY[0]){return function(_oZ){return new F(function(){return A(_oZ,[new T(function(){return B(A(_oX,[_d]));})]);});};}else{var _p0=E(_oY[1])[1],_p1=function(_p2){return function(_p3){return [0,function(_p4){return E(new T(function(){return B(A(new T(function(){return B(_oV(_oY[2],function(_p5){return new F(function(){return A(_oX,[[1,_p2,_p5]]);});}));}),[_p3]));}));}];};};switch(E(E(_oT)[1])){case 8:if(48>_p0){return function(_p6){return new F(function(){return A(_p6,[new T(function(){return B(A(_oX,[_d]));})]);});};}else{if(_p0>55){return function(_p7){return new F(function(){return A(_p7,[new T(function(){return B(A(_oX,[_d]));})]);});};}else{return new F(function(){return _p1([0,_p0-48|0]);});}}break;case 10:if(48>_p0){return function(_p8){return new F(function(){return A(_p8,[new T(function(){return B(A(_oX,[_d]));})]);});};}else{if(_p0>57){return function(_p9){return new F(function(){return A(_p9,[new T(function(){return B(A(_oX,[_d]));})]);});};}else{return new F(function(){return _p1([0,_p0-48|0]);});}}break;case 16:if(48>_p0){if(97>_p0){if(65>_p0){return function(_pa){return new F(function(){return A(_pa,[new T(function(){return B(A(_oX,[_d]));})]);});};}else{if(_p0>70){return function(_pb){return new F(function(){return A(_pb,[new T(function(){return B(A(_oX,[_d]));})]);});};}else{return new F(function(){return _p1([0,(_p0-65|0)+10|0]);});}}}else{if(_p0>102){if(65>_p0){return function(_pc){return new F(function(){return A(_pc,[new T(function(){return B(A(_oX,[_d]));})]);});};}else{if(_p0>70){return function(_pd){return new F(function(){return A(_pd,[new T(function(){return B(A(_oX,[_d]));})]);});};}else{return new F(function(){return _p1([0,(_p0-65|0)+10|0]);});}}}else{return new F(function(){return _p1([0,(_p0-97|0)+10|0]);});}}}else{if(_p0>57){if(97>_p0){if(65>_p0){return function(_pe){return new F(function(){return A(_pe,[new T(function(){return B(A(_oX,[_d]));})]);});};}else{if(_p0>70){return function(_pf){return new F(function(){return A(_pf,[new T(function(){return B(A(_oX,[_d]));})]);});};}else{return new F(function(){return _p1([0,(_p0-65|0)+10|0]);});}}}else{if(_p0>102){if(65>_p0){return function(_pg){return new F(function(){return A(_pg,[new T(function(){return B(A(_oX,[_d]));})]);});};}else{if(_p0>70){return function(_ph){return new F(function(){return A(_ph,[new T(function(){return B(A(_oX,[_d]));})]);});};}else{return new F(function(){return _p1([0,(_p0-65|0)+10|0]);});}}}else{return new F(function(){return _p1([0,(_p0-97|0)+10|0]);});}}}else{return new F(function(){return _p1([0,_p0-48|0]);});}}break;default:return E(_oR);}}};return function(_pi){return new F(function(){return A(_oV,[_pi,_jE,function(_pj){var _pk=E(_pj);return _pk[0]==0?[2]:B(A(_oU,[_pk]));}]);});};},_pl=[0,10],_pm=[0,1],_pn=[0,2147483647],_po=function(_pp,_pq){while(1){var _pr=E(_pp);if(!_pr[0]){var _ps=_pr[1],_pt=E(_pq);if(!_pt[0]){var _pu=_pt[1],_pv=addC(_ps,_pu);if(!E(_pv[2])){return [0,_pv[1]];}else{_pp=[1,I_fromInt(_ps)];_pq=[1,I_fromInt(_pu)];continue;}}else{_pp=[1,I_fromInt(_ps)];_pq=_pt;continue;}}else{var _pw=E(_pq);if(!_pw[0]){_pp=_pr;_pq=[1,I_fromInt(_pw[1])];continue;}else{return [1,I_add(_pr[1],_pw[1])];}}}},_px=new T(function(){return B(_po(_pn,_pm));}),_py=function(_pz){var _pA=E(_pz);if(!_pA[0]){var _pB=E(_pA[1]);return _pB==(-2147483648)?E(_px):[0, -_pB];}else{return [1,I_negate(_pA[1])];}},_pC=[0,10],_pD=[0,0],_pE=function(_pF){return [0,_pF];},_pG=function(_pH,_pI){while(1){var _pJ=E(_pH);if(!_pJ[0]){var _pK=_pJ[1],_pL=E(_pI);if(!_pL[0]){var _pM=_pL[1];if(!(imul(_pK,_pM)|0)){return [0,imul(_pK,_pM)|0];}else{_pH=[1,I_fromInt(_pK)];_pI=[1,I_fromInt(_pM)];continue;}}else{_pH=[1,I_fromInt(_pK)];_pI=_pL;continue;}}else{var _pN=E(_pI);if(!_pN[0]){_pH=_pJ;_pI=[1,I_fromInt(_pN[1])];continue;}else{return [1,I_mul(_pJ[1],_pN[1])];}}}},_pO=function(_pP,_pQ,_pR){while(1){var _pS=E(_pR);if(!_pS[0]){return E(_pQ);}else{var _pT=B(_po(B(_pG(_pQ,_pP)),B(_pE(E(_pS[1])[1]))));_pR=_pS[2];_pQ=_pT;continue;}}},_pU=function(_pV){var _pW=new T(function(){return B(_nw(B(_nw([0,function(_pX){return E(E(_pX)[1])==45?[1,B(_oS(_pl,function(_pY){return new F(function(){return A(_pV,[[1,new T(function(){return B(_py(B(_pO(_pC,_pD,_pY))));})]]);});}))]:[2];}],[0,function(_pZ){return E(E(_pZ)[1])==43?[1,B(_oS(_pl,function(_q0){return new F(function(){return A(_pV,[[1,new T(function(){return B(_pO(_pC,_pD,_q0));})]]);});}))]:[2];}])),new T(function(){return [1,B(_oS(_pl,function(_q1){return new F(function(){return A(_pV,[[1,new T(function(){return B(_pO(_pC,_pD,_q1));})]]);});}))];})));});return new F(function(){return _nw([0,function(_q2){return E(E(_q2)[1])==101?E(_pW):[2];}],[0,function(_q3){return E(E(_q3)[1])==69?E(_pW):[2];}]);});},_q4=function(_q5){return new F(function(){return A(_q5,[_0]);});},_q6=function(_q7){return new F(function(){return A(_q7,[_0]);});},_q8=function(_q9){return function(_qa){return E(E(_qa)[1])==46?[1,B(_oS(_pl,function(_qb){return new F(function(){return A(_q9,[[1,_qb]]);});}))]:[2];};},_qc=function(_qd){return [0,B(_q8(_qd))];},_qe=function(_qf){return new F(function(){return _oS(_pl,function(_qg){return [1,B(_oi(_qc,_q4,function(_qh){return [1,B(_oi(_pU,_q6,function(_qi){return new F(function(){return A(_qf,[[5,[1,_qg,_qh,_qi]]]);});}))];}))];});});},_qj=function(_qk){return [1,B(_qe(_qk))];},_ql=new T(function(){return B(unCStr("!@#$%&*+./<=>?\\^|:-~"));}),_qm=function(_qn){return new F(function(){return _eC(_mo,_qn,_ql);});},_qo=[0,8],_qp=[0,16],_qq=function(_qr){var _qs=function(_qt){return new F(function(){return A(_qr,[[5,[0,_qo,_qt]]]);});},_qu=function(_qv){return new F(function(){return A(_qr,[[5,[0,_qp,_qv]]]);});};return function(_qw){return E(E(_qw)[1])==48?E([0,function(_qx){switch(E(E(_qx)[1])){case 79:return [1,B(_oS(_qo,_qs))];case 88:return [1,B(_oS(_qp,_qu))];case 111:return [1,B(_oS(_qo,_qs))];case 120:return [1,B(_oS(_qp,_qu))];default:return [2];}}]):[2];};},_qy=function(_qz){return [0,B(_qq(_qz))];},_qA=false,_qB=function(_qC){var _qD=new T(function(){return B(A(_qC,[_qo]));}),_qE=new T(function(){return B(A(_qC,[_qp]));});return function(_qF){switch(E(E(_qF)[1])){case 79:return E(_qD);case 88:return E(_qE);case 111:return E(_qD);case 120:return E(_qE);default:return [2];}};},_qG=function(_qH){return [0,B(_qB(_qH))];},_qI=[0,92],_qJ=function(_qK){return new F(function(){return A(_qK,[_pl]);});},_qL=function(_qM){return new F(function(){return err(B(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return B(_2v(9,_qM,_d));}))));});},_qN=function(_qO){var _qP=E(_qO);return _qP[0]==0?E(_qP[1]):I_toInt(_qP[1]);},_qQ=function(_qR,_qS){var _qT=E(_qR);if(!_qT[0]){var _qU=_qT[1],_qV=E(_qS);return _qV[0]==0?_qU<=_qV[1]:I_compareInt(_qV[1],_qU)>=0;}else{var _qW=_qT[1],_qX=E(_qS);return _qX[0]==0?I_compareInt(_qW,_qX[1])<=0:I_compare(_qW,_qX[1])<=0;}},_qY=function(_qZ){return [2];},_r0=function(_r1){var _r2=E(_r1);if(!_r2[0]){return E(_qY);}else{var _r3=_r2[1],_r4=E(_r2[2]);return _r4[0]==0?E(_r3):function(_r5){return new F(function(){return _nw(B(A(_r3,[_r5])),new T(function(){return B(A(new T(function(){return B(_r0(_r4));}),[_r5]));}));});};}},_r6=function(_r7){return [2];},_r8=function(_r9,_ra){var _rb=function(_rc,_rd){var _re=E(_rc);if(!_re[0]){return function(_rf){return new F(function(){return A(_rf,[_r9]);});};}else{var _rg=E(_rd);return _rg[0]==0?E(_r6):E(_re[1])[1]!=E(_rg[1])[1]?E(_r6):function(_rh){return [0,function(_ri){return E(new T(function(){return B(A(new T(function(){return B(_rb(_re[2],_rg[2]));}),[_rh]));}));}];};}};return function(_rj){return new F(function(){return A(_rb,[_r9,_rj,_ra]);});};},_rk=new T(function(){return B(unCStr("SOH"));}),_rl=[0,1],_rm=function(_rn){return [1,B(_r8(_rk,function(_ro){return E(new T(function(){return B(A(_rn,[_rl]));}));}))];},_rp=new T(function(){return B(unCStr("SO"));}),_rq=[0,14],_rr=function(_rs){return [1,B(_r8(_rp,function(_rt){return E(new T(function(){return B(A(_rs,[_rq]));}));}))];},_ru=function(_rv){return [1,B(_oi(_rm,_rr,_rv))];},_rw=new T(function(){return B(unCStr("NUL"));}),_rx=[0,0],_ry=function(_rz){return [1,B(_r8(_rw,function(_rA){return E(new T(function(){return B(A(_rz,[_rx]));}));}))];},_rB=new T(function(){return B(unCStr("STX"));}),_rC=[0,2],_rD=function(_rE){return [1,B(_r8(_rB,function(_rF){return E(new T(function(){return B(A(_rE,[_rC]));}));}))];},_rG=new T(function(){return B(unCStr("ETX"));}),_rH=[0,3],_rI=function(_rJ){return [1,B(_r8(_rG,function(_rK){return E(new T(function(){return B(A(_rJ,[_rH]));}));}))];},_rL=new T(function(){return B(unCStr("EOT"));}),_rM=[0,4],_rN=function(_rO){return [1,B(_r8(_rL,function(_rP){return E(new T(function(){return B(A(_rO,[_rM]));}));}))];},_rQ=new T(function(){return B(unCStr("ENQ"));}),_rR=[0,5],_rS=function(_rT){return [1,B(_r8(_rQ,function(_rU){return E(new T(function(){return B(A(_rT,[_rR]));}));}))];},_rV=new T(function(){return B(unCStr("ACK"));}),_rW=[0,6],_rX=function(_rY){return [1,B(_r8(_rV,function(_rZ){return E(new T(function(){return B(A(_rY,[_rW]));}));}))];},_s0=new T(function(){return B(unCStr("BEL"));}),_s1=[0,7],_s2=function(_s3){return [1,B(_r8(_s0,function(_s4){return E(new T(function(){return B(A(_s3,[_s1]));}));}))];},_s5=new T(function(){return B(unCStr("BS"));}),_s6=[0,8],_s7=function(_s8){return [1,B(_r8(_s5,function(_s9){return E(new T(function(){return B(A(_s8,[_s6]));}));}))];},_sa=new T(function(){return B(unCStr("HT"));}),_sb=[0,9],_sc=function(_sd){return [1,B(_r8(_sa,function(_se){return E(new T(function(){return B(A(_sd,[_sb]));}));}))];},_sf=new T(function(){return B(unCStr("LF"));}),_sg=[0,10],_sh=function(_si){return [1,B(_r8(_sf,function(_sj){return E(new T(function(){return B(A(_si,[_sg]));}));}))];},_sk=new T(function(){return B(unCStr("VT"));}),_sl=[0,11],_sm=function(_sn){return [1,B(_r8(_sk,function(_so){return E(new T(function(){return B(A(_sn,[_sl]));}));}))];},_sp=new T(function(){return B(unCStr("FF"));}),_sq=[0,12],_sr=function(_ss){return [1,B(_r8(_sp,function(_st){return E(new T(function(){return B(A(_ss,[_sq]));}));}))];},_su=new T(function(){return B(unCStr("CR"));}),_sv=[0,13],_sw=function(_sx){return [1,B(_r8(_su,function(_sy){return E(new T(function(){return B(A(_sx,[_sv]));}));}))];},_sz=new T(function(){return B(unCStr("SI"));}),_sA=[0,15],_sB=function(_sC){return [1,B(_r8(_sz,function(_sD){return E(new T(function(){return B(A(_sC,[_sA]));}));}))];},_sE=new T(function(){return B(unCStr("DLE"));}),_sF=[0,16],_sG=function(_sH){return [1,B(_r8(_sE,function(_sI){return E(new T(function(){return B(A(_sH,[_sF]));}));}))];},_sJ=new T(function(){return B(unCStr("DC1"));}),_sK=[0,17],_sL=function(_sM){return [1,B(_r8(_sJ,function(_sN){return E(new T(function(){return B(A(_sM,[_sK]));}));}))];},_sO=new T(function(){return B(unCStr("DC2"));}),_sP=[0,18],_sQ=function(_sR){return [1,B(_r8(_sO,function(_sS){return E(new T(function(){return B(A(_sR,[_sP]));}));}))];},_sT=new T(function(){return B(unCStr("DC3"));}),_sU=[0,19],_sV=function(_sW){return [1,B(_r8(_sT,function(_sX){return E(new T(function(){return B(A(_sW,[_sU]));}));}))];},_sY=new T(function(){return B(unCStr("DC4"));}),_sZ=[0,20],_t0=function(_t1){return [1,B(_r8(_sY,function(_t2){return E(new T(function(){return B(A(_t1,[_sZ]));}));}))];},_t3=new T(function(){return B(unCStr("NAK"));}),_t4=[0,21],_t5=function(_t6){return [1,B(_r8(_t3,function(_t7){return E(new T(function(){return B(A(_t6,[_t4]));}));}))];},_t8=new T(function(){return B(unCStr("SYN"));}),_t9=[0,22],_ta=function(_tb){return [1,B(_r8(_t8,function(_tc){return E(new T(function(){return B(A(_tb,[_t9]));}));}))];},_td=new T(function(){return B(unCStr("ETB"));}),_te=[0,23],_tf=function(_tg){return [1,B(_r8(_td,function(_th){return E(new T(function(){return B(A(_tg,[_te]));}));}))];},_ti=new T(function(){return B(unCStr("CAN"));}),_tj=[0,24],_tk=function(_tl){return [1,B(_r8(_ti,function(_tm){return E(new T(function(){return B(A(_tl,[_tj]));}));}))];},_tn=new T(function(){return B(unCStr("EM"));}),_to=[0,25],_tp=function(_tq){return [1,B(_r8(_tn,function(_tr){return E(new T(function(){return B(A(_tq,[_to]));}));}))];},_ts=new T(function(){return B(unCStr("SUB"));}),_tt=[0,26],_tu=function(_tv){return [1,B(_r8(_ts,function(_tw){return E(new T(function(){return B(A(_tv,[_tt]));}));}))];},_tx=new T(function(){return B(unCStr("ESC"));}),_ty=[0,27],_tz=function(_tA){return [1,B(_r8(_tx,function(_tB){return E(new T(function(){return B(A(_tA,[_ty]));}));}))];},_tC=new T(function(){return B(unCStr("FS"));}),_tD=[0,28],_tE=function(_tF){return [1,B(_r8(_tC,function(_tG){return E(new T(function(){return B(A(_tF,[_tD]));}));}))];},_tH=new T(function(){return B(unCStr("GS"));}),_tI=[0,29],_tJ=function(_tK){return [1,B(_r8(_tH,function(_tL){return E(new T(function(){return B(A(_tK,[_tI]));}));}))];},_tM=new T(function(){return B(unCStr("RS"));}),_tN=[0,30],_tO=function(_tP){return [1,B(_r8(_tM,function(_tQ){return E(new T(function(){return B(A(_tP,[_tN]));}));}))];},_tR=new T(function(){return B(unCStr("US"));}),_tS=[0,31],_tT=function(_tU){return [1,B(_r8(_tR,function(_tV){return E(new T(function(){return B(A(_tU,[_tS]));}));}))];},_tW=new T(function(){return B(unCStr("SP"));}),_tX=[0,32],_tY=function(_tZ){return [1,B(_r8(_tW,function(_u0){return E(new T(function(){return B(A(_tZ,[_tX]));}));}))];},_u1=new T(function(){return B(unCStr("DEL"));}),_u2=[0,127],_u3=function(_u4){return [1,B(_r8(_u1,function(_u5){return E(new T(function(){return B(A(_u4,[_u2]));}));}))];},_u6=[1,_u3,_d],_u7=[1,_tY,_u6],_u8=[1,_tT,_u7],_u9=[1,_tO,_u8],_ua=[1,_tJ,_u9],_ub=[1,_tE,_ua],_uc=[1,_tz,_ub],_ud=[1,_tu,_uc],_ue=[1,_tp,_ud],_uf=[1,_tk,_ue],_ug=[1,_tf,_uf],_uh=[1,_ta,_ug],_ui=[1,_t5,_uh],_uj=[1,_t0,_ui],_uk=[1,_sV,_uj],_ul=[1,_sQ,_uk],_um=[1,_sL,_ul],_un=[1,_sG,_um],_uo=[1,_sB,_un],_up=[1,_sw,_uo],_uq=[1,_sr,_up],_ur=[1,_sm,_uq],_us=[1,_sh,_ur],_ut=[1,_sc,_us],_uu=[1,_s7,_ut],_uv=[1,_s2,_uu],_uw=[1,_rX,_uv],_ux=[1,_rS,_uw],_uy=[1,_rN,_ux],_uz=[1,_rI,_uy],_uA=[1,_rD,_uz],_uB=[1,_ry,_uA],_uC=[1,_ru,_uB],_uD=new T(function(){return B(_r0(_uC));}),_uE=[0,1114111],_uF=[0,34],_uG=[0,39],_uH=function(_uI){var _uJ=new T(function(){return B(A(_uI,[_s1]));}),_uK=new T(function(){return B(A(_uI,[_s6]));}),_uL=new T(function(){return B(A(_uI,[_sb]));}),_uM=new T(function(){return B(A(_uI,[_sg]));}),_uN=new T(function(){return B(A(_uI,[_sl]));}),_uO=new T(function(){return B(A(_uI,[_sq]));}),_uP=new T(function(){return B(A(_uI,[_sv]));});return new F(function(){return _nw([0,function(_uQ){switch(E(E(_uQ)[1])){case 34:return E(new T(function(){return B(A(_uI,[_uF]));}));case 39:return E(new T(function(){return B(A(_uI,[_uG]));}));case 92:return E(new T(function(){return B(A(_uI,[_qI]));}));case 97:return E(_uJ);case 98:return E(_uK);case 102:return E(_uO);case 110:return E(_uM);case 114:return E(_uP);case 116:return E(_uL);case 118:return E(_uN);default:return [2];}}],new T(function(){return B(_nw([1,B(_oi(_qG,_qJ,function(_uR){return [1,B(_oS(_uR,function(_uS){var _uT=B(_pO(new T(function(){return B(_pE(E(_uR)[1]));}),_pD,_uS));return !B(_qQ(_uT,_uE))?[2]:B(A(_uI,[new T(function(){var _uU=B(_qN(_uT));if(_uU>>>0>1114111){var _uV=B(_qL(_uU));}else{var _uV=[0,_uU];}var _uW=_uV,_uX=_uW,_uY=_uX;return _uY;})]));}))];}))],new T(function(){return B(_nw([0,function(_uZ){return E(E(_uZ)[1])==94?E([0,function(_v0){switch(E(E(_v0)[1])){case 64:return E(new T(function(){return B(A(_uI,[_rx]));}));case 65:return E(new T(function(){return B(A(_uI,[_rl]));}));case 66:return E(new T(function(){return B(A(_uI,[_rC]));}));case 67:return E(new T(function(){return B(A(_uI,[_rH]));}));case 68:return E(new T(function(){return B(A(_uI,[_rM]));}));case 69:return E(new T(function(){return B(A(_uI,[_rR]));}));case 70:return E(new T(function(){return B(A(_uI,[_rW]));}));case 71:return E(_uJ);case 72:return E(_uK);case 73:return E(_uL);case 74:return E(_uM);case 75:return E(_uN);case 76:return E(_uO);case 77:return E(_uP);case 78:return E(new T(function(){return B(A(_uI,[_rq]));}));case 79:return E(new T(function(){return B(A(_uI,[_sA]));}));case 80:return E(new T(function(){return B(A(_uI,[_sF]));}));case 81:return E(new T(function(){return B(A(_uI,[_sK]));}));case 82:return E(new T(function(){return B(A(_uI,[_sP]));}));case 83:return E(new T(function(){return B(A(_uI,[_sU]));}));case 84:return E(new T(function(){return B(A(_uI,[_sZ]));}));case 85:return E(new T(function(){return B(A(_uI,[_t4]));}));case 86:return E(new T(function(){return B(A(_uI,[_t9]));}));case 87:return E(new T(function(){return B(A(_uI,[_te]));}));case 88:return E(new T(function(){return B(A(_uI,[_tj]));}));case 89:return E(new T(function(){return B(A(_uI,[_to]));}));case 90:return E(new T(function(){return B(A(_uI,[_tt]));}));case 91:return E(new T(function(){return B(A(_uI,[_ty]));}));case 92:return E(new T(function(){return B(A(_uI,[_tD]));}));case 93:return E(new T(function(){return B(A(_uI,[_tI]));}));case 94:return E(new T(function(){return B(A(_uI,[_tN]));}));case 95:return E(new T(function(){return B(A(_uI,[_tS]));}));default:return [2];}}]):[2];}],new T(function(){return B(A(_uD,[_uI]));})));})));}));});},_v1=function(_v2){return new F(function(){return A(_v2,[_iG]);});},_v3=function(_v4){var _v5=E(_v4);if(!_v5[0]){return E(_v1);}else{var _v6=_v5[2],_v7=E(E(_v5[1])[1]);switch(_v7){case 9:return function(_v8){return [0,function(_v9){return E(new T(function(){return B(A(new T(function(){return B(_v3(_v6));}),[_v8]));}));}];};case 10:return function(_va){return [0,function(_vb){return E(new T(function(){return B(A(new T(function(){return B(_v3(_v6));}),[_va]));}));}];};case 11:return function(_vc){return [0,function(_vd){return E(new T(function(){return B(A(new T(function(){return B(_v3(_v6));}),[_vc]));}));}];};case 12:return function(_ve){return [0,function(_vf){return E(new T(function(){return B(A(new T(function(){return B(_v3(_v6));}),[_ve]));}));}];};case 13:return function(_vg){return [0,function(_vh){return E(new T(function(){return B(A(new T(function(){return B(_v3(_v6));}),[_vg]));}));}];};case 32:return function(_vi){return [0,function(_vj){return E(new T(function(){return B(A(new T(function(){return B(_v3(_v6));}),[_vi]));}));}];};case 160:return function(_vk){return [0,function(_vl){return E(new T(function(){return B(A(new T(function(){return B(_v3(_v6));}),[_vk]));}));}];};default:var _vm=u_iswspace(_v7),_vn=_vm;return E(_vn)==0?E(_v1):function(_vo){return [0,function(_vp){return E(new T(function(){return B(A(new T(function(){return B(_v3(_v6));}),[_vo]));}));}];};}}},_vq=function(_vr){var _vs=new T(function(){return B(_vq(_vr));}),_vt=[1,function(_vu){return new F(function(){return A(_v3,[_vu,function(_vv){return E([0,function(_vw){return E(E(_vw)[1])==92?E(_vs):[2];}]);}]);});}];return new F(function(){return _nw([0,function(_vx){return E(E(_vx)[1])==92?E([0,function(_vy){var _vz=E(E(_vy)[1]);switch(_vz){case 9:return E(_vt);case 10:return E(_vt);case 11:return E(_vt);case 12:return E(_vt);case 13:return E(_vt);case 32:return E(_vt);case 38:return E(_vs);case 160:return E(_vt);default:var _vA=u_iswspace(_vz),_vB=_vA;return E(_vB)==0?[2]:E(_vt);}}]):[2];}],[0,function(_vC){var _vD=E(_vC);return E(_vD[1])==92?E(new T(function(){return B(_uH(function(_vE){return new F(function(){return A(_vr,[[0,_vE,_eX]]);});}));})):B(A(_vr,[[0,_vD,_qA]]));}]);});},_vF=function(_vG,_vH){return new F(function(){return _vq(function(_vI){var _vJ=E(_vI),_vK=E(_vJ[1]);if(E(_vK[1])==34){if(!E(_vJ[2])){return E(new T(function(){return B(A(_vH,[[1,new T(function(){return B(A(_vG,[_d]));})]]));}));}else{return new F(function(){return _vF(function(_vL){return new F(function(){return A(_vG,[[1,_vK,_vL]]);});},_vH);});}}else{return new F(function(){return _vF(function(_vM){return new F(function(){return A(_vG,[[1,_vK,_vM]]);});},_vH);});}});});},_vN=new T(function(){return B(unCStr("_\'"));}),_vO=function(_vP){var _vQ=u_iswalnum(_vP),_vR=_vQ;return E(_vR)==0?B(_eC(_mo,[0,_vP],_vN)):true;},_vS=function(_vT){return new F(function(){return _vO(E(_vT)[1]);});},_vU=new T(function(){return B(unCStr(",;()[]{}`"));}),_vV=new T(function(){return B(unCStr(".."));}),_vW=new T(function(){return B(unCStr("::"));}),_vX=new T(function(){return B(unCStr("->"));}),_vY=[0,64],_vZ=[1,_vY,_d],_w0=[0,126],_w1=[1,_w0,_d],_w2=new T(function(){return B(unCStr("=>"));}),_w3=[1,_w2,_d],_w4=[1,_w1,_w3],_w5=[1,_vZ,_w4],_w6=[1,_vX,_w5],_w7=new T(function(){return B(unCStr("<-"));}),_w8=[1,_w7,_w6],_w9=[0,124],_wa=[1,_w9,_d],_wb=[1,_wa,_w8],_wc=[1,_qI,_d],_wd=[1,_wc,_wb],_we=[0,61],_wf=[1,_we,_d],_wg=[1,_wf,_wd],_wh=[1,_vW,_wg],_wi=[1,_vV,_wh],_wj=function(_wk){return new F(function(){return _nw([1,function(_wl){return E(_wl)[0]==0?E(new T(function(){return B(A(_wk,[_oP]));})):[2];}],new T(function(){return B(_nw([0,function(_wm){return E(E(_wm)[1])==39?E([0,function(_wn){var _wo=E(_wn);switch(E(_wo[1])){case 39:return [2];case 92:return E(new T(function(){return B(_uH(function(_wp){return [0,function(_wq){return E(E(_wq)[1])==39?E(new T(function(){return B(A(_wk,[[0,_wp]]));})):[2];}];}));}));default:return [0,function(_wr){return E(E(_wr)[1])==39?E(new T(function(){return B(A(_wk,[[0,_wo]]));})):[2];}];}}]):[2];}],new T(function(){return B(_nw([0,function(_ws){return E(E(_ws)[1])==34?E(new T(function(){return B(_vF(_jE,_wk));})):[2];}],new T(function(){return B(_nw([0,function(_wt){return !B(_eC(_mo,_wt,_vU))?[2]:B(A(_wk,[[2,[1,_wt,_d]]]));}],new T(function(){return B(_nw([0,function(_wu){return !B(_eC(_mo,_wu,_ql))?[2]:[1,B(_oE(_qm,function(_wv){var _ww=[1,_wu,_wv];return !B(_eC(_mx,_ww,_wi))?B(A(_wk,[[4,_ww]])):B(A(_wk,[[2,_ww]]));}))];}],new T(function(){return B(_nw([0,function(_wx){var _wy=E(_wx),_wz=_wy[1],_wA=u_iswalpha(_wz),_wB=_wA;return E(_wB)==0?E(_wz)==95?[1,B(_oE(_vS,function(_wC){return new F(function(){return A(_wk,[[3,[1,_wy,_wC]]]);});}))]:[2]:[1,B(_oE(_vS,function(_wD){return new F(function(){return A(_wk,[[3,[1,_wy,_wD]]]);});}))];}],new T(function(){return [1,B(_oi(_qy,_qj,_wk))];})));})));})));})));})));}));});},_wE=function(_wF){return E(E(_wF)[3]);},_wG=function(_wH,_wI,_wJ){return function(_wK){return new F(function(){return A(new T(function(){return B(A(_wE,[_wH,_wJ]));}),[function(_wL){return [1,function(_wM){return new F(function(){return A(_v3,[_wM,function(_wN){return E(new T(function(){return B(_wj(function(_wO){var _wP=E(_wO);return _wP[0]==2?!B(_md(_wP[1],_mc))?[2]:E(new T(function(){return B(A(new T(function(){return B(_wE(_wI));}),[_wJ,function(_wQ){return new F(function(){return A(_wK,[[0,_wL,_wQ]]);});}]));})):[2];}));}));}]);});}];}]);});};},_wR=[0,41],_wS=[1,_wR,_d],_wT=[0,40],_wU=[1,_wT,_d],_wV=[0,0],_wW=function(_wX,_wY){return function(_wZ){return new F(function(){return A(_v3,[_wZ,function(_x0){return E(new T(function(){return B(_wj(function(_x1){var _x2=E(_x1);return _x2[0]==2?!B(_md(_x2[1],_wU))?[2]:E(new T(function(){return B(A(_wX,[_wV,function(_x3){return [1,function(_x4){return new F(function(){return A(_v3,[_x4,function(_x5){return E(new T(function(){return B(_wj(function(_x6){var _x7=E(_x6);return _x7[0]==2?!B(_md(_x7[1],_wS))?[2]:E(new T(function(){return B(A(_wY,[_x3]));})):[2];}));}));}]);});}];}]));})):[2];}));}));}]);});};},_x8=function(_x9,_xa){var _xb=function(_xc){return new F(function(){return _nw([1,B(_wW(_x9,_xc))],new T(function(){return [1,B(_wW(function(_xd,_xe){return new F(function(){return _xb(_xe);});},_xc))];}));});};return new F(function(){return _xb(_xa);});},_xf=function(_xg,_xh,_xi,_xj){return new F(function(){return _x8(function(_xk){return new F(function(){return _wG(_xg,_xh,_xk);});},_xj);});},_xl=[0,91],_xm=[1,_xl,_d],_xn=function(_xo,_xp){var _xq=function(_xr,_xs){return [1,function(_xt){return new F(function(){return A(_v3,[_xt,function(_xu){return E(new T(function(){return B(_wj(function(_xv){var _xw=E(_xv);if(_xw[0]==2){var _xx=E(_xw[1]);if(!_xx[0]){return [2];}else{var _xy=_xx[2];switch(E(E(_xx[1])[1])){case 44:return E(_xy)[0]==0?!E(_xr)?[2]:E(new T(function(){return B(A(_xo,[_wV,function(_xz){return new F(function(){return _xq(_eX,function(_xA){return new F(function(){return A(_xs,[[1,_xz,_xA]]);});});});}]));})):[2];case 93:return E(_xy)[0]==0?E(new T(function(){return B(A(_xs,[_d]));})):[2];default:return [2];}}}else{return [2];}}));}));}]);});}];},_xB=function(_xC){return new F(function(){return _nw([1,function(_xD){return new F(function(){return A(_v3,[_xD,function(_xE){return E(new T(function(){return B(_wj(function(_xF){var _xG=E(_xF);return _xG[0]==2?!B(_md(_xG[1],_xm))?[2]:E(new T(function(){return B(_nw(B(_xq(_qA,_xC)),new T(function(){return B(A(_xo,[_wV,function(_xH){return new F(function(){return _xq(_eX,function(_xI){return new F(function(){return A(_xC,[[1,_xH,_xI]]);});});});}]));})));})):[2];}));}));}]);});}],new T(function(){return [1,B(_wW(function(_xJ,_xK){return new F(function(){return _xB(_xK);});},_xC))];}));});};return new F(function(){return _xB(_xp);});},_xL=function(_xM,_xN,_xO,_xP){return new F(function(){return _xn(function(_xQ,_xk){return new F(function(){return _xf(_xM,_xN,_xQ,_xk);});},_xP);});},_xR=function(_xS,_xT){return function(_oB){return new F(function(){return _nm(new T(function(){return B(_xn(function(_xQ,_xk){return new F(function(){return _xf(_xS,_xT,_xQ,_xk);});},_ob));}),_oB);});};},_xU=function(_xV,_xW,_xX){return function(_oB){return new F(function(){return _nm(new T(function(){return B(_x8(function(_xk){return new F(function(){return _wG(_xV,_xW,_xk);});},_ob));}),_oB);});};},_xY=function(_xZ,_y0){return [0,function(_xk){return new F(function(){return _xU(_xZ,_y0,_xk);});},new T(function(){return B(_xR(_xZ,_y0));}),function(_xQ,_xk){return new F(function(){return _xf(_xZ,_y0,_xQ,_xk);});},function(_xQ,_xk){return new F(function(){return _xL(_xZ,_y0,_xQ,_xk);});}];},_y1=function(_y2,_y3,_y4){var _y5=function(_y6,_y7){return new F(function(){return _nw([1,function(_y8){return new F(function(){return A(_v3,[_y8,function(_y9){return E(new T(function(){return B(_wj(function(_ya){var _yb=E(_ya);if(_yb[0]==4){var _yc=E(_yb[1]);if(!_yc[0]){return new F(function(){return A(_y2,[_yb,_y6,_y7]);});}else{return E(E(_yc[1])[1])==45?E(_yc[2])[0]==0?E([1,function(_yd){return new F(function(){return A(_v3,[_yd,function(_ye){return E(new T(function(){return B(_wj(function(_yf){return new F(function(){return A(_y2,[_yf,_y6,function(_yg){return new F(function(){return A(_y7,[new T(function(){return [0, -E(_yg)[1]];})]);});}]);});}));}));}]);});}]):B(A(_y2,[_yb,_y6,_y7])):B(A(_y2,[_yb,_y6,_y7]));}}else{return new F(function(){return A(_y2,[_yb,_y6,_y7]);});}}));}));}]);});}],new T(function(){return [1,B(_wW(_y5,_y7))];}));});};return new F(function(){return _y5(_y3,_y4);});},_yh=function(_yi,_yj){return [2];},_yk=function(_yl){var _ym=E(_yl);return _ym[0]==0?[1,new T(function(){return B(_pO(new T(function(){return B(_pE(E(_ym[1])[1]));}),_pD,_ym[2]));})]:E(_ym[2])[0]==0?E(_ym[3])[0]==0?[1,new T(function(){return B(_pO(_pC,_pD,_ym[1]));})]:[0]:[0];},_yn=function(_yo){var _yp=E(_yo);if(_yp[0]==5){var _yq=B(_yk(_yp[1]));return _yq[0]==0?E(_yh):function(_yr,_ys){return new F(function(){return A(_ys,[new T(function(){return [0,B(_qN(_yq[1]))];})]);});};}else{return E(_yh);}},_yt=function(_xQ,_xk){return new F(function(){return _y1(_yn,_xQ,_xk);});},_yu=function(_yv,_yw){return new F(function(){return _xn(_yt,_yw);});},_yx=function(_yy){return function(_oB){return new F(function(){return _nm(new T(function(){return B(_y1(_yn,_yy,_ob));}),_oB);});};},_yz=new T(function(){return B(_xn(_yt,_ob));}),_yA=function(_xk){return new F(function(){return _nm(_yz,_xk);});},_yB=[0,_yx,_yA,_yt,_yu],_yC=new T(function(){return B(_xY(_yB,_yB));}),_yD=new T(function(){return B(unCStr("Ring"));}),_yE=new T(function(){return B(unCStr("Marker"));}),_yF=[0,11],_yG=function(_yH,_yI){return new F(function(){return A(_yI,[_d4]);});},_yJ=[1,_1Z,_d],_yK=[0,_yJ,_yG],_yL=function(_yM,_yN){return new F(function(){return A(_yN,[_d6]);});},_yO=[1,_1Y,_d],_yP=[0,_yO,_yL],_yQ=[1,_yP,_d],_yR=[1,_yK,_yQ],_yS=function(_yT,_yU,_yV){var _yW=E(_yT);if(!_yW[0]){return [2];}else{var _yX=E(_yW[1]),_yY=_yX[1],_yZ=new T(function(){return B(A(_yX[2],[_yU,_yV]));});return new F(function(){return _nw([1,function(_z0){return new F(function(){return A(_v3,[_z0,function(_z1){return E(new T(function(){return B(_wj(function(_z2){var _z3=E(_z2);switch(_z3[0]){case 3:return !B(_md(_yY,_z3[1]))?[2]:E(_yZ);case 4:return !B(_md(_yY,_z3[1]))?[2]:E(_yZ);default:return [2];}}));}));}]);});}],new T(function(){return B(_yS(_yW[2],_yU,_yV));}));});}},_z4=function(_z5,_z6){return new F(function(){return _yS(_yR,_z5,_z6);});},_z7=function(_z8,_z9){var _za=function(_zb){return function(_zc){return new F(function(){return _nw(B(A(new T(function(){return B(A(_z8,[_zb]));}),[_zc])),new T(function(){return [1,B(_wW(_za,_zc))];}));});};};return new F(function(){return _za(_z9);});},_zd=function(_ze,_zf){var _zg=new T(function(){if(_ze>10){var _zh=[2];}else{var _zh=[1,function(_zi){return new F(function(){return A(_v3,[_zi,function(_zj){return E(new T(function(){return B(_wj(function(_zk){var _zl=E(_zk);return _zl[0]==3?!B(_md(_zl[1],_yE))?[2]:E(new T(function(){return B(A(_z7,[_z4,_yF,function(_zm){return new F(function(){return A(_zf,[[1,_zm]]);});}]));})):[2];}));}));}]);});}];}var _zn=_zh;return _zn;});if(_ze>10){return new F(function(){return _nw(_oa,_zg);});}else{return new F(function(){return _nw([1,function(_zo){return new F(function(){return A(_v3,[_zo,function(_zp){return E(new T(function(){return B(_wj(function(_zq){var _zr=E(_zq);return _zr[0]==3?!B(_md(_zr[1],_yD))?[2]:E(new T(function(){return B(A(_z7,[_z4,_yF,function(_zs){return new F(function(){return A(_zf,[[0,_zs]]);});}]));})):[2];}));}));}]);});}],_zg);});}},_zt=function(_zu,_zv){return new F(function(){return _zd(E(_zu)[1],_zv);});},_zw=function(_2j){return new F(function(){return _z7(_zt,_2j);});},_zx=function(_zy,_zz){return new F(function(){return _xn(_zw,_zz);});},_zA=new T(function(){return B(_xn(_zw,_ob));}),_zB=function(_2j){return new F(function(){return _nm(_zA,_2j);});},_zC=function(_zD){return function(_oB){return new F(function(){return _nm(new T(function(){return B(A(_z7,[_zt,_zD,_ob]));}),_oB);});};},_zE=[0,_zC,_zB,_zw,_zx],_zF=new T(function(){return B(unCStr("fromList"));}),_zG=function(_zH,_zI,_zJ,_zK){var _zL=E(_zI),_zM=E(_zK);if(!_zM[0]){var _zN=_zM[2],_zO=_zM[3],_zP=_zM[4],_zQ=_zM[5];switch(B(A(_kY,[_zH,_zL,_zN]))){case 0:return new F(function(){return _7M(_zN,_zO,B(_zG(_zH,_zL,_zJ,_zP)),_zQ);});break;case 1:return [0,_zM[1],E(_zL),_zJ,E(_zP),E(_zQ)];default:return new F(function(){return _8t(_zN,_zO,_zP,B(_zG(_zH,_zL,_zJ,_zQ)));});}}else{return [0,1,E(_zL),_zJ,E(_7H),E(_7H)];}},_zR=function(_zS,_zT,_zU,_zV){return new F(function(){return _zG(_zS,_zT,_zU,_zV);});},_zW=function(_zX,_zY,_zZ){return new F(function(){return (function(_A0,_A1){while(1){var _A2=E(_A1);if(!_A2[0]){return E(_A0);}else{var _A3=E(_A2[1]),_A4=B(_zR(_zX,_A3[1],_A3[2],_A0));_A1=_A2[2];_A0=_A4;continue;}}})(_zY,_zZ);});},_A5=function(_A6,_A7){return [0,1,E(E(_A6)),_A7,E(_7H),E(_7H)];},_A8=function(_A9,_Aa,_Ab){var _Ac=E(_Ab);if(!_Ac[0]){return new F(function(){return _8t(_Ac[2],_Ac[3],_Ac[4],B(_A8(_A9,_Aa,_Ac[5])));});}else{return new F(function(){return _A5(_A9,_Aa);});}},_Ad=function(_Ae,_Af,_Ag){var _Ah=E(_Ag);if(!_Ah[0]){return new F(function(){return _7M(_Ah[2],_Ah[3],B(_Ad(_Ae,_Af,_Ah[4])),_Ah[5]);});}else{return new F(function(){return _A5(_Ae,_Af);});}},_Ai=function(_Aj,_Ak,_Al,_Am,_An,_Ao,_Ap){return new F(function(){return _7M(_Am,_An,B(_Ad(_Aj,_Ak,_Ao)),_Ap);});},_Aq=function(_Ar,_As,_At,_Au,_Av,_Aw,_Ax,_Ay){var _Az=E(_At);if(!_Az[0]){var _AA=_Az[1],_AB=_Az[2],_AC=_Az[3],_AD=_Az[4],_AE=_Az[5];if((imul(3,_AA)|0)>=_Au){if((imul(3,_Au)|0)>=_AA){return [0,(_AA+_Au|0)+1|0,E(E(_Ar)),_As,E(_Az),E([0,_Au,E(_Av),_Aw,E(_Ax),E(_Ay)])];}else{return new F(function(){return _8t(_AB,_AC,_AD,B(_Aq(_Ar,_As,_AE,_Au,_Av,_Aw,_Ax,_Ay)));});}}else{return new F(function(){return _7M(_Av,_Aw,B(_AF(_Ar,_As,_AA,_AB,_AC,_AD,_AE,_Ax)),_Ay);});}}else{return new F(function(){return _Ai(_Ar,_As,_Au,_Av,_Aw,_Ax,_Ay);});}},_AF=function(_AG,_AH,_AI,_AJ,_AK,_AL,_AM,_AN){var _AO=E(_AN);if(!_AO[0]){var _AP=_AO[1],_AQ=_AO[2],_AR=_AO[3],_AS=_AO[4],_AT=_AO[5];if((imul(3,_AI)|0)>=_AP){if((imul(3,_AP)|0)>=_AI){return [0,(_AI+_AP|0)+1|0,E(E(_AG)),_AH,E([0,_AI,E(_AJ),_AK,E(_AL),E(_AM)]),E(_AO)];}else{return new F(function(){return _8t(_AJ,_AK,_AL,B(_Aq(_AG,_AH,_AM,_AP,_AQ,_AR,_AS,_AT)));});}}else{return new F(function(){return _7M(_AQ,_AR,B(_AF(_AG,_AH,_AI,_AJ,_AK,_AL,_AM,_AS)),_AT);});}}else{return new F(function(){return _A8(_AG,_AH,[0,_AI,E(_AJ),_AK,E(_AL),E(_AM)]);});}},_AU=function(_AV,_AW,_AX,_AY){var _AZ=E(_AX);if(!_AZ[0]){var _B0=_AZ[1],_B1=_AZ[2],_B2=_AZ[3],_B3=_AZ[4],_B4=_AZ[5],_B5=E(_AY);if(!_B5[0]){var _B6=_B5[1],_B7=_B5[2],_B8=_B5[3],_B9=_B5[4],_Ba=_B5[5];if((imul(3,_B0)|0)>=_B6){if((imul(3,_B6)|0)>=_B0){return [0,(_B0+_B6|0)+1|0,E(E(_AV)),_AW,E(_AZ),E(_B5)];}else{return new F(function(){return _8t(_B1,_B2,_B3,B(_Aq(_AV,_AW,_B4,_B6,_B7,_B8,_B9,_Ba)));});}}else{return new F(function(){return _7M(_B7,_B8,B(_AF(_AV,_AW,_B0,_B1,_B2,_B3,_B4,_B9)),_Ba);});}}else{return new F(function(){return _A8(_AV,_AW,_AZ);});}}else{return new F(function(){return _Ad(_AV,_AW,_AY);});}},_Bb=function(_Bc,_Bd){var _Be=new T(function(){return B(_kH(_Bc));}),_Bf=E(_Bd);if(!_Bf[0]){return [1];}else{var _Bg=E(_Bf[1]),_Bh=_Bg[1],_Bi=_Bg[2],_Bj=E(_Bf[2]);if(!_Bj[0]){return [0,1,E(E(_Bh)),_Bi,E(_7H),E(_7H)];}else{var _Bk=E(_Bj[1]),_Bl=_Bk[1];if(!B(A(_kH,[_Bc,_Bh,_Bl]))){var _Bm=function(_Bn,_Bo,_Bp,_Bq){var _Br=E(_Bn);if(_Br==1){var _Bs=E(_Bq);return _Bs[0]==0?[0,new T(function(){return [0,1,E(E(_Bo)),_Bp,E(_7H),E(_7H)];}),_d,_d]:!B(A(_Be,[_Bo,E(_Bs[1])[1]]))?[0,new T(function(){return [0,1,E(E(_Bo)),_Bp,E(_7H),E(_7H)];}),_Bs,_d]:[0,new T(function(){return [0,1,E(E(_Bo)),_Bp,E(_7H),E(_7H)];}),_d,_Bs];}else{var _Bt=B(_Bm(_Br>>1,_Bo,_Bp,_Bq)),_Bu=_Bt[1],_Bv=_Bt[3],_Bw=E(_Bt[2]);if(!_Bw[0]){return [0,_Bu,_d,_Bv];}else{var _Bx=E(_Bw[1]),_By=_Bx[1],_Bz=_Bx[2],_BA=E(_Bw[2]);if(!_BA[0]){return [0,new T(function(){return B(_A8(_By,_Bz,_Bu));}),_d,_Bv];}else{var _BB=E(_BA[1]),_BC=_BB[1];if(!B(A(_Be,[_By,_BC]))){var _BD=B(_Bm(_Br>>1,_BC,_BB[2],_BA[2]));return [0,new T(function(){return B(_AU(_By,_Bz,_Bu,_BD[1]));}),_BD[2],_BD[3]];}else{return [0,_Bu,_d,_Bw];}}}}};return new F(function(){return (function(_BE,_BF,_BG,_BH,_BI){var _BJ=E(_BI);if(!_BJ[0]){return new F(function(){return _A8(_BG,_BH,_BF);});}else{var _BK=E(_BJ[1]),_BL=_BK[1];if(!B(A(_Be,[_BG,_BL]))){var _BM=B(_Bm(_BE,_BL,_BK[2],_BJ[2])),_BN=_BM[1],_BO=E(_BM[3]);if(!_BO[0]){return new F(function(){return (function(_BP,_BQ,_BR){while(1){var _BS=E(_BR);if(!_BS[0]){return E(_BQ);}else{var _BT=E(_BS[1]),_BU=_BT[1],_BV=_BT[2],_BW=E(_BS[2]);if(!_BW[0]){return new F(function(){return _A8(_BU,_BV,_BQ);});}else{var _BX=E(_BW[1]),_BY=_BX[1];if(!B(A(_Be,[_BU,_BY]))){var _BZ=B(_Bm(_BP,_BY,_BX[2],_BW[2])),_C0=_BZ[1],_C1=E(_BZ[3]);if(!_C1[0]){var _C2=_BP<<1,_C3=B(_AU(_BU,_BV,_BQ,_C0));_BR=_BZ[2];_BP=_C2;_BQ=_C3;continue;}else{return new F(function(){return _zW(_Bc,B(_AU(_BU,_BV,_BQ,_C0)),_C1);});}}else{return new F(function(){return _zW(_Bc,_BQ,_BS);});}}}}})(_BE<<1,B(_AU(_BG,_BH,_BF,_BN)),_BM[2]);});}else{return new F(function(){return _zW(_Bc,B(_AU(_BG,_BH,_BF,_BN)),_BO);});}}else{return new F(function(){return _zW(_Bc,_BF,[1,[0,_BG,_BH],_BJ]);});}}})(1,[0,1,E(E(_Bh)),_Bi,E(_7H),E(_7H)],_Bl,_Bk[2],_Bj[2]);});}else{return new F(function(){return _zW(_Bc,[0,1,E(E(_Bh)),_Bi,E(_7H),E(_7H)],_Bj);});}}}},_C4=function(_C5,_C6,_C7,_C8){return new F(function(){return _z7(function(_C9){return E(_C9)[1]>10?E(_qY):function(_Ca){return [1,function(_Cb){return new F(function(){return A(_v3,[_Cb,function(_Cc){return E(new T(function(){return B(_wj(function(_Cd){var _Ce=E(_Cd);return _Ce[0]==3?!B(_md(_Ce[1],_zF))?[2]:E(new T(function(){return B(_xn(function(_Cf,_Cg){return new F(function(){return _xf(_C6,_C7,_Cf,_Cg);});},function(_Ch){return new F(function(){return A(_Ca,[new T(function(){return B(_Bb(_C5,_Ch));})]);});}));})):[2];}));}));}]);});}];};},_C8);});},_Ci=[0,123],_Cj=[1,_Ci,_d],_Ck=new T(function(){return B(unCStr("bmap"));}),_Cl=new T(function(){return B(unCStr("Board"));}),_Cm=new T(function(){return B(unCStr("markersW"));}),_Cn=new T(function(){return B(unCStr("markersB"));}),_Co=new T(function(){return B(unCStr("ringsW"));}),_Cp=new T(function(){return B(unCStr("ringsB"));}),_Cq=[1,_20,_d],_Cr=function(_2j){return new F(function(){return _wG(_yB,_yB,_2j);});},_Cs=function(_Ct,_Cu){return new F(function(){return _x8(_Cr,_Cu);});},_Cv=[0,61],_Cw=[1,_Cv,_d],_Cx=[0,44],_Cy=[1,_Cx,_d],_Cz=function(_CA,_CB){return _CA>11?[2]:[1,function(_CC){return new F(function(){return A(_v3,[_CC,function(_CD){return E(new T(function(){return B(_wj(function(_CE){var _CF=E(_CE);return _CF[0]==3?!B(_md(_CF[1],_Cl))?[2]:E([1,function(_CG){return new F(function(){return A(_v3,[_CG,function(_CH){return E(new T(function(){return B(_wj(function(_CI){var _CJ=E(_CI);return _CJ[0]==2?!B(_md(_CJ[1],_Cj))?[2]:E([1,function(_CK){return new F(function(){return A(_v3,[_CK,function(_CL){return E(new T(function(){return B(_wj(function(_CM){var _CN=E(_CM);return _CN[0]==3?!B(_md(_CN[1],_Ck))?[2]:E([1,function(_CO){return new F(function(){return A(_v3,[_CO,function(_CP){return E(new T(function(){return B(_wj(function(_CQ){var _CR=E(_CQ);return _CR[0]==2?!B(_md(_CR[1],_Cw))?[2]:E(new T(function(){return B(A(_C4,[_ma,_yC,_zE,_wV,function(_CS){return [1,function(_CT){return new F(function(){return A(_v3,[_CT,function(_CU){return E(new T(function(){return B(_wj(function(_CV){var _CW=E(_CV);return _CW[0]==2?!B(_md(_CW[1],_Cy))?[2]:E([1,function(_CX){return new F(function(){return A(_v3,[_CX,function(_CY){return E(new T(function(){return B(_wj(function(_CZ){var _D0=E(_CZ);return _D0[0]==3?!B(_md(_D0[1],_Cp))?[2]:E([1,function(_D1){return new F(function(){return A(_v3,[_D1,function(_D2){return E(new T(function(){return B(_wj(function(_D3){var _D4=E(_D3);return _D4[0]==2?!B(_md(_D4[1],_Cw))?[2]:E(new T(function(){return B(_xn(_Cs,function(_D5){return [1,function(_D6){return new F(function(){return A(_v3,[_D6,function(_D7){return E(new T(function(){return B(_wj(function(_D8){var _D9=E(_D8);return _D9[0]==2?!B(_md(_D9[1],_Cy))?[2]:E([1,function(_Da){return new F(function(){return A(_v3,[_Da,function(_Db){return E(new T(function(){return B(_wj(function(_Dc){var _Dd=E(_Dc);return _Dd[0]==3?!B(_md(_Dd[1],_Co))?[2]:E([1,function(_De){return new F(function(){return A(_v3,[_De,function(_Df){return E(new T(function(){return B(_wj(function(_Dg){var _Dh=E(_Dg);return _Dh[0]==2?!B(_md(_Dh[1],_Cw))?[2]:E(new T(function(){return B(_xn(_Cs,function(_Di){return [1,function(_Dj){return new F(function(){return A(_v3,[_Dj,function(_Dk){return E(new T(function(){return B(_wj(function(_Dl){var _Dm=E(_Dl);return _Dm[0]==2?!B(_md(_Dm[1],_Cy))?[2]:E([1,function(_Dn){return new F(function(){return A(_v3,[_Dn,function(_Do){return E(new T(function(){return B(_wj(function(_Dp){var _Dq=E(_Dp);return _Dq[0]==3?!B(_md(_Dq[1],_Cn))?[2]:E([1,function(_Dr){return new F(function(){return A(_v3,[_Dr,function(_Ds){return E(new T(function(){return B(_wj(function(_Dt){var _Du=E(_Dt);return _Du[0]==2?!B(_md(_Du[1],_Cw))?[2]:E(new T(function(){return B(_xn(_Cs,function(_Dv){return [1,function(_Dw){return new F(function(){return A(_v3,[_Dw,function(_Dx){return E(new T(function(){return B(_wj(function(_Dy){var _Dz=E(_Dy);return _Dz[0]==2?!B(_md(_Dz[1],_Cy))?[2]:E([1,function(_DA){return new F(function(){return A(_v3,[_DA,function(_DB){return E(new T(function(){return B(_wj(function(_DC){var _DD=E(_DC);return _DD[0]==3?!B(_md(_DD[1],_Cm))?[2]:E([1,function(_DE){return new F(function(){return A(_v3,[_DE,function(_DF){return E(new T(function(){return B(_wj(function(_DG){var _DH=E(_DG);return _DH[0]==2?!B(_md(_DH[1],_Cw))?[2]:E(new T(function(){return B(_xn(_Cs,function(_DI){return [1,function(_DJ){return new F(function(){return A(_v3,[_DJ,function(_DK){return E(new T(function(){return B(_wj(function(_DL){var _DM=E(_DL);return _DM[0]==2?!B(_md(_DM[1],_Cq))?[2]:E(new T(function(){return B(A(_CB,[[0,_CS,_D5,_Di,_Dv,_DI]]));})):[2];}));}));}]);});}];}));})):[2];}));}));}]);});}]):[2];}));}));}]);});}]):[2];}));}));}]);});}];}));})):[2];}));}));}]);});}]):[2];}));}));}]);});}]):[2];}));}));}]);});}];}));})):[2];}));}));}]);});}]):[2];}));}));}]);});}]):[2];}));}));}]);});}];}));})):[2];}));}));}]);});}]):[2];}));}));}]);});}]):[2];}));}));}]);});}];}]));})):[2];}));}));}]);});}]):[2];}));}));}]);});}]):[2];}));}));}]);});}]):[2];}));}));}]);});}];},_DN=function(_DO,_DP){return new F(function(){return _Cz(E(_DO)[1],_DP);});},_DQ=[0],_DR=function(_DS,_DT){return new F(function(){return A(_DT,[_DQ]);});},_DU=[0,_2k,_DR],_DV=function(_DW,_DX){return new F(function(){return A(_DX,[_ex]);});},_DY=[0,_2h,_DV],_DZ=[6],_E0=function(_E1,_E2){return new F(function(){return A(_E2,[_DZ]);});},_E3=[0,_2m,_E0],_E4=[1,_E3,_d],_E5=[1,_DY,_E4],_E6=[1,_DU,_E5],_E7=new T(function(){return B(unCStr("WaitRemoveRun"));}),_E8=new T(function(){return B(unCStr("RemoveRing"));}),_E9=new T(function(){return B(unCStr("RemoveRun"));}),_Ea=new T(function(){return B(unCStr("MoveRing"));}),_Eb=function(_Ec,_Ed){return new F(function(){return _nw(B(_yS(_E6,_Ec,_Ed)),new T(function(){var _Ee=E(_Ec)[1],_Ef=new T(function(){var _Eg=new T(function(){var _Eh=new T(function(){if(_Ee>10){var _Ei=[2];}else{var _Ei=[1,function(_Ej){return new F(function(){return A(_v3,[_Ej,function(_Ek){return E(new T(function(){return B(_wj(function(_El){var _Em=E(_El);return _Em[0]==3?!B(_md(_Em[1],_E7))?[2]:E(new T(function(){return B(A(_z7,[_z4,_yF,function(_En){return new F(function(){return A(_Ed,[[5,_En]]);});}]));})):[2];}));}));}]);});}];}var _Eo=_Ei;return _Eo;});if(_Ee>10){var _Ep=B(_nw(_oa,_Eh));}else{var _Ep=B(_nw([1,function(_Eq){return new F(function(){return A(_v3,[_Eq,function(_Er){return E(new T(function(){return B(_wj(function(_Es){var _Et=E(_Es);return _Et[0]==3?!B(_md(_Et[1],_E8))?[2]:E(new T(function(){return B(A(_z7,[_z4,_yF,function(_Eu){return new F(function(){return A(_Ed,[[4,_Eu]]);});}]));})):[2];}));}));}]);});}],_Eh));}var _Ev=_Ep;return _Ev;});if(_Ee>10){var _Ew=B(_nw(_oa,_Eg));}else{var _Ew=B(_nw([1,function(_Ex){return new F(function(){return A(_v3,[_Ex,function(_Ey){return E(new T(function(){return B(_wj(function(_Ez){var _EA=E(_Ez);return _EA[0]==3?!B(_md(_EA[1],_E9))?[2]:E(new T(function(){return B(A(_z7,[_z4,_yF,function(_EB){return new F(function(){return A(_Ed,[[3,_EB]]);});}]));})):[2];}));}));}]);});}],_Eg));}var _EC=_Ew;return _EC;});if(_Ee>10){var _ED=B(_nw(_oa,_Ef));}else{var _ED=B(_nw([1,function(_EE){return new F(function(){return A(_v3,[_EE,function(_EF){return E(new T(function(){return B(_wj(function(_EG){var _EH=E(_EG);return _EH[0]==3?!B(_md(_EH[1],_Ea))?[2]:E(new T(function(){return B(_x8(_Cr,function(_EI){return new F(function(){return A(_Ed,[[2,_EI]]);});}));})):[2];}));}));}]);});}],_Ef));}var _EJ=_ED,_EK=_EJ;return _EK;}));});},_EL=new T(function(){return B(unCStr("activePlayer"));}),_EM=new T(function(){return B(unCStr("GameState"));}),_EN=new T(function(){return B(unCStr("board"));}),_EO=new T(function(){return B(unCStr("turnMode"));}),_EP=new T(function(){return B(unCStr("pointsW"));}),_EQ=new T(function(){return B(unCStr("pointsB"));}),_ER=function(_ES,_ET){return _ES>11?[2]:[1,function(_EU){return new F(function(){return A(_v3,[_EU,function(_EV){return E(new T(function(){return B(_wj(function(_EW){var _EX=E(_EW);return _EX[0]==3?!B(_md(_EX[1],_EM))?[2]:E([1,function(_EY){return new F(function(){return A(_v3,[_EY,function(_EZ){return E(new T(function(){return B(_wj(function(_F0){var _F1=E(_F0);return _F1[0]==2?!B(_md(_F1[1],_Cj))?[2]:E([1,function(_F2){return new F(function(){return A(_v3,[_F2,function(_F3){return E(new T(function(){return B(_wj(function(_F4){var _F5=E(_F4);return _F5[0]==3?!B(_md(_F5[1],_EL))?[2]:E([1,function(_F6){return new F(function(){return A(_v3,[_F6,function(_F7){return E(new T(function(){return B(_wj(function(_F8){var _F9=E(_F8);return _F9[0]==2?!B(_md(_F9[1],_Cw))?[2]:E(new T(function(){return B(A(_z7,[_z4,_wV,function(_Fa){return [1,function(_Fb){return new F(function(){return A(_v3,[_Fb,function(_Fc){return E(new T(function(){return B(_wj(function(_Fd){var _Fe=E(_Fd);return _Fe[0]==2?!B(_md(_Fe[1],_Cy))?[2]:E([1,function(_Ff){return new F(function(){return A(_v3,[_Ff,function(_Fg){return E(new T(function(){return B(_wj(function(_Fh){var _Fi=E(_Fh);return _Fi[0]==3?!B(_md(_Fi[1],_EO))?[2]:E([1,function(_Fj){return new F(function(){return A(_v3,[_Fj,function(_Fk){return E(new T(function(){return B(_wj(function(_Fl){var _Fm=E(_Fl);return _Fm[0]==2?!B(_md(_Fm[1],_Cw))?[2]:E(new T(function(){return B(A(_z7,[_Eb,_wV,function(_Fn){return [1,function(_Fo){return new F(function(){return A(_v3,[_Fo,function(_Fp){return E(new T(function(){return B(_wj(function(_Fq){var _Fr=E(_Fq);return _Fr[0]==2?!B(_md(_Fr[1],_Cy))?[2]:E([1,function(_Fs){return new F(function(){return A(_v3,[_Fs,function(_Ft){return E(new T(function(){return B(_wj(function(_Fu){var _Fv=E(_Fu);return _Fv[0]==3?!B(_md(_Fv[1],_EN))?[2]:E([1,function(_Fw){return new F(function(){return A(_v3,[_Fw,function(_Fx){return E(new T(function(){return B(_wj(function(_Fy){var _Fz=E(_Fy);return _Fz[0]==2?!B(_md(_Fz[1],_Cw))?[2]:E(new T(function(){return B(A(_z7,[_DN,_wV,function(_FA){return [1,function(_FB){return new F(function(){return A(_v3,[_FB,function(_FC){return E(new T(function(){return B(_wj(function(_FD){var _FE=E(_FD);return _FE[0]==2?!B(_md(_FE[1],_Cy))?[2]:E([1,function(_FF){return new F(function(){return A(_v3,[_FF,function(_FG){return E(new T(function(){return B(_wj(function(_FH){var _FI=E(_FH);return _FI[0]==3?!B(_md(_FI[1],_EQ))?[2]:E([1,function(_FJ){return new F(function(){return A(_v3,[_FJ,function(_FK){return E(new T(function(){return B(_wj(function(_FL){var _FM=E(_FL);return _FM[0]==2?!B(_md(_FM[1],_Cw))?[2]:E(new T(function(){return B(_y1(_yn,_wV,function(_FN){return [1,function(_FO){return new F(function(){return A(_v3,[_FO,function(_FP){return E(new T(function(){return B(_wj(function(_FQ){var _FR=E(_FQ);return _FR[0]==2?!B(_md(_FR[1],_Cy))?[2]:E([1,function(_FS){return new F(function(){return A(_v3,[_FS,function(_FT){return E(new T(function(){return B(_wj(function(_FU){var _FV=E(_FU);return _FV[0]==3?!B(_md(_FV[1],_EP))?[2]:E([1,function(_FW){return new F(function(){return A(_v3,[_FW,function(_FX){return E(new T(function(){return B(_wj(function(_FY){var _FZ=E(_FY);return _FZ[0]==2?!B(_md(_FZ[1],_Cw))?[2]:E(new T(function(){return B(_y1(_yn,_wV,function(_G0){return [1,function(_G1){return new F(function(){return A(_v3,[_G1,function(_G2){return E(new T(function(){return B(_wj(function(_G3){var _G4=E(_G3);return _G4[0]==2?!B(_md(_G4[1],_Cq))?[2]:E(new T(function(){return B(A(_ET,[[0,_Fa,_Fn,_FA,_FN,_G0]]));})):[2];}));}));}]);});}];}));})):[2];}));}));}]);});}]):[2];}));}));}]);});}]):[2];}));}));}]);});}];}));})):[2];}));}));}]);});}]):[2];}));}));}]);});}]):[2];}));}));}]);});}];}]));})):[2];}));}));}]);});}]):[2];}));}));}]);});}]):[2];}));}));}]);});}];}]));})):[2];}));}));}]);});}]):[2];}));}));}]);});}]):[2];}));}));}]);});}];}]));})):[2];}));}));}]);});}]):[2];}));}));}]);});}]):[2];}));}));}]);});}]):[2];}));}));}]);});}];},_G5=function(_G6,_G7){return new F(function(){return _ER(E(_G6)[1],_G7);});},_G8=function(_G9){return [1,function(_Ga){return new F(function(){return A(_v3,[_Ga,function(_Gb){return E([3,_G9,_oa]);}]);});}];},_Gc=new T(function(){return B(A(_z7,[_G5,_wV,_G8]));}),_Gd=[0],_Ge=[0,_d4,_DQ,_jp,_28,_28],_Gf=[1,_Ge,_d],_Gg=[0,_Gf,_Gd],_Gh=function(_Gi,_Gj,_Gk,_Gl,_){var _Gm=jsPushState(_Gl),_Gn=jsTranslate(_Gl,_Gi,_Gj),_Go=B(A(_Gk,[[0,_Gl],_])),_Gp=_Go,_Gq=jsPopState(_Gl);return _iG;},_Gr=function(_Gs,_Gt,_){while(1){var _Gu=E(_Gs);if(!_Gu[0]){return _iG;}else{var _Gv=B(A(_Gu[1],[_Gt,_])),_Gw=_Gv;_Gs=_Gu[2];continue;}}},_Gx=function(_Gy,_Gz,_GA,_){var _GB=E(_Gy);return new F(function(){return _Gh(E(_GB[1])[1],E(_GB[2])[1],_Gz,E(_GA)[1],_);});},_GC=function(_GD,_GE){var _GF=E(_GD);return _GF[0]==0?[0]:[1,function(_GG,_){return new F(function(){return _Gx(_GF[1],_GE,_GG,_);});},new T(function(){return B(_GC(_GF[2],_GE));})];},_GH=[0,30],_GI=function(_GJ,_GK,_GL,_){var _GM=jsPushState(_GL),_GN=jsRotate(_GL,_GJ),_GO=B(A(_GK,[[0,_GL],_])),_GP=_GO,_GQ=jsPopState(_GL);return _iG;},_GR=function(_GS){return [0, -E(_GS)[1]];},_GT=[0,0],_GU=function(_GV,_){return _iG;},_GW=function(_GX){var _GY=E(_GX);if(!_GY[0]){return E(_GU);}else{var _GZ=E(_GY[1]);return function(_H0,_){var _H1=E(_H0)[1],_H2=jsMoveTo(_H1,E(_GZ[1])[1],E(_GZ[2])[1]);return new F(function(){return (function(_H3,_){while(1){var _H4=E(_H3);if(!_H4[0]){return _iG;}else{var _H5=E(_H4[1]),_H6=jsLineTo(_H1,E(_H5[1])[1],E(_H5[2])[1]);_H3=_H4[2];continue;}}})(_GY[2],_);});};}},_H7=function(_H8,_H9,_){var _Ha=jsBeginPath(_H9),_Hb=B(A(_H8,[[0,_H9],_])),_Hc=_Hb,_Hd=jsStroke(_H9);return _iG;},_He=function(_Hf,_Hg,_){return new F(function(){return _H7(_Hf,E(_Hg)[1],_);});},_Hh=function(_Hi){var _Hj=new T(function(){return B(_GW([1,[0,_GT,new T(function(){return B(_GR(_Hi));})],[1,[0,_GT,_Hi],_d]]));});return function(_Hk,_){var _Hl=E(_Hk)[1],_Hm=jsBeginPath(_Hl),_Hn=B(A(_Hj,[[0,_Hl],_])),_Ho=_Hn,_Hp=jsStroke(_Hl),_Hq=jsPushState(_Hl),_Hr=jsRotate(_Hl,2.0943951023931953),_Hs=jsBeginPath(_Hl),_Ht=B(A(_Hj,[[0,_Hl],_])),_Hu=_Ht,_Hv=jsStroke(_Hl),_Hw=jsPopState(_Hl);return new F(function(){return _GI(4.1887902047863905,function(_GG,_){return new F(function(){return _He(_Hj,_GG,_);});},_Hl,_);});};},_Hx=new T(function(){return B(_Hh(_GH));}),_Hy=new T(function(){return B(_GC(_6u,_Hx));}),_Hz=[1,_d6,_d],_HA=function(_HB,_){return _iG;},_HC=new T(function(){return [0,"rgb("];}),_HD=new T(function(){return [0,"rgba("];}),_HE=new T(function(){return [0,toJSStr(_d)];}),_HF=[0,41],_HG=[1,_HF,_d],_HH=new T(function(){return [0,toJSStr(_HG)];}),_HI=[1,_HH,_d],_HJ=[0,44],_HK=[1,_HJ,_d],_HL=new T(function(){return [0,toJSStr(_HK)];}),_HM=function(_HN){var _HO=String(E(_HN)[1]),_HP=_HO;return [0,_HP];},_HQ=function(_HR){var _HS=E(_HR);if(!_HS[0]){var _HT=jsCat([1,_HC,[1,new T(function(){return B(_HM(_HS[1]));}),[1,_HL,[1,new T(function(){return B(_HM(_HS[2]));}),[1,_HL,[1,new T(function(){return B(_HM(_HS[3]));}),_HI]]]]]],E(_HE)[1]),_HU=_HT;return E(_HU);}else{var _HV=jsCat([1,_HD,[1,new T(function(){return B(_HM(_HS[1]));}),[1,_HL,[1,new T(function(){return B(_HM(_HS[2]));}),[1,_HL,[1,new T(function(){return B(_HM(_HS[3]));}),[1,_HL,[1,new T(function(){return B(_HM(_HS[4]));}),_HI]]]]]]]],E(_HE)[1]),_HW=_HV;return E(_HW);}},_HX=[0,228],_HY=[0,173],_HZ=[0,38],_I0=[0,_HZ,_HY,_HX],_I1=new T(function(){return [0,"fillStyle"];}),_I2=[0,81],_I3=[0,231],_I4=[0,209],_I5=[0,_I4,_I3,_I2],_I6=function(_I7,_I8,_I9,_Ia,_){var _Ib=jsMoveTo(_Ia,_I7+_I9,_I8),_Ic=jsArc(_Ia,_I7,_I8,_I9,0,6.283185307179586);return _iG;},_Id=function(_Ie,_){return new F(function(){return _I6(0,0,20,E(_Ie)[1],_);});},_If=function(_Ig){return function(_Ih,_){var _Ii=E(_Ih)[1],_Ij=jsSet(_Ii,E(_I1)[1],E(new T(function(){if(!E(_Ig)){var _Ik=[0,B(_HQ(_I0))];}else{var _Ik=[0,B(_HQ(_I5))];}return _Ik;}))[1]),_Il=jsBeginPath(_Ii),_Im=jsMoveTo(_Ii,20,0),_In=jsArc(_Ii,0,0,20,0,6.283185307179586),_Io=jsFill(_Ii);return new F(function(){return _H7(_Id,_Ii,_);});};},_Ip=function(_Iq){return function(_Ir,_){var _Is=jsSet(E(_Ir)[1],E(_I1)[1],E(new T(function(){return [0,B(_HQ(_Iq))];}))[1]);return _iG;};},_It=[0,255],_Iu=[0,_It,_It,_It],_Iv=new T(function(){return B(_Ip(_Iu));}),_Iw=[0,22],_Ix=new T(function(){return B(_Hh(_Iw));}),_Iy=function(_Iz,_IA){return function(_IB,_){var _IC=E(_IB),_ID=_IC[1],_IE=jsSet(_ID,E(_I1)[1],E(new T(function(){if(!E(_Iz)){var _IF=[0,B(_HQ(_I0))];}else{var _IF=[0,B(_HQ(_I5))];}return _IF;}))[1]),_IG=jsBeginPath(_ID),_IH=jsMoveTo(_ID,28,0),_II=jsArc(_ID,0,0,28,0,6.283185307179586),_IJ=jsFill(_ID),_IK=jsBeginPath(_ID),_IL=jsMoveTo(_ID,28,0),_IM=jsArc(_ID,0,0,28,0,6.283185307179586),_IN=jsStroke(_ID),_IO=B(A(_Iv,[_IC,_])),_IP=_IO,_IQ=jsBeginPath(_ID),_IR=jsMoveTo(_ID,22,0),_IS=jsArc(_ID,0,0,22,0,6.283185307179586),_IT=jsFill(_ID),_IU=jsBeginPath(_ID),_IV=jsMoveTo(_ID,22,0),_IW=jsArc(_ID,0,0,22,0,6.283185307179586),_IX=jsStroke(_ID);return !E(_IA)?_iG:B(A(_Ix,[_IC,_]));};},_IY=function(_IZ){return function(_J0,_){var _J1=B(_Gr(_Hy,_J0,_)),_J2=_J1;return new F(function(){return A(new T(function(){var _J3=function(_J4){var _J5=E(_J4);if(!_J5[0]){return E(_HA);}else{var _J6=_J5[1],_J7=function(_J8,_J9,_){var _Ja=E(_J8),_Jb=60*E(_Ja[1])[1];return new F(function(){return _Gh(0.5*Math.sqrt(3)*_Jb+300, -(60*E(_Ja[2])[1])+0.5*_Jb+315,new T(function(){return B(_Iy(_J6,_eX));}),E(_J9)[1],_);});},_Jc=function(_Jd,_Je,_){var _Jf=E(_Jd),_Jg=60*E(_Jf[1])[1];return new F(function(){return _Gh(0.5*Math.sqrt(3)*_Jg+300, -(60*E(_Jf[2])[1])+0.5*_Jg+315,new T(function(){return B(_If(_J6));}),E(_Je)[1],_);});};return function(_Jh,_){var _Ji=B(A(new T(function(){if(!E(_J6)){var _Jj=function(_oB,_Jk){return new F(function(){return (function(_Jl,_Jm,_){while(1){var _Jn=E(_Jl);if(!_Jn[0]){return _iG;}else{var _Jo=B(_Jc(_Jn[1],_Jm,_)),_Jp=_Jo;_Jl=_Jn[2];continue;}}})(E(_IZ)[4],_oB,_Jk);});};}else{var _Jj=function(_oB,_Jk){return new F(function(){return (function(_Jq,_Jr,_){while(1){var _Js=E(_Jq);if(!_Js[0]){return _iG;}else{var _Jt=B(_Jc(_Js[1],_Jr,_)),_Ju=_Jt;_Jq=_Js[2];continue;}}})(E(_IZ)[5],_oB,_Jk);});};}return _Jj;}),[_Jh,_])),_Jv=_Ji,_Jw=B(A(new T(function(){if(!E(_J6)){var _Jx=function(_oB,_Jk){return new F(function(){return (function(_Jy,_Jz,_){while(1){var _JA=E(_Jy);if(!_JA[0]){return _iG;}else{var _JB=B(_J7(_JA[1],_Jz,_)),_JC=_JB;_Jy=_JA[2];continue;}}})(E(_IZ)[2],_oB,_Jk);});};}else{var _Jx=function(_oB,_Jk){return new F(function(){return (function(_JD,_JE,_){while(1){var _JF=E(_JD);if(!_JF[0]){return _iG;}else{var _JG=B(_J7(_JF[1],_JE,_)),_JH=_JG;_JD=_JF[2];continue;}}})(E(_IZ)[3],_oB,_Jk);});};}return _Jx;}),[_Jh,_])),_JI=_Jw;return new F(function(){return A(new T(function(){return B(_J3(_J5[2]));}),[_Jh,_]);});};}};return B((function(_JJ){return function(_JK,_){var _JL=B(A(new T(function(){return function(_oB,_Jk){return new F(function(){return (function(_JM,_JN,_){while(1){var _JO=E(_JM);if(!_JO[0]){return _iG;}else{var _JP=E(_JO[1]),_JQ=60*E(_JP[1])[1],_JR=E(_JN),_JS=_JR[1],_JT=jsPushState(_JS),_JU=jsTranslate(_JS,0.5*Math.sqrt(3)*_JQ+300, -(60*E(_JP[2])[1])+0.5*_JQ+315),_JV=B(A(new T(function(){return B(_If(_d4));}),[[0,_JS],_])),_JW=_JV,_JX=jsPopState(_JS);_JM=_JO[2];_JN=_JR;continue;}}})(E(_IZ)[4],_oB,_Jk);});};}),[_JK,_])),_JY=_JL,_JZ=B(A(new T(function(){return function(_oB,_Jk){return new F(function(){return (function(_K0,_K1,_){while(1){var _K2=E(_K0);if(!_K2[0]){return _iG;}else{var _K3=E(_K2[1]),_K4=60*E(_K3[1])[1],_K5=E(_K1),_K6=_K5[1],_K7=jsPushState(_K6),_K8=jsTranslate(_K6,0.5*Math.sqrt(3)*_K4+300, -(60*E(_K3[2])[1])+0.5*_K4+315),_K9=B(A(new T(function(){return B(_Iy(_d4,_eX));}),[[0,_K6],_])),_Ka=_K9,_Kb=jsPopState(_K6);_K0=_K2[2];_K1=_K5;continue;}}})(E(_IZ)[2],_oB,_Jk);});};}),[_JK,_])),_Kc=_JZ;return new F(function(){return A(new T(function(){return B(_J3(_JJ));}),[_JK,_]);});};})(_Hz));}),[_J0,_]);});};},_Kd=function(_Ke,_Kf,_Kg){var _Kh=E(_Kg)==0?E(_Ke):E(_Kf),_Ki=function(_Kj){while(1){var _Kk=(function(_Kl){var _Km=E(_Kl);if(!_Km[0]){return E(_HA);}else{var _Kn=_Km[1],_Ko=_Km[2];if(!B(_fT(_Kh,_Kn))){_Kj=_Ko;return null;}else{return function(_Kp,_){var _Kq=E(_Kn),_Kr=E(_Kp),_Ks=_Kr[1],_Kt=jsPushState(_Ks),_Ku=60*E(_Kq[1])[1],_Kv=jsTranslate(_Ks,0.5*Math.sqrt(3)*_Ku+300, -(60*E(_Kq[2])[1])+0.5*_Ku+315),_Kw=jsBeginPath(_Ks),_Kx=jsMoveTo(_Ks,22,0),_Ky=jsArc(_Ks,0,0,22,0,6.283185307179586),_Kz=jsFill(_Ks),_KA=jsPopState(_Ks);return new F(function(){return A(new T(function(){return B(_Ki(_Ko));}),[_Kr,_]);});};}}})(_Kj);if(_Kk!=null){return _Kk;}}};return new F(function(){return _Ki(_Kh);});},_KB=function(_KC,_KD,_KE){return new F(function(){return A(_KC,[_KE,new T(function(){return B(_KB(_KC,_KD,new T(function(){return B(A(_KD,[_KE]));})));})]);});},_KF=[0,45],_KG=new T(function(){return [0,0.5*Math.sqrt(3)*(-300)+300];}),_KH=[0,_KG,_KF],_KI=[0,585],_KJ=new T(function(){return [0,0.5*Math.sqrt(3)*300+300];}),_KK=[0,_KJ,_KI],_KL=function(_KM,_KN){if(_KN>0){var _KO=function(_KP,_KQ,_KR,_KS,_){var _KT=jsPushState(_KS),_KU=jsTranslate(_KS,_KP,_KQ),_KV=B(A(new T(function(){return B(_Iy(_KM,_qA));}),[[0,_KS],_])),_KW=_KV,_KX=jsPopState(_KS);return new F(function(){return A(_KR,[[0,_KS],_]);});};return new F(function(){return A(_KB,[function(_KY,_KZ,_L0){return _L0>1?function(_L1,_){var _L2=E(_KY);return new F(function(){return _KO(E(_L2[1])[1],E(_L2[2])[1],new T(function(){return B(A(_KZ,[_L0-1|0]));}),E(_L1)[1],_);});}:function(_L3,_){var _L4=E(_KY);return new F(function(){return _KO(E(_L4[1])[1],E(_L4[2])[1],_HA,E(_L3)[1],_);});};},function(_L5){if(!E(_KM)){var _L6=E(_L5);return [0,new T(function(){return [0,E(_L6[1])[1]-20];}),_L6[2]];}else{var _L7=E(_L5);return [0,new T(function(){return [0,E(_L7[1])[1]+20];}),_L7[2]];}},new T(function(){return E(_KM)==0?E(_KK):E(_KH);}),_KN]);});}else{return E(_HA);}},_L8=function(_L9,_La,_Lb){return function(_Lc,_){var _Ld=jsDrawText(E(_Lc)[1],E(new T(function(){return [0,toJSStr(E(_Lb))];}))[1],E(_L9)[1],E(_La)[1]);return _iG;};},_Le=new T(function(){return [0,"font"];}),_Lf=function(_Lg,_Lh){return function(_Li,_){var _Lj=E(_Li),_Lk=_Lj[1],_Ll=E(_Le)[1],_Lm=jsGet(_Lk,_Ll),_Ln=_Lm,_Lo=jsSet(_Lk,_Ll,E(new T(function(){return [0,toJSStr(E(_Lg))];}))[1]),_Lp=B(A(_Lh,[_Lj,_])),_Lq=_Lp,_Lr=jsSet(_Lk,_Ll,_Ln);return _iG;};},_Ls=new T(function(){return B(unCStr("15px \'Open Sans\', sans-serif"));}),_Lt=new T(function(){return B(unCStr("Floyd is thinking ..."));}),_Lu=new T(function(){return [0,toJSStr(E(_Lt))];}),_Lv=function(_Lw,_){var _Lx=jsDrawText(E(_Lw)[1],E(_Lu)[1],420,20);return _iG;},_Ly=new T(function(){return B(_Lf(_Ls,_Lv));}),_Lz=new T(function(){return B(unCStr("Floyd wins!"));}),_LA=new T(function(){return [0,toJSStr(E(_Lz))];}),_LB=function(_LC,_){var _LD=jsDrawText(E(_LC)[1],E(_LA)[1],420,20);return _iG;},_LE=new T(function(){return B(_Lf(_Ls,_LB));}),_LF=new T(function(){return B(unCStr("You win!"));}),_LG=new T(function(){return [0,toJSStr(E(_LF))];}),_LH=function(_LI,_){var _LJ=jsDrawText(E(_LI)[1],E(_LG)[1],420,20);return _iG;},_LK=new T(function(){return B(_Lf(_Ls,_LH));}),_LL=[1,_2t,_d],_LM=[0,550],_LN=[0,620],_LO=new T(function(){return B(_7u(_7F,_7F));}),_LP=[0,0],_LQ=[0,_LP,_LP,_LP],_LR=new T(function(){return B(_Ip(_LQ));}),_LS=function(_LT,_LU,_LV,_){while(1){var _LW=E(_LT);if(!_LW[0]){return _iG;}else{var _LX=E(_LW[1]),_LY=jsPushState(_LU),_LZ=60*E(_LX[1])[1],_M0=jsTranslate(_LU,0.5*Math.sqrt(3)*_LZ+300, -(60*E(_LX[2])[1])+0.5*_LZ+315),_M1=B(A(_LR,[[0,_LU],_])),_M2=_M1,_M3=jsBeginPath(_LU),_M4=jsMoveTo(_LU,5,0),_M5=jsArc(_LU,0,0,5,0,6.283185307179586),_M6=jsFill(_LU),_M7=jsPopState(_LU);_LT=_LW[2];var _M8=_;_LV=_M8;continue;}}},_M9=function(_Ma,_Mb,_Mc,_){while(1){var _Md=E(_Ma);if(!_Md[0]){return _iG;}else{var _Me=E(_Md[1]),_Mf=jsPushState(_Mb),_Mg=60*E(_Me[1])[1],_Mh=jsTranslate(_Mb,0.5*Math.sqrt(3)*_Mg+300, -(60*E(_Me[2])[1])+0.5*_Mg+315),_Mi=jsBeginPath(_Mb),_Mj=jsMoveTo(_Mb,22,0),_Mk=jsArc(_Mb,0,0,22,0,6.283185307179586),_Ml=jsFill(_Mb),_Mm=jsPopState(_Mb);_Ma=_Md[2];var _Mn=_;_Mc=_Mn;continue;}}},_Mo=[0,0.5],_Mp=[1,_It,_LP,_LP,_Mo],_Mq=new T(function(){return B(_Ip(_Mp));}),_Mr=function(_Ms,_Mt,_Mu,_Mv){var _Mw=E(_Mt);switch(_Mw[0]){case 0:var _Mx=E(_Mu),_My=_Mx[1],_Mz=_Mx[2];if(!B(_el(_My,_Mz,E(_Ms)[1]))){var _MA=new T(function(){return [0,60*E(_My)[1]];});return function(_MB,_){return new F(function(){return _Gh(E(new T(function(){return [0,0.5*Math.sqrt(3)*E(_MA)[1]+300];}))[1],E(new T(function(){return [0, -(60*E(_Mz)[1])+0.5*E(_MA)[1]+315];}))[1],new T(function(){return B(_Iy(_Mv,_eX));}),E(_MB)[1],_);});};}else{return E(_HA);}break;case 1:return !B(_eC(_LO,_Mu,B(_gU(_Mv,_Ms))))?E(_HA):function(_MC,_){var _MD=E(new T(function(){var _ME=E(_Mu),_MF=new T(function(){return [0,60*E(_ME[1])[1]];});return [0,new T(function(){return [0,0.5*Math.sqrt(3)*E(_MF)[1]+300];}),new T(function(){return [0, -(60*E(_ME[2])[1])+0.5*E(_MF)[1]+315];})];}));return new F(function(){return _Gh(E(_MD[1])[1],E(_MD[2])[1],new T(function(){return B(_If(_Mv));}),E(_MC)[1],_);});};case 2:var _MG=new T(function(){return B(_gK(_Ms,_Mw[1]));}),_MH=new T(function(){if(!B(_eC(_LO,_Mu,_MG))){var _MI=E(_HA);}else{var _MI=function(_MJ,_){var _MK=E(new T(function(){var _ML=E(_Mu),_MM=new T(function(){return [0,60*E(_ML[1])[1]];});return [0,new T(function(){return [0,0.5*Math.sqrt(3)*E(_MM)[1]+300];}),new T(function(){return [0, -(60*E(_ML[2])[1])+0.5*E(_MM)[1]+315];})];}));return new F(function(){return _Gh(E(_MK[1])[1],E(_MK[2])[1],new T(function(){return B(_Iy(_Mv,_eX));}),E(_MJ)[1],_);});};}return _MI;});return function(_MN,_){var _MO=E(_MG);if(!_MO[0]){return new F(function(){return A(_MH,[_MN,_]);});}else{var _MP=E(_MO[1]),_MQ=E(_MN),_MR=_MQ[1],_MS=jsPushState(_MR),_MT=60*E(_MP[1])[1],_MU=jsTranslate(_MR,0.5*Math.sqrt(3)*_MT+300, -(60*E(_MP[2])[1])+0.5*_MT+315),_MV=B(A(_LR,[[0,_MR],_])),_MW=_MV,_MX=jsBeginPath(_MR),_MY=jsMoveTo(_MR,5,0),_MZ=jsArc(_MR,0,0,5,0,6.283185307179586),_N0=jsFill(_MR),_N1=jsPopState(_MR),_N2=B(_LS(_MO[2],_MR,_,_)),_N3=_N2;return new F(function(){return A(_MH,[_MQ,_]);});}};case 3:return function(_N4,_){var _N5=B(A(_Mq,[_N4,_])),_N6=_N5,_N7=E(new T(function(){return B(_gX(new T(function(){if(!E(_Mv)){var _N8=E(E(_Ms)[4]);}else{var _N8=E(E(_Ms)[5]);}return _N8;}),_Mu));}));if(!_N7[0]){return _iG;}else{var _N9=E(_N7[1]),_Na=E(_N4)[1],_Nb=jsPushState(_Na),_Nc=60*E(_N9[1])[1],_Nd=jsTranslate(_Na,0.5*Math.sqrt(3)*_Nc+300, -(60*E(_N9[2])[1])+0.5*_Nc+315),_Ne=jsBeginPath(_Na),_Nf=jsMoveTo(_Na,22,0),_Ng=jsArc(_Na,0,0,22,0,6.283185307179586),_Nh=jsFill(_Na),_Ni=jsPopState(_Na);return new F(function(){return _M9(_N7[2],_Na,_,_);});}};default:return E(_HA);}},_Nj=function(_Nk,_Nl){var _Nm=new T(function(){var _Nn=E(_Nl);if(!_Nn[0]){var _No=E(_HA);}else{var _Np=_Nn[1],_Nq=E(_Nk),_Nr=_Nq[1];if(E(E(_Nq[4])[1])==3){var _Ns=E(_HA);}else{if(E(E(_Nq[5])[1])==3){var _Nt=E(_HA);}else{var _Nt=function(_Nu,_){var _Nv=B(A(new T(function(){return B(_Lf(_Ls,new T(function(){return B(_L8(_LM,_LN,new T(function(){var _Nw=E(_Np);return [1,_2u,new T(function(){return B(A(_2H,[_29,[1,function(_Nx){return new F(function(){return _2v(0,E(_Nw[1])[1],_Nx);});},[1,function(_Ny){return new F(function(){return _2v(0,E(_Nw[2])[1],_Ny);});},_d]],_LL]));})];})));})));}),[_Nu,_])),_Nz=_Nv,_NA=B(A(new T(function(){return B(_Mr(_Nq[3],_Nq[2],_Np,_Nr));}),[_Nu,_])),_NB=_NA;return E(_Nr)==0?_iG:B(A(_Ly,[_Nu,_]));};}var _NC=_Nt,_Ns=_NC;}var _ND=_Ns,_NE=_ND,_No=_NE;}return _No;}),_NF=new T(function(){var _NG=E(_Nk),_NH=_NG[3];if(E(_NG[2])[0]==3){var _NI=function(_NJ){var _NK=E(_NJ);return _NK[0]==0?E(_HA):function(_NL,_){var _NM=B(A(new T(function(){var _NN=E(_NH);return B(_Kd(_NN[4],_NN[5],_NK[1]));}),[_NL,_])),_NO=_NM;return new F(function(){return A(new T(function(){return B(_NI(_NK[2]));}),[_NL,_]);});};},_NP=B((function(_NQ,_NR){return function(_NS,_){var _NT=B(A(new T(function(){var _NU=E(_NH);return B(_Kd(_NU[4],_NU[5],_NQ));}),[_NS,_])),_NV=_NT;return new F(function(){return A(new T(function(){return B(_NI(_NR));}),[_NS,_]);});};})(_d4,_Hz));}else{var _NP=E(_HA);}var _NW=_NP;return _NW;}),_NX=new T(function(){return B(_KL(_d6,E(E(_Nk)[5])[1]));}),_NY=new T(function(){return B(_KL(_d4,E(E(_Nk)[4])[1]));}),_NZ=new T(function(){return B(_IY(new T(function(){return E(E(_Nk)[3]);})));});return function(_O0,_){var _O1=E(_Nk);if(E(E(_O1[4])[1])==3){var _O2=B(A(_LK,[_O0,_])),_O3=_O2,_O4=B(A(_NZ,[_O0,_])),_O5=_O4,_O6=B(A(_NY,[_O0,_])),_O7=_O6,_O8=B(A(_NX,[_O0,_])),_O9=_O8,_Oa=B(A(_NF,[_O0,_])),_Ob=_Oa;return new F(function(){return A(_Nm,[_O0,_]);});}else{if(E(E(_O1[5])[1])==3){var _Oc=B(A(_LE,[_O0,_])),_Od=_Oc,_Oe=B(A(_NZ,[_O0,_])),_Of=_Oe,_Og=B(A(_NY,[_O0,_])),_Oh=_Og,_Oi=B(A(_NX,[_O0,_])),_Oj=_Oi,_Ok=B(A(_NF,[_O0,_])),_Ol=_Ok;return new F(function(){return A(_Nm,[_O0,_]);});}else{var _Om=B(A(_NZ,[_O0,_])),_On=_Om,_Oo=B(A(_NY,[_O0,_])),_Op=_Oo,_Oq=B(A(_NX,[_O0,_])),_Or=_Oq,_Os=B(A(_NF,[_O0,_])),_Ot=_Os;return new F(function(){return A(_Nm,[_O0,_]);});}}};},_Ou=function(_Ov){while(1){var _Ow=(function(_Ox){var _Oy=E(_Ox);if(!_Oy[0]){return [0];}else{var _Oz=_Oy[2],_OA=E(_Oy[1]);if(!E(_OA[2])[0]){return [1,_OA[1],new T(function(){return B(_Ou(_Oz));})];}else{_Ov=_Oz;return null;}}})(_Ov);if(_Ow!=null){return _Ow;}}},_OB=function(_OC,_){return _OC;},_OD=function(_){var _=0,_OE=jsMkStdout(),_OF=_OE;return [0,_OF];},_OG=function(_OH){var _OI=B(A(_OH,[_])),_OJ=_OI;return E(_OJ);},_OK=new T(function(){return B(_OG(_OD));}),_OL=function(_OM,_ON,_){var _OO=B(A(_OM,[_])),_OP=_OO;return new F(function(){return A(_ON,[_]);});},_OQ=function(_,_OR){var _OS=E(_OR);if(!_OS[0]){return new F(function(){return _1S(_jK,_);});}else{var _OT=jsFind("canvas"),_OU=_OT,_OV=E(_OU);if(!_OV[0]){return new F(function(){return _1S(_jL,_);});}else{var _OW=nMV(_Gg),_OX=_OW,_OY=E(_OS[1]),_OZ=_OY[1],_P0=E(_OY[2])[1],_P1=jsResetCanvas(_P0),_P2=B(A(_IY,[_jp,_OZ,_])),_P3=_P2,_P4=E(_OV[1])[1],_P5=jsSetCB(_P4,E(_jr)[1],function(_P6,_){var _P7=rMV(_OX),_P8=_P7,_P9=E(_P8),_Pa=E(_P9[1]);if(!_Pa[0]){return new F(function(){return _1S(_jf,_);});}else{switch(E(_P9[2])[0]){case 0:var _Pb=jsResetCanvas(_P0);return new F(function(){return A(_Nj,[_Pa[1],[1,new T(function(){return B(_jm(_P6));})],_OZ,_]);});break;case 1:return _iG;case 2:return _iG;default:return _iG;}}}),_Pc=_P5,_Pd=jsSetCB(_P4,E(_jq)[1],function(_Pe,_){switch(E(E(_Pe)[1])){case 37:var _Pf=rMV(_OX),_Pg=_Pf,_Ph=E(_Pg),_Pi=_Ph[1],_Pj=_Ph[2],_Pk=B(_6V(_Pi,0));if(_Pk<=1){return _iG;}else{var _Pl=function(_Pm){var _=wMV(_OX,[0,_Pi,_jN]),_Pn=jsResetCanvas(_P0);return new F(function(){return A(_Nj,[new T(function(){return B(_5(_Pi,1));}),_0,_OZ,_]);});},_Po=function(_Pp){var _Pq=E(_Pj);if(_Pq[0]==3){var _Pr=E(_Pq[1])[1];if((_Pr+1|0)>=_Pk){return _iG;}else{var _=wMV(_OX,[0,_Pi,[3,[0,_Pr+1|0]]]),_Ps=jsResetCanvas(_P0),_Pt=new T(function(){var _Pu=_Pr+1|0;return _Pu>=0?B(_5(_Pi,_Pu)):E(_2);}),_Pv=B(A(_Nj,[_Pt,_0,_OZ,_])),_Pw=_Pv;return new F(function(){return _jz(_OK,B(unAppCStr("DEBUG: let gs = ",new T(function(){var _Px=E(_Pt);return B(A(_4F,[0,_Px[1],_Px[2],_Px[3],_Px[4],_Px[5],_d]));}))),_);});}}else{return _iG;}};switch(E(_Pj)[0]){case 0:var _Py=B(_Pl(_)),_Pz=_Py;return _iG;case 1:var _PA=B(_Po(_)),_PB=_PA;return _iG;case 2:var _PC=B(_Pl(_)),_PD=_PC;return _iG;default:var _PE=B(_Po(_)),_PF=_PE;return _iG;}}break;case 39:var _PG=rMV(_OX),_PH=_PG,_PI=E(_PH),_PJ=_PI[1],_PK=E(_PI[2]);if(_PK[0]==3){var _PL=_PK[1],_=wMV(_OX,[0,_PJ,new T(function(){var _PM=E(E(_PL)[1]);if(_PM==1){var _PN=[0];}else{var _PN=[3,[0,_PM-1|0]];}var _PO=_PN;return _PO;})]),_PP=jsResetCanvas(_P0),_PQ=new T(function(){var _PR=E(_PL)[1]-1|0;return _PR>=0?B(_5(_PJ,_PR)):E(_2);}),_PS=B(A(_Nj,[_PQ,_0,_OZ,_])),_PT=_PS;return new F(function(){return _jz(_OK,B(unAppCStr("DEBUG: let gs = ",new T(function(){var _PU=E(_PQ);return B(A(_4F,[0,_PU[1],_PU[2],_PU[3],_PU[4],_PU[5],_d]));}))),_);});}else{return _iG;}break;default:return _iG;}}),_PV=_Pd,_PW=jsSetCB(_P4,E(_js)[1],function(_PX,_PY,_){var _PZ=rMV(_OX),_Q0=_PZ,_Q1=E(_Q0),_Q2=E(_Q1[1]);if(!_Q2[0]){return new F(function(){return _1S(_jg,_);});}else{switch(E(_Q1[2])[0]){case 0:var _Q3=jsResetCanvas(_P0),_Q4=new T(function(){var _Q5=E(_Q2[1]),_Q6=B(_hd(_Q5[1],_Q5[2],_Q5[3],_Q5[4],_Q5[5],new T(function(){return B(_jm(_PY));})));return _Q6[0]==0?E(_Q5):E(_Q6[1]);}),_Q7=[1,new T(function(){var _Q8=E(_PY);return B(_6B(_Q8[1],_Q8[2]));})],_Q9=B(A(_Nj,[_Q4,_Q7,_OZ,_])),_Qa=_Q9,_Qb=B(_jz(_OK,B(unAppCStr("DEBUG: let gs = ",new T(function(){var _Qc=E(_Q4);return B(A(_4F,[0,_Qc[1],_Qc[2],_Qc[3],_Qc[4],_Qc[5],_d]));}))),_)),_Qd=_Qb,_Qe=E(_Q4),_Qf=_Qe[4],_Qg=_Qe[5];if(!E(_Qe[1])){var _=wMV(_OX,[0,[1,_Qe,_Q2],new T(function(){if(E(E(_Qf)[1])==3){var _Qh=[2];}else{var _Qh=E(E(_Qg)[1])==3?[2]:[0];}var _Qi=_Qh;return _Qi;})]);return _iG;}else{var _Qj=E(_Qf);if(E(_Qj[1])==3){var _=wMV(_OX,[0,[1,_Qe,_Q2],_jd]);return _iG;}else{var _Qk=E(_Qg);if(E(_Qk[1])==3){var _=wMV(_OX,[0,[1,_Qe,_Q2],_jd]);return _iG;}else{var _=wMV(_OX,[0,[1,_Qe,_Q2],_je]),_Ql=B(_iS(_jh,_OL,_OB,_1W,_jG,_jc,_jI,[1,[0,_jJ,new T(function(){return B(A(_4F,[0,_d6,_Qe[2],_Qe[3],_Qj,_Qk,_d]));})],_d],function(_Qm,_){var _Qn=E(_Qm);if(!_Qn[0]){return _iG;}else{var _Qo=jsResetCanvas(_P0),_Qp=new T(function(){var _Qq=B(_Ou(B(_nm(_Gc,_Qn[1]))));return _Qq[0]==0?E(_jP):E(_Qq[2])[0]==0?E(_Qq[1]):E(_jR);}),_Qr=B(A(_Nj,[_Qp,_Q7,_OZ,_])),_Qs=_Qr,_=wMV(_OX,[0,[1,_Qp,[1,_Qe,_Q2]],new T(function(){var _Qt=E(_Qp);if(E(E(_Qt[4])[1])==3){var _Qu=[2];}else{var _Qu=E(E(_Qt[5])[1])==3?[2]:[0];}var _Qv=_Qu,_Qw=_Qv;return _Qw;})]);return _iG;}})),_Qx=jsSetTimeout(0,_Ql);return _iG;}}}break;case 1:return _iG;case 2:return _iG;default:return _iG;}}}),_Qy=_PW;return _iG;}}},_Qz=function(_){var _QA=jsFind("canvas"),_QB=_QA,_QC=E(_QB);if(!_QC[0]){return new F(function(){return _OQ(_,_0);});}else{var _QD=E(_QC[1])[1],_QE=jsHasCtx2D(_QD),_QF=_QE;if(!E(_QF)){return new F(function(){return _OQ(_,_0);});}else{var _QG=jsGetCtx2D(_QD),_QH=_QG;return new F(function(){return _OQ(_,[1,[0,[0,_QH],[0,_QD]]]);});}}},_QI=function(_){return new F(function(){return _Qz(_);});};
var hasteMain = function() {B(A(_QI, [0]));};window.onload = hasteMain;
