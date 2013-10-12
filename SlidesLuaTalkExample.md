% Haskell Reactive Game Programming with Netwire, Vinyl and OpenGL
% Handré Stolp
% 14 October 2013

Introduction
========================================

What is Lua
----------------------------------------
* It is an embeddable scripting language
* It is dynamically typed
* It runs on a register-based virtual machine
* It has automatic memory management with incremental GC
* It has simple procedural _C_ and _Pascal_ like syntax
* It has good support for functional and declarative paradigms
    * Functions are first class objects
    * Lexically scoped closures
    * Tail recursion
* Its is portable and minimalist
	* Portable ANSI C
	* Does not come with batteries
	* Can squeeze runtime and standard libraries into approximately 98 KB (usually 182K to 243K)

Where did come from where is it used
---------------------------------------
* Tecgraf group at PUC-Rio Brazil
* Lua means moon in Portugese
* Used a lot in gaming (World of Warcaft, Angry Birds)
* Used to add scripting to applications (SciTE, WireShark, Awesome)
* Full blown systems 
    * Premake - cross platform building
    * Sputnik - web content management system
* Embedded systems through eLua
    * Targets running directly on microcontroller
    * eLuaBrain - eLua based full dev environment on MCU
* Mobile - Supported on iOS and Android

What flavours are there
---------------------------------------
Lua 5.1.x 
:   Most widely used standard Lua distribution
:   Supports any system that can compile ANSI C
Lua 5.2.x
:    The newest Lua. 
:    Not that widely supported yet.
:    Supports any system that can compile ANSI C
LuaJit
:   Blindingly fast just in time compiler that is Lua 5.1.x compatible.
:   Supports dynamic FFI to _C_ shared libraries
:   
    Supported OS

    * Linux
    * Android
    * BSD
    * OSX
    * iOS
    * Windows

:   
    Supported CPU

    * x86 32 bit 64 bit
    * ARMv5+, ARM9E+
    * PPC/e500v2
    * MIPS

How to get it
------------------------------------------------------
* Get the source and compile into your application
    * <http://www.lua.org/versions.html>
    * <http://luajit.org/download.html>
* Use LuaRocks <http://luarocks.org/>
    * Deployment and management system for Lua modules
* Use LuaDist <http://luadist.org/>
    * Deployment and management system for Lua modules
* Install Lua for Windows <https://code.google.com/p/luaforwindows/>
    * Batteries included installation.

The Language 
========================================

Comments
----------------------------------------

```lua
-- This is a single line comment
--[[ This is a multiline 
     comment
--]]
--[=[ This is a multiline 
     comment
    --[[ 
        With a nested multiline 
        comment
    --]]

    Any amount of `=` allowed the start and end count must just match
--]=]
```

Statements
----------------------------------------
* Each line is a new statement and default scope is global

    ```lua
    a = 1       -- global variable 
    local b = 2 -- prepend with 'local' to bind to lexical scope
    ```

* Standard standard *C* and *Pascal* like binary operators

    ```lua
    e = a + b       -- add
    e = a - b       -- subtract
    e = a * b       -- multiply
    e = a % b       -- modulus
    e = a ^ b       -- a to power b
    e = a == b      -- equality
    e = a ~= b      -- inequality
    e = a < b       -- less than
    e = a > b       -- greater than
    e = a <= b      -- less than equal
    e = a >= b;     -- greater than equal
    e = "ss" .. "XX"-- string concatenation
    ```

Statements continued
----------------------------------------
* Multiple assignment supported and multiple statements per line separated by **;**

    ```lua
    a, b = b, a         -- this swaps the values of a and b
    local c = 3; d = 4; -- multiple statements per line
    -- define local variable e with no value bound to it
    local e;            -- ';' not necessary but OK.
    ```

* Logical operators a bit different (**nil** and **false** is false everything else is true)

    ```lua
    -- stops on false or nil , returns last evaluated value
    e = a and b         -- if a true then b else false
    -- stops on true (not false or nil ), returns last evaluated value
    e = a or b          -- if a true then a else b
    -- Logically invert 
    e = not a           -- if a true then true else false
    -- using and and or for conditional assignment
    e = IAmNotDefined or 'So default value' 
    e = e and IAmNotDefinedButDoesntMatter
    ```

Types
----------------------------------------
All values in Lua are _first-class_ (stored in variables; passed as arguments; returned as results)

nil
:   The bottom type signifying nothing
boolean
:   The logical type can either be `true` or  `false`.
:   `nil` and `false` counts as false everything else is true.
number
:   The only arithmetic type. 
:   Runtime dependant usually `double` (could be `char`, `short`, `int`, `float`).
string
:   Immutable array of 8-bit `char` values.
:   8-bit clean i.e. may include embedded zeros `'\0'`


Types continued
----------------------------------------
function 
:   Any function defined in _Lua_
:   Any function defined in _C_ and exposed to _Lua_
:   Any compiled file or string of _Lua_ statements
thread 
:   Represents independent thread of execution.
:   Not _OS_ threads used for coroutines
userdata
:   Arbitrary _C_ data to be stored in _Lua_
:   Only operations are assignment and identity test
table
:   The only data structure in _Lua_
:   Associative arrays
:   Heterogeneous in value and key (any type except `nil`)

Strings
---------------------------------------
* Escaped strings
    * `'Some\n "blah" string'` don't need to escape `"`
    * `"Some\n 'blah' string"` don't need to escape `'`
* Literal strings

    ```lua
    local someString = [[
    first new line is ignored
    this is a new line but this \n is not
    ]]

    local anotherString = [=[the string does not end here]]
    [[some text]]
    the string ends here]=]
    ```

Tables
--------------------------------------
* Indexing

    ```lua
    a = t[1]        -- access array element 1
    a = t['blah']   -- access element string key 'blah'
    a = t.blah      -- syntactic sugar for t['blah']
    ```

* Constructors

    ```lua
    -- empty table
    t = {}

    -- simple array elements are t[1], t[2], t[3]
    t = {"blah", 1, true}

    -- simple map elements t['blah'], t['foo']
    t = {blah = 5, foo=true}

    -- Explicit keys
    t = {['blah'] = 5, [100] = true}

    -- mixed elements at t[1], t[2], t[100], t.foo t.bar
    t = 
    {   'alpha', 'bravo';   -- element separator either , or ;
        foo = 5; bar=4
        [100] = false,
    }
    ```
* Insertion and deletion

    ```lua 
    t.foo = 5   -- Insert value 5 at key foo
    t.foo = nil -- Delete key foo from table
    ```

Functions
---------------------------------------------
* All functions have an environment
    * The environment is a Lua table
    * Global write inserts into the table
    * Global read performs table lookup
    * Local values shadow global values
* A Lua file is actually a function
* Function definition

    ```lua
        function (arg1, arg2) return arg1 + arg2 end
    ```

    * Start function scope with keyword `function`
    * Follow it with argument list `(a1, a2, ...)`
    * End function scope with keyword `end`
    * All statements between start and end are in function scope
    * Optional keyword `return` returns functions result 
* Global function assignment

    ```lua
    -- This is syntax sugar
    function arb (arg1, arg2) return arg1 + arg2 end
    -- for this assignment of a function to the global 'arb'
    arb = function (arg1, arg2) return arg1 + arg2 end 
    ```

* Local function assignment

    ```lua
    -- This is syntax sugar
    local function arb (arg1, arg2) return arg1 + arg2 end
    -- for this assignment of a function to the local 'arb'
    local arb = function (arg1, arg2) return arg1 + arg2 end 
    ```

Functions continued
---------------------------------------------
* Table function insertion

    ```lua 
    -- This is syntax sugar
    function t.arb (arg1, arg2) return arg1 + arg2 end
    -- for this insertion of a function into table t at key arb
    t['arb'] = function (arg1, arg2) return arg1 + arg2 end
    ```

* Table function insertion with implicit `self`

    ```lua 
    -- This is syntax sugar
    function t:arb (arg1, arg2) 
        return arg1 + arg2 + self[1]
    end
    -- for this insertion of a function into table t at key arb
    -- and implicit self parameter
    t['arb'] = function (self, arg1, arg2) 
        return arg1 + arg2 + self[1]
    end
    ```

* Function called by applying argument list to function value

    ```lua
    local arb = function (a) return a + a end
    local x = arb(2)    -- x is 4
    ```

* Calling function implicitly passing `self`

    ```lua
    local t = {
        arb = function(self, a) 
            return self.x + a 
        end; 
        x = 1; -- trailing delimiter is fine
    }
    -- This is syntax sugar for 
    local b = t:arb(2)
    -- this lookup of arb in t and passing it t and 2
    local c = t.arb(t, 2)
    ```

Functions continued
---------------------------------------------
* Parentheses optional when calling function with literal string 

    ```lua 
    -- This is syntax sugar
    print "blah"
    -- for this
    print("blah")
    ```

* Parentheses optional when calling function with table constructor

    ```lua 
    -- This is syntax sugar
    ipairs {1,2,3,4}
    -- for this
    ipairs ({1,2,3,4})
    ```

* Function return types are dynamic

    ```lua 
    function arb(x)
        if      x == 1      then   return 3,4
        elseif  x == 'j'    then   return 'bob',8
        elseif  x == 'm'    then   return nil,'s'
        elseif  x == 2      then   return 4
        else
            -- returning nothing
        end
    end
    a,b = arb(1)    -- a == 3, b == 4
    a,b = arb('j')  -- a == 'bob', b == 8
    a,b = arb('m')  -- a == nil, b == 's'
    a,b = arb(2)    -- a == 4, b == nil
    a,b = arb(3)    -- a == nil, b == nil
    ```

Functions continued 
---------------------------------------------
* Functions support closures

    ```lua
    local function makeCounter()
        local count = -1            -- local variable
        return function ()          -- return the function
            -- capture `count` updating on each invocation
            return count = count + 1
        end
    end
    local c1 = makeCounter()
    local _1, _2, _3 = c1(), c1(), c1() -- _1 == 0, _2 == 1, _3 == 2
    local c2 = makeCounter()
    _1, _2, _3 = c2(), c1(), c2()       -- _1 == 0, _2 == 4, _3 == 1
    ```

* Functions support tail call recursion

    ```lua
    function fib(n)
        local _fib(fn2, fn1, at)
            if at == n then 
                return fn1 + fn2
            else
                return _fib(fn1, fn1 + fn2, at+1)
            end
        end
        if n <= 0 then return 0
        elseif n == 1 then return 1
        else return _fib(0, 1, 2)
    end
    -- no stack overflow
    local c = fib(10000)
    ```

Control structures
----------------------------------------
do block end
:   Introduces local scope

    ```lua
    do
        local onlyVisibleHere = 5
    end
    ```
if then else
:   Conditional branching with each branch introducing a local scope.

    ```lua
    if a == b then      -- start if branch
        e = 2
    elseif a == c then  -- optional else if branch
        e = 3 
    else                -- optional else branch
        e = 4
    end                 -- required terminator
    ```

Control structures continued
----------------------------------------
while
:   If condition is true then repeatedly execute block until false

    ```lua
    local a,b = 1,5
    while a < b do
        a = a + 1       -- executes 4 times
    end
    ```
repeat
:   Execute block and if condition is true repeat until false

    ```lua
    local a,b = 4,5
    repeat
       a = a + 1        -- executes 2 times
    until a == b
    ```
break
:   Forces enclosing looping structure to terminate immediately

    ```lua
    local a = 1
    while true do
        break
        a = a + 1       -- never executes a remains 1
    end
    ```

Control structures continued
----------------------------------------
numerical for
:   Repeat the block until initial value passes limit incrementing by optional step size. 
    Default step size is 1.

    ```lua
    local a = 1
    for u=1,10 do
        a = u       -- executes 10 times (at end a == 10)
    end
    for u=10,1,-1 do
        a = u       -- executes 10 times (at end a == 1)
    end
    ```

generic for
:   The generic for loop works over functions called iterators. On each iteration, the 
    iterator function is called to produce a new value, stopping when this new value is nil.

    ```lua
    local a = 1
    for k,v in ipairs {0,1,2,3,4,5,6,7,8,9} do
        a = k + v       -- executes 10 times (at end a == 10 + 9)
    end
    ```

Extensibility (Meta table)
-------------------------------------
* Can extend tables and userdata using meta tables
* Meta table is a plain _Lua_ table
* Meta table bound to userdata or table using `setmetatable(t, mt)` function
* Inserting functions at specific keys specialize behaviour
    * Arithmetic : `__add`, `__sub` etc.
    * Comparison : `__eq`, `__lt` and `__le`.
    * Key lookup and new key insertion : `__index` and `__newindex`
    * Function call (using object as a function): `__call`
    * Finalizers : `__gc` must be set from _C_
* The meta table mechanism is used to extend _Lua_ semantically as needed.

Metatable example
--------------------------------------

> * Fibber object based on some number `n`
> * Fibbers can be added or subtracked 
> * Can't add any values to fibbers
> * Applying function call to fibbers returns n_th fibonacci number

```lua
local mtFibber = {}
function makeFibber(n)
    local f = {n = n}
    setmetatable(f, mtFibber)
    return f
end
do
    function mtFibber:__call()
        local n = math.max(self.n,0)
        local function _fib(fn2, fn1, at)
            if at == n then 
                return fn1 + fn2
            else
                return _fib(fn1, fn1 + fn2, at+1)
            end
        end
        if n <= 0 then return 0
        elseif n == 1 then return 1
        else return _fib(0, 1, 2) end
    end
    function mtFibber.__add(lhs, rhs) return makeFibber(lhs.n + rhs.n) end
    function mtFibber.__sub(lhs, rhs) return makeFibber(lhs.n - rhs.n) end
    function mtFibber:__newindex(k,v) assert(false, "can't add members") end
end
local f1 = makeFibber(10)
local f2 = makeFibber(5)
local f3 = makeFibber(100)
local f4 = (f3 + f2 - f1)
-- The 95th fibonacci is 3.194043463499e+019
print ("The " .. f4.n .. "nth fibonacci number is " .. f4())
```

Addons
============================================

IUP
--------------------------------------------
> * Cross platform native GUI toolkit.
> * Supports declerative way of GUI definition.

![IUP Dialog](IupDialog.png)

```lua
require "iuplua"
require "iupluacontrols"
local label
local text = ""
local function onText(self)
	text = string.upper(self.value)
end
local function modifyLabel(self)
	if label then 
		label.title = text
	end
end

dlg = iup.dialog
{
	iup.vbox
	{
		iup.label{title = 'A silly little dialog', map_cb = function(self) label = self end},
		iup.vbox
			{
				iup.hbox
				{
					iup.label{title='Write text', size="80"},
					iup.text{size="80", valuechanged_cb = onText}
					;margin="0", gap="10"
				};
				iup.hbox
				{
					iup.button{title="Ok",size="40", action = modifyLabel},
					iup.button{title="Cancel",size="40" , action = function () return iup.CLOSE end}
					;margin="0", gap="10"
				};	
			}
		;margin="5x5", gap="5"
	}
	;title="Some dialog", resize="NO"
}
dlg:popup()
```

Cosmo
--------------------------------------------
> * Cosmo is a “safe templates” engine.
> * It allows you to fill nested templates.
> * Many of the advantages of Turing-complete template engines, 
    without without the downside of allowing arbitrary code in the templates.

```lua
local cosmo = require "cosmo"
mycards = { {rank="Ace", suit="Spades"}
		  , {rank="Queen", suit="Diamonds"}
		  , {rank="10", suit="Hearts"} 
		  } 
template = "$do_cards[[$rank of $suit, ]]"
-- prints Ace of Spades, Queen of Diamonds, 10 of Hearts,
print (cosmo.fill(template, {do_cards = mycards}))
```

Penlight 
--------------------------------------------
> * Your batteries for general development
> * Adds support for functional style

```lua
local List = require 'pl.List'	
local func = require 'pl.func'	
print (List{10,20,30}:map(_1+1):reduce '+')	-- prints 63
```

> * Standard class system using meta tables

```lua
 class = require 'pl.class'

 class.Animal()
 function Animal:_init(name)
     self.name = name
 end
 function Animal:__tostring()
   return self.name..': '..self:speak()
 end

 class.Dog(Animal)
 function Dog:speak()
   return 'bark'
 end

 class.Cat(Animal)
 function Cat:_init(name,breed)
     self:super(name)  -- must init base!
     self.breed = breed
 end
 function Cat:speak()
   return 'meow'
 end

fido = Dog('Fido')
felix = Cat('Felix','Tabby')

print(fido,felix)        -- Fido: bark Felix: meow
print(felix:is_a(Animal) -- true
print(felix:is_a(Dog))   -- false
print(felix:is_a(Cat))   -- true
```

MetaLua
----------------------------------------------
> * Static meta programming system for Lua
> * Alter compilation process in arbitrary, powerful and maintainable ways
> * Comes with some useful extensions built in
>    * Pattern matching like in Haskell an ML
>    * list comprehension

```lua
-{extension "clist"}

-- integers from 2 to 50, by steps of 2:
x = { i for i = 2, 50, 2 }

-- the same, obtained by filtering over all integers <= 50:
y = { i for i = 1, 50 if i%2==0 }


-{extension 'match'}

match { { 'o', 'k', '!' } } with
| { t } -> match t with
   | { a, b }      -> print 'KO'
   | { a, b, ... } -> print (a..b)
   | _             -> print 'KO'
   end
| _ -> print 'KO'
end
```

How do we use it
==================================

Build Automation
-----------------------------------
* Scripts to pull repository version info into builds
* Scripts to generate project skeletons
* Scripts to generate code

Data definition language
-----------------------------------
> * Instead of writing a compiler
> * We defined a DSL based on _Lua_ tables
> * Use _Lua_ as the compiler
> * Generate _C++_ code

```lua
FLiDataCompiler
{INCLUDE_PCH = [[#include "fliModelsPCH.h"]];
 INCLUDE_LIB = [[#include "../fliModelsLib.h"]];
 EXPORT_DECL = [[fliMODELS_API]];
 INCLUDES = {"dmlGoalState.dsd", "dmlGoalChildrenConstraint.dsd", "dmlTrigger.dmd"};
 [[	@brief	Goals are a hierarchical way to define what an entity is required to achieve. 
 ]];
 MODEL = {"fli::act::DGoal";
 	[[@brief String used to categorize the goal with categories seperated by / also used to identify the goal]];
 	{"Category", {"string", 1}};
 
 	[[@brief The current active state of the goal]];
 	{"State", {"EGoalState", 2, default='GOAL_INACTIVE'}};
 
 	[[@brief Constrain how the goal's children become active when it is active]];
 	{"ChildrenConstraint", {"EGoalChildrenConstraint", 3, default="GOAL_CHILDREN_ACTIVE_CONCURRENTLY"}};

 	[[@brief Child goals]];
 	{"Children", {"array<DGoal>", 4}};

 	...
 	...

 	[[@brief The number of times the goal has passed]];
 	{"PassedCount", {"int32", 9, set=false}};

	[[@brief The number of times the goal has failed]];
	{"FailedCount", {"int32", 10, set=false}};

 };
};
```

Data description language
--------------------------------------
> * We have component hierarchies which can be serialized as XML, binary etc.
> * XML is not human write friendly
> * We use _Lua_'s tables to declaritively build the hierarchies
> * This is more succint
> * This is more flexible
> * Still have full power of _Lua_ where needed

```lua
fli_data.ScopeModelCreate()
local DoNotRegister = true	-- Do not register with fli::data::CManager

-----------------------------------------------------
-- Scenario world
-----------------------------------------------------
-- Define the whole world for the scenario defining which extensions the world has as well as all the entities which are
-- present in the world. 
return Root(fli__ent__DWorld("World"), DoNotRegister)
{
	Extensions =
	{
		-- Extension that defines information related to the scenario but not a specific entity
		fli__ent__DScenarioInfo
		{
			Name = "Tutorial_03_SimpleVehicle";		
			FocusEntity = RefModel("World:Entities.1");
		};
	};
	
	Entities = 
	{
		Import(assert(RelToData "FLiTutorials/Tutorial_03_SimpleVehicle/Scripts/Terrain.lua"))
		{			
			InInitialTimeOfDay = 16 * 60 * 60;
		};
	
		Import(assert(RelToData "FLiTutorials/Tutorial_03_SimpleVehicle/Scripts/SimpleEntityOfTutorial2.lua"))
		{			
			InName = "My Imported Entity";
			InPhysicsEnableDebugDrawing = true;
			InPosition = {x=-100;y=50;z=0;};
			InOrientation = QuaternionToTable( OrientFromYPRd(0, 0, 0) );
			InVisualColour = {r=0, g=1, b=0, a=1};
		};
		
		Import(assert(RelToData "FLiTutorials/Tutorial_03_SimpleVehicle/Scripts/SimpleEntityOfTutorial2.lua"))
		{			
			InName = "My Second Imported Entity";
			InPhysicsEnableDebugDrawing = true;
			InPosition = {x=-100;y=50;z=10;};
			InOrientation = QuaternionToTable( OrientFromYPRd(DEG_TO_RAD(60), 0, 0) );
			InVisualColour = {r=0, g=1, b=1, a=1};
		};
		
		Import(assert(RelToData "FLiTutorials/Tutorial_03_SimpleVehicle/Scripts/SimpleEntityOfTutorial2.lua"))
		{			
			InName = "My Third Imported Entity";
			InPhysicsEnableDebugDrawing = true;
			InPosition = {x=-110;y=50;z=10;};
			InOrientation = QuaternionToTable( OrientFromYPRd(0, 0, DEG_TO_RAD(30)) );
			InVisualColour = {r=1, g=0, b=1, a=0.5};
		};
	};
}
```

The End
==================================
