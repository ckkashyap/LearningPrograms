type
  Animal = object
    name: cstring
    age: int


type
  Dingo* = array[3, Animal]

var x: Dingo = [Animal(), Animal(), Animal()]

x[0].name="ABCD"

echo x[0].age
echo x[0].name

#echo x.name
#echo x.age
#
#x.age = 20 
#echo x.name
#
#
#proc useAnimal( p : ptr Animal) = 
#  echo p.name
#
#
#useAnimal(addr x)


type
  MyType = object
    param: int


proc `bit1=`(p: var MyType, value: bool) {.inline.} = 
  if value:
    p.param = p.param or 1
  else:
    p.param = p.param and ((not 0) xor 1)

proc `bit2=`(p: var MyType, value: bool) {.inline.} = 
  if value:
    p.param = p.param or 2
  else:
    p.param = p.param and ((not 0) xor 2)

proc lowByte(p: var MyType) : int8 {.inline.} =
  var pp : ptr array [4, int8] = cast[ptr array [4, int8]](addr p.param)
  pp[1]



var y = MyType (param: 0x7)

y.bit2 = false

y.param = 0x010203040506070a



echo y.param

echo "lobyte = ", y.lowByte

var v1 : array[3,int] = [123,2,3]

var pv1 : ptr array [4, int8] = cast[ptr array [4, int8]] (addr v1)

echo pv1[0]


echo "END"
