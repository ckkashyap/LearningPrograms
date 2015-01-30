import strutils

type Dingo = ptr array[8, uint8]

type Descriptor = int64

type GDT = array[3, Descriptor]


proc setLowByte(entryPointer: Dingo, val: uint8) = 
  let x: uint32 = cast[uint32](entryPointer[0])
  echo "after cast x = ", x
  entryPointer[0] = val



var d1: Descriptor = 0x1122334455667788
var d2: Descriptor = 0xaabbccddeeffaaff
var d3: Descriptor = 0x0102030405060708

var table: GDT = [d1, d2, d3]


for i in 0..table.len-1:
  echo table[i].toHex(16)

var x:uint8 = cast[uint8](255)

setLowByte(cast[Dingo](addr table[1]), x)

echo "----"
for i in 0..table.len-1:
  echo table[i].toHex(16)



var x_2 = 2
