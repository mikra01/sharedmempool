# shared memory pool implementation in Nim
# Copyright (c) 2018 Michael Krauter
# please see the LICENSE-file for details.

import locks,os 
## sharedMemPool is an alloc/dealloc module with an initial system alloc/dealloc call.
## The given chunk of sharedmem is split into parts of equal sizes
## according to the parameters given.
## suitable for zero-copy protocol stacks, custom buffering and memory layout
## 
## this implementation is without exceptions; it is threadsafe and no worker thread is spawned.
## It runs with or without GC. No memory allocation calls are performed within the implementation.
##  
## It could also be used to synchronise access of memory mapped
## I/O. In this case use the proc **requestBufferBySlotNum** to obtain the desired
## memory address (buffer initialisation should be turned off)
##
##
## example (see tests for multithread examples) : 
##
## .. code-block:: Nim
##
##  import sharedmempool
##
##  let mempool = newSharedMemPool( SharedMemPoolBuffersize.b16 ,512, 
##             allocShared0(sharedmempool.calculateMemBufferSize(
##             SharedMemPoolBuffersize.b16,512)),
##             allocShared0(sharedmempool.getMemHelperBaseSize()))
##
##  let tststring : string = "teststring"
##
##  let (bufferptr,slotidOrErrno) =  mempool.requestBuffer()
##
##  if slotidOrErrno.isValid:
##    echo $slotidOrErrno 
##    copyMem( bufferptr,unsafeAddr(tststring),tststring.len)
##    var tst : ptr string = cast[ptr string](bufferptr)
##    echo tst[]
##    echo "buffers in use " & $mempool.getUsedBufferCount
##    mempool.releaseBuffer(slotidOrErrno)
##    echo "buffers in use after release " & $mempool.getUsedBufferCount
##
##  mempool.deinitSharedMemPool        
##  deallocShared(mempool.getMemBufferBasePtr)
##  deallocShared(mempool.getMemHelperBasePtr)
##  
##
## the maximum buffer count is 1024 ; the minimum buffsize is 1byte 
## and the maximum segmentation is 4096kB
## 
## if no buffer is free while requesting for it, an errorcode is returned and the
## contention count is raised. this situation should never occur and it's
## the implementors task to finetune the buffer consumption.
## if a buffer is not needed anymore it should be pushed back to the pool
## immediately.
## 
## while requesting the buffer, the caller can specifiy a bitpattern
## the buffer is initialised with (optional; the default value is 0)
##

when not compileOption("threads"):
  {.error: "SharedMemPool requires --threads:on option.".}

# atomics for vcc
when defined(vcc):
  when defined(cpp):
    when sizeof(int) == 8:
      proc interlExchange*[T: bool|int|ptr](p:ptr T,val:T) : T {.
        importcpp: "_InterlockedExchange64(static_cast<NI volatile *>(#), #)",
          header: "<intrin.h>".}  
      proc interlAnd*[T: bool|int|ptr](p:ptr T,val:T) : T {.
        importcpp: "_InterlockedAnd64(static_cast<NI volatile *>(#), #)",
          header: "<intrin.h>".} 
      proc interlOr*[T: bool|int|ptr](p:ptr T,val:T) : T {.
        importcpp: "_InterlockedOr64(static_cast<NI volatile *>(#), #)",
          header: "<intrin.h>".}   
    else:
      proc interlExchange*[T: bool|int|ptr](p : ptr T, val: T): T {.
        importcpp: "_InterlockedExchange(reinterpret_cast<LONG volatile *>(#), static_cast<LONG>(#))",
          header: "<intrin.h>".}
      proc interlAnd*[T: bool|int|ptr](p:ptr T,val:T) : T {.
        importcpp: "_InterlockedAnd(reinterpret_cast<LONG volatile *>(#), static_cast<LONG>(#))",
          header: "<intrin.h>".}
      proc interlOr*[T: bool|int|ptr](p:ptr T,val:T) : T {.
        importcpp: "_InterlockedOr(reinterpret_cast<LONG volatile *>(#), static_cast<LONG>(#))",
          header: "<intrin.h>".}
  else:
    when sizeof(int) == 8:
      pro c interlExchange*[T: bool|int|ptr](p:ptr T, val : T) : T {.
        importc: "_InterlockedExchange64", header: "<intrin.h>".}
      proc interlAnd*[T: bool|int|ptr](p:ptr T, val : T) : T {.
        importc: "_InterlockedAnd64", header: "<intrin.h>".}
      proc interlOr*[T: bool|int|ptr](p:ptr T, val : T) : T {.
        importc: "_InterlockedOr64", header: "<intrin.h>".}
    else:
      proc interlExchange*[T: bool|int|ptr](p : ptr T, val: T) : T {.
        importc: "_InterlockedExchange", header: "<intrin.h>".}
      proc interlAnd*[T: bool|int|ptr](p:ptr T, val : T) : T {.
        importc: "_InterlockedAnd", header: "<intrin.h>".}
      proc interlOr*[T: bool|int|ptr](p:ptr T, val : T) : T {.
        importc: "_InterlockedOr", header: "<intrin.h>".}

when defined(gcc):
  template atomicLoad[T](t: T ) : T =
            atomicLoadN[T](t.addr,ATOMIC_ACQ_REL) 
        
  template atomicStore[T](t : T, t1 : T) =  
            atomicStoreN[T](t.addr,t1,ATOMIC_ACQ_REL) 
        
when defined(vcc):
  template atomicLoad[T](t: T ) : T =
            interlOr[T](t.addr,cast[T](0)) 
        
  template atomicStore[T](t : T, t1 : T) =  
            discard interlExchange[T](t.addr,t1) 

# bitmasks to access each bit within a byte
const constBitmasks : array[0..7,uint8] = [0:0b10000000.uint8,
                                           1:0b01000000.uint8,
                                           2:0b00100000.uint8,
                                           3:0b00010000.uint8,
                                           4:0b00001000.uint8,
                                           5:0b00000100.uint8,
                                           6:0b00000010.uint8,
                                           7:0b00000001.uint8]

const InvalidPointer* : pointer = cast[pointer](0.int)

type 
  SharedMemPoolErrno* = enum slotNotOccupied = -9, offsetOutOfRange = -8,
                             reservedErrno = -7, slotInUse = -6, invalidSlot = -5 
                             outOfBuffer = -4, waitLimitExceed = -3,
                             threadNotObjectsOwner= -2, genericError= -1

  SharedMemPoolBuffersize*  = enum b01 = 1, b02 = 2,b04 = 4, b08 = 8, 
                                   b16 = 16,b32=32,b64=64,b128=128,
                                   b256=256,b512=512,k1=1024,k2=2048,
                                   k4=4096,k8=8192,k16=16348,
                                   k32=32768,k64=65536,k128=131072,
                                   k256=262144,k512=524288,k1024=1048576,
                                   k2048=2097152,k4096=4194304
  # the buffersizes b01 to b08 makes mostly sense if memory-mapped
  # access is utilized   
  
  SharedMemPoolSlot* = range[slotNotOccupied.int..1024.int]
    ## returntype which indicates error or contains the allocated slotnumber

  SharedBufferHandle* = tuple[sharedBufferPtr : pointer, 
                              slotidOrErrno : SharedMemPoolSlot ]
    ## type returned by the allocation proc. if the operation was successful a slotid 
    ## is returned (slotidOrErrno > -1 ). Otherwise
    ## it´s an errorcode (slotidOrErrno < 0).
    ## the isValid template should be used for the condition check

  # 32 int32 words - each set bit indicates a used buffer
  SBitContainer = array[32,uint32] # arrsize should be multiple of 4 (32bit-word)

  SharedMemPool = object of RootObj
    spawningThreadId : int               # used for context validation
    memBase : int                        # pointer to the membase of sharedMem
    contentionCount : int                # count over entire obj-lifetime
    maxBuffers : SharedMemPoolSlot       # applval
    bufferSize : SharedMemPoolBuffersize # applval 
    bufferUsed  : int                    # calculated at runtime
    bitbuffer : SBitContainer            # each set bit represents an allocated buffer
    waitLock : Lock  
  
  SharedMemPoolRef*  = ref SharedMemPool
  ## for single threaded only or within the object-spawning thread context
  SharedMemPoolPtr* = ptr SharedMemPool  
  ## use the pointer type for passing pool around between threads

# ptrutil template taken from https://forum.nim-lang.org/t/1188
template usePtr[T](body : untyped) =
  template `+`(p: ptr T, off: SomeInteger): ptr T =
    cast[ptr type(p[])](cast[ByteAddress](p) +% int(off) * sizeof(p[]))
        
  template `+=`(p: ptr T, off: SomeInteger) =
    p = p + off
        
  template `-`(p: ptr T, off: SomeInteger): ptr T =
    cast[ptr type(p[])](cast[ByteAddress](p) -% int(off) * sizeof(p[]))
        
  template `-=`(p: ptr T, off: SomeInteger) =
    p = p - int(off)
        
  template `[]`(p: ptr T, off: SomeInteger): T =
    (p + int(off))[]
        
  template `[]=`(p: ptr T, off: SomeInteger, val: T) =
    (p + off)[] = val

  body    


proc getBitval(c: var SBitContainer,bitnum:SharedMemPoolSlot) : bool =
# returns true if the bit at the specified index location is set
# otherwise false
  usePtr[uint8]:
    var bptr : ptr uint8 = cast[ptr uint8](unsafeAddr(c))
    bptr += ( (bitnum shr 5) shl 2)   # /32 * 4
    result = ((bptr[bitnum shr 3] and constBitmasks[bitnum mod 8]) != 0 ) 
  return result 

proc setBitval(c: var SBitContainer,bitnum:SharedMemPoolSlot) = 
# sets the bit (set to 1) at the specified index location
  usePtr[uint8]:
    var bptr : ptr uint8 = cast[ptr uint8](unsafeAddr(c))
    bptr += ( (bitnum shr 5) shl 2)   # /32 * 4 # bitmaskwidth 4byte
    let byteidx = bitnum shr 3 # / 8
    bptr[byteidx] = bptr[byteidx] or constBitmasks[bitnum mod 8]  
 

proc clearBitval(c: var SBitContainer,bitnum:SharedMemPoolSlot)=
# clr bit at idx pos 
  usePtr[uint8]:
    var bptr : ptr uint8 = cast[ptr uint8](unsafeAddr(c))
    bptr += ( (bitnum shr 5) shl 2)   # (/32) * 8
    let byteidx = bitnum shr 3 # /8
    bptr[byteidx] = bptr[byteidx] and not(constBitmasks[bitnum mod 8]) 
    

proc getEmptySlotIdx(c: var SBitContainer, 
                     maxslots : SharedMemPoolSlot) : SharedMemPoolSlot =
  # checks for an empty slot by simply iterating over all bits
  # it returns outOfBuffer if all slots occupied
  result = SharedMemPoolErrNo.outOfBuffer.int # preset no empty slot found
  var bitnum : SharedMemPoolSlot = 0
  block zerobitsearch:
    usePtr[uint8]:
      let bptr : ptr uint8 = cast[ptr uint8](unsafeAddr(c))
      var wordbase : int = 0
      var bytebase : int = 0
      var bitbase : int = 0
# TODO: check word and byte if completely occupied
      for i32_idx in c.low..c.high:
        wordbase =  i32_idx shr 5 # div 32        
        for i in 0 .. 3:          # visit bytes
          bytebase = i shl 3      # mul 8
          bitbase = wordbase + bytebase # startbit
          if bptr[i] < 255:
            for x_idx in constBitmasks.low..constBitmasks.high:
              bitnum = bitbase+x_idx
              if bitnum < maxslots:
                if (bptr[i] and constBitmasks[x_idx]) == 0:  
                  result = bitnum         # cleared bit found
                  break zerobitsearch
              else:
                break zerobitsearch # limit reached                

proc isPoolIdle(bitc: SBitContainer) : bool =
# returns true if no buffer in use
  result = true

  for b in bitc:
    if b != 0:
     result = false
     break;
  
  return result

# public api

template isValid*(slotnum : SharedMemPoolSlot) : bool =
  ## checks if the specified slotnum is valid 
  ## or contains an errorcode (< 0)
  slotnum.int >= 0

func calculateMemBufferSize*(buffersize : SharedMemPoolBuffersize, 
                             buffercount : SharedMemPoolSlot) : int =
  ## helper proc to get the total memory size needed 
  if buffercount.isValid:   
    (buffersize.int * buffercount.int) 
  else:
    (buffersize.int * 1.int) 
    
func getMemHelperBaseSize* : int =
  ## helper proc to get the total memory size for the internals
  sizeof(SharedMemPool)  

proc newSharedMemPool*(buffersize : SharedMemPoolBuffersize, 
                       buffercount : SharedMemPoolSlot, 
                       memBufferBasePtr : pointer, 
                       memHelperBasePtr : pointer) :  SharedMemPoolRef =  
  ## returns a new sharedMemPool
  result = cast[SharedMemPoolRef](memHelperBasePtr)
  result.spawningThreadId  = getThreadId()
  result.contentionCount = 0   # if allocBuffer is blocked counter is inced by 1 
  
  if buffercount.isValid:
    result.maxBuffers = buffercount
  else:
    result.maxBuffers = 1.int

  result.bufferSize = buffersize
  result.bufferUsed = 0
  result.bitbuffer = [0.uint32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                      ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0] #32 32bit words
  initLock(result.waitLock)
 
  # memorybase provided by the caller plus object size (alignment needed?)
  result.memBase = cast[int](memBufferBasePtr) 

proc getMemBufferBasePtr*(poolref : SharedMemPoolRef) : pointer =
  ## convenience proc to obtain the ptr of the memory buffer (dealloc)
  return cast[pointer](poolref.memBase)
  
proc getMemHelperBasePtr*(poolref : SharedMemPoolRef) : pointer =
  ## convenience proc to obtain the ptr of the worker memory (dealloc)
  return unsafeAddr(poolref[])

proc getContentionCount*(poolref: SharedMemPoolRef): int =
  ## retrieves the contention count within the overall pool´s lifetime
  atomicLoad(cast[SharedMemPoolPtr](poolref).contentionCount)
  
proc getContentionCount*(poolptr : SharedMemPoolPtr): int =
  ## retrieves the contention count within the overall pool´s lifetime
  atomicLoad(poolptr.contentionCount)

proc getUsedBufferCount*(poolref : SharedMemPoolRef): int =
  ## returns the count of the currently allocated buffers (ref version)
  atomicLoad(cast[SharedMemPoolPtr](poolref).bufferUsed)  
  
proc getUsedBufferCount*(poolptr : SharedMemPoolPtr): int =
  ## returns the count of the currently allocated buffers (ptr version)
  atomicLoad(poolptr.bufferUsed)  

proc isSpawningThread*(poolptr : SharedMemPoolPtr) : bool =
  ## returns true if objects context belongs to current thread
  return getThreadId() == poolptr.spawningThreadId
      
proc deinitSharedMemPool*(poolref : SharedMemPoolRef) =
  ## deinits the pool and frees resources.
  ## ensure that no ptr is in use anymore
  deinitLock(poolref.waitLock)


proc clearMem(hdl: SharedBufferHandle,bsize : SharedMemPoolBuffersize, clearVal : int) =
     # determine integer width to wipe the buffers memory with the given preset pattern
     var shiftval : int = 0
     when sizeof(int) == 8:
       shiftval = 3  # 64bit arch
     elif sizeof(int) == 4:
       shiftval = 2  # 32bit arch
     elif sizeof(int) == 2:
       shiftval = 1  # 16bit arch
     else:
       shiftval = 0  # no shift 8 bit arch

     let loopval : int = bsize.int shr shiftval
     var tp : ptr int = cast[ptr int](hdl.sharedBufferPtr)

     usePtr[int]: 
       for i in 0..loopval:
         tp[i] = clearVal 

# public api
template slotnum2BufferPointer*(poolptr: SharedMemPoolPtr, 
                                slotnum : SharedMemPoolSlot, 
                                offset : int = 0) : pointer =
  ## convenience template to obtain a buffer from the specified slotId. 
  ## InvalidPointer is returned if the offset 
  ## does not match the buffer´s context (bounds check)
  if offset < poolptr.bufferSize.int and offset >= 0 and slotnum.isValid:
    cast[pointer]( poolptr.memBase + 
                  (poolptr.bufferSize.int * slotnum.int) + 
                   offset)         
  else:
    InvalidPointer

template handle2BufferPointer*(poolptr:SharedMemPoolPtr, 
                               handle : SharedBufferHandle,
                               offset : int = 0) : pointer =
  ## convenience template to obtain a buffer from the specified handle. 
  ## InvalidPointer is returned if the offset does not match 
  ## the buffer´s context (bounds check)
  slotnum2BufferPointer(poolptr,handle.slotIdOrErrno,offset)

template allocSlot(poolptr : SharedMemPoolPtr, 
                            slotnum: SharedMemPoolSlot, 
                            result : SharedBufferHandle) =
  setBitval(poolptr.bitbuffer,slotnum); # alloc slot
  result.slotidOrErrno = slotnum
  result.sharedBufferPtr = poolptr.slotnum2BufferPointer(slotnum)
  
  
proc requestBuffer*( poolptr: SharedMemPoolPtr, 
                     fillval : int = 0, 
                     wipeBufferMem : bool = true ) : SharedBufferHandle =
  ## after returning the field slotidOrErrno indicates if the pointer is valid or not
  ##
  ## if **slotidOrErrno.isValid == false** sharedBufferPointer contains **InvalidPointer**   
  result.slotidOrErrno = cast[SharedMemPoolSlot](genericError)
  result.sharedBufferPtr = InvalidPointer

  withLock(poolptr.waitLock):
    result.slotidOrErrno = getEmptySlotIdx(poolptr.bitbuffer,poolptr.maxBuffers)
  
    if not result.slotidOrErrno.isValid:
      # out of buffer condition
      inc poolptr.contentionCount     
      return result
      
    allocSlot(poolptr,result.slotidOrErrno,result)
    inc poolptr.bufferUsed

  if wipeBufferMem:
    result.clearMem(poolptr.bufferSize,fillval)


proc requestBuffer*( poolref: SharedMemPoolRef, 
                     fillval : int = 0 , 
                     wipeBufferMem : bool = true ) : SharedBufferHandle =
  ## after returning the field slotidOrErrno indicates if the pointer is valid or not
  ##
  ## if **slotidOrErrno.isValid == false** sharedBufferPointer contains **InvalidPointer** 
  return requestBuffer(cast[SharedMemPoolPtr](poolref),fillval,wipeBufferMem)

proc requestBufferBySlotNum*(poolptr: SharedMemPoolPtr, 
                             slotnum: SharedMemPoolSlot, 
                             fillval : int = 0, 
                             wipeBufferMem : bool = false): SharedBufferHandle =
  ## requests a buffer by fixed slotnum. only suitable for memory mapped access  
  result.slotidOrErrno = cast[SharedMemPoolSlot](SharedMemPoolErrno.slotInUse)
  result.sharedBufferPtr = InvalidPointer
  
  if not slotnum.isValid:
    result.slotidOrErrno = cast[SharedMemPoolSlot](SharedMemPoolErrno.invalidSlot)
    return

  withLock(poolptr.waitLock):
   let prevval = getBitval(poolptr.bitbuffer,slotnum)
   if prevval and 
     poolptr.spawningThreadId != getThreadId():
    # slot already allocated
     result.slotidOrErrno = cast[SharedMemPoolSlot](SharedMemPoolErrno.slotInUse)
     inc poolptr.contentionCount     
     return result

   allocSlot(poolptr,result.slotidOrErrno,result)
   if not prevval:
     inc poolptr.bufferUsed

  if wipeBufferMem:
    result.clearMem(poolptr.bufferSize,fillval)
  
  
proc requestBufferBySlotNum*(poolref: SharedMemPoolRef, 
                             slotnum: SharedMemPoolSlot, 
                             fillval : int = 0, 
                             wipeBufferMem : bool = false): SharedBufferHandle =
  ## requests a buffer by fixed slotnum. only suitable for memory mapped access  
  return requestBufferBySlotNum(cast[SharedMemPoolPtr](poolref),
                                  slotnum,fillval,wipeBufferMem)
 
proc releaseBuffer*( poolptr: SharedMemPoolPtr, slotnum: SharedMemPoolSlot )  =
  ## marks the specified buffer as unused. if there are waiting threads a signal is fired 
  if slotnum.isValid:    
    withLock(poolptr.waitLock):
      if getBitval(poolptr.bitbuffer,slotnum):
        clearBitval(poolptr.bitbuffer,slotnum)
        dec poolptr.bufferUsed


proc releaseBuffer*( poolref: SharedMemPoolRef, slotnum : SharedMemPoolSlot )  =
  ## ref version
  releaseBuffer(cast[SharedMemPoolPtr](poolref),slotnum)

