import ../sharedmempool
import threadpool,os
import unittest


suite "api_tests":
    setup:
      const Maxbuffercnt : int = 600
      var
        memptr = allocShared0(calculateMemBufferSize(SharedMemPoolBufferSize.b64,Maxbuffercnt) )
        sharedbufferpoolref = sharedmempool.newSharedMemPool(
          SharedMemPoolBufferSize.b64,Maxbuffercnt,memptr,
          allocShared0(sharedmempool.getMemHelperBaseSize()))
      
    teardown:
      sharedbufferpoolref.deinitSharedMemPool        
      deallocShared(memptr) # dealloc the buffer
      deallocShared(sharedbufferpoolref.getMemHelperBasePtr) # dealloc helper memory


    test "allocdeallocdeinit":
      let (buffhdl,errNo) =  sharedbufferpoolref.requestBuffer(0,true)
      sharedbufferpoolref.releaseBuffer(errNo)
            
      check:
        errNo.isValid # check for alloc error
        0 == sharedbufferpoolref.getContentionCount
        
    test "outofbuffer_threads":
      # alloc entire slots to get the buffererror
      var hdls = newSeq[SharedBufferHandle](Maxbuffercnt+10)
      var outofbufferhdl : SharedBufferHandle

      for i in hdls.low..hdls.high:
        hdls[i] = sharedbufferpoolref.requestBuffer(0,true)
        if not hdls[i].slotidOrErrno.isValid: # exit loop if error detected
          break;

      for i in hdls.low..hdls.high:
        if hdls[i].slotidOrErrno.isValid:
          sharedbufferpoolref.releaseBuffer(hdls[i].slotidOrErrno) 
        else:
          outofbufferhdl = hdls[i] # sentinel reached
          break
      
      check:
        # sentinel should be outOfBuffer
        outofbufferhdl.slotidOrErrno == SharedMemPoolErrNo.outOfBuffer.int  
      
     
    test "outofbuffer_multithread":
      # alloc entire slots to get buffererror
      # 5 threads are spawned. one thread is grabbing the entire pool buffers
      # and releases some buffers - the other threads are trying to allocate
      # and the contention-counter should raised
      # commands are alloc, free, free entire threads buffer
      # after allocating the spawner is trying to release the pool and will block till all slots
      # free. the other threads
      # should getting the error: about_to_shutdown. if this msg  the entire buffers
      # should be released
      proc dosomething_fastalloc(sharedbufferpoolptr : SharedMemPoolPtr) : bool  =
        # we only empty the bufferpool here (consuming everything)          
        var buffers =  newSeq[SharedBufferHandle](Maxbuffercnt)
        result = true

        for i in buffers.low..buffers.high:
          buffers[i] = sharedbufferpoolptr.requestBuffer()
          if not buffers[i].slotidOrErrno.isValid:    
            break # stop allocating
          else:
            sleep(50)

        sleep(500)  # after consuming we wait here a little bit    

        for i in buffers.low..buffers.high:
          if buffers[i].slotidOrErrno.isValid:
            sharedbufferpoolptr.releaseBuffer(buffers[i].slotidOrErrno)
            sleep(50)
          else:
            break # sentinel reached

      proc dosomething_slowalloc(sharedbufferpoolptr : SharedMemPoolPtr) : bool =
        var buffers =  newSeq[SharedBufferHandle](Maxbuffercnt)
      
        result = true
        for i in buffers.low..buffers.high:
          buffers[i] = sharedbufferpoolptr.requestBuffer()         
          
          if not buffers[i].slotidOrErrno.isValid:    
            # stop allocating
            break
          else:
            sleep(400)
        
        sleep(500)

        for i in buffers.low..buffers.high:
          # copy two strings into buffer
        
          if buffers[i].slotidOrErrno.isValid:
            var tststring : cstring = "teststring"
            var tststring2 : cstring = "anotherteststring"
          
            let hdl : SharedBufferHandle = buffers[i]
            copyMem( hdl.sharedBufferPtr,addr(tststring),tststring.len)
            copyMem(sharedBufferPoolPtr.handle2BufferPointer(hdl,tststring.len),addr(tststring2),tststring2.len)
            var tst : ptr cstring = cast[ptr cstring](hdl.sharedBufferPtr)
            var strcomp = tst[] == tststring
            var tst2 : ptr cstring = cast[ptr cstring](sharedBufferPoolPtr.handle2BufferPointer(hdl,tststring.len) )
            var strcomp2 = tst2[] == tststring2
            
            result = result and (strcomp and strcomp2)
          
          if buffers[i].slotidOrErrno.isValid:
            sharedbufferpoolptr.releaseBuffer(buffers[i].slotidOrErrno)
            sleep(100)
          else:
            break # sentinel reached                

      var presults = newSeq[FlowVar[bool]](16)
      var presults2 = newSeq[FlowVar[bool]](16)
  
      for i in presults.low..presults.high:
        presults[i] = spawn dosomething_slowalloc(cast[SharedMemPoolPtr](sharedbufferpoolref))
  
      for i in presults2.low..presults2.high:
        presults2[i] = spawn dosomething_fastalloc(cast[SharedMemPoolPtr](sharedbufferpoolref))
  
      sync() # wait till all worker finished
      
      var boolresult : bool = true
      for i in presults.low .. presults.high:
        boolresult = boolresult and ^presults[i] # aggregate the subresults     
      check:
        sharedbufferpoolref.getContentionCount > 0
        boolresult == true        
