#tag Module
Protected Module AppleEvents
	#tag Method, Flags = &h21
		Private Function LookupPSNFromBundleID(bundleId As String) As ProcessSerialNumber
		  Dim list() As ProcessManager.Process
		  list = ProcessManager.Process.ProcessList()
		  For Each p As ProcessManager.Process In list
		    If p.BundleIdentifier() = bundleId Then
		      Return p.SerialNumber()
		    End If
		  Next
		  
		  Dim psn As ProcessSerialNumber
		  psn.highLongOfPSN = 0
		  psn.lowLongOfPSN = 0
		  Return psn
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function PrintDesc(extends ae as AppleEvent, getReply as boolean = false) As string
		  Soft declare function AEPrintDescToHandle lib "Carbon" (theEvent as integer, hdl as Ptr) as integer
		  Soft declare sub DisposeHandle lib "Carbon" (hdl as ptr)
		  
		  dim myHandle as MemoryBlock
		  dim err as integer
		  dim mb as MemoryBlock
		  dim result as string
		  
		  //Will hold the pointer to the data
		  myHandle = New MemoryBlock( 4 )
		  
		  if getReply then
		    err = AEPrintDescToHandle( ae.ReplyPtr, myHandle )
		  else
		    err = AEPrintDescToHandle( ae.Ptr, myHandle )
		  end if
		  
		  if err<>0 then  return  ""  //Check for error
		  
		  //Get the data
		  mb = myHandle.Ptr( 0 )
		  mb = mb.Ptr( 0 )
		  result = mb.CString( 0 )
		  
		  DisposeHandle   myHandle.Ptr(0)  //We must free the handle to get memory back
		  
		  return result
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function RawData(extends ae as AppleEvent, param as string, byref type as string, inReply as boolean = false) As MemoryBlock
		  //Get a binary data param in the reply AppleEvent
		  
		  declare function AEGetParamPtr Lib "Carbon" (AEPtr as integer, AEKeyword as OSType, inType as OSType, byref outType as OSType, data as Ptr, maxSize as integer, byref actSize as integer) as short
		  
		  dim data as MemoryBlock
		  dim err as integer
		  dim oType as OSType
		  dim aSize as integer
		  dim paramSize as integer
		  dim paramType as string
		  dim p as integer
		  
		  ae.SizeAndTypeOfParam( param, true, paramSize, paramType )
		  if paramType="" then  //No parameter with this key
		    return  nil
		  end if
		  
		  data = newMemoryBlock( paramSize )
		  
		  //Get the data
		  if inReply then
		    p = ae.ReplyPtr
		  else
		    p = ae.Ptr
		  end if
		  err = AEGetParamPtr( p, param, type, oType, data, data.Size, aSize )
		  if err<>0 then
		    return  nil
		  else
		    //Update the actual type and return the data
		    type = oType
		    return data.StringValue( 0, aSize )
		  end if
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub RawData(extends ae as AppleEvent, param as string, type as string, inReply as boolean = false, assigns data as MemoryBlock)
		  //Add some binary data as a reply AppleEvent parameter
		  
		  declare function AEPutParamPtr Lib "Carbon" (AEPtr as integer, AEKey as OSType, dType as OSType, data as Ptr, dsize as integer) as short
		  
		  dim err as integer
		  dim p as integer
		  
		  if inReply then
		    p = ae.ReplyPtr
		  else
		    p = ae.Ptr
		  end if
		  err = AEPutParamPtr( p, param, type, data, data.size )
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function SendUsingPSN(Extends ae As AppleEvent, autoStart As Boolean = False) As Boolean
		  // Send AppleEvent using PSN instead of Bundle ID
		  
		  Soft Declare Function AEGetAttributeDesc Lib "Carbon" (theAppleEvent As Integer, theAEKeyword As OSType, desiredType As OSType, ByRef result As AEDesc) As Short
		  Soft Declare Function AEPutAttributeDesc Lib "Carbon" (theAppleEvent As Integer, theAEKeyword As OSType, ByRef result As AEDesc) As Short
		  Soft Declare Function AEReplaceDescData Lib "Carbon" (typeCode As OSType, dataPtr As Ptr, dataSize As Integer, ByRef theAEDesc As AEDesc) As Short
		  Soft Declare Function AEGetDescData Lib "Carbon" (ByRef theAEDesc As AEDesc, dataPtr As Ptr, maximumSize As Integer) As Short
		  Soft Declare Function AEDisposeDesc Lib "Carbon" (ByRef theAEDesc As AEDesc) As Short
		  
		  Dim err As Short = 0
		  Dim aed As AEDesc
		  Dim mb As New MemoryBlock(256)
		  Dim bundleId As String
		  Dim psn As ProcessSerialNumber
		  
		  // Retrieve bundle ID set via the constructor
		  err =  AEGetAttributeDesc(ae.Ptr, "addr", "bund", aed)
		  If err <> 0 Then Return ae.Send()
		  
		  err = AEGetDescData(aed, mb, mb.Size)
		  If err <> 0 Then Return False
		  
		  bundleId = mb.CString(0)
		  err = AEDisposeDesc(aed)
		  If err <> 0 Then Return False
		  
		  // bundle ID seems like an OSType
		  If bundleId.Len() = 4 Then Return ae.Send()
		  
		  psn = LookupPSNFromBundleID(bundleId)
		  
		  If psn.highLongOfPSN = 0 And psn.lowLongOfPSN = 0 Then
		    // App is not running...
		    If autoStart Then
		      // Launch the app from bundle ID
		      Dim url As NSURL = NSWorkspace.URLForApplicationWithBundleIdentifier(bundleId)
		      If url.Item = Nil Then Return False
		      url.Item.Launch(False)
		      
		      // Look up again
		      psn = LookupPSNFromBundleID(bundleId)
		      If psn.highLongOfPSN = 0 And psn.lowLongOfPSN = 0 Then Return False
		    Else
		      Return False
		    End If
		  End If
		  
		  mb.CString(0) = psn.StringValue(True)
		  
		  err = AEReplaceDescData("psn ", mb, psn.Size, aed)
		  If err <> 0 Then Return False
		  
		  err = AEPutAttributeDesc(ae.Ptr, "addr", aed)
		  err = AEDisposeDesc(aed)
		  If err <> 0 Then Return False
		  
		  Return ae.Send()
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SizeAndTypeOfParam(extends ae as AppleEvent, param as string, inReply as boolean, byref size as integer, byref type as string)
		  //Get the size and type of one parameter. Set inReply to true if you want to access the reply AppleEvent
		  
		  declare function AESizeOfParam lib "Carbon" (evnt as integer, AEKeyword as OSType, byref oDesc as OSType, byref oSize as integer) as short
		  
		  dim err as integer
		  dim oDesc as OSType
		  dim oSize as integer
		  
		  if inReply then
		    err = AESizeOfParam( ae.replyptr, param, oDesc, oSize )
		  else
		    err = AESizeOfParam( ae.ptr, param, oDesc, oSize )
		  end if
		  
		  if err<>0 then //We get a -1701 error if there is no parameter with this keyword
		    type = ""
		    size = 0
		  else
		    type = oDesc
		    size = oSize
		  end if
		End Sub
	#tag EndMethod


	#tag Note, Name = About
		This is part of the open source "MacOSLib"
		
		Original sources are located here:  http://code.google.com/p/macoslib
	#tag EndNote


	#tag Constant, Name = typeBoolean, Type = String, Dynamic = False, Default = \"bool", Scope = Public
	#tag EndConstant

	#tag Constant, Name = typeFSRef, Type = String, Dynamic = False, Default = \"fsrf", Scope = Public
	#tag EndConstant

	#tag Constant, Name = typeStyledUnicodeText, Type = String, Dynamic = False, Default = \"sutx", Scope = Public
	#tag EndConstant


	#tag Structure, Name = AEDesc, Flags = &h0
		descriptorType as OSType
		dataHandle as Ptr
	#tag EndStructure


	#tag ViewBehavior
		#tag ViewProperty
			Name="Index"
			Visible=true
			Group="ID"
			InitialValue="2147483648"
			InheritedFrom="Object"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Left"
			Visible=true
			Group="Position"
			InitialValue="0"
			InheritedFrom="Object"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Name"
			Visible=true
			Group="ID"
			InheritedFrom="Object"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Super"
			Visible=true
			Group="ID"
			InheritedFrom="Object"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Top"
			Visible=true
			Group="Position"
			InitialValue="0"
			InheritedFrom="Object"
		#tag EndViewProperty
	#tag EndViewBehavior
End Module
#tag EndModule
