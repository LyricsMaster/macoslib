#tag Module
Protected Module MacDiskUtil
	#tag Method, Flags = &h1
		Protected Function Device(identifier As String) As MacDeviceItem
		  dim dict as Dictionary = pGetDiskUtilInfo( identifier )
		  dim r as MacDeviceItem = MacDeviceItem.CreateFromDictionary( dict )
		  return r
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function Devices() As MacDeviceItem()
		  dim r() as MacDeviceItem
		  
		  dim info as Dictionary = pGetDiskUtilList( )
		  if info is nil then return r
		  
		  dim disksArr() as Variant = info.Lookup( "WholeDisks", nil )
		  if disksArr <> nil then
		    for i as integer = 0 to disksArr.Ubound
		      dim thisDisk as string = disksArr( i )
		      r.Append Device( thisDisk )
		    next
		  end if
		  return r
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function Mount(item As MacDeviceItem, force As Boolean = false) As Boolean
		  dim cmd as string
		  if item IsA MacPartitionItem then
		    cmd = "mount "
		  else
		    cmd = "mountDisk "
		  end if
		  if force then
		    cmd = cmd + "force "
		  end if
		  cmd = kDiskUtilCmd + cmd + item.Identifier
		  
		  dim r as shell = pExecuteShellCommand( cmd )
		  return r.ErrorCode = 0
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function Partition(identifier As String) As MacPartitionItem
		  return MacPartitionItem( Device( identifier ) )
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function Personalities() As MacFormatPersonality()
		  dim r() as MacFormatPersonality
		  
		  #if TargetMacOS
		    
		    dim sh as shell = pExecuteShellCommand( kDiskUtilCmd + "listFileSystems -plist" )
		    dim s as string
		    if sh <> nil then
		      s = sh.Result
		      dim plist as CoreFoundation.CFPropertyList = CFType.CreateFromPListString( s, CoreFoundation.kCFPropertyListMutableContainersAndLeaves )
		      if plist <> nil then
		        dim arr() as Variant = plist.VariantValue
		        for each d As Dictionary in arr
		          dim p as MacFormatPersonality = MacFormatPersonality.CreateFromDictionary( d )
		          r.Append p
		        next
		      end if
		      
		    end if
		    
		  #endif
		  
		  return r
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function pExecuteShellCommand(cmd As String) As Shell
		  dim sh as new shell
		  sh.Execute cmd
		  return sh
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function pGetDiskUtilInfo(device As String = "") As Dictionary
		  #if TargetMacOS
		    
		    dim cmd as string = kDiskUtilCmd + "info -plist " + device
		    dim r as shell = pExecuteShellCommand( cmd )
		    dim plist as CoreFoundation.CFPropertyList = CFTYpe.CreateFromPListString( r.Result, CoreFoundation.kCFPropertyListMutableContainersAndLeaves )
		    if plist <> nil then
		      return plist.VariantValue
		    else
		      return nil
		    end if
		    
		  #else
		    
		    #pragma unused device
		    
		  #endif
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function pGetDiskUtilList(deviceIdentifier As String = "") As Dictionary
		  #if TargetMacOS
		    
		    dim cmd as string = kDiskUtilCmd + "list -plist " + deviceIdentifier
		    dim r as shell = pExecuteShellCommand( cmd )
		    dim plist as CoreFoundation.CFPropertyList = CFType.CreateFromPListString( r.Result, CoreFoundation.kCFPropertyListMutableContainersAndLeaves )
		    if plist <> nil then
		      return plist.VariantValue
		    else
		      return nil
		    end if
		    
		  #else
		    
		    #pragma unused deviceIdentifier
		    
		  #endif
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Attributes( Hidden ) Protected Sub SelfTest()
		  dim devicesArr() as MacDeviceItem = Devices
		  dim partitionArr() as MacPartitionItem
		  for each d as MacDiskUtil.MacDeviceItem in devicesArr
		    partitionArr = d.Partitions
		    partitionArr = partitionArr // A place to break
		    
		    dim id as string = d
		    dim b as boolean = d.Mounted
		    dim status as MacDiskUtil.MacDeviceItem.MountType = d.MountStatus
		    dim f as UInt64 = d.FreeSpace
		    
		    for each p as MacDiskUtil.MacPartitionItem in partitionArr
		      f = p.FreeSpace
		      
		      f = f // A place to break
		    next
		  next d
		  
		  dim arr() as MacDiskUtil.MacFormatPersonality = Personalities
		  
		End Sub
	#tag EndMethod


	#tag Constant, Name = kDiskUtilCmd, Type = String, Dynamic = False, Default = \"/usr/sbin/diskutil ", Scope = Protected
	#tag EndConstant


	#tag ViewBehavior
		#tag ViewProperty
			Name="Index"
			Visible=true
			Group="ID"
			InitialValue="-2147483648"
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
