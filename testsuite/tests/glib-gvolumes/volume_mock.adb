--  Implement the GVolume interface with fixed metadata and queued GTasks.
--  This avoids depending on removable media attached to the test host.

with Ada.Unchecked_Conversion;
with Glib;
with Glib.Error;
with Glib.GEnums;
with Glib.GVolumes;
with Glib.Object;
with Glib.Types;
with Gtkada.Bindings;
with Gtkada.Types;
with System;

package body Volume_Mock is

   use type Glib.Gboolean;
   use type System.Address;

   Volume_Class : aliased Glib.Object.Ada_GObject_Class :=
     Glib.Object.Uninitialized_Class;

   --  GTask expects a C callback pointer, matching the interface vfunc ABI.
   type Raw_Async_Callback is access procedure
     (Source_Object : System.Address;
      Result        : Glib.G_Async_Result;
      User_Data     : System.Address);
   pragma Convention (C, Raw_Async_Callback);

   function To_Async_Callback is new Ada.Unchecked_Conversion
     (System.Address, Raw_Async_Callback);

   Finish_Succeeds : Boolean := True;
   Ejects          : Natural := 0;
   Mounts          : Natural := 0;

   --  These GLib entry points provide a real asynchronous result for tests.
   function New_Task
     (Source_Object : System.Address;
      Cancellable   : System.Address;
      Callback      : Raw_Async_Callback;
      User_Data     : System.Address) return System.Address;
   pragma Import (C, New_Task, "g_task_new");
   procedure Return_Boolean
     (Task_Object : System.Address; Success : Glib.Gboolean);
   pragma Import (C, Return_Boolean, "g_task_return_boolean");
   procedure Unref_Raw_Object (Object : System.Address);
   pragma Import (C, Unref_Raw_Object, "g_object_unref");

   procedure Queue_Async
     (Source_Object, Callback, User_Data : System.Address);

   procedure Interface_Init
     (Iface : Glib.Object.Interface_Description;
      Data  : System.Address);
   pragma Convention (C, Interface_Init);

   function Volume_Type return Glib.GType;
   procedure Make_Object (Self : not null access Volume_Record'Class);

   --  The vfuncs return stable values that the public Ada wrappers assert.
   function Can_Eject (Self : Glib.GVolumes.Gvolume) return Glib.Gboolean;
   pragma Convention (C, Can_Eject);
   function Can_Mount (Self : Glib.GVolumes.Gvolume) return Glib.Gboolean;
   pragma Convention (C, Can_Mount);
   function Enumerate_Identifiers
     (Self : Glib.GVolumes.Gvolume)
      return Gtkada.Bindings.chars_ptr_array_access;
   pragma Convention (C, Enumerate_Identifiers);
   function Get_Identifier
     (Self : Glib.GVolumes.Gvolume;
      Kind : Gtkada.Types.Chars_Ptr) return Gtkada.Types.Chars_Ptr;
   pragma Convention (C, Get_Identifier);
   function Get_Name
     (Self : Glib.GVolumes.Gvolume) return Gtkada.Types.Chars_Ptr;
   pragma Convention (C, Get_Name);
   function Get_Sort_Key
     (Self : Glib.GVolumes.Gvolume) return Gtkada.Types.Chars_Ptr;
   pragma Convention (C, Get_Sort_Key);
   function Get_Uuid
     (Self : Glib.GVolumes.Gvolume) return Gtkada.Types.Chars_Ptr;
   pragma Convention (C, Get_Uuid);
   function Should_Automount
     (Self : Glib.GVolumes.Gvolume) return Glib.Gboolean;
   pragma Convention (C, Should_Automount);

   --  The async vfuncs pass their callback and user data through GTask.
   procedure Eject
     (Self            : Glib.GVolumes.Gvolume;
      Flags           : Glib.GEnums.GMount_Unmount_Flags;
      Mount_Operation : System.Address;
      Cancellable     : System.Address;
      Callback        : System.Address;
      User_Data       : System.Address);
   pragma Convention (C, Eject);
   function Eject_Finish
     (Self   : Glib.GVolumes.Gvolume;
      Result : Glib.G_Async_Result;
      Error  : out Glib.Error.GError) return Glib.Gboolean;
   pragma Convention (C, Eject_Finish);
   procedure Mount
     (Self            : Glib.GVolumes.Gvolume;
      Flags           : Glib.GEnums.GMount_Mount_Flags;
      Mount_Operation : System.Address;
      Cancellable     : System.Address;
      Callback        : System.Address;
      User_Data       : System.Address);
   pragma Convention (C, Mount);
   function Mount_Finish
     (Self   : Glib.GVolumes.Gvolume;
      Result : Glib.G_Async_Result;
      Error  : out Glib.Error.GError) return Glib.Gboolean;
   pragma Convention (C, Mount_Finish);

   -----------------
   -- Volume_Type --
   -----------------

   function Volume_Type return Glib.GType is
   begin
      if Glib.Object.Initialize_Class_Record
        (Ancestor     => Glib.GType_Object,
         Class_Record => Volume_Class'Access,
         Type_Name    => "GtkAdaCoverageVolume")
      then
         Glib.Object.Add_Interface
           (Volume_Class,
            Glib.GVolumes.Get_Type,
            new Glib.Object.GInterface_Info'
              (Interface_Init     => Interface_Init'Access,
               Interface_Finalize => null,
               Interface_Data     => System.Null_Address));
      end if;
      return Volume_Class.The_Type;
   end Volume_Type;

   ------------------
   -- Make_Object --
   ------------------

   procedure Make_Object (Self : not null access Volume_Record'Class) is
   begin
      Glib.Object.G_New (Self, Volume_Type);
   end Make_Object;

   --------------------
   -- Interface_Init --
   --------------------

   procedure Interface_Init
     (Iface : Glib.Object.Interface_Description;
      Data  : System.Address)
   is
      pragma Unreferenced (Data);
      use Glib.GVolumes;
   begin
      Set_Can_Eject (Iface, Can_Eject'Access);
      Set_Can_Mount (Iface, Can_Mount'Access);
      Set_Eject_With_Operation (Iface, Eject'Access);
      Set_Eject_With_Operation_Finish (Iface, Eject_Finish'Access);
      Set_Enumerate_Identifiers (Iface, Enumerate_Identifiers'Access);
      Set_Get_Identifier (Iface, Get_Identifier'Access);
      Set_Get_Name (Iface, Get_Name'Access);
      Set_Get_Sort_Key (Iface, Get_Sort_Key'Access);
      Set_Get_Uuid (Iface, Get_Uuid'Access);
      Set_Mount_Fn (Iface, Mount'Access);
      Set_Mount_Finish (Iface, Mount_Finish'Access);
      Set_Should_Automount (Iface, Should_Automount'Access);
   end Interface_Init;

   ------------------
   -- Queue_Async --
   ------------------

   procedure Queue_Async
     (Source_Object, Callback, User_Data : System.Address)
   is
      Task_Object : System.Address;
   begin
      if Callback /= System.Null_Address then
         --  A GTask callback runs on the main context with a valid result.
         Task_Object := New_Task
           (Source_Object, System.Null_Address,
            To_Async_Callback (Callback), User_Data);
         Return_Boolean (Task_Object, 1);
         Unref_Raw_Object (Task_Object);
      end if;
   end Queue_Async;

   ---------------
   -- Can_Eject --
   ---------------

   function Can_Eject (Self : Glib.GVolumes.Gvolume) return Glib.Gboolean is
      pragma Unreferenced (Self);
   begin
      return 1;
   end Can_Eject;

   --------------
   -- Can_Mount --
   --------------

   function Can_Mount (Self : Glib.GVolumes.Gvolume) return Glib.Gboolean is
      pragma Unreferenced (Self);
   begin
      return 1;
   end Can_Mount;

   ---------------------------
   -- Enumerate_Identifiers --
   ---------------------------

   function Enumerate_Identifiers
     (Self : Glib.GVolumes.Gvolume)
      return Gtkada.Bindings.chars_ptr_array_access
   is
      pragma Unreferenced (Self);
   begin
      --  A null-terminated empty vector is a valid result.
      return null;
   end Enumerate_Identifiers;

   --------------------
   -- Get_Identifier --
   --------------------

   function Get_Identifier
     (Self : Glib.GVolumes.Gvolume;
      Kind : Gtkada.Types.Chars_Ptr) return Gtkada.Types.Chars_Ptr
   is
      pragma Unreferenced (Self, Kind);
   begin
      return Gtkada.Types.New_String ("volume-test-id");
   end Get_Identifier;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Self : Glib.GVolumes.Gvolume) return Gtkada.Types.Chars_Ptr
   is
      pragma Unreferenced (Self);
   begin
      return Gtkada.Types.New_String ("Coverage Volume");
   end Get_Name;

   ------------------
   -- Get_Sort_Key --
   ------------------

   function Get_Sort_Key
     (Self : Glib.GVolumes.Gvolume) return Gtkada.Types.Chars_Ptr
   is
      pragma Unreferenced (Self);
   begin
      return Gtkada.Types.Null_Ptr;
   end Get_Sort_Key;

   --------------
   -- Get_Uuid --
   --------------

   function Get_Uuid
     (Self : Glib.GVolumes.Gvolume) return Gtkada.Types.Chars_Ptr
   is
      pragma Unreferenced (Self);
   begin
      return Gtkada.Types.New_String ("volume-test-uuid");
   end Get_Uuid;

   ---------------------
   -- Should_Automount --
   ---------------------

   function Should_Automount
     (Self : Glib.GVolumes.Gvolume) return Glib.Gboolean
   is
      pragma Unreferenced (Self);
   begin
      return 1;
   end Should_Automount;

   -----------
   -- Eject --
   -----------

   procedure Eject
     (Self            : Glib.GVolumes.Gvolume;
      Flags           : Glib.GEnums.GMount_Unmount_Flags;
      Mount_Operation : System.Address;
      Cancellable     : System.Address;
      Callback        : System.Address;
      User_Data       : System.Address)
   is
      pragma Unreferenced (Flags, Mount_Operation, Cancellable);
   begin
      Ejects := Ejects + 1;
      Queue_Async (System.Address (Self), Callback, User_Data);
   end Eject;

   -----------------
   -- Eject_Finish --
   -----------------

   function Eject_Finish
     (Self   : Glib.GVolumes.Gvolume;
      Result : Glib.G_Async_Result;
      Error  : out Glib.Error.GError) return Glib.Gboolean
   is
      pragma Unreferenced (Self, Result);
   begin
      if Finish_Succeeds then
         Error := null;
         return 1;
      else
         Error := Glib.Error.Error_New
           (Glib.Quark_From_String ("coverage-volume"), 1,
            "mock eject error");
         return 0;
      end if;
   end Eject_Finish;

   -----------
   -- Mount --
   -----------

   procedure Mount
     (Self            : Glib.GVolumes.Gvolume;
      Flags           : Glib.GEnums.GMount_Mount_Flags;
      Mount_Operation : System.Address;
      Cancellable     : System.Address;
      Callback        : System.Address;
      User_Data       : System.Address)
   is
      pragma Unreferenced (Flags, Mount_Operation, Cancellable);
   begin
      Mounts := Mounts + 1;
      Queue_Async (System.Address (Self), Callback, User_Data);
   end Mount;

   ------------------
   -- Mount_Finish --
   ------------------

   function Mount_Finish
     (Self   : Glib.GVolumes.Gvolume;
      Result : Glib.G_Async_Result;
      Error  : out Glib.Error.GError) return Glib.Gboolean
   is
      pragma Unreferenced (Self, Result);
   begin
      if Finish_Succeeds then
         Error := null;
         return 1;
      else
         Error := Glib.Error.Error_New
           (Glib.Quark_From_String ("coverage-volume"), 2,
            "mock mount error");
         return 0;
      end if;
   end Mount_Finish;

   ------------
   -- Create --
   ------------

   procedure Create (Self : out Volume) is
   begin
      Self := new Volume_Record;
      Make_Object (Self.all'Access);
   end Create;

   ------------
   -- Destroy --
   ------------

   procedure Destroy (Self : in out Volume) is
   begin
      if Self /= null then
         Glib.Object.Unref (Glib.Object.GObject (Self));
         Self := null;
      end if;
   end Destroy;

   -----------------
   -- As_Interface --
   -----------------

   function As_Interface (Self : Volume) return Glib.GVolumes.Gvolume is
   begin
      return Glib.GVolumes.Gvolume
        (Glib.Types.GType_Interface
           (Glib.Object.Get_Object (Self.all'Access)));
   end As_Interface;

   ----------------
   -- Raw_Object --
   ----------------

   function Raw_Object (Self : Volume) return System.Address is
   begin
      return Glib.Object.Get_Object (Self.all'Access);
   end Raw_Object;

   -------------------------
   -- Set_Finish_Succeeds --
   -------------------------

   procedure Set_Finish_Succeeds (Success : Boolean) is
   begin
      Finish_Succeeds := Success;
   end Set_Finish_Succeeds;

   -----------------
   -- Eject_Count --
   -----------------

   function Eject_Count return Natural is
   begin
      return Ejects;
   end Eject_Count;

   -----------------
   -- Mount_Count --
   -----------------

   function Mount_Count return Natural is
   begin
      return Mounts;
   end Mount_Count;

end Volume_Mock;
