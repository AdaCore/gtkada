--  Implement just the GMount virtual methods used by main.adb. GTask gives
--  asynchronous methods a real GAsyncResult and calls back on the main loop.

with Ada.Unchecked_Conversion;
with Glib;
with Glib.Error;
with Glib.GEnums;
with Glib.GMounts;
with Glib.Object;
with Glib.Types;
with Gtkada.Bindings;
with Gtkada.Types;
with System;

package body Mount_Mock is

   use type Glib.Gboolean;
   use type Glib.GMounts.Gmount;
   use type System.Address;

   Mount_Class : aliased Glib.Object.Ada_GObject_Class :=
     Glib.Object.Uninitialized_Class;

   type Raw_Async_Callback is access procedure
     (Source_Object : System.Address;
      Result        : Glib.G_Async_Result;
      User_Data     : System.Address);
   pragma Convention (C, Raw_Async_Callback);

   function To_Async_Callback is new Ada.Unchecked_Conversion
     (System.Address, Raw_Async_Callback);

   Finish_Succeeds : Boolean := True;
   Sync_Succeeds   : Boolean := True;
   Ejects          : Natural := 0;
   Guesses         : Natural := 0;
   Remounts        : Natural := 0;
   Unmounts        : Natural := 0;

   --  These GTask entry points are used only to schedule the mock callback.
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

   function Mount_Type return Glib.GType;
   procedure Make_Object (Self : not null access Mount_Record'Class);

   --  Interface callbacks return fixed metadata so the wrappers can be
   --  checked without consulting a host volume monitor.
   function Can_Eject (Self : Glib.GMounts.Gmount) return Glib.Gboolean;
   pragma Convention (C, Can_Eject);
   function Can_Unmount (Self : Glib.GMounts.Gmount) return Glib.Gboolean;
   pragma Convention (C, Can_Unmount);
   function Get_Name
     (Self : Glib.GMounts.Gmount) return Gtkada.Types.Chars_Ptr;
   pragma Convention (C, Get_Name);
   function Get_Sort_Key
     (Self : Glib.GMounts.Gmount) return Gtkada.Types.Chars_Ptr;
   pragma Convention (C, Get_Sort_Key);
   function Get_Uuid
     (Self : Glib.GMounts.Gmount) return Gtkada.Types.Chars_Ptr;
   pragma Convention (C, Get_Uuid);

   procedure Eject
     (Self            : Glib.GMounts.Gmount;
      Flags           : Glib.GEnums.GMount_Unmount_Flags;
      Mount_Operation : System.Address;
      Cancellable     : System.Address;
      Callback        : System.Address;
      User_Data       : System.Address);
   pragma Convention (C, Eject);
   function Eject_Finish
     (Self   : Glib.GMounts.Gmount;
      Result : Glib.G_Async_Result;
      Error  : out Glib.Error.GError) return Glib.Gboolean;
   pragma Convention (C, Eject_Finish);

   procedure Guess_Content_Type
     (Self         : Glib.GMounts.Gmount;
      Force_Rescan : Glib.Gboolean;
      Cancellable  : System.Address;
      Callback     : System.Address;
      User_Data    : System.Address);
   pragma Convention (C, Guess_Content_Type);
   function Guess_Content_Type_Finish
     (Self   : Glib.GMounts.Gmount;
      Result : Glib.G_Async_Result;
      Error  : out Glib.Error.GError)
      return Gtkada.Bindings.chars_ptr_array_access;
   pragma Convention (C, Guess_Content_Type_Finish);
   function Guess_Content_Type_Sync
     (Self         : Glib.GMounts.Gmount;
      Force_Rescan : Glib.Gboolean;
      Cancellable  : System.Address;
      Error        : out Glib.Error.GError)
      return Gtkada.Bindings.chars_ptr_array_access;
   pragma Convention (C, Guess_Content_Type_Sync);

   procedure Remount
     (Self            : Glib.GMounts.Gmount;
      Flags           : Glib.GEnums.GMount_Mount_Flags;
      Mount_Operation : System.Address;
      Cancellable     : System.Address;
      Callback        : System.Address;
      User_Data       : System.Address);
   pragma Convention (C, Remount);
   function Remount_Finish
     (Self   : Glib.GMounts.Gmount;
      Result : Glib.G_Async_Result;
      Error  : out Glib.Error.GError) return Glib.Gboolean;
   pragma Convention (C, Remount_Finish);

   procedure Unmount
     (Self            : Glib.GMounts.Gmount;
      Flags           : Glib.GEnums.GMount_Unmount_Flags;
      Mount_Operation : System.Address;
      Cancellable     : System.Address;
      Callback        : System.Address;
      User_Data       : System.Address);
   pragma Convention (C, Unmount);
   function Unmount_Finish
     (Self   : Glib.GMounts.Gmount;
      Result : Glib.G_Async_Result;
      Error  : out Glib.Error.GError) return Glib.Gboolean;
   pragma Convention (C, Unmount_Finish);

   ----------------
   -- Mount_Type --
   ----------------

   function Mount_Type return Glib.GType is
   begin
      if Glib.Object.Initialize_Class_Record
        (Ancestor     => Glib.GType_Object,
         Class_Record => Mount_Class'Access,
         Type_Name    => "GtkAdaCoverageMount")
      then
         Glib.Object.Add_Interface
           (Mount_Class,
            Glib.GMounts.Get_Type,
            new Glib.Object.GInterface_Info'
              (Interface_Init     => Interface_Init'Access,
               Interface_Finalize => null,
               Interface_Data     => System.Null_Address));
      end if;
      return Mount_Class.The_Type;
   end Mount_Type;

   ------------------
   -- Make_Object --
   ------------------

   procedure Make_Object (Self : not null access Mount_Record'Class) is
   begin
      Glib.Object.G_New (Self, Mount_Type);
   end Make_Object;

   --------------------
   -- Interface_Init --
   --------------------

   procedure Interface_Init
     (Iface : Glib.Object.Interface_Description;
      Data  : System.Address)
   is
      pragma Unreferenced (Data);
      use Glib.GMounts;
   begin
      Set_Can_Eject (Iface, Can_Eject'Access);
      Set_Can_Unmount (Iface, Can_Unmount'Access);
      Set_Eject_With_Operation (Iface, Eject'Access);
      Set_Eject_With_Operation_Finish (Iface, Eject_Finish'Access);
      Set_Get_Name (Iface, Get_Name'Access);
      Set_Get_Sort_Key (Iface, Get_Sort_Key'Access);
      Set_Get_Uuid (Iface, Get_Uuid'Access);
      Set_Guess_Content_Type (Iface, Guess_Content_Type'Access);
      Set_Guess_Content_Type_Finish
        (Iface, Guess_Content_Type_Finish'Access);
      Set_Guess_Content_Type_Sync
        (Iface, Guess_Content_Type_Sync'Access);
      Set_Remount (Iface, Remount'Access);
      Set_Remount_Finish (Iface, Remount_Finish'Access);
      Set_Unmount_With_Operation (Iface, Unmount'Access);
      Set_Unmount_With_Operation_Finish (Iface, Unmount_Finish'Access);
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
         --  A null callback is a documented fire-and-forget call path.
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

   function Can_Eject (Self : Glib.GMounts.Gmount) return Glib.Gboolean is
      pragma Unreferenced (Self);
   begin
      return 1;
   end Can_Eject;

   -----------------
   -- Can_Unmount --
   -----------------

   function Can_Unmount (Self : Glib.GMounts.Gmount) return Glib.Gboolean is
      pragma Unreferenced (Self);
   begin
      return 1;
   end Can_Unmount;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Self : Glib.GMounts.Gmount) return Gtkada.Types.Chars_Ptr
   is
      pragma Unreferenced (Self);
   begin
      return Gtkada.Types.New_String ("Coverage Mount");
   end Get_Name;

   ------------------
   -- Get_Sort_Key --
   ------------------

   function Get_Sort_Key
     (Self : Glib.GMounts.Gmount) return Gtkada.Types.Chars_Ptr
   is
      pragma Unreferenced (Self);
   begin
      --  GMount permits a missing sort key; this also tests null conversion.
      return Gtkada.Types.Null_Ptr;
   end Get_Sort_Key;

   --------------
   -- Get_Uuid --
   --------------

   function Get_Uuid
     (Self : Glib.GMounts.Gmount) return Gtkada.Types.Chars_Ptr
   is
      pragma Unreferenced (Self);
   begin
      return Gtkada.Types.New_String ("coverage-mount-uuid");
   end Get_Uuid;

   -----------
   -- Eject --
   -----------

   procedure Eject
     (Self            : Glib.GMounts.Gmount;
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

   ------------------
   -- Eject_Finish --
   ------------------

   function Eject_Finish
     (Self   : Glib.GMounts.Gmount;
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
           (Glib.Quark_From_String ("coverage-mount"), 1, "mock eject error");
         return 0;
      end if;
   end Eject_Finish;

   -----------------------
   -- Guess_Content_Type --
   -----------------------

   procedure Guess_Content_Type
     (Self         : Glib.GMounts.Gmount;
      Force_Rescan : Glib.Gboolean;
      Cancellable  : System.Address;
      Callback     : System.Address;
      User_Data    : System.Address)
   is
      pragma Unreferenced (Force_Rescan, Cancellable);
   begin
      Guesses := Guesses + 1;
      Queue_Async (System.Address (Self), Callback, User_Data);
   end Guess_Content_Type;

   -------------------------------
   -- Guess_Content_Type_Finish --
   -------------------------------

   function Guess_Content_Type_Finish
     (Self   : Glib.GMounts.Gmount;
      Result : Glib.G_Async_Result;
      Error  : out Glib.Error.GError)
      return Gtkada.Bindings.chars_ptr_array_access
   is
      pragma Unreferenced (Self, Result);
   begin
      if Finish_Succeeds then
         Error := null;
      else
         Error := Glib.Error.Error_New
           (Glib.Quark_From_String ("coverage-mount"), 2,
            "mock content guessing error");
      end if;
      --  An empty NULL-terminated vector is a valid empty content list.
      return null;
   end Guess_Content_Type_Finish;

   ----------------------------
   -- Guess_Content_Type_Sync --
   ----------------------------

   function Guess_Content_Type_Sync
     (Self         : Glib.GMounts.Gmount;
      Force_Rescan : Glib.Gboolean;
      Cancellable  : System.Address;
      Error        : out Glib.Error.GError)
      return Gtkada.Bindings.chars_ptr_array_access
   is
      pragma Unreferenced (Self, Force_Rescan, Cancellable);
   begin
      if Sync_Succeeds then
         Error := null;
      else
         Error := Glib.Error.Error_New
           (Glib.Quark_From_String ("coverage-mount"), 3,
            "mock synchronous guessing error");
      end if;
      return null;
   end Guess_Content_Type_Sync;

   -------------
   -- Remount --
   -------------

   procedure Remount
     (Self            : Glib.GMounts.Gmount;
      Flags           : Glib.GEnums.GMount_Mount_Flags;
      Mount_Operation : System.Address;
      Cancellable     : System.Address;
      Callback        : System.Address;
      User_Data       : System.Address)
   is
      pragma Unreferenced (Flags, Mount_Operation, Cancellable);
   begin
      Remounts := Remounts + 1;
      Queue_Async (System.Address (Self), Callback, User_Data);
   end Remount;

   --------------------
   -- Remount_Finish --
   --------------------

   function Remount_Finish
     (Self   : Glib.GMounts.Gmount;
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
           (Glib.Quark_From_String ("coverage-mount"), 4,
            "mock remount error");
         return 0;
      end if;
   end Remount_Finish;

   --------------------
   -- Unmount --
   --------------------

   procedure Unmount
     (Self            : Glib.GMounts.Gmount;
      Flags           : Glib.GEnums.GMount_Unmount_Flags;
      Mount_Operation : System.Address;
      Cancellable     : System.Address;
      Callback        : System.Address;
      User_Data       : System.Address)
   is
      pragma Unreferenced (Flags, Mount_Operation, Cancellable);
   begin
      Unmounts := Unmounts + 1;
      Queue_Async (System.Address (Self), Callback, User_Data);
   end Unmount;

   ---------------------
   -- Unmount_Finish --
   ---------------------

   function Unmount_Finish
     (Self   : Glib.GMounts.Gmount;
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
           (Glib.Quark_From_String ("coverage-mount"), 5,
            "mock unmount error");
         return 0;
      end if;
   end Unmount_Finish;

   ------------
   -- Create --
   ------------

   procedure Create (Self : out Mount) is
   begin
      Self := new Mount_Record;
      Make_Object (Self.all'Access);
   end Create;

   ------------
   -- Destroy --
   ------------

   procedure Destroy (Self : in out Mount) is
   begin
      if Self /= null then
         Glib.Object.Unref (Glib.Object.GObject (Self));
         Self := null;
      end if;
   end Destroy;

   -----------------
   -- As_Interface --
   -----------------

   function As_Interface (Self : Mount) return Glib.GMounts.Gmount is
   begin
      return Glib.GMounts.Gmount
        (Glib.Types.GType_Interface
           (Glib.Object.Get_Object (Self.all'Access)));
   end As_Interface;

   ----------------
   -- Raw_Object --
   ----------------

   function Raw_Object (Self : Mount) return System.Address is
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

   ----------------------
   -- Set_Sync_Succeeds --
   ----------------------

   procedure Set_Sync_Succeeds (Success : Boolean) is
   begin
      Sync_Succeeds := Success;
   end Set_Sync_Succeeds;

   -----------------
   -- Eject_Count --
   -----------------

   function Eject_Count return Natural is
   begin
      return Ejects;
   end Eject_Count;

   -----------------
   -- Guess_Count --
   -----------------

   function Guess_Count return Natural is
   begin
      return Guesses;
   end Guess_Count;

   -------------------
   -- Remount_Count --
   -------------------

   function Remount_Count return Natural is
   begin
      return Remounts;
   end Remount_Count;

   -------------------
   -- Unmount_Count --
   -------------------

   function Unmount_Count return Natural is
   begin
      return Unmounts;
   end Unmount_Count;

end Mount_Mock;
