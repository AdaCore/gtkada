--  The mock implements the public GDrive interface through GtkAda's
--  Initialize_Class_Record/Add_Interface helpers. Async callbacks are queued
--  on the GLib main context so the test uses the documented async contract.

with Ada.Unchecked_Conversion;
with Glib;
with Glib.Error;
with Glib.GEnums;
with Glib.GMountOperations;
with Glib.Cancellable;
with Glib.GVolumes;
with Glib.Object;
with Glib.Types;
with Gtkada.Bindings;
with Gtkada.Types;
with System;

package body Drive_Mock is

   use type Glib.Gboolean;
   use type Glib.GDrives.Gdrive;
   use type System.Address;

   Drive_Class : aliased Glib.Object.Ada_GObject_Class :=
     Glib.Object.Uninitialized_Class;

   type Raw_Async_Callback is access procedure
     (Source_Object : System.Address;
      Result        : Glib.G_Async_Result;
      User_Data     : System.Address);
   pragma Convention (C, Raw_Async_Callback);

   function To_Async_Callback is new Ada.Unchecked_Conversion
     (System.Address, Raw_Async_Callback);

   Finishes_Succeed : Boolean := True;
   Polls_Started    : Natural := 0;

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
     (Source, Callback, User_Data : System.Address);

   procedure Interface_Init
     (Iface : Glib.Object.Interface_Description;
      Data  : System.Address);
   pragma Convention (C, Interface_Init);

   function Drive_Type return Glib.GType;
   procedure Make_Object (Self : not null access Drive_Record'Class);

   --  The virtual methods below return fixed values; the assertions in the
   --  main test verify that the Ada wrappers preserve those values.
   function Can_Eject (Self : Glib.GDrives.Gdrive) return Glib.Gboolean;
   pragma Convention (C, Can_Eject);
   function Can_Poll (Self : Glib.GDrives.Gdrive) return Glib.Gboolean;
   pragma Convention (C, Can_Poll);
   function Can_Start (Self : Glib.GDrives.Gdrive) return Glib.Gboolean;
   pragma Convention (C, Can_Start);
   function Can_Start_Degraded
     (Self : Glib.GDrives.Gdrive) return Glib.Gboolean;
   pragma Convention (C, Can_Start_Degraded);
   function Can_Stop (Self : Glib.GDrives.Gdrive) return Glib.Gboolean;
   pragma Convention (C, Can_Stop);
   function Has_Media (Self : Glib.GDrives.Gdrive) return Glib.Gboolean;
   pragma Convention (C, Has_Media);
   function Has_Volumes (Self : Glib.GDrives.Gdrive) return Glib.Gboolean;
   pragma Convention (C, Has_Volumes);
   function Automatic_Check
     (Self : Glib.GDrives.Gdrive) return Glib.Gboolean;
   pragma Convention (C, Automatic_Check);
   function Media_Removable
     (Self : Glib.GDrives.Gdrive) return Glib.Gboolean;
   pragma Convention (C, Media_Removable);
   function Is_Removable (Self : Glib.GDrives.Gdrive) return Glib.Gboolean;
   pragma Convention (C, Is_Removable);

   function Enumerate_Identifiers
     (Self : Glib.GDrives.Gdrive)
      return Gtkada.Bindings.chars_ptr_array_access;
   pragma Convention (C, Enumerate_Identifiers);
   function Get_Identifier
     (Self : Glib.GDrives.Gdrive;
      Kind : Gtkada.Types.Chars_Ptr) return Gtkada.Types.Chars_Ptr;
   pragma Convention (C, Get_Identifier);
   function Get_Name
     (Self : Glib.GDrives.Gdrive) return Gtkada.Types.Chars_Ptr;
   pragma Convention (C, Get_Name);
   function Get_Sort_Key
     (Self : Glib.GDrives.Gdrive) return Gtkada.Types.Chars_Ptr;
   pragma Convention (C, Get_Sort_Key);
   function Get_Volumes
     (Self : Glib.GDrives.Gdrive) return System.Address;
   pragma Convention (C, Get_Volumes);

   procedure Poll_For_Media
     (Self        : Glib.GDrives.Gdrive;
      Cancellable : System.Address;
      Callback    : System.Address;
      User_Data   : System.Address);
   pragma Convention (C, Poll_For_Media);
   procedure Eject
     (Self            : Glib.GDrives.Gdrive;
      Flags           : Glib.GEnums.GMount_Unmount_Flags;
      Mount_Operation : System.Address;
      Cancellable     : System.Address;
      Callback        : System.Address;
      User_Data       : System.Address);
   pragma Convention (C, Eject);
   procedure Start
     (Self            : Glib.GDrives.Gdrive;
      Flags           : Glib.GEnums.GDrive_Start_Flags;
      Mount_Operation : System.Address;
      Cancellable     : System.Address;
      Callback        : System.Address;
      User_Data       : System.Address);
   pragma Convention (C, Start);
   procedure Stop
     (Self            : Glib.GDrives.Gdrive;
      Flags           : Glib.GEnums.GMount_Unmount_Flags;
      Mount_Operation : System.Address;
      Cancellable     : System.Address;
      Callback        : System.Address;
      User_Data       : System.Address);
   pragma Convention (C, Stop);

   function Poll_Finish
     (Self   : Glib.GDrives.Gdrive;
      Result : Glib.G_Async_Result;
      Error  : out Glib.Error.GError) return Glib.Gboolean;
   pragma Convention (C, Poll_Finish);
   function Eject_Finish
     (Self   : Glib.GDrives.Gdrive;
      Result : Glib.G_Async_Result;
      Error  : out Glib.Error.GError) return Glib.Gboolean;
   pragma Convention (C, Eject_Finish);
   function Start_Finish
     (Self   : Glib.GDrives.Gdrive;
      Result : Glib.G_Async_Result;
      Error  : out Glib.Error.GError) return Glib.Gboolean;
   pragma Convention (C, Start_Finish);
   function Stop_Finish
     (Self   : Glib.GDrives.Gdrive;
      Result : Glib.G_Async_Result;
      Error  : out Glib.Error.GError) return Glib.Gboolean;
   pragma Convention (C, Stop_Finish);
   function Finish_Result
     (Error : out Glib.Error.GError) return Glib.Gboolean;

   -----------------
   -- Drive_Type --
   -----------------

   function Drive_Type return Glib.GType is
   begin
      if Glib.Object.Initialize_Class_Record
        (Ancestor     => Glib.GType_Object,
         Class_Record => Drive_Class'Access,
         Type_Name    => "GtkAdaCoverageDrive")
      then
         Glib.Object.Add_Interface
           (Drive_Class,
            Glib.GDrives.Get_Type,
            new Glib.Object.GInterface_Info'
              (Interface_Init     => Interface_Init'Access,
               Interface_Finalize => null,
               Interface_Data     => System.Null_Address));
      end if;
      return Drive_Class.The_Type;
   end Drive_Type;

   ------------------
   -- Make_Object --
   ------------------

   procedure Make_Object (Self : not null access Drive_Record'Class) is
   begin
      Glib.Object.G_New (Self, Drive_Type);
   end Make_Object;

   ------------------
   -- Interface_Init --
   ------------------

   procedure Interface_Init
     (Iface : Glib.Object.Interface_Description;
      Data  : System.Address)
   is
      pragma Unreferenced (Data);
      use Glib.GDrives;
   begin
      Set_Can_Eject (Iface, Can_Eject'Access);
      Set_Can_Poll_For_Media (Iface, Can_Poll'Access);
      Set_Can_Start (Iface, Can_Start'Access);
      Set_Can_Start_Degraded (Iface, Can_Start_Degraded'Access);
      Set_Can_Stop (Iface, Can_Stop'Access);
      Set_Eject_With_Operation (Iface, Eject'Access);
      Set_Eject_With_Operation_Finish (Iface, Eject_Finish'Access);
      Set_Enumerate_Identifiers (Iface, Enumerate_Identifiers'Access);
      Set_Get_Identifier (Iface, Get_Identifier'Access);
      Set_Get_Name (Iface, Get_Name'Access);
      Set_Get_Sort_Key (Iface, Get_Sort_Key'Access);
      Set_Get_Volumes (Iface, Get_Volumes'Access);
      Set_Has_Media (Iface, Has_Media'Access);
      Set_Has_Volumes (Iface, Has_Volumes'Access);
      Set_Is_Media_Check_Automatic (Iface, Automatic_Check'Access);
      Set_Is_Media_Removable (Iface, Media_Removable'Access);
      Set_Is_Removable (Iface, Is_Removable'Access);
      Set_Poll_For_Media (Iface, Poll_For_Media'Access);
      Set_Poll_For_Media_Finish (Iface, Poll_Finish'Access);
      Set_Start (Iface, Start'Access);
      Set_Start_Finish (Iface, Start_Finish'Access);
      Set_Stop (Iface, Stop'Access);
      Set_Stop_Finish (Iface, Stop_Finish'Access);
   end Interface_Init;

   ---------------
   -- Can_Eject --
   ---------------

   function Can_Eject (Self : Glib.GDrives.Gdrive) return Glib.Gboolean is
      pragma Unreferenced (Self);
   begin
      return 1;
   end Can_Eject;

   --------------
   -- Can_Poll --
   --------------

   function Can_Poll (Self : Glib.GDrives.Gdrive) return Glib.Gboolean is
      pragma Unreferenced (Self);
   begin
      return 1;
   end Can_Poll;

   ---------------
   -- Can_Start --
   ---------------

   function Can_Start (Self : Glib.GDrives.Gdrive) return Glib.Gboolean is
      pragma Unreferenced (Self);
   begin
      return 1;
   end Can_Start;

   -----------------------
   -- Can_Start_Degraded --
   -----------------------

   function Can_Start_Degraded
     (Self : Glib.GDrives.Gdrive) return Glib.Gboolean
   is
      pragma Unreferenced (Self);
   begin
      return 1;
   end Can_Start_Degraded;

   --------------
   -- Can_Stop --
   --------------

   function Can_Stop (Self : Glib.GDrives.Gdrive) return Glib.Gboolean is
      pragma Unreferenced (Self);
   begin
      return 1;
   end Can_Stop;

   ----------------
   -- Has_Media --
   ----------------

   function Has_Media (Self : Glib.GDrives.Gdrive) return Glib.Gboolean is
      pragma Unreferenced (Self);
   begin
      return 1;
   end Has_Media;

   ------------------
   -- Has_Volumes --
   ------------------

   function Has_Volumes (Self : Glib.GDrives.Gdrive) return Glib.Gboolean is
      pragma Unreferenced (Self);
   begin
      return 0;
   end Has_Volumes;

   --------------------
   -- Automatic_Check --
   --------------------

   function Automatic_Check
     (Self : Glib.GDrives.Gdrive) return Glib.Gboolean
   is
      pragma Unreferenced (Self);
   begin
      return 1;
   end Automatic_Check;

   --------------------
   -- Media_Removable --
   --------------------

   function Media_Removable
     (Self : Glib.GDrives.Gdrive) return Glib.Gboolean
   is
      pragma Unreferenced (Self);
   begin
      return 1;
   end Media_Removable;

   ------------------
   -- Is_Removable --
   ------------------

   function Is_Removable (Self : Glib.GDrives.Gdrive) return Glib.Gboolean is
      pragma Unreferenced (Self);
   begin
      return 1;
   end Is_Removable;

   ---------------------------
   -- Enumerate_Identifiers --
   ---------------------------

   function Enumerate_Identifiers
     (Self : Glib.GDrives.Gdrive)
      return Gtkada.Bindings.chars_ptr_array_access
   is
      pragma Unreferenced (Self);
   begin
      --  An empty NULL-terminated vector is a valid empty identifier list.
      return null;
   end Enumerate_Identifiers;

   --------------------
   -- Get_Identifier --
   --------------------

   function Get_Identifier
     (Self : Glib.GDrives.Gdrive;
      Kind : Gtkada.Types.Chars_Ptr) return Gtkada.Types.Chars_Ptr
   is
      pragma Unreferenced (Self, Kind);
   begin
      return Gtkada.Types.New_String ("test-device");
   end Get_Identifier;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Self : Glib.GDrives.Gdrive) return Gtkada.Types.Chars_Ptr
   is
      pragma Unreferenced (Self);
   begin
      return Gtkada.Types.New_String ("Coverage Drive");
   end Get_Name;

   ------------------
   -- Get_Sort_Key --
   ------------------

   function Get_Sort_Key
     (Self : Glib.GDrives.Gdrive) return Gtkada.Types.Chars_Ptr
   is
      pragma Unreferenced (Self);
   begin
      return Gtkada.Types.New_String ("drive-sort-key");
   end Get_Sort_Key;

   -----------------
   -- Get_Volumes --
   -----------------

   function Get_Volumes
     (Self : Glib.GDrives.Gdrive) return System.Address
   is
      pragma Unreferenced (Self);
   begin
      return System.Null_Address;
   end Get_Volumes;

   -----------------
   -- Queue_Async --
   -----------------

   procedure Queue_Async
     (Source, Callback, User_Data : System.Address)
   is
      Task_Object : System.Address;
   begin
      if Callback /= System.Null_Address then
         --  GTask supplies the valid GAsyncResult required by the matching
         --  GDrive finish method and invokes the callback on the main context.
         Task_Object := New_Task
           (Source, System.Null_Address, To_Async_Callback (Callback), User_Data);
         Return_Boolean (Task_Object, 1);
         Unref_Raw_Object (Task_Object);
      end if;
   end Queue_Async;

   --------------------
   -- Poll_For_Media --
   --------------------

   procedure Poll_For_Media
     (Self        : Glib.GDrives.Gdrive;
      Cancellable : System.Address;
      Callback    : System.Address;
      User_Data   : System.Address)
   is
      pragma Unreferenced (Cancellable);
   begin
      Polls_Started := Polls_Started + 1;
      Queue_Async (System.Address (Self), Callback, User_Data);
   end Poll_For_Media;

   -----------
   -- Eject --
   -----------

   procedure Eject
     (Self            : Glib.GDrives.Gdrive;
      Flags           : Glib.GEnums.GMount_Unmount_Flags;
      Mount_Operation : System.Address;
      Cancellable     : System.Address;
      Callback        : System.Address;
      User_Data       : System.Address)
   is
      pragma Unreferenced (Flags, Mount_Operation, Cancellable);
   begin
      Queue_Async (System.Address (Self), Callback, User_Data);
   end Eject;

   -----------
   -- Start --
   -----------

   procedure Start
     (Self            : Glib.GDrives.Gdrive;
      Flags           : Glib.GEnums.GDrive_Start_Flags;
      Mount_Operation : System.Address;
      Cancellable     : System.Address;
      Callback        : System.Address;
      User_Data       : System.Address)
   is
      pragma Unreferenced (Flags, Mount_Operation, Cancellable);
   begin
      Queue_Async (System.Address (Self), Callback, User_Data);
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop
     (Self            : Glib.GDrives.Gdrive;
      Flags           : Glib.GEnums.GMount_Unmount_Flags;
      Mount_Operation : System.Address;
      Cancellable     : System.Address;
      Callback        : System.Address;
      User_Data       : System.Address)
   is
      pragma Unreferenced (Flags, Mount_Operation, Cancellable);
   begin
      Queue_Async (System.Address (Self), Callback, User_Data);
   end Stop;

   ----------------
   -- Poll_Finish --
   ----------------

   function Finish_Result
     (Error : out Glib.Error.GError) return Glib.Gboolean
   is
   begin
      if Finishes_Succeed then
         Error := null;
         return 1;
      else
         Error := Glib.Error.Error_New
           (Glib.Quark_From_String ("gtkada-coverage-drive"), 1,
            "synthetic drive operation failure");
         return 0;
      end if;
   end Finish_Result;

   function Poll_Finish
     (Self   : Glib.GDrives.Gdrive;
      Result : Glib.G_Async_Result;
      Error  : out Glib.Error.GError) return Glib.Gboolean
   is
      pragma Unreferenced (Self, Result);
   begin
      return Finish_Result (Error);
   end Poll_Finish;

   -----------------
   -- Eject_Finish --
   -----------------

   function Eject_Finish
     (Self   : Glib.GDrives.Gdrive;
      Result : Glib.G_Async_Result;
      Error  : out Glib.Error.GError) return Glib.Gboolean
   is
      pragma Unreferenced (Self, Result);
   begin
      return Finish_Result (Error);
   end Eject_Finish;

   ----------------
   -- Start_Finish --
   ----------------

   function Start_Finish
     (Self   : Glib.GDrives.Gdrive;
      Result : Glib.G_Async_Result;
      Error  : out Glib.Error.GError) return Glib.Gboolean
   is
      pragma Unreferenced (Self, Result);
   begin
      return Finish_Result (Error);
   end Start_Finish;

   ---------------
   -- Stop_Finish --
   ---------------

   function Stop_Finish
     (Self   : Glib.GDrives.Gdrive;
      Result : Glib.G_Async_Result;
      Error  : out Glib.Error.GError) return Glib.Gboolean
   is
      pragma Unreferenced (Self, Result);
   begin
      return Finish_Result (Error);
   end Stop_Finish;

   ------------
   -- Create --
   ------------

   procedure Create (Self : out Drive) is
   begin
      Self := new Drive_Record;
      Make_Object (Self.all'Access);
   end Create;

   ------------
   -- Destroy --
   ------------

   procedure Destroy (Self : in out Drive) is
   begin
      if Self /= null then
         Glib.Object.Unref (Glib.Object.GObject (Self));
         Self := null;
      end if;
   end Destroy;

   -----------------
   -- As_Interface --
   -----------------

   function As_Interface (Self : Drive) return Glib.GDrives.Gdrive is
   begin
      return Glib.GDrives.Gdrive
        (Glib.Types.GType_Interface
           (Glib.Object.Get_Object (Self.all'Access)));
   end As_Interface;

   ---------------
   -- Raw_Object --
   ---------------

   function Raw_Object (Self : Drive) return System.Address is
   begin
      return Glib.Object.Get_Object (Self.all'Access);
   end Raw_Object;

   ------------------------
   -- Set_Finish_Succeeds --
   ------------------------

   procedure Set_Finish_Succeeds (Success : Boolean) is
   begin
      Finishes_Succeed := Success;
   end Set_Finish_Succeeds;

   ----------------
   -- Poll_Count --
   ----------------

   function Poll_Count return Natural is
   begin
      return Polls_Started;
   end Poll_Count;

end Drive_Mock;
