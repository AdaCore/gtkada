with Interfaces.C.Strings;
with Unchecked_Conversion;
with Unchecked_Deallocation;

with Glib.Type_Conversion_Hooks;

package body Glib.GObjects is

   procedure Free_User_Data (Data : in System.Address);
   --  Free the user data Data. This function should not be called directly

   -------------------------
   -- Conversion_Function --
   -------------------------

   function Conversion_Function
     (Obj : System.Address; Stub : GObject_Record'Class) return GObject
   is
      function Get_Type (Obj : System.Address) return GType;
      pragma Import (C, Get_Type, "ada_gobject_get_type");

      Name  : constant String := Type_Name (Get_Type (Obj));
      Hooks : Glib.Type_Conversion_Hooks.Hook_List_Access;

      use type Glib.Type_Conversion_Hooks.Hook_List_Access;

   begin
      Hooks := Glib.Type_Conversion_Hooks.Conversion_Hooks;

      while Hooks /= null loop
         declare
            R : GObject := Hooks.Func (Name);
         begin
            if R /= null then
               return R;
            end if;
         end;

         Hooks := Hooks.Next;
      end loop;

      return new GObject_Record'Class' (Stub);
   end Conversion_Function;

   --------------------
   -- Free_User_Data --
   --------------------

   procedure Free_User_Data (Data : in System.Address) is
      function Convert is new Unchecked_Conversion (System.Address, GObject);
      procedure Free is new Unchecked_Deallocation
        (GObject_Record'Class, GObject);
      Obj : GObject := Convert (Data);
   begin
      Free (Obj);
   end Free_User_Data;

   ----------------
   -- Get_Object --
   ----------------

   function Get_Object (Object : access GObject_Record'Class)
                        return System.Address is
   begin
      return Object.Ptr;
   end Get_Object;

   -------------------
   -- Get_User_Data --
   -------------------

   function Get_User_Data
     (Obj  : in System.Address;
      Stub : in GObject_Record'Class) return GObject
   is
      function Internal
        (Object : in System.Address;
         Quark  : in Glib.GQuark) return GObject;
      pragma Import (C, Internal, "g_object_get_qdata");

      use type System.Address;

      R : GObject;
   begin
      if Obj = System.Null_Address then
         return null;
      end if;

      if GtkAda_String_Quark = Glib.Unknown_Quark then
         GtkAda_String_Quark := Glib.Quark_From_String (GtkAda_String);
      end if;

      R := Internal (Obj, GtkAda_String_Quark);

      if R = null then
         R := Conversion_Function (Obj, Stub);
         --  This function will either simply return what we expect (Stub), or
         --  try to create the exact Ada type corresponding to the C type.
         Set_Object (R, Obj);
         Initialize_User_Data (R);
      end if;

      return R;
   end Get_User_Data;

   --------------------------
   -- Initialize_User_Data --
   --------------------------

   procedure Initialize_User_Data (Obj : access GObject_Record'Class) is
      function Internal
        (Object : in System.Address;
         Quark  : in Glib.GQuark) return GObject;
      pragma Import (C, Internal, "g_object_get_qdata");

      procedure Set_User_Data
        (Obj     : System.Address;
         Quark   : Glib.GQuark;
         Data    : System.Address;
         Destroy : System.Address);
      pragma Import (C, Set_User_Data, "g_object_set_qdata_full");

   begin
      if GtkAda_String_Quark = Glib.Unknown_Quark then
         GtkAda_String_Quark := Glib.Quark_From_String (GtkAda_String);
      end if;

      if Internal (Get_Object (Obj), GtkAda_String_Quark) = null then
         Set_User_Data (Get_Object (Obj), GtkAda_String_Quark,
                        Obj.all'Address, Free_User_Data'Address);
      end if;
   end Initialize_User_Data;

   ----------------
   -- Is_Created --
   ----------------

   function Is_Created (Object : in GObject_Record'Class) return Boolean is
      use type System.Address;
   begin
      return Object.Ptr /= System.Null_Address;
   end Is_Created;

   ----------------
   -- Set_Object --
   ----------------

   procedure Set_Object
     (Object : access GObject_Record'Class;
      Value  : in     System.Address) is
   begin
      Object.Ptr := Value;
   end Set_Object;

   ---------------
   -- Type_Name --
   ---------------

   function Type_Name (Type_Num : in GType) return String is
      function Internal (Type_Num : in GType)
                         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "g_type_name");
   begin
      return Interfaces.C.Strings.Value (Internal (Type_Num));
   end Type_Name;

   --------------------
   -- Type_From_Name --
   --------------------

   function Type_From_Name (Name : in String) return GType is
      function Internal (Name : String) return GType;
      pragma Import (C, Internal, "g_type_from_name");
   begin
      return Internal (Name & ASCII.NUL);
   end Type_From_Name;

   --------------------
   -- Unchecked_Cast --
   --------------------

   function Unchecked_Cast
     (Obj  : access GObject_Record'Class;
      Stub : GObject_Record'Class) return GObject
   is
      Object : GObject := GObject (Obj);
      Result : GObject := new GObject_Record'Class' (Stub);

      procedure Set_User_Data
        (Obj     : System.Address;
         Quark   : Glib.GQuark;
         Data    : GObject;
         Destroy : System.Address);
      pragma Import (C, Set_User_Data, "g_object_set_qdata_full");

      procedure Free is new Unchecked_Deallocation
        (GObject_Record'Class, GObject);

   begin
      Result.Ptr := Obj.Ptr;
      Set_User_Data
        (Obj.Ptr, GtkAda_String_Quark, Result, Free_User_Data'Address);
      Free (Object);
      return Result;
   end Unchecked_Cast;

end Glib.GObjects;
