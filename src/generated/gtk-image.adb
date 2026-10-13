------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Image is

   package Type_Conversion_Gtk_Image is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Image_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Image);

   -------------------
   -- Gtk_Image_New --
   -------------------

   function Gtk_Image_New return Gtk_Image is
      Self : constant Gtk_Image := new Gtk_Image_Record;
   begin
      Gtk.Image.Initialize (Self);
      return Self;
   end Gtk_Image_New;

   -----------------------------
   -- Gtk_Image_New_From_File --
   -----------------------------

   function Gtk_Image_New_From_File
      (Filename : UTF8_String) return Gtk_Image
   is
      Self : constant Gtk_Image := new Gtk_Image_Record;
   begin
      Gtk.Image.Initialize_From_File (Self, Filename);
      return Self;
   end Gtk_Image_New_From_File;

   ------------------------------
   -- Gtk_Image_New_From_Gicon --
   ------------------------------

   function Gtk_Image_New_From_Gicon
      (Icon : Glib.G_Icon.G_Icon) return Gtk_Image
   is
      Self : constant Gtk_Image := new Gtk_Image_Record;
   begin
      Gtk.Image.Initialize_From_Gicon (Self, Icon);
      return Self;
   end Gtk_Image_New_From_Gicon;

   ----------------------------------
   -- Gtk_Image_New_From_Icon_Name --
   ----------------------------------

   function Gtk_Image_New_From_Icon_Name
      (Icon_Name : UTF8_String := "") return Gtk_Image
   is
      Self : constant Gtk_Image := new Gtk_Image_Record;
   begin
      Gtk.Image.Initialize_From_Icon_Name (Self, Icon_Name);
      return Self;
   end Gtk_Image_New_From_Icon_Name;

   ----------------------------------
   -- Gtk_Image_New_From_Paintable --
   ----------------------------------

   function Gtk_Image_New_From_Paintable
      (Paintable : Gdk.Paintable.Gdk_Paintable) return Gtk_Image
   is
      Self : constant Gtk_Image := new Gtk_Image_Record;
   begin
      Gtk.Image.Initialize_From_Paintable (Self, Paintable);
      return Self;
   end Gtk_Image_New_From_Paintable;

   -------------------------------
   -- Gtk_Image_New_From_Pixbuf --
   -------------------------------

   function Gtk_Image_New_From_Pixbuf
      (Pixbuf : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class) return Gtk_Image
   is
      Self : constant Gtk_Image := new Gtk_Image_Record;
   begin
      Gtk.Image.Initialize_From_Pixbuf (Self, Pixbuf);
      return Self;
   end Gtk_Image_New_From_Pixbuf;

   ---------------------------------
   -- Gtk_Image_New_From_Resource --
   ---------------------------------

   function Gtk_Image_New_From_Resource
      (Resource_Path : UTF8_String) return Gtk_Image
   is
      Self : constant Gtk_Image := new Gtk_Image_Record;
   begin
      Gtk.Image.Initialize_From_Resource (Self, Resource_Path);
      return Self;
   end Gtk_Image_New_From_Resource;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Image) is
   begin
      Self := new Gtk_Image_Record;
      Gtk.Image.Initialize (Self);
   end Gtk_New;

   -----------------------
   -- Gtk_New_From_File --
   -----------------------

   procedure Gtk_New_From_File
      (Self     : out Gtk_Image;
       Filename : UTF8_String)
   is
   begin
      Self := new Gtk_Image_Record;
      Gtk.Image.Initialize_From_File (Self, Filename);
   end Gtk_New_From_File;

   ------------------------
   -- Gtk_New_From_Gicon --
   ------------------------

   procedure Gtk_New_From_Gicon
      (Self : out Gtk_Image;
       Icon : Glib.G_Icon.G_Icon)
   is
   begin
      Self := new Gtk_Image_Record;
      Gtk.Image.Initialize_From_Gicon (Self, Icon);
   end Gtk_New_From_Gicon;

   ----------------------------
   -- Gtk_New_From_Icon_Name --
   ----------------------------

   procedure Gtk_New_From_Icon_Name
      (Self      : out Gtk_Image;
       Icon_Name : UTF8_String := "")
   is
   begin
      Self := new Gtk_Image_Record;
      Gtk.Image.Initialize_From_Icon_Name (Self, Icon_Name);
   end Gtk_New_From_Icon_Name;

   ----------------------------
   -- Gtk_New_From_Paintable --
   ----------------------------

   procedure Gtk_New_From_Paintable
      (Self      : out Gtk_Image;
       Paintable : Gdk.Paintable.Gdk_Paintable)
   is
   begin
      Self := new Gtk_Image_Record;
      Gtk.Image.Initialize_From_Paintable (Self, Paintable);
   end Gtk_New_From_Paintable;

   -------------------------
   -- Gtk_New_From_Pixbuf --
   -------------------------

   procedure Gtk_New_From_Pixbuf
      (Self   : out Gtk_Image;
       Pixbuf : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
   begin
      Self := new Gtk_Image_Record;
      Gtk.Image.Initialize_From_Pixbuf (Self, Pixbuf);
   end Gtk_New_From_Pixbuf;

   ---------------------------
   -- Gtk_New_From_Resource --
   ---------------------------

   procedure Gtk_New_From_Resource
      (Self          : out Gtk_Image;
       Resource_Path : UTF8_String)
   is
   begin
      Self := new Gtk_Image_Record;
      Gtk.Image.Initialize_From_Resource (Self, Resource_Path);
   end Gtk_New_From_Resource;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gtk_Image_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_image_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   --------------------------
   -- Initialize_From_File --
   --------------------------

   procedure Initialize_From_File
      (Self     : not null access Gtk_Image_Record'Class;
       Filename : UTF8_String)
   is
      function Internal
         (Filename : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_image_new_from_file");
      Tmp_Filename : Gtkada.Types.Chars_Ptr := New_String (Filename);
      Tmp_Return   : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_Filename);
         Set_Object (Self, Tmp_Return);
      end if;
      Free (Tmp_Filename);
   end Initialize_From_File;

   ---------------------------
   -- Initialize_From_Gicon --
   ---------------------------

   procedure Initialize_From_Gicon
      (Self : not null access Gtk_Image_Record'Class;
       Icon : Glib.G_Icon.G_Icon)
   is
      function Internal (Icon : Glib.G_Icon.G_Icon) return System.Address;
      pragma Import (C, Internal, "gtk_image_new_from_gicon");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Icon));
      end if;
   end Initialize_From_Gicon;

   -------------------------------
   -- Initialize_From_Icon_Name --
   -------------------------------

   procedure Initialize_From_Icon_Name
      (Self      : not null access Gtk_Image_Record'Class;
       Icon_Name : UTF8_String := "")
   is
      function Internal
         (Icon_Name : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_image_new_from_icon_name");
      Tmp_Icon_Name : Gtkada.Types.Chars_Ptr;
      Tmp_Return    : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Icon_Name :=
           (if Icon_Name = ""
            then Gtkada.Types.Null_Ptr
            else New_String (Icon_Name));
         Tmp_Return := Internal (Tmp_Icon_Name);
         Set_Object (Self, Tmp_Return);
      end if;
      Free (Tmp_Icon_Name);
   end Initialize_From_Icon_Name;

   -------------------------------
   -- Initialize_From_Paintable --
   -------------------------------

   procedure Initialize_From_Paintable
      (Self      : not null access Gtk_Image_Record'Class;
       Paintable : Gdk.Paintable.Gdk_Paintable)
   is
      function Internal
         (Paintable : Gdk.Paintable.Gdk_Paintable) return System.Address;
      pragma Import (C, Internal, "gtk_image_new_from_paintable");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Paintable));
      end if;
   end Initialize_From_Paintable;

   ----------------------------
   -- Initialize_From_Pixbuf --
   ----------------------------

   procedure Initialize_From_Pixbuf
      (Self   : not null access Gtk_Image_Record'Class;
       Pixbuf : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
      function Internal (Pixbuf : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_image_new_from_pixbuf");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object_Or_Null (GObject (Pixbuf))));
      end if;
   end Initialize_From_Pixbuf;

   ------------------------------
   -- Initialize_From_Resource --
   ------------------------------

   procedure Initialize_From_Resource
      (Self          : not null access Gtk_Image_Record'Class;
       Resource_Path : UTF8_String)
   is
      function Internal
         (Resource_Path : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_image_new_from_resource");
      Tmp_Resource_Path : Gtkada.Types.Chars_Ptr := New_String (Resource_Path);
      Tmp_Return        : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_Resource_Path);
         Set_Object (Self, Tmp_Return);
      end if;
      Free (Tmp_Resource_Path);
   end Initialize_From_Resource;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : not null access Gtk_Image_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_image_clear");
   begin
      Internal (Get_Object (Self));
   end Clear;

   ---------------
   -- Get_Gicon --
   ---------------

   function Get_Gicon
      (Self : not null access Gtk_Image_Record) return Glib.G_Icon.G_Icon
   is
      function Internal (Self : System.Address) return Glib.G_Icon.G_Icon;
      pragma Import (C, Internal, "gtk_image_get_gicon");
   begin
      return Internal (Get_Object (Self));
   end Get_Gicon;

   -------------------
   -- Get_Icon_Name --
   -------------------

   function Get_Icon_Name
      (Self : not null access Gtk_Image_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_image_get_icon_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Icon_Name;

   -------------------
   -- Get_Icon_Size --
   -------------------

   function Get_Icon_Size
      (Self : not null access Gtk_Image_Record)
       return Gtk.Enums.Gtk_Icon_Size
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Icon_Size;
      pragma Import (C, Internal, "gtk_image_get_icon_size");
   begin
      return Internal (Get_Object (Self));
   end Get_Icon_Size;

   -------------------
   -- Get_Paintable --
   -------------------

   function Get_Paintable
      (Self : not null access Gtk_Image_Record)
       return Gdk.Paintable.Gdk_Paintable
   is
      function Internal
         (Self : System.Address) return Gdk.Paintable.Gdk_Paintable;
      pragma Import (C, Internal, "gtk_image_get_paintable");
   begin
      return Internal (Get_Object (Self));
   end Get_Paintable;

   --------------------
   -- Get_Pixel_Size --
   --------------------

   function Get_Pixel_Size
      (Self : not null access Gtk_Image_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_image_get_pixel_size");
   begin
      return Internal (Get_Object (Self));
   end Get_Pixel_Size;

   ----------------------
   -- Get_Storage_Type --
   ----------------------

   function Get_Storage_Type
      (Self : not null access Gtk_Image_Record)
       return Gtk.Enums.Gtk_Image_Type
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Image_Type;
      pragma Import (C, Internal, "gtk_image_get_storage_type");
   begin
      return Internal (Get_Object (Self));
   end Get_Storage_Type;

   -------------------
   -- Set_From_File --
   -------------------

   procedure Set_From_File
      (Self     : not null access Gtk_Image_Record;
       Filename : UTF8_String := "")
   is
      procedure Internal
         (Self     : System.Address;
          Filename : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_image_set_from_file");
      Tmp_Filename : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Filename :=
        (if Filename = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Filename));
      Internal (Get_Object (Self), Tmp_Filename);
      Free (Tmp_Filename);
   end Set_From_File;

   --------------------
   -- Set_From_Gicon --
   --------------------

   procedure Set_From_Gicon
      (Self : not null access Gtk_Image_Record;
       Icon : Glib.G_Icon.G_Icon)
   is
      procedure Internal (Self : System.Address; Icon : Glib.G_Icon.G_Icon);
      pragma Import (C, Internal, "gtk_image_set_from_gicon");
   begin
      Internal (Get_Object (Self), Icon);
   end Set_From_Gicon;

   ------------------------
   -- Set_From_Icon_Name --
   ------------------------

   procedure Set_From_Icon_Name
      (Self      : not null access Gtk_Image_Record;
       Icon_Name : UTF8_String := "")
   is
      procedure Internal
         (Self      : System.Address;
          Icon_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_image_set_from_icon_name");
      Tmp_Icon_Name : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Icon_Name :=
        (if Icon_Name = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Icon_Name));
      Internal (Get_Object (Self), Tmp_Icon_Name);
      Free (Tmp_Icon_Name);
   end Set_From_Icon_Name;

   ------------------------
   -- Set_From_Paintable --
   ------------------------

   procedure Set_From_Paintable
      (Self      : not null access Gtk_Image_Record;
       Paintable : Gdk.Paintable.Gdk_Paintable)
   is
      procedure Internal
         (Self      : System.Address;
          Paintable : Gdk.Paintable.Gdk_Paintable);
      pragma Import (C, Internal, "gtk_image_set_from_paintable");
   begin
      Internal (Get_Object (Self), Paintable);
   end Set_From_Paintable;

   -----------------------
   -- Set_From_Resource --
   -----------------------

   procedure Set_From_Resource
      (Self          : not null access Gtk_Image_Record;
       Resource_Path : UTF8_String := "")
   is
      procedure Internal
         (Self          : System.Address;
          Resource_Path : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_image_set_from_resource");
      Tmp_Resource_Path : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Resource_Path :=
        (if Resource_Path = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Resource_Path));
      Internal (Get_Object (Self), Tmp_Resource_Path);
      Free (Tmp_Resource_Path);
   end Set_From_Resource;

   -------------------
   -- Set_Icon_Size --
   -------------------

   procedure Set_Icon_Size
      (Self      : not null access Gtk_Image_Record;
       Icon_Size : Gtk.Enums.Gtk_Icon_Size)
   is
      procedure Internal
         (Self      : System.Address;
          Icon_Size : Gtk.Enums.Gtk_Icon_Size);
      pragma Import (C, Internal, "gtk_image_set_icon_size");
   begin
      Internal (Get_Object (Self), Icon_Size);
   end Set_Icon_Size;

   --------------------
   -- Set_Pixel_Size --
   --------------------

   procedure Set_Pixel_Size
      (Self       : not null access Gtk_Image_Record;
       Pixel_Size : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Pixel_Size : Glib.Gint);
      pragma Import (C, Internal, "gtk_image_set_pixel_size");
   begin
      Internal (Get_Object (Self), Pixel_Size);
   end Set_Pixel_Size;

   --------------
   -- Announce --
   --------------

   procedure Announce
      (Self     : not null access Gtk_Image_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority)
   is
      procedure Internal
         (Self     : System.Address;
          Message  : Gtkada.Types.Chars_Ptr;
          Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);
      pragma Import (C, Internal, "gtk_accessible_announce");
      Tmp_Message : Gtkada.Types.Chars_Ptr := New_String (Message);
   begin
      Internal (Get_Object (Self), Tmp_Message, Priority);
      Free (Tmp_Message);
   end Announce;

   -----------------------
   -- Get_Accessible_Id --
   -----------------------

   function Get_Accessible_Id
      (Self : not null access Gtk_Image_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_accessible_get_accessible_id");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self)));
   end Get_Accessible_Id;

   ---------------------------
   -- Get_Accessible_Parent --
   ---------------------------

   function Get_Accessible_Parent
      (Self : not null access Gtk_Image_Record)
       return Gtk.Accessible.Gtk_Accessible
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible;
      pragma Import (C, Internal, "gtk_accessible_get_accessible_parent");
   begin
      return Internal (Get_Object (Self));
   end Get_Accessible_Parent;

   -------------------------
   -- Get_Accessible_Role --
   -------------------------

   function Get_Accessible_Role
      (Self : not null access Gtk_Image_Record)
       return Gtk.Accessible.Gtk_Accessible_Role
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible_Role;
      pragma Import (C, Internal, "gtk_accessible_get_accessible_role");
   begin
      return Internal (Get_Object (Self));
   end Get_Accessible_Role;

   --------------------
   -- Get_At_Context --
   --------------------

   function Get_At_Context
      (Self : not null access Gtk_Image_Record)
       return Gtk.Atcontext.Gtk_Atcontext
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_accessible_get_at_context");
      Stub_Gtk_Atcontext : Gtk.Atcontext.Gtk_Atcontext_Record;
   begin
      return Gtk.Atcontext.Gtk_Atcontext (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Atcontext));
   end Get_At_Context;

   ----------------
   -- Get_Bounds --
   ----------------

   function Get_Bounds
      (Self   : not null access Gtk_Image_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean
   is
      function Internal
         (Self       : System.Address;
          Acc_X      : access Glib.Gint;
          Acc_Y      : access Glib.Gint;
          Acc_Width  : access Glib.Gint;
          Acc_Height : access Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_accessible_get_bounds");
      Acc_X      : aliased Glib.Gint;
      Acc_Y      : aliased Glib.Gint;
      Acc_Width  : aliased Glib.Gint;
      Acc_Height : aliased Glib.Gint;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_X'Access, Acc_Y'Access, Acc_Width'Access, Acc_Height'Access);
      X := Acc_X;
      Y := Acc_Y;
      Width := Acc_Width;
      Height := Acc_Height;
      return Tmp_Return /= 0;
   end Get_Bounds;

   --------------------------------
   -- Get_First_Accessible_Child --
   --------------------------------

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Image_Record)
       return Gtk.Accessible.Gtk_Accessible
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible;
      pragma Import (C, Internal, "gtk_accessible_get_first_accessible_child");
   begin
      return Internal (Get_Object (Self));
   end Get_First_Accessible_Child;

   ---------------------------------
   -- Get_Next_Accessible_Sibling --
   ---------------------------------

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Image_Record)
       return Gtk.Accessible.Gtk_Accessible
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible;
      pragma Import (C, Internal, "gtk_accessible_get_next_accessible_sibling");
   begin
      return Internal (Get_Object (Self));
   end Get_Next_Accessible_Sibling;

   ------------------------
   -- Get_Platform_State --
   ------------------------

   function Get_Platform_State
      (Self  : not null access Gtk_Image_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean
   is
      function Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_Platform_State)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_accessible_get_platform_state");
   begin
      return Internal (Get_Object (Self), State) /= 0;
   end Get_Platform_State;

   --------------------
   -- Reset_Property --
   --------------------

   procedure Reset_Property
      (Self     : not null access Gtk_Image_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property)
   is
      procedure Internal
         (Self     : System.Address;
          Property : Gtk.Accessible.Gtk_Accessible_Property);
      pragma Import (C, Internal, "gtk_accessible_reset_property");
   begin
      Internal (Get_Object (Self), Property);
   end Reset_Property;

   --------------------
   -- Reset_Relation --
   --------------------

   procedure Reset_Relation
      (Self     : not null access Gtk_Image_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation)
   is
      procedure Internal
         (Self     : System.Address;
          Relation : Gtk.Accessible.Gtk_Accessible_Relation);
      pragma Import (C, Internal, "gtk_accessible_reset_relation");
   begin
      Internal (Get_Object (Self), Relation);
   end Reset_Relation;

   -----------------
   -- Reset_State --
   -----------------

   procedure Reset_State
      (Self  : not null access Gtk_Image_Record;
       State : Gtk.Accessible.Gtk_Accessible_State)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_State);
      pragma Import (C, Internal, "gtk_accessible_reset_state");
   begin
      Internal (Get_Object (Self), State);
   end Reset_State;

   ---------------------------
   -- Set_Accessible_Parent --
   ---------------------------

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Image_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible)
   is
      procedure Internal
         (Self         : System.Address;
          Parent       : Gtk.Accessible.Gtk_Accessible;
          Next_Sibling : Gtk.Accessible.Gtk_Accessible);
      pragma Import (C, Internal, "gtk_accessible_set_accessible_parent");
   begin
      Internal (Get_Object (Self), Parent, Next_Sibling);
   end Set_Accessible_Parent;

   ------------------------------------
   -- Update_Next_Accessible_Sibling --
   ------------------------------------

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Image_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible)
   is
      procedure Internal
         (Self        : System.Address;
          New_Sibling : Gtk.Accessible.Gtk_Accessible);
      pragma Import (C, Internal, "gtk_accessible_update_next_accessible_sibling");
   begin
      Internal (Get_Object (Self), New_Sibling);
   end Update_Next_Accessible_Sibling;

   ---------------------------
   -- Update_Platform_State --
   ---------------------------

   procedure Update_Platform_State
      (Self  : not null access Gtk_Image_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_Platform_State);
      pragma Import (C, Internal, "gtk_accessible_update_platform_state");
   begin
      Internal (Get_Object (Self), State);
   end Update_Platform_State;

end Gtk.Image;
