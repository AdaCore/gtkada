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

package body Gtk.Picture is

   package Type_Conversion_Gtk_Picture is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Picture_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Picture);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Picture) is
   begin
      Self := new Gtk_Picture_Record;
      Gtk.Picture.Initialize (Self);
   end Gtk_New;

   ----------------------
   -- Gtk_New_For_File --
   ----------------------

   procedure Gtk_New_For_File
      (Self : out Gtk_Picture;
       File : Glib.GFile.Gfile)
   is
   begin
      Self := new Gtk_Picture_Record;
      Gtk.Picture.Initialize_For_File (Self, File);
   end Gtk_New_For_File;

   --------------------------
   -- Gtk_New_For_Filename --
   --------------------------

   procedure Gtk_New_For_Filename
      (Self     : out Gtk_Picture;
       Filename : UTF8_String := "")
   is
   begin
      Self := new Gtk_Picture_Record;
      Gtk.Picture.Initialize_For_Filename (Self, Filename);
   end Gtk_New_For_Filename;

   ---------------------------
   -- Gtk_New_For_Paintable --
   ---------------------------

   procedure Gtk_New_For_Paintable
      (Self      : out Gtk_Picture;
       Paintable : Gdk.Paintable.Gdk_Paintable)
   is
   begin
      Self := new Gtk_Picture_Record;
      Gtk.Picture.Initialize_For_Paintable (Self, Paintable);
   end Gtk_New_For_Paintable;

   ------------------------
   -- Gtk_New_For_Pixbuf --
   ------------------------

   procedure Gtk_New_For_Pixbuf
      (Self   : out Gtk_Picture;
       Pixbuf : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
   begin
      Self := new Gtk_Picture_Record;
      Gtk.Picture.Initialize_For_Pixbuf (Self, Pixbuf);
   end Gtk_New_For_Pixbuf;

   --------------------------
   -- Gtk_New_For_Resource --
   --------------------------

   procedure Gtk_New_For_Resource
      (Self          : out Gtk_Picture;
       Resource_Path : UTF8_String := "")
   is
   begin
      Self := new Gtk_Picture_Record;
      Gtk.Picture.Initialize_For_Resource (Self, Resource_Path);
   end Gtk_New_For_Resource;

   ---------------------
   -- Gtk_Picture_New --
   ---------------------

   function Gtk_Picture_New return Gtk_Picture is
      Self : constant Gtk_Picture := new Gtk_Picture_Record;
   begin
      Gtk.Picture.Initialize (Self);
      return Self;
   end Gtk_Picture_New;

   ------------------------------
   -- Gtk_Picture_New_For_File --
   ------------------------------

   function Gtk_Picture_New_For_File
      (File : Glib.GFile.Gfile) return Gtk_Picture
   is
      Self : constant Gtk_Picture := new Gtk_Picture_Record;
   begin
      Gtk.Picture.Initialize_For_File (Self, File);
      return Self;
   end Gtk_Picture_New_For_File;

   ----------------------------------
   -- Gtk_Picture_New_For_Filename --
   ----------------------------------

   function Gtk_Picture_New_For_Filename
      (Filename : UTF8_String := "") return Gtk_Picture
   is
      Self : constant Gtk_Picture := new Gtk_Picture_Record;
   begin
      Gtk.Picture.Initialize_For_Filename (Self, Filename);
      return Self;
   end Gtk_Picture_New_For_Filename;

   -----------------------------------
   -- Gtk_Picture_New_For_Paintable --
   -----------------------------------

   function Gtk_Picture_New_For_Paintable
      (Paintable : Gdk.Paintable.Gdk_Paintable) return Gtk_Picture
   is
      Self : constant Gtk_Picture := new Gtk_Picture_Record;
   begin
      Gtk.Picture.Initialize_For_Paintable (Self, Paintable);
      return Self;
   end Gtk_Picture_New_For_Paintable;

   --------------------------------
   -- Gtk_Picture_New_For_Pixbuf --
   --------------------------------

   function Gtk_Picture_New_For_Pixbuf
      (Pixbuf : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
       return Gtk_Picture
   is
      Self : constant Gtk_Picture := new Gtk_Picture_Record;
   begin
      Gtk.Picture.Initialize_For_Pixbuf (Self, Pixbuf);
      return Self;
   end Gtk_Picture_New_For_Pixbuf;

   ----------------------------------
   -- Gtk_Picture_New_For_Resource --
   ----------------------------------

   function Gtk_Picture_New_For_Resource
      (Resource_Path : UTF8_String := "") return Gtk_Picture
   is
      Self : constant Gtk_Picture := new Gtk_Picture_Record;
   begin
      Gtk.Picture.Initialize_For_Resource (Self, Resource_Path);
      return Self;
   end Gtk_Picture_New_For_Resource;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gtk_Picture_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_picture_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   -------------------------
   -- Initialize_For_File --
   -------------------------

   procedure Initialize_For_File
      (Self : not null access Gtk_Picture_Record'Class;
       File : Glib.GFile.Gfile)
   is
      function Internal (File : Glib.GFile.Gfile) return System.Address;
      pragma Import (C, Internal, "gtk_picture_new_for_file");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (File));
      end if;
   end Initialize_For_File;

   -----------------------------
   -- Initialize_For_Filename --
   -----------------------------

   procedure Initialize_For_Filename
      (Self     : not null access Gtk_Picture_Record'Class;
       Filename : UTF8_String := "")
   is
      function Internal
         (Filename : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_picture_new_for_filename");
      Tmp_Filename : Gtkada.Types.Chars_Ptr;
      Tmp_Return   : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Filename :=
           (if Filename = ""
            then Gtkada.Types.Null_Ptr
            else New_String (Filename));
         Tmp_Return := Internal (Tmp_Filename);
         Set_Object (Self, Tmp_Return);
      end if;
      Free (Tmp_Filename);
   end Initialize_For_Filename;

   ------------------------------
   -- Initialize_For_Paintable --
   ------------------------------

   procedure Initialize_For_Paintable
      (Self      : not null access Gtk_Picture_Record'Class;
       Paintable : Gdk.Paintable.Gdk_Paintable)
   is
      function Internal
         (Paintable : Gdk.Paintable.Gdk_Paintable) return System.Address;
      pragma Import (C, Internal, "gtk_picture_new_for_paintable");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Paintable));
      end if;
   end Initialize_For_Paintable;

   ---------------------------
   -- Initialize_For_Pixbuf --
   ---------------------------

   procedure Initialize_For_Pixbuf
      (Self   : not null access Gtk_Picture_Record'Class;
       Pixbuf : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
      function Internal (Pixbuf : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_picture_new_for_pixbuf");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object_Or_Null (GObject (Pixbuf))));
      end if;
   end Initialize_For_Pixbuf;

   -----------------------------
   -- Initialize_For_Resource --
   -----------------------------

   procedure Initialize_For_Resource
      (Self          : not null access Gtk_Picture_Record'Class;
       Resource_Path : UTF8_String := "")
   is
      function Internal
         (Resource_Path : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_picture_new_for_resource");
      Tmp_Resource_Path : Gtkada.Types.Chars_Ptr;
      Tmp_Return        : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Resource_Path :=
           (if Resource_Path = ""
            then Gtkada.Types.Null_Ptr
            else New_String (Resource_Path));
         Tmp_Return := Internal (Tmp_Resource_Path);
         Set_Object (Self, Tmp_Return);
      end if;
      Free (Tmp_Resource_Path);
   end Initialize_For_Resource;

   --------------------------
   -- Get_Alternative_Text --
   --------------------------

   function Get_Alternative_Text
      (Self : not null access Gtk_Picture_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_picture_get_alternative_text");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Alternative_Text;

   --------------------
   -- Get_Can_Shrink --
   --------------------

   function Get_Can_Shrink
      (Self : not null access Gtk_Picture_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_picture_get_can_shrink");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Can_Shrink;

   ---------------------
   -- Get_Content_Fit --
   ---------------------

   function Get_Content_Fit
      (Self : not null access Gtk_Picture_Record) return Gtk_Content_Fit
   is
      function Internal (Self : System.Address) return Gtk_Content_Fit;
      pragma Import (C, Internal, "gtk_picture_get_content_fit");
   begin
      return Internal (Get_Object (Self));
   end Get_Content_Fit;

   --------------
   -- Get_File --
   --------------

   function Get_File
      (Self : not null access Gtk_Picture_Record) return Glib.GFile.Gfile
   is
      function Internal (Self : System.Address) return Glib.GFile.Gfile;
      pragma Import (C, Internal, "gtk_picture_get_file");
   begin
      return Internal (Get_Object (Self));
   end Get_File;

   --------------------------
   -- Get_Isolate_Contents --
   --------------------------

   function Get_Isolate_Contents
      (Self : not null access Gtk_Picture_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_picture_get_isolate_contents");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Isolate_Contents;

   ---------------------------
   -- Get_Keep_Aspect_Ratio --
   ---------------------------

   function Get_Keep_Aspect_Ratio
      (Self : not null access Gtk_Picture_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_picture_get_keep_aspect_ratio");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Keep_Aspect_Ratio;

   -------------------
   -- Get_Paintable --
   -------------------

   function Get_Paintable
      (Self : not null access Gtk_Picture_Record)
       return Gdk.Paintable.Gdk_Paintable
   is
      function Internal
         (Self : System.Address) return Gdk.Paintable.Gdk_Paintable;
      pragma Import (C, Internal, "gtk_picture_get_paintable");
   begin
      return Internal (Get_Object (Self));
   end Get_Paintable;

   --------------------------
   -- Set_Alternative_Text --
   --------------------------

   procedure Set_Alternative_Text
      (Self             : not null access Gtk_Picture_Record;
       Alternative_Text : UTF8_String := "")
   is
      procedure Internal
         (Self             : System.Address;
          Alternative_Text : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_picture_set_alternative_text");
      Tmp_Alternative_Text : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Alternative_Text :=
        (if Alternative_Text = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Alternative_Text));
      Internal (Get_Object (Self), Tmp_Alternative_Text);
      Free (Tmp_Alternative_Text);
   end Set_Alternative_Text;

   --------------------
   -- Set_Can_Shrink --
   --------------------

   procedure Set_Can_Shrink
      (Self       : not null access Gtk_Picture_Record;
       Can_Shrink : Boolean)
   is
      procedure Internal (Self : System.Address; Can_Shrink : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_picture_set_can_shrink");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Can_Shrink));
   end Set_Can_Shrink;

   ---------------------
   -- Set_Content_Fit --
   ---------------------

   procedure Set_Content_Fit
      (Self        : not null access Gtk_Picture_Record;
       Content_Fit : Gtk_Content_Fit)
   is
      procedure Internal
         (Self        : System.Address;
          Content_Fit : Gtk_Content_Fit);
      pragma Import (C, Internal, "gtk_picture_set_content_fit");
   begin
      Internal (Get_Object (Self), Content_Fit);
   end Set_Content_Fit;

   --------------
   -- Set_File --
   --------------

   procedure Set_File
      (Self : not null access Gtk_Picture_Record;
       File : Glib.GFile.Gfile)
   is
      procedure Internal (Self : System.Address; File : Glib.GFile.Gfile);
      pragma Import (C, Internal, "gtk_picture_set_file");
   begin
      Internal (Get_Object (Self), File);
   end Set_File;

   ------------------
   -- Set_Filename --
   ------------------

   procedure Set_Filename
      (Self     : not null access Gtk_Picture_Record;
       Filename : UTF8_String := "")
   is
      procedure Internal
         (Self     : System.Address;
          Filename : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_picture_set_filename");
      Tmp_Filename : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Filename :=
        (if Filename = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Filename));
      Internal (Get_Object (Self), Tmp_Filename);
      Free (Tmp_Filename);
   end Set_Filename;

   --------------------------
   -- Set_Isolate_Contents --
   --------------------------

   procedure Set_Isolate_Contents
      (Self             : not null access Gtk_Picture_Record;
       Isolate_Contents : Boolean)
   is
      procedure Internal
         (Self             : System.Address;
          Isolate_Contents : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_picture_set_isolate_contents");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Isolate_Contents));
   end Set_Isolate_Contents;

   ---------------------------
   -- Set_Keep_Aspect_Ratio --
   ---------------------------

   procedure Set_Keep_Aspect_Ratio
      (Self              : not null access Gtk_Picture_Record;
       Keep_Aspect_Ratio : Boolean)
   is
      procedure Internal
         (Self              : System.Address;
          Keep_Aspect_Ratio : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_picture_set_keep_aspect_ratio");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Keep_Aspect_Ratio));
   end Set_Keep_Aspect_Ratio;

   -------------------
   -- Set_Paintable --
   -------------------

   procedure Set_Paintable
      (Self      : not null access Gtk_Picture_Record;
       Paintable : Gdk.Paintable.Gdk_Paintable)
   is
      procedure Internal
         (Self      : System.Address;
          Paintable : Gdk.Paintable.Gdk_Paintable);
      pragma Import (C, Internal, "gtk_picture_set_paintable");
   begin
      Internal (Get_Object (Self), Paintable);
   end Set_Paintable;

   ----------------
   -- Set_Pixbuf --
   ----------------

   procedure Set_Pixbuf
      (Self   : not null access Gtk_Picture_Record;
       Pixbuf : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
      procedure Internal (Self : System.Address; Pixbuf : System.Address);
      pragma Import (C, Internal, "gtk_picture_set_pixbuf");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Pixbuf)));
   end Set_Pixbuf;

   ------------------
   -- Set_Resource --
   ------------------

   procedure Set_Resource
      (Self          : not null access Gtk_Picture_Record;
       Resource_Path : UTF8_String := "")
   is
      procedure Internal
         (Self          : System.Address;
          Resource_Path : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_picture_set_resource");
      Tmp_Resource_Path : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Resource_Path :=
        (if Resource_Path = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Resource_Path));
      Internal (Get_Object (Self), Tmp_Resource_Path);
      Free (Tmp_Resource_Path);
   end Set_Resource;

   --------------
   -- Announce --
   --------------

   procedure Announce
      (Self     : not null access Gtk_Picture_Record;
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
      (Self : not null access Gtk_Picture_Record) return UTF8_String
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
      (Self : not null access Gtk_Picture_Record)
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
      (Self : not null access Gtk_Picture_Record)
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
      (Self : not null access Gtk_Picture_Record)
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
      (Self   : not null access Gtk_Picture_Record;
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
      (Self : not null access Gtk_Picture_Record)
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
      (Self : not null access Gtk_Picture_Record)
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
      (Self  : not null access Gtk_Picture_Record;
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
      (Self     : not null access Gtk_Picture_Record;
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
      (Self     : not null access Gtk_Picture_Record;
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
      (Self  : not null access Gtk_Picture_Record;
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
      (Self         : not null access Gtk_Picture_Record;
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
      (Self        : not null access Gtk_Picture_Record;
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
      (Self  : not null access Gtk_Picture_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_Platform_State);
      pragma Import (C, Internal, "gtk_accessible_update_platform_state");
   begin
      Internal (Get_Object (Self), State);
   end Update_Platform_State;

end Gtk.Picture;
