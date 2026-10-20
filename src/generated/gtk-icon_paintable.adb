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

package body Gtk.Icon_Paintable is

   package Type_Conversion_Gtk_Icon_Paintable is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Icon_Paintable_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Icon_Paintable);

   -------------------------------------
   -- Gtk_Icon_Paintable_New_For_File --
   -------------------------------------

   function Gtk_Icon_Paintable_New_For_File
      (File  : Glib.GFile.Gfile;
       Size  : Glib.Gint;
       Scale : Glib.Gint) return Gtk_Icon_Paintable
   is
      Self : constant Gtk_Icon_Paintable := new Gtk_Icon_Paintable_Record;
   begin
      Gtk.Icon_Paintable.Initialize_For_File (Self, File, Size, Scale);
      return Self;
   end Gtk_Icon_Paintable_New_For_File;

   ----------------------
   -- Gtk_New_For_File --
   ----------------------

   procedure Gtk_New_For_File
      (Self  : out Gtk_Icon_Paintable;
       File  : Glib.GFile.Gfile;
       Size  : Glib.Gint;
       Scale : Glib.Gint)
   is
   begin
      Self := new Gtk_Icon_Paintable_Record;
      Gtk.Icon_Paintable.Initialize_For_File (Self, File, Size, Scale);
   end Gtk_New_For_File;

   -------------------------
   -- Initialize_For_File --
   -------------------------

   procedure Initialize_For_File
      (Self  : not null access Gtk_Icon_Paintable_Record'Class;
       File  : Glib.GFile.Gfile;
       Size  : Glib.Gint;
       Scale : Glib.Gint)
   is
      function Internal
         (File  : Glib.GFile.Gfile;
          Size  : Glib.Gint;
          Scale : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "gtk_icon_paintable_new_for_file");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (File, Size, Scale));
      end if;
   end Initialize_For_File;

   --------------
   -- Get_File --
   --------------

   function Get_File
      (Self : not null access Gtk_Icon_Paintable_Record)
       return Glib.GFile.Gfile
   is
      function Internal (Self : System.Address) return Glib.GFile.Gfile;
      pragma Import (C, Internal, "gtk_icon_paintable_get_file");
   begin
      return Internal (Get_Object (Self));
   end Get_File;

   ---------------------------
   -- Compute_Concrete_Size --
   ---------------------------

   procedure Compute_Concrete_Size
      (Self             : not null access Gtk_Icon_Paintable_Record;
       Specified_Width  : Gdouble;
       Specified_Height : Gdouble;
       Default_Width    : Gdouble;
       Default_Height   : Gdouble;
       Concrete_Width   : out Gdouble;
       Concrete_Height  : out Gdouble)
   is
      procedure Internal
         (Self             : System.Address;
          Specified_Width  : Gdouble;
          Specified_Height : Gdouble;
          Default_Width    : Gdouble;
          Default_Height   : Gdouble;
          Concrete_Width   : out Gdouble;
          Concrete_Height  : out Gdouble);
      pragma Import (C, Internal, "gdk_paintable_compute_concrete_size");
   begin
      Internal (Get_Object (Self), Specified_Width, Specified_Height, Default_Width, Default_Height, Concrete_Width, Concrete_Height);
   end Compute_Concrete_Size;

   -----------------------
   -- Get_Current_Image --
   -----------------------

   function Get_Current_Image
      (Self : not null access Gtk_Icon_Paintable_Record)
       return Gdk.Paintable.Gdk_Paintable
   is
      function Internal
         (Self : System.Address) return Gdk.Paintable.Gdk_Paintable;
      pragma Import (C, Internal, "gdk_paintable_get_current_image");
   begin
      return Internal (Get_Object (Self));
   end Get_Current_Image;

   ---------------
   -- Get_Flags --
   ---------------

   function Get_Flags
      (Self : not null access Gtk_Icon_Paintable_Record)
       return Gdk.Paintable.Gdk_Paintable_Flags
   is
      function Internal
         (Self : System.Address) return Gdk.Paintable.Gdk_Paintable_Flags;
      pragma Import (C, Internal, "gdk_paintable_get_flags");
   begin
      return Internal (Get_Object (Self));
   end Get_Flags;

   --------------------------------
   -- Get_Intrinsic_Aspect_Ratio --
   --------------------------------

   function Get_Intrinsic_Aspect_Ratio
      (Self : not null access Gtk_Icon_Paintable_Record) return Gdouble
   is
      function Internal (Self : System.Address) return Gdouble;
      pragma Import (C, Internal, "gdk_paintable_get_intrinsic_aspect_ratio");
   begin
      return Internal (Get_Object (Self));
   end Get_Intrinsic_Aspect_Ratio;

   --------------------------
   -- Get_Intrinsic_Height --
   --------------------------

   function Get_Intrinsic_Height
      (Self : not null access Gtk_Icon_Paintable_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gdk_paintable_get_intrinsic_height");
   begin
      return Internal (Get_Object (Self));
   end Get_Intrinsic_Height;

   -------------------------
   -- Get_Intrinsic_Width --
   -------------------------

   function Get_Intrinsic_Width
      (Self : not null access Gtk_Icon_Paintable_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gdk_paintable_get_intrinsic_width");
   begin
      return Internal (Get_Object (Self));
   end Get_Intrinsic_Width;

   -------------------------
   -- Invalidate_Contents --
   -------------------------

   procedure Invalidate_Contents
      (Self : not null access Gtk_Icon_Paintable_Record)
   is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gdk_paintable_invalidate_contents");
   begin
      Internal (Get_Object (Self));
   end Invalidate_Contents;

   ---------------------
   -- Invalidate_Size --
   ---------------------

   procedure Invalidate_Size
      (Self : not null access Gtk_Icon_Paintable_Record)
   is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gdk_paintable_invalidate_size");
   begin
      Internal (Get_Object (Self));
   end Invalidate_Size;

   --------------
   -- Snapshot --
   --------------

   procedure Snapshot
      (Self     : not null access Gtk_Icon_Paintable_Record;
       Snapshot : not null access Gdk.Snapshot.Gdk_Snapshot_Record'Class;
       Width    : Gdouble;
       Height   : Gdouble)
   is
      procedure Internal
         (Self     : System.Address;
          Snapshot : System.Address;
          Width    : Gdouble;
          Height   : Gdouble);
      pragma Import (C, Internal, "gdk_paintable_snapshot");
   begin
      Internal (Get_Object (Self), Get_Object (Snapshot), Width, Height);
   end Snapshot;

end Gtk.Icon_Paintable;
