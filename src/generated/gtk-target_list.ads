------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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

--  <description>
--  A Gtk.Target_List.Gtk_Target_List-struct is a reference counted list of
--  Gtk_Target_Pair and should be treated as opaque.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Types;               use Gdk.Types;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Gtk.Target_Entry;        use Gtk.Target_Entry;

package Gtk.Target_List is

   type Gtk_Target_List is new Glib.C_Boxed with null record;
   Null_Gtk_Target_List : constant Gtk_Target_List;

   function From_Object (Object : System.Address) return Gtk_Target_List;
   function From_Object_Free (B : access Gtk_Target_List'Class) return Gtk_Target_List;
   pragma Inline (From_Object_Free, From_Object);

   type Gtk_Accel_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, Gtk_Accel_Flags);
   --  Accelerator flags used with Gtk.Accel_Group.Connect.

   Accel_Visible : constant Gtk_Accel_Flags := 1;
   Accel_Locked : constant Gtk_Accel_Flags := 2;
   Accel_Mask : constant Gtk_Accel_Flags := 7;

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Accel_Flags_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Accel_Flags);
   type Property_Gtk_Accel_Flags is new Gtk_Accel_Flags_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_target_list_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add
      (List   : Gtk_Target_List;
       Target : Gdk.Types.Gdk_Atom;
       Flags  : Guint;
       Info   : Guint);
   --  Appends another target to a Gtk.Target_List.Gtk_Target_List.
   --  "target": the interned atom representing the target
   --  "flags": the flags for this target
   --  "info": an ID that will be passed back to the application

   procedure Add_Image_Targets
      (List     : Gtk_Target_List;
       Info     : Guint;
       Writable : Boolean);
   --  Appends the image targets supported by
   --  Gtk.Selection_Data.Gtk_Selection_Data to the target list. All targets
   --  are added with the same Info.
   --  Since: gtk+ 2.6
   --  "info": an ID that will be passed back to the application
   --  "writable": whether to add only targets for which GTK+ knows how to
   --  convert a pixbuf into the format

   procedure Add_Text_Targets (List : Gtk_Target_List; Info : Guint);
   --  Appends the text targets supported by
   --  Gtk.Selection_Data.Gtk_Selection_Data to the target list. All targets
   --  are added with the same Info.
   --  Since: gtk+ 2.6
   --  "info": an ID that will be passed back to the application

   procedure Add_Uri_Targets (List : Gtk_Target_List; Info : Guint);
   --  Appends the URI targets supported by
   --  Gtk.Selection_Data.Gtk_Selection_Data to the target list. All targets
   --  are added with the same Info.
   --  Since: gtk+ 2.6
   --  "info": an ID that will be passed back to the application

   procedure Find
      (List   : Gtk_Target_List;
       Target : Gdk.Types.Gdk_Atom;
       Info   : out Guint;
       Found  : out Boolean);
   --  Looks up a given target in a Gtk.Target_List.Gtk_Target_List.
   --  "target": an interned atom representing the target to search for
   --  "info": a pointer to the location to store application info for target,
   --  or null

   function Ref (List : Gtk_Target_List) return Gtk_Target_List;
   --  Increases the reference count of a Gtk.Target_List.Gtk_Target_List by
   --  one.

   procedure Remove (List : Gtk_Target_List; Target : Gdk.Types.Gdk_Atom);
   --  Removes a target from a target list.
   --  "target": the interned atom representing the target

   procedure Unref (List : Gtk_Target_List);
   --  Decreases the reference count of a Gtk.Target_List.Gtk_Target_List by
   --  one. If the resulting reference count is zero, frees the list.

   ----------------------
   -- GtkAda additions --
   ----------------------

   type Target_Entry_Array is array (Natural range <>) of Gtk_Target_Entry;

   No_Target_Entry : Target_Entry_Array (1 .. 0);
   --  To be used for drop sites that accept no data.
   --  You will in general need to call Gtk.Dnd.Add_Text_Targets or some such

   Any_Target_Entry : constant Target_Entry_Array := No_Target_Entry;
   pragma Obsolescent (Any_Target_Entry);
   --  Old name for this constant, which leads to confusion. You should use
   --  No_Target_Entry instead.

   procedure Gtk_New
     (List   : out Gtk_Target_List;
      Targets : Target_Entry_Array);
   --  Creates a new Gtk.Target_List.Gtk_Target_List from an array of
   --  Gtk.Target_Entry.Gtk_Target_Entry.

   procedure Add_Table
     (List    : Gtk_Target_List;
      Targets : Target_Entry_Array);
   --  Prepends a table of Gtk.Target_Entry.Gtk_Target_Entry to a target list.

private

   Null_Gtk_Target_List : constant Gtk_Target_List := (Glib.C_Boxed with null record);

end Gtk.Target_List;
