-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2004                            --
--                         ACT-Europe                                --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with System;

with Gdk.Pixbuf;   use Gdk.Pixbuf;
with Gtk; use Gtk;

package body Gtk.Icon_Factory is

   -------------
   -- Gtk_New --
   -------------

   function Gtk_New return Gtk_Icon_Set is
      function Internal return Gtk_Icon_Set;
      pragma Import (C, Internal, "gtk_icon_set_new");
   begin
      return Internal;
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   function Gtk_New return Gtk_Icon_Source is
      function Internal return Gtk_Icon_Source;
      pragma Import (C, Internal, "gtk_icon_source_new");
   begin
      return Internal;
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Icon_Factory_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_icon_factory_new");
   begin
      Set_Object (Widget, Internal);
   end Initialize;

   ---------
   -- Add --
   ---------

   procedure Add
     (Factory  : access Gtk_Icon_Factory_Record;
      Stock_Id : String;
      Set      : Gtk_Icon_Set)
   is
      procedure Internal
        (Factory  : System.Address;
         Stock_Id : String;
         Set      : Gtk_Icon_Set);
      pragma Import (C, Internal, "gtk_icon_factory_add");

   begin
      Internal (Get_Object (Factory), Stock_Id & ASCII.NUL, Set);
   end Add;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (Factory  : access Gtk_Icon_Factory_Record;
      Stock_Id : String) return Gtk_Icon_Set
   is
      function Internal
        (Factory  : System.Address;
         Stock_Id : String) return Gtk_Icon_Set;
      pragma Import (C, Internal, "gtk_icon_factory_lookup");

   begin
      return Internal (Get_Object (Factory), Stock_Id & ASCII.NUL);
   end Lookup;

   -----------------
   -- Add_Default --
   -----------------

   procedure Add_Default (Factory : access Gtk_Icon_Factory_Record) is
      procedure Internal (Factory : System.Address);
      pragma Import (C, Internal, "gtk_icon_factory_add_default");
   begin
      Internal (Get_Object (Factory));
   end Add_Default;

   --------------------
   -- Remove_Default --
   --------------------

   procedure Remove_Default (Factory : access Gtk_Icon_Factory_Record) is
      procedure Internal (Factory : System.Address);
      pragma Import (C, Internal, "gtk_icon_factory_remove_default");
   begin
      Internal (Get_Object (Factory));
   end Remove_Default;

   --------------------
   -- Lookup_Default --
   --------------------

   function Lookup_Default (Stock_Id : String) return Gtk_Icon_Set is
      function Internal (Stock_Id : String) return Gtk_Icon_Set;
      pragma Import (C, Internal, "gtk_icon_factory_lookup_default");
   begin
      return Internal (Stock_Id & ASCII.NUL);
   end Lookup_Default;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Icon_Factory) is
   begin
      Widget := new Gtk_Icon_Factory_Record;
      Gtk.Icon_Factory.Initialize (Widget);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   function Gtk_New (Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf) return Gtk_Icon_Set is
      function Internal (Pixbuf : Gdk_Pixbuf) return Gtk_Icon_Set;
      pragma Import (C, Internal, "gtk_icon_set_new_from_pixbuf");
   begin
      return Internal (Pixbuf);
   end Gtk_New;

   ----------------
   -- Add_Source --
   ----------------

   procedure Add_Source
     (Set    : Gtk_Icon_Set;
      Source : Gtk_Icon_Source)
   is
      procedure Internal
        (Set    : Gtk_Icon_Set;
         Source : Gtk_Icon_Source);
      pragma Import (C, Internal, "gtk_icon_set_add_source");

   begin
      Internal (Set, Source);
   end Add_Source;

   ----------
   -- Free --
   ----------

   procedure Free (Source : Gtk_Icon_Source) is
      procedure Internal (Source : Gtk_Icon_Source);
      pragma Import (C, Internal, "gtk_icon_source_free");
   begin
      Internal (Source);
   end Free;

   ------------------
   -- Set_Filename --
   ------------------

   procedure Set_Filename
     (Source   : Gtk_Icon_Source;
      Filename : String)
   is
      procedure Internal
        (Source   : Gtk_Icon_Source;
         Filename : String);
      pragma Import (C, Internal, "gtk_icon_source_set_filename");

   begin
      Internal (Source, Filename & ASCII.NUL);
   end Set_Filename;

   ----------------
   -- Set_Pixbuf --
   ----------------

   procedure Set_Pixbuf
     (Source : Gtk_Icon_Source;
      Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf)
   is
      procedure Internal
        (Source : Gtk_Icon_Source;
         Pixbuf : Gdk_Pixbuf);
      pragma Import (C, Internal, "gtk_icon_source_set_pixbuf");

   begin
      Internal (Source, Pixbuf);
   end Set_Pixbuf;

end Gtk.Icon_Factory;
