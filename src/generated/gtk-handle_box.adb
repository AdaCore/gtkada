------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

package body Gtk.Handle_Box is

   package Type_Conversion_Gtk_Handle_Box is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Handle_Box_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Handle_Box);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Handle_Box : out Gtk_Handle_Box) is
   begin
      Handle_Box := new Gtk_Handle_Box_Record;
      Gtk.Handle_Box.Initialize (Handle_Box);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Handle_Box : not null access Gtk_Handle_Box_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_handle_box_new");
   begin
      Set_Object (Handle_Box, Internal);
   end Initialize;

   ------------------------
   -- Get_Child_Detached --
   ------------------------

   function Get_Child_Detached
      (Handle_Box : not null access Gtk_Handle_Box_Record) return Boolean
   is
      function Internal (Handle_Box : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_handle_box_get_child_detached");
   begin
      return Boolean'Val (Internal (Get_Object (Handle_Box)));
   end Get_Child_Detached;

   -------------------------
   -- Get_Handle_Position --
   -------------------------

   function Get_Handle_Position
      (Handle_Box : not null access Gtk_Handle_Box_Record)
       return Gtk.Enums.Gtk_Position_Type
   is
      function Internal
         (Handle_Box : System.Address) return Gtk.Enums.Gtk_Position_Type;
      pragma Import (C, Internal, "gtk_handle_box_get_handle_position");
   begin
      return Internal (Get_Object (Handle_Box));
   end Get_Handle_Position;

   ---------------------
   -- Get_Shadow_Type --
   ---------------------

   function Get_Shadow_Type
      (Handle_Box : not null access Gtk_Handle_Box_Record)
       return Gtk.Enums.Gtk_Shadow_Type
   is
      function Internal
         (Handle_Box : System.Address) return Gtk.Enums.Gtk_Shadow_Type;
      pragma Import (C, Internal, "gtk_handle_box_get_shadow_type");
   begin
      return Internal (Get_Object (Handle_Box));
   end Get_Shadow_Type;

   -------------------
   -- Get_Snap_Edge --
   -------------------

   function Get_Snap_Edge
      (Handle_Box : not null access Gtk_Handle_Box_Record)
       return Gtk.Enums.Gtk_Position_Type
   is
      function Internal
         (Handle_Box : System.Address) return Gtk.Enums.Gtk_Position_Type;
      pragma Import (C, Internal, "gtk_handle_box_get_snap_edge");
   begin
      return Internal (Get_Object (Handle_Box));
   end Get_Snap_Edge;

   -------------------------
   -- Set_Handle_Position --
   -------------------------

   procedure Set_Handle_Position
      (Handle_Box : not null access Gtk_Handle_Box_Record;
       Position   : Gtk.Enums.Gtk_Position_Type)
   is
      procedure Internal
         (Handle_Box : System.Address;
          Position   : Gtk.Enums.Gtk_Position_Type);
      pragma Import (C, Internal, "gtk_handle_box_set_handle_position");
   begin
      Internal (Get_Object (Handle_Box), Position);
   end Set_Handle_Position;

   ---------------------
   -- Set_Shadow_Type --
   ---------------------

   procedure Set_Shadow_Type
      (Handle_Box : not null access Gtk_Handle_Box_Record;
       The_Type   : Gtk.Enums.Gtk_Shadow_Type)
   is
      procedure Internal
         (Handle_Box : System.Address;
          The_Type   : Gtk.Enums.Gtk_Shadow_Type);
      pragma Import (C, Internal, "gtk_handle_box_set_shadow_type");
   begin
      Internal (Get_Object (Handle_Box), The_Type);
   end Set_Shadow_Type;

   -------------------
   -- Set_Snap_Edge --
   -------------------

   procedure Set_Snap_Edge
      (Handle_Box : not null access Gtk_Handle_Box_Record;
       Edge       : Gtk.Enums.Gtk_Position_Type)
   is
      procedure Internal
         (Handle_Box : System.Address;
          Edge       : Gtk.Enums.Gtk_Position_Type);
      pragma Import (C, Internal, "gtk_handle_box_set_snap_edge");
   begin
      Internal (Get_Object (Handle_Box), Edge);
   end Set_Snap_Edge;

end Gtk.Handle_Box;
