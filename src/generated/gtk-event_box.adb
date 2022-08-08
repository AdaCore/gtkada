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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;

package body Gtk.Event_Box is

   package Type_Conversion_Gtk_Event_Box is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Event_Box_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Event_Box);

   -----------------------
   -- Gtk_Event_Box_New --
   -----------------------

   function Gtk_Event_Box_New return Gtk_Event_Box is
      Event_Box : constant Gtk_Event_Box := new Gtk_Event_Box_Record;
   begin
      Gtk.Event_Box.Initialize (Event_Box);
      return Event_Box;
   end Gtk_Event_Box_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Event_Box : out Gtk_Event_Box) is
   begin
      Event_Box := new Gtk_Event_Box_Record;
      Gtk.Event_Box.Initialize (Event_Box);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Event_Box : not null access Gtk_Event_Box_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_event_box_new");
   begin
      if not Event_Box.Is_Created then
         Set_Object (Event_Box, Internal);
      end if;
   end Initialize;

   ---------------------
   -- Get_Above_Child --
   ---------------------

   function Get_Above_Child
      (Event_Box : not null access Gtk_Event_Box_Record) return Boolean
   is
      function Internal (Event_Box : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_event_box_get_above_child");
   begin
      return Internal (Get_Object (Event_Box)) /= 0;
   end Get_Above_Child;

   ------------------------
   -- Get_Visible_Window --
   ------------------------

   function Get_Visible_Window
      (Event_Box : not null access Gtk_Event_Box_Record) return Boolean
   is
      function Internal (Event_Box : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_event_box_get_visible_window");
   begin
      return Internal (Get_Object (Event_Box)) /= 0;
   end Get_Visible_Window;

   ---------------------
   -- Set_Above_Child --
   ---------------------

   procedure Set_Above_Child
      (Event_Box   : not null access Gtk_Event_Box_Record;
       Above_Child : Boolean)
   is
      procedure Internal
         (Event_Box   : System.Address;
          Above_Child : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_event_box_set_above_child");
   begin
      Internal (Get_Object (Event_Box), Boolean'Pos (Above_Child));
   end Set_Above_Child;

   ------------------------
   -- Set_Visible_Window --
   ------------------------

   procedure Set_Visible_Window
      (Event_Box      : not null access Gtk_Event_Box_Record;
       Visible_Window : Boolean)
   is
      procedure Internal
         (Event_Box      : System.Address;
          Visible_Window : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_event_box_set_visible_window");
   begin
      Internal (Get_Object (Event_Box), Boolean'Pos (Visible_Window));
   end Set_Visible_Window;

end Gtk.Event_Box;
