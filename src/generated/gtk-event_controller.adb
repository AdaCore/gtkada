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

package body Gtk.Event_Controller is

   package Type_Conversion_Gtk_Event_Controller is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Event_Controller_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Event_Controller);

   ---------------------------
   -- Get_Propagation_Phase --
   ---------------------------

   function Get_Propagation_Phase
      (Self : not null access Gtk_Event_Controller_Record)
       return Gtk.Enums.Gtk_Propagation_Phase
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Propagation_Phase;
      pragma Import (C, Internal, "gtk_event_controller_get_propagation_phase");
   begin
      return Internal (Get_Object (Self));
   end Get_Propagation_Phase;

   ----------------
   -- Get_Widget --
   ----------------

   function Get_Widget
      (Self : not null access Gtk_Event_Controller_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_event_controller_get_widget");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_Widget;

   ------------------
   -- Handle_Event --
   ------------------

   function Handle_Event
      (Self  : not null access Gtk_Event_Controller_Record;
       Event : Gdk.Event.Gdk_Event) return Boolean
   is
      function Internal
         (Self  : System.Address;
          Event : Gdk.Event.Gdk_Event) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_event_controller_handle_event");
   begin
      return Internal (Get_Object (Self), Event) /= 0;
   end Handle_Event;

   -----------
   -- Reset --
   -----------

   procedure Reset (Self : not null access Gtk_Event_Controller_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_event_controller_reset");
   begin
      Internal (Get_Object (Self));
   end Reset;

   ---------------------------
   -- Set_Propagation_Phase --
   ---------------------------

   procedure Set_Propagation_Phase
      (Self  : not null access Gtk_Event_Controller_Record;
       Phase : Gtk.Enums.Gtk_Propagation_Phase)
   is
      procedure Internal
         (Self  : System.Address;
          Phase : Gtk.Enums.Gtk_Propagation_Phase);
      pragma Import (C, Internal, "gtk_event_controller_set_propagation_phase");
   begin
      Internal (Get_Object (Self), Phase);
   end Set_Propagation_Phase;

end Gtk.Event_Controller;
