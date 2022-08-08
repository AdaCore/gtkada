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

package body Gtk.Search_Bar is

   package Type_Conversion_Gtk_Search_Bar is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Search_Bar_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Search_Bar);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Search_Bar) is
   begin
      Self := new Gtk_Search_Bar_Record;
      Gtk.Search_Bar.Initialize (Self);
   end Gtk_New;

   ------------------------
   -- Gtk_Search_Bar_New --
   ------------------------

   function Gtk_Search_Bar_New return Gtk_Search_Bar is
      Self : constant Gtk_Search_Bar := new Gtk_Search_Bar_Record;
   begin
      Gtk.Search_Bar.Initialize (Self);
      return Self;
   end Gtk_Search_Bar_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gtk_Search_Bar_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_search_bar_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   -------------------
   -- Connect_Entry --
   -------------------

   procedure Connect_Entry
      (Self   : not null access Gtk_Search_Bar_Record;
       GEntry : not null access Gtk.GEntry.Gtk_Entry_Record'Class)
   is
      procedure Internal (Self : System.Address; GEntry : System.Address);
      pragma Import (C, Internal, "gtk_search_bar_connect_entry");
   begin
      Internal (Get_Object (Self), Get_Object (GEntry));
   end Connect_Entry;

   ---------------------
   -- Get_Search_Mode --
   ---------------------

   function Get_Search_Mode
      (Self : not null access Gtk_Search_Bar_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_search_bar_get_search_mode");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Search_Mode;

   ---------------------------
   -- Get_Show_Close_Button --
   ---------------------------

   function Get_Show_Close_Button
      (Self : not null access Gtk_Search_Bar_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_search_bar_get_show_close_button");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Show_Close_Button;

   ------------------
   -- Handle_Event --
   ------------------

   function Handle_Event
      (Self  : not null access Gtk_Search_Bar_Record;
       Event : Gdk.Event.Gdk_Event) return Boolean
   is
      function Internal
         (Self  : System.Address;
          Event : Gdk.Event.Gdk_Event) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_search_bar_handle_event");
   begin
      return Internal (Get_Object (Self), Event) /= 0;
   end Handle_Event;

   ---------------------
   -- Set_Search_Mode --
   ---------------------

   procedure Set_Search_Mode
      (Self        : not null access Gtk_Search_Bar_Record;
       Search_Mode : Boolean)
   is
      procedure Internal
         (Self        : System.Address;
          Search_Mode : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_search_bar_set_search_mode");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Search_Mode));
   end Set_Search_Mode;

   ---------------------------
   -- Set_Show_Close_Button --
   ---------------------------

   procedure Set_Show_Close_Button
      (Self    : not null access Gtk_Search_Bar_Record;
       Visible : Boolean)
   is
      procedure Internal (Self : System.Address; Visible : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_search_bar_set_show_close_button");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Visible));
   end Set_Show_Close_Button;

end Gtk.Search_Bar;
