------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 1998-2018, AdaCore                     --
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

with Gtk.Widget; use Gtk.Widget;
with Gtk.Button; use Gtk.Button;
with Gtk.Box; use Gtk.Box;
with Gtk.Style_Provider; use Gtk.Style_Provider;
with Gtk.Style_Context; use Gtk.Style_Context;
with Gtk.Css_Provider; use Gtk.Css_Provider;
with Ada.Text_IO; use Ada.Text_IO;
with Glib.Error;
with Glib;
with Gtk.Container; use Gtk.Container;

package body Create_Css_Accordion is

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box : Gtk_Box;
      Provider : constant Gtk_Css_Provider
        := Gtk_Css_Provider_New;
      Error : aliased Glib.Error.GError;
      package FA is new Forall_User_Data (Gtk_Style_Provider);

      procedure Apply_Css
        (Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class;
         Provider : Gtk_Style_Provider)
      is
      begin
         Get_Style_Context (Widget).Add_Provider (Provider, Glib.Guint'Last);

         if Widget.all in Gtk_Container_Record'Class then
            declare
               Container : constant Gtk_Container := Gtk_Container (Widget);
            begin
               FA.Forall (Container, Apply_Css'Unrestricted_Access, Provider);
            end;
         end if;

      end Apply_Css;

   begin

      Gtk_New_Hbox (Box, False, 5);
      Set_Border_Width (Box, 10);
      Frame.Add (Box);

      Box.Add (Gtk_Button_New_With_Label ("This"));
      Box.Add (Gtk_Button_New_With_Label ("is"));
      Box.Add (Gtk_Button_New_With_Label ("a"));
      Box.Add (Gtk_Button_New_With_Label ("CSS"));
      Box.Add (Gtk_Button_New_With_Label ("Accordion"));
      Box.Add (Gtk_Button_New_With_Label (":-)"));

      if not Provider.Load_From_Path ("./css_accordion.css", Error'Access) then
         Put_Line ("Failed to load css_accordion.css !");
         Put_Line (Glib.Error.Get_Message (Error));
         return;
      end if;

      Apply_Css (Frame, +Provider);

      Show_All (Frame);
   end Run;

   function Help return String is
   begin
      return
        "This demo showcases the use of CSS for styling in GTK+ 3.x";
   end Help;

end Create_Css_Accordion;
