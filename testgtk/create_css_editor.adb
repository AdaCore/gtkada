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
with Gtk.Text_Buffer; use Gtk.Text_Buffer;
with Gtk.Text_View; use Gtk.Text_View;
with Gtk.Style_Provider; use Gtk.Style_Provider;
with Gtk.Style_Context; use Gtk.Style_Context;
with Gtk.Css_Provider; use Gtk.Css_Provider;
with Glib.Error;
with Glib;
with Gtk.Container; use Gtk.Container;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Text_Iter; use Gtk.Text_Iter;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Create_Css_Editor is

   CSS : constant String :=
      "/* You can edit the text in this window to change the" & ASCII.LF
     & " * appearance of this Window." & ASCII.LF
     & " * Be careful, if you screw it up, nothing might be visible" & ASCII.LF
     & " * anymore. :)" & ASCII.LF
     & " */" & ASCII.LF & ASCII.LF

     & "/* This CSS resets all properties to their defaults values" & ASCII.LF
     & " * and overrides all user settings and the theme in use */" & ASCII.LF
     & ASCII.LF

     & "/* Set a very futuristic style by default */" & ASCII.LF & ASCII.LF

     & "* {" & ASCII.LF
     & "  color: green;" & ASCII.LF
     & "  font-family: Monospace;" & ASCII.LF
     & "  border: 1px solid;" & ASCII.LF
     & "}" & ASCII.LF

     & "/* Make sure selections are visible */" & ASCII.LF
     & ":selected {" & ASCII.LF
     & "  background-color: darkGreen;" & ASCII.LF
     & "  color: black;" & ASCII.LF
     & "}";

   Last_Working_Css : Unbounded_String := To_Unbounded_String (CSS);
   Error : aliased Glib.Error.GError;

   Provider : constant Gtk_Css_Provider
     := Gtk_Css_Provider_New;

   procedure On_Text_Buffer_Changed
     (Self : access Gtk_Text_Buffer_Record'Class)
   is
      Start_Iter, End_Iter : Gtk_Text_Iter;

   begin
      Self.Get_Start_Iter (Start_Iter);
      Self.Get_End_Iter (End_Iter);
      declare
         Css : constant String := Self.Get_Text (Start_Iter, End_Iter);
         Dummy : Boolean;
         pragma Unreferenced (Dummy);
      begin
         if not Provider.Load_From_Data (Css, Error'Access) then
            Dummy := Provider.Load_From_Data
              (To_String (Last_Working_Css), Error'Access);
         else
            Last_Working_Css := To_Unbounded_String (Css);
         end if;
      end;
   end On_Text_Buffer_Changed;

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Container : Gtk_Scrolled_Window;
      Text_Buffer : Gtk_Text_Buffer;
      Text_View : Gtk_Text_View;

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

      Text_Buffer := Gtk_Text_Buffer_New (null);
      Container := Gtk_Scrolled_Window_New (null, null);
      Frame.Add (Container);
      Text_Buffer.On_Changed (On_Text_Buffer_Changed'Access);
      Text_Buffer.Set_Text (CSS);
      Text_View := Gtk_Text_View_New_With_Buffer (Text_Buffer);
      Container.Add (Text_View);

      Apply_Css (Frame, +Provider);

      Show_All (Frame);
   end Run;

   function Help return String is
   begin
      return
        "This demo showcases the use of CSS for styling in GTK+ 3.x";
   end Help;

end Create_Css_Editor;
