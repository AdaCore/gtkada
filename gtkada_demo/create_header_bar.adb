------------------------------------------------------------------------------
--               GtkAda - Ada binding for the Gimp Toolkit                  --
--                                                                          --
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

with Gtk.Box;              use Gtk.Box;
with Gtk.Button;           use Gtk.Button;
with Gtk.Check_Button;     use Gtk.Check_Button;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Frame;            use Gtk.Frame;
with Gtk.Header_Bar;       use Gtk.Header_Bar;
with Gtk.Label;            use Gtk.Label;
with Gtk.Scrolled_Window;  use Gtk.Scrolled_Window;
with Gtk.Text_View;        use Gtk.Text_View;
with Gtk.Widget;           use Gtk.Widget;
with Gtk.Window;           use Gtk.Window;

package body Create_Header_Bar is

   Bar : Gtk_Header_Bar;
   --  The bar embedded in the demo frame, driven by the controls below it.

   Layout : Gtk_Label;
   --  Echoes the decoration layout currently set on Bar, so that the string
   --  and the buttons it produces can be read side by side.

   procedure On_Title_Buttons_Toggled
     (Self : access Gtk_Check_Button_Record'Class);
   procedure On_Full_Layout (Self : access Gtk_Button_Record'Class);
   procedure On_Close_Layout (Self : access Gtk_Button_Record'Class);
   procedure On_Real_Window (Self : access Gtk_Button_Record'Class);

   function Navigation_Buttons return Gtk_Box;
   --  The linked back/forward pair packed at the start of a bar.

   procedure Set_Layout (To : String);
   --  Put To on Bar and report it in the Layout label.

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return
        "A @bGtk_Header_Bar@B is a horizontal bar that places children at"
        & " its start and its end while keeping a title centred between"
        & " them, whatever room those children take. It is the natural"
        & " titlebar for a @bGtk_Window@B, through"
        & " @bGtk.Window.Set_Titlebar@B."
        & ASCII.LF
        & "The bar at the top of this frame is packed like any other"
        & " widget: @bPack_Start@B holds a linked back/forward pair,"
        & " @bPack_End@B a single button, and @bSet_Title_Widget@B the"
        & " label between them. Left to itself the bar would instead show"
        & " the title of the window containing it."
        & ASCII.LF
        & "@bSet_Decoration_Layout@B names the window buttons and which"
        & " side they fall on -- the part before the colon sits at the"
        & " start, the part after it at the end -- and"
        & " @bSet_Show_Title_Buttons@B decides whether they are drawn at"
        & " all. @bFull layout@B and @bClose only@B switch between two"
        & " layouts, and the check button turns the buttons off entirely."
        & ASCII.LF
        & "Those buttons act on whatever window the bar finds itself in,"
        & " which here is the demo's own -- the bar is packed into this"
        & " frame, so it decorates a window it is not the titlebar of."
        & " @bShow in a real window@B opens a window that does hand its"
        & " titlebar to a @bGtk_Header_Bar@B, and that one sets no title"
        & " widget, so it falls back to showing the window's title.";
   end Help;

   ------------------------
   -- Navigation_Buttons --
   ------------------------

   function Navigation_Buttons return Gtk_Box is
      Box : Gtk_Box;
   begin
      Gtk_New (Box, Orientation_Horizontal, 0);

      --  "linked" is the theme's own class for a row of buttons drawn as a
      --  single unit, which is how upstream's header bar shows this pair.
      Box.Add_Css_Class ("linked");

      Box.Append (Gtk_Button_New_From_Icon_Name ("pan-start-symbolic"));
      Box.Append (Gtk_Button_New_From_Icon_Name ("pan-end-symbolic"));

      return Box;
   end Navigation_Buttons;

   ----------------
   -- Set_Layout --
   ----------------

   procedure Set_Layout (To : String) is
   begin
      Bar.Set_Decoration_Layout (To);
      Layout.Set_Text ("Layout: " & Bar.Get_Decoration_Layout);
   end Set_Layout;

   ------------------------------
   -- On_Title_Buttons_Toggled --
   ------------------------------

   procedure On_Title_Buttons_Toggled
     (Self : access Gtk_Check_Button_Record'Class) is
   begin
      Bar.Set_Show_Title_Buttons (Self.Get_Active);
   end On_Title_Buttons_Toggled;

   --------------------
   -- On_Full_Layout --
   --------------------

   procedure On_Full_Layout (Self : access Gtk_Button_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Set_Layout ("icon:minimize,maximize,close");
   end On_Full_Layout;

   ---------------------
   -- On_Close_Layout --
   ---------------------

   procedure On_Close_Layout (Self : access Gtk_Button_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Set_Layout (":close");
   end On_Close_Layout;

   --------------------
   -- On_Real_Window --
   --------------------

   procedure On_Real_Window (Self : access Gtk_Button_Record'Class) is
      Window   : constant Gtk_Window := Gtk_Window_New;
      Titlebar : constant Gtk_Header_Bar := Gtk_Header_Bar_New;
      Scrolled : constant Gtk_Scrolled_Window := Gtk_Scrolled_Window_New;
   begin
      Titlebar.Pack_Start (Navigation_Buttons);
      Titlebar.Pack_End
        (Gtk_Button_New_From_Icon_Name ("mail-send-receive-symbolic"));

      --  No Set_Title_Widget here, so the bar falls back to showing the
      --  window's own title -- the other half of what the widget does.
      Window.Set_Title ("Header Bar");
      Window.Set_Titlebar (Titlebar);
      Window.Set_Default_Size (400, 300);
      Window.Set_Transient_For
        (Gtk_Window (Self.Get_Ancestor (Gtk.Window.Get_Type)));

      Scrolled.Set_Child (Gtk_Text_View_New);
      Window.Set_Child (Scrolled);

      Window.Present;
   end On_Real_Window;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box      : Gtk_Box;
      Controls : Gtk_Box;
      Title    : constant Gtk_Label := Gtk_Label_New ("Header Bar");
      Buttons  : Gtk_Check_Button;
      Scrolled : constant Gtk_Scrolled_Window := Gtk_Scrolled_Window_New;
      Button   : Gtk_Button;
   begin
      Set_Label (Frame, "Header Bar");

      Gtk_New (Box, Orientation_Vertical, 0);
      Frame.Set_Child (Box);

      --  The bar itself, standing in for a titlebar over the content below.

      Gtk_New (Bar);
      Bar.Pack_Start (Navigation_Buttons);
      Bar.Pack_End
        (Gtk_Button_New_From_Icon_Name ("mail-send-receive-symbolic"));

      --  "title" is the style class the builtin title label carries, so a
      --  replacement that wants to look the same must ask for it.
      Title.Add_Css_Class ("title");
      Bar.Set_Title_Widget (Title);

      Box.Append (Bar);

      Scrolled.Set_Child (Gtk_Text_View_New);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Scrolled.Set_Vexpand (True);
      Box.Append (Scrolled);

      --  The controls.

      Gtk_New (Controls, Orientation_Horizontal, 6);
      Controls.Set_Margin_Top (6);
      Controls.Set_Margin_Bottom (6);
      Controls.Set_Margin_Start (6);
      Controls.Set_Margin_End (6);

      Buttons := Gtk_Check_Button_New_With_Label ("Show title buttons");
      Buttons.Set_Active (Bar.Get_Show_Title_Buttons);
      Buttons.On_Toggled (On_Title_Buttons_Toggled'Access);
      Controls.Append (Buttons);

      Button := Gtk_Button_New_With_Label ("Full layout");
      Button.On_Clicked (On_Full_Layout'Access);
      Controls.Append (Button);

      Button := Gtk_Button_New_With_Label ("Close only");
      Button.On_Clicked (On_Close_Layout'Access);
      Controls.Append (Button);

      Button := Gtk_Button_New_With_Label ("Show in a real window");
      Button.On_Clicked (On_Real_Window'Access);
      Controls.Append (Button);

      Layout := Gtk_Label_New ("");
      Layout.Set_Hexpand (True);
      Layout.Set_Halign (Align_End);
      Controls.Append (Layout);

      Set_Layout ("icon:minimize,maximize,close");

      Box.Append (Controls);
   end Run;

end Create_Header_Bar;
