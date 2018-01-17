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

with Glib;        use Glib;
with Glib.Object; use Glib.Object;
with Gdk.RGBA;    use Gdk.RGBA;
with Gtk;                   use Gtk;
with Gtkada.Handlers;       use Gtkada.Handlers;
with Gtk.Box;               use Gtk.Box;
with Gtk.Button;            use Gtk.Button;
with Gtk.Combo_Box_Text;    use Gtk.Combo_Box_Text;
with Gtk.Menu;              use Gtk.Menu;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Menu_Bar;          use Gtk.Menu_Bar;
with Gtk.Menu_Button;       use Gtk.Menu_Button;
with Gtk.Menu_Item;         use Gtk.Menu_Item;
with Gtk.Spin_Button;       use Gtk.Spin_Button;
with Gtk.Radio_Menu_Item;   use Gtk.Radio_Menu_Item;
with Gtk.Tearoff_Menu_Item; use Gtk.Tearoff_Menu_Item;
with Gtk.Widget;            use Gtk.Widget;

with Common;                use Common;

package body Create_Menu is

   package My_Popup is new Gtk.Menu.Popup_User_Data (Gint);
   use My_Popup;

   procedure Position_At_0
     (Menu : not null access Gtk_Menu_Record'Class;
      X    : out Gint;
      Y    : out Gint;
      Push_In : out Boolean);
   --  Position function at coordinates 0,0.

   procedure Position_At_Data
     (Menu : not null access Gtk_Menu_Record'Class;
      X    : out Gint;
      Y    : out Gint;
      Push_In : out Boolean;
      Val  : Gint);
   --  Position function at coordinates Val,Val.

   procedure Popup_At_Position (Widget : access GObject_Record'Class);
   --  Callback for the "Popup at given coordinates" button

   procedure Popup (Widget : access Gtk_Button_Record'Class);
   --  Callback for the "Popup at 0,0 coordinates" button

   -------------------
   -- Position_At_0 --
   -------------------

   procedure Position_At_0
     (Menu : not null access Gtk_Menu_Record'Class;
      X    : out Gint;
      Y    : out Gint;
      Push_In : out Boolean)
   is
      pragma Unreferenced (Menu);
   begin
      X := 0;
      Y := 0;
      Push_In := True;
   end Position_At_0;

   ----------------------
   -- Position_At_Data --
   ----------------------

   procedure Position_At_Data
     (Menu : not null access Gtk_Menu_Record'Class;
      X    : out Gint;
      Y    : out Gint;
      Push_In : out Boolean;
      Val  : Gint)
   is
      pragma Unreferenced (Menu);
   begin
      X := Val;
      Y := Val;
      Push_In := True;
   end Position_At_Data;

   -----------------------
   -- Popup_At_Position --
   -----------------------

   procedure Popup_At_Position
      (Widget : access GObject_Record'Class)
   is
      Spin : constant Gtk_Spin_Button := Gtk_Spin_Button (Widget);
      Menu : Gtk_Menu;
      Menu_Item : Gtk_Menu_Item;
      Val : constant Gint := Get_Value_As_Int (Spin);
   begin
      Gtk_New (Menu);

      Gtk_New (Menu_Item, "this");
      Append (Menu, Menu_Item);
      Gtk_New (Menu_Item, "menu");
      Append (Menu, Menu_Item);
      Gtk_New (Menu_Item, "should be positioned");
      Append (Menu, Menu_Item);
      Gtk_New (Menu_Item, "at " & Val'Img & "," & Val'Img);
      Append (Menu, Menu_Item);
      Show_All (Menu);
      My_Popup.Popup
        (Menu => Menu,
         Func => Position_At_Data'Access,
         Data => Val);
   end Popup_At_Position;

   -----------
   -- Popup --
   -----------

   procedure Popup (Widget : access Gtk_Button_Record'Class) is
      pragma Unreferenced (Widget);
      Menu : Gtk_Menu;
      Menu_Item : Gtk_Menu_Item;
   begin
      Gtk_New (Menu);

      Gtk_New (Menu_Item, "this");
      Append (Menu, Menu_Item);
      Gtk_New (Menu_Item, "menu");
      Append (Menu, Menu_Item);
      Gtk_New (Menu_Item, "should be positioned");
      Append (Menu, Menu_Item);
      Gtk_New (Menu_Item, "in the top-left corner");
      Append (Menu, Menu_Item);
      Show_All (Menu);
      Popup
        (Menu,
         Parent_Menu_Shell => null,
         Parent_Menu_Item  => null,
         Func              => Position_At_0'Access,
         Button            => 1,
         Activate_Time     => 0);
   end Popup;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return
        "There are several widgets involved in displaying menus. The"
        & " @bGtk_Menu_Bar@B widget is a horizontal menu bar, which normally"
        & " appears at the top of an application. The @bGtk_Menu@B widget is"
        & " the actual menu that pops up. Both @bGtk_Menu_Bar@B and"
        & " @bGtk_Menu@B are subclasses of @bGtk_Menu_Shell@B; a"
        & " @bGtk_Menu_Shell@B contains menu items (@bGtk_Menu_Item@B)."
        & " Each menu item contains text and/or images and can be selected"
        & " by the user."
        & ASCII.LF
        & "This demo shows how to create a @bGtk_Menu_Bar@B, with multiple"
        & " @bGtk_Menu@Bs. Each of this submenu is actually a @btearoff@B menu"
        & ", which means by that clicking on the dashed line, you can simply"
        & " glue the submenu to another place on your desktop, and keep it"
        & " around. To hide it, simply click on the dashed line again."
        & ASCII.LF
        & "There are several kinds of menu item, including plain"
        & " @bGtk_Menu_Item@B, @bGtk_Check_Menu_Item@B which can be"
        & " checked/unchecked, @bGtk_Radio_Menu_Item@B which is a check menu"
        & " item that's in a mutually exclusive group,"
        & " @bGtk_Separator_Menu_Item@B which is a separator bar,"
        & " @bGtk_Tearoff_Menu_Item@B which allows a @bGtk_Menu@B to be torn"
        & " off, and @bGtk_Image_Menu_Item@B which can place a @bGtk_Image@B"
        & " or other widget next to the menu text. A @bGtk_Menu_Item can have"
        & " a submenu, which is simply a @bGtk_Menu@B to pop up when the menu"
        & " item is selected. Typically, all menu items in a menu bar have"
        & " submenus."
        & ASCII.LF
        & "This page also includes a @bGtk_Menu_Button@B (with an arrow)"
        & " which pops up a menu when clicked on.";
   end Help;

   -----------------
   -- Create_Menu --
   -----------------

   function Create_Menu
     (Depth : Integer; Tearoff : Boolean) return Gtk_Menu
   is
      Menu      : Gtk_Menu;
      Group     : Widget_SList.GSlist;
      Menu_Item : Gtk_Radio_Menu_Item;
      Red : constant Gdk_RGBA := (Red   => 1.0,
                                  Green => 0.0,
                                  Blue  => 0.0,
                                  Alpha => 1.0);
   begin
      Gtk_New (Menu);

      if Tearoff then
         declare
            Tear_Menu : Gtk_Tearoff_Menu_Item;
         begin
            Gtk_New (Tear_Menu);
            Append (Menu, Tear_Menu);
            Show (Tear_Menu);
         end;
      end if;

      for J in 0 .. 5 loop
         Gtk_New (Menu_Item, Group, "Item" & Integer'Image (Depth)
                  & " -" & Integer'Image (J + 1));
         Group := Gtk.Radio_Menu_Item.Get_Group (Menu_Item);
         Append (Menu, Menu_Item);
         Show (Menu_Item);

         if J = 1 then
            Menu_Item.Get_Child.Override_Color (0, Red);
         end if;

         if J = 3 then
            Set_Sensitive (Menu_Item, False);
         end if;

         if Depth > 1 then
            Set_Submenu (Menu_Item, Create_Menu (Depth - 1, Tearoff));
         end if;
      end loop;
      return Menu;
   end Create_Menu;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1 : Gtk_Box;
      Box2 : Gtk_Box;
      Menu_Bar  : Gtk_Menu_Bar;
      Menu : Gtk_Menu;
      Menu_Button    : Gtk_Menu_Button;
      Menu_Item : Gtk_Menu_Item;
      Combo : Gtk_Combo_Box_Text;

      Button : Gtk_Button;
      Spin   : Gtk_Spin_Button;

   begin
      Set_Label (Frame, "Menus");
      Gtk_New_Vbox (Box1, False, 0);
      Add (Frame, Box1);

      Gtk_New (Menu_Bar);
      Pack_Start (Box1, Menu_Bar, False, False, 0);

      Menu := Create_Menu (2, True);

      Gtk_New (Menu_Item, "test" & ASCII.LF & "line2");
      Set_Submenu (Menu_Item, Menu);
      Append (Menu_Bar, Menu_Item);

      Gtk_New (Menu_Item, "foo");
      Set_Submenu (Menu_Item, Create_Menu (3, True));
      Append (Menu_Bar, Menu_Item);

      Gtk_New (Menu_Item, "bar");
      Set_Submenu (Menu_Item, Create_Menu (4, True));

      Append (Menu_Bar, Menu_Item);

      Gtk_New_Vbox (Box2, False, 10);
      Set_Border_Width (Box2, 10);
      Pack_Start (Box1, Box2, False, False, 0);

      Gtk_New (Combo);
      for J in 0 .. 5 loop
         Combo.Append_Text ("Item" & Integer'Image (J));
      end loop;
      Pack_Start (Box2, Combo, False, False, 0);

      Gtk_New (Button, "Popup at 0,0 coordinates");
      Pack_Start (Box1, Button, False, False, 3);

      Button_Handler.Connect (Button, "clicked", Popup'Access);

      Gtk_New_Hbox (Box2, False, 10);
      Pack_Start (Box1, Box2, False, False, 0);

      Gtk_New (Spin, 0.0, 800.0, 100.0);
      Set_Value (Spin, 200.0);
      Pack_Start (Box2, Spin, False, False, 3);

      Gtk_New (Button, "Popup at given coordinates");
      Pack_Start (Box2, Button, False, False, 3);

      Object_Callback.Object_Connect
        (Button, "clicked",
         Popup_At_Position'Access,
         Spin);

      ---------------------
      -- Gtk_Menu_Button --
      ---------------------

      Gtk_New (Menu_Button);
      Box1.Pack_Start (Menu_Button, False, False, 10);
      Menu_Button.Set_Tooltip_Text ("A Gtk_Menu_Button");

      Menu := Create_Menu (2, True);
      Menu_Button.Set_Popup (Menu);

      Show_All (Frame);
   end Run;

end Create_Menu;
