-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
--         General Public License for more details.                  --
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

with Glib; use Glib;
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Object; use Gtk.Object;
with Gtk.Separator; use Gtk.Separator;
with Gtk.Status_Bar; use Gtk.Status_Bar;
with Gtk.Tips_Query; use Gtk.Tips_Query;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;
with Gtk.Tooltips; use Gtk.Tooltips;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk; use Gtk;
with Common; use Common;

with Ada.Text_IO;

package body Create_Tooltips is

   package Tooltips_Data is new User_Data (Gtk_Tooltips);
   package Query_Cb is new Object_Callback (Gtk_Tips_Query);
   package Entered_Cb is new Tips_Query_Callback (Gtk_Toggle_Button);

   Window  : aliased Gtk.Window.Gtk_Window;

   procedure Tooltips_Destroy (Widget : in out Gtk_Widget) is
      Tt : Gtk_Tooltips;
   begin
      Tt := Tooltips_Data.Get (Widget, "tooltips");
      Destroy (Tt);
      Destroy (Widget);
   end Tooltips_Destroy;

   procedure Widget_Entered (Tips_Query  : in out Gtk_Tips_Query;
                             Widget      : in out Gtk_Widget;
                             Tip_Text    : in String;
                             Tip_Private : in String;
                             Toggle      : in out Gtk_Toggle_Button)
   is
      pragma Warnings (Off, Widget);
      pragma Warnings (Off, Tip_Private);
   begin
      if Is_Active (Toggle) then
         if Tip_Text'Length /= 0 then
            Set (Tips_Query, "There is a Tip!");
         else
            Set (Tips_Query, "There is no Tip!");
         end if;
         --  Don't let GtkTipsQuery reset it's label
         Emit_Stop_By_Name (Tips_Query, "widget_entered");
      end if;
   end Widget_Entered;

   procedure Widget_Selected (Tips_Query  : in out Gtk_Tips_Query;
                              Widget      : in out Gtk_Widget;
                              Tip_Text    : in String;
                              Tip_Private : in String;
                              Data        : in out Gtk_Toggle_Button)
   is
      pragma Warnings (Off, Tips_Query);
      pragma Warnings (Off, Tip_Text);
      pragma Warnings (Off, Data);
   begin
      if Is_Created (Widget) then
         Ada.Text_IO.Put ("Help ");
         if Tip_Private'Length = 0 then
            Ada.Text_IO.Put ("None");
         else
            Ada.Text_IO.Put (Tip_Private);
         end if;
         Ada.Text_IO.Put_Line (" requested for "
                               & Type_Name (Get_Type (Widget)));
      end if;
--      return True;
   end Widget_Selected;

   procedure Run (Widget : in out Gtk.Button.Gtk_Button) is
      Id         : Guint;
      Box1,
        Box2,
        Box3     : Gtk_Box;
      Button     : Gtk_Button;
      Separator  : Gtk_Separator;
      Tooltips   : Gtk_Tooltips;
      Toggle     : Gtk_Toggle_Button;
      Tips_Query : Gtk_Tips_Query;
      Frame      : Gtk_Frame;

   begin

      if not Is_Created (Window) then
         Gtk_New (Window, Window_Toplevel);
         Id := Widget2_Cb.Connect (Window, "destroy", Destroyed'Access,
                                   Window'Access);
         Set_Title (Window, "Tooltips");
         Border_Width (Window, Border_Width => 0);
         Set_Policy (Window, True, False, False);
         Set_Usize (Window, 220, 280);

         Gtk_New (Tooltips);
         Tooltips_Data.Set (Window, Tooltips, "tooltips");

         Gtk_New_Vbox (Box1, False, 0);
         Add (Window, Box1);
         Show (Box1);

         Gtk_New_Vbox (Box2, False, 10);
         Border_Width (Box2, 10);
         Pack_Start (Box1, Box2, True, True, 0);
         Show (Box2);

         Gtk_New (Button, "Button1");
         Show (Button);
         Pack_Start (Box2, Button, False, False, 0);
         Set_Tip (Tooltips, button, "This is button 1",
                  "ContextHelp/buttons/1");

         Gtk_New (Button, "Button2");
         Show (Button);
         Pack_Start (Box2, Button, False, False, 0);
         Set_Tip (Tooltips, Button,
                  "This is button 2. This is also a really long tool tip which"
                  &" probably won't fit on a single line and will therefore "
                  & "need to be wrapped. Hopefully the wrapping will work "
                  & "correctly.", "ContextHelp/buttons/2");

         Gtk_New (Toggle, "Override TipsQuery Label");
         Show (Toggle);
         Pack_Start (Box2, Toggle, False, False, 0);
         Set_Tip (Tooltips, Toggle, "Toggle TipsQuery view.", "Hi msw! ;)");

         Gtk_New_Vbox (Box3, False, 5);
         Border_Width (Box3, 5);
         Show (Box3);

         Gtk_New (Tips_Query);

         Gtk_New (Button, "[?]");
         Pack_Start (Box3, Button, False, False, 0);
         Id := Query_Cb.Connect (Button, "clicked", Start_Query'Access,
                                 Tips_Query);
         Set_Tip (Tooltips, Button, "Start the Tooltips Inspector",
                  "ContextHelp/buttons/?");
         Show (Button);

         Pack_Start (Box3, Tips_Query, False, False, 0);
         Show (Tips_Query);
         Set_Caller (Tips_Query, Button);
         Id := Entered_Cb.Connect (Tips_Query, "widget_entered",
                                   Widget_Entered'Access, Toggle);
         Id := Entered_Cb.Connect (Tips_Query, "widget_selected",
                                   Widget_Selected'Access, Toggle);

         Gtk_New (Frame, "Tooltips Inspector");
         Border_Width (Frame, 0);
         Show (Frame);
         Pack_Start (Box2, Frame, True, True, 10);

         Add (Frame, Box3);

         Gtk_New_Hseparator (Separator);
         Pack_Start (Box1, Separator, False, True, 0);
         Show (Separator);

         Gtk_New_Vbox (Box2, False, 10);
         Border_Width (Box2, 10);
         Pack_Start (Box1, Box2, False, True, 0);
         Show (Box2);

         Gtk_New (Button, "close");
         Id := Widget_Cb.Connect (Button, "clicked", Tooltips_Destroy'Access,
                                  Window);
         Pack_Start (Box2, Button, True, True, 0);
         Set_Flags (Button, Can_Default);
         Grab_Default (Button);
         Show (Button);

         Set_Tip (Tooltips, Button, "Push this button to close window",
                  "ContextHelp/buttons/Close");
      end if;

      if not Gtk.Widget.Visible_Is_Set (Window) then
         Show (Window);
      else
         Destroy (Window);
      end if;

   end Run;

end Create_Tooltips;

