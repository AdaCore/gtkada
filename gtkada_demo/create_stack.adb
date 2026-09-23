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

with Ada.Characters.Handling;

with Gtk.Box;            use Gtk.Box;
with Gtk.Button;         use Gtk.Button;
with Gtk.Check_Button;   use Gtk.Check_Button;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Frame;          use Gtk.Frame;
with Gtk.Stack;          use Gtk.Stack;
with Gtk.Stack_Page;     use Gtk.Stack_Page;
with Gtk.Stack_Switcher; use Gtk.Stack_Switcher;
with Gtk.Text_View;      use Gtk.Text_View;
with Gtk.Widget;         use Gtk.Widget;

package body Create_Stack is

   Transitions : constant array (Positive range <>)
     of Gtk_Stack_Transition_Type :=
       (Stack_Transition_Type_None,
        Stack_Transition_Type_Crossfade,
        Stack_Transition_Type_Slide_Left_Right,
        Stack_Transition_Type_Slide_Up_Down,
        Stack_Transition_Type_Over_Up_Down,
        Stack_Transition_Type_Rotate_Left_Right);
   --  The transitions the demo cycles through; a readable subset of the
   --  twenty-odd literals of Gtk_Stack_Transition_Type.

   Stack   : Gtk_Stack;
   Current : Positive := Transitions'First;

   function Transition_Label (Transition : Gtk_Stack_Transition_Type)
      return String;
   --  "Slide Left Right" for Stack_Transition_Type_Slide_Left_Right.

   procedure Next_Transition (Self : access Gtk_Button_Record'Class);
   --  Move the stack on to the next transition of Transitions and relabel
   --  Self accordingly.

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return
        "A @bGtk_Stack@B shows one of its children at a time, animating the"
        & " change from one to the next."
        & ASCII.LF
        & "Each child is added with @bAdd_Titled@B, which returns the"
        & " @bGtk_Stack_Page@B that carries the child's name, title and"
        & " flags — in gtk4 the page object has replaced the gtk3 child"
        & " properties."
        & ASCII.LF
        & "The @bGtk_Stack_Switcher@B above the stack is driven entirely by"
        & " @bSet_Stack@B: it builds its buttons from the pages' titles. The"
        & " third page is marked with @bSet_Needs_Attention@B, which is what"
        & " colours its button red through the demo's stylesheet."
        & ASCII.LF
        & "Use the button below to cycle through a few of the transition"
        & " types; they are slowed down to 1.5 seconds so they can be seen.";
   end Help;

   ----------------------
   -- Transition_Label --
   ----------------------

   function Transition_Label (Transition : Gtk_Stack_Transition_Type)
      return String
   is
      Prefix : constant String := "STACK_TRANSITION_TYPE_";
      Image  : constant String := Gtk_Stack_Transition_Type'Image (Transition);
      Name   : String := Image (Image'First + Prefix'Length .. Image'Last);
   begin
      for I in Name'Range loop
         if Name (I) = '_' then
            Name (I) := ' ';
         elsif I > Name'First and then Name (I - 1) /= ' ' then
            Name (I) := Ada.Characters.Handling.To_Lower (Name (I));
         end if;
      end loop;
      return Name;
   end Transition_Label;

   ---------------------
   -- Next_Transition --
   ---------------------

   procedure Next_Transition (Self : access Gtk_Button_Record'Class) is
   begin
      if Current = Transitions'Last then
         Current := Transitions'First;
      else
         Current := Current + 1;
      end if;

      Stack.Set_Transition_Type (Transitions (Current));
      Self.Set_Label
        ("Transition: " & Transition_Label (Transitions (Current)));
   end Next_Transition;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box       : Gtk_Box;
      Switcher  : Gtk_Stack_Switcher;
      Text      : Gtk_Text_View;
      Button    : Gtk_Button;
      Check     : Gtk_Check_Button;
      Cycle     : Gtk_Button;
      Page      : Gtk_Stack_Page;
   begin
      Set_Label (Frame, "Stack");

      Current := Transitions'First;

      Gtk_New (Box, Orientation_Vertical, 6);
      Frame.Set_Child (Box);

      Gtk_New (Switcher);
      Switcher.Set_Halign (Align_Center);
      Box.Append (Switcher);

      Gtk_New (Stack);
      Stack.Set_Vexpand (True);
      Stack.Set_Transition_Duration (1_500);  --  slower, to better see them
      Stack.Set_Transition_Type (Transitions (Current));
      Box.Append (Stack);

      Switcher.Set_Stack (Stack);

      Gtk_New (Text);
      Text.Get_Buffer.Set_Text ("This is a test");
      Page := Stack.Add_Titled (Text, "text", "Text");

      Button := Gtk_Button_New_With_Label ("A button");
      Button.Set_Halign (Align_Center);
      Button.Set_Valign (Align_Center);
      Page := Stack.Add_Titled (Button, "button", "Button");

      Check := Gtk_Check_Button_New_With_Label ("A check button");
      Check.Set_Halign (Align_Center);
      Check.Set_Valign (Align_Center);
      Page := Stack.Add_Titled (Check, "check", "Check");

      --  Calling attention to a page reddens its switcher button; see the
      --  .needs-attention rule in gtkada_demo.css.
      Page.Set_Needs_Attention (True);

      Cycle :=
        Gtk_Button_New_With_Label
          ("Transition: " & Transition_Label (Transitions (Current)));
      Cycle.Set_Halign (Align_Center);
      Cycle.On_Clicked (Next_Transition'Access);
      Box.Append (Cycle);
   end Run;

end Create_Stack;
