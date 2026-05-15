------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                --
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

with Glib.Xml_Int;            use Glib.Xml_Int;
with Gtk.Widget;              use Gtk.Widget;

separate (Gtkada.MDI)
package body Maximize_Action is

   procedure Free is new Ada.Unchecked_Deallocation
     (Saved_Perspective_Record, Saved_Perspective);

   function Get_Container (Child : MDI_Child) return Gtk.Widget.Gtk_Widget;
   --  Returns the container to hide Child when not maximized.
   --  Always return null is Child is in a floating window.

   procedure Show_Hidden_Children (MDI : access MDI_Window_Record'Class);
   --  Go through all the MDI children and call show on them

   -----------------------
   -- Is_Maximized_Mode --
   -----------------------

   function Is_Maximized_Mode
     (MDI : access MDI_Window_Record'Class) return Boolean
   is
      Perspective : constant String := Current_Perspective (MDI);
   begin
      return MDI.Saved_Sizes.Contains (Perspective)
        and then MDI.Saved_Sizes (Perspective) /= null;
   end Is_Maximized_Mode;

   ------------------------
   -- On_Toggle_Maximize --
   ------------------------

   procedure On_Toggle_Maximize
     (Child : access Gtk_Widget_Record'Class)
   is
      C           : constant MDI_Child      := MDI_Child (Child);
      MDI         : constant MDI_Window     := C.Get_MDI;
      Perspective : constant String         := Current_Perspective (MDI);
      Dummy       : Boolean;
   begin
      if not Is_Maximized_Mode (MDI) then
         declare
            New_Element : constant Saved_Perspective :=
              new Saved_Perspective_Record;
         begin
            Print_Debug ("Maximize Container of """ & String (C.Get_Title)
                         & """ in perspective """ & Perspective & """");
            --  Save the widget currently maximized
            New_Element.Container := Get_Container (C);
            --  Save the current dimensions of the Central area
            New_Element.Width := Get_Allocated_Width (New_Element.Container);
            New_Element.Height := Get_Allocated_Height (New_Element.Container);
            MDI.Saved_Sizes.Include (Perspective, New_Element);
         end;
         --  Hide the other containers
         Hide_When_Maximized (MDI);
      else
         --  Restore the desktop and Free the saved data
         if MDI.Saved_Sizes.Contains (Perspective) then
            Print_Debug ("Unmaximize perspective """ & Perspective & """");
            MDI.Set_Size
              (Widget           => MDI.Saved_Sizes (Perspective).Container,
               Width            => MDI.Saved_Sizes (Perspective).Width,
               Height           => MDI.Saved_Sizes (Perspective).Height,
               Fixed_Size       => False,
               Force_Given_Size => True);
            Show_Hidden_Children (MDI);
            Free (MDI.Saved_Sizes (Perspective));
         end if;
      end if;
   end On_Toggle_Maximize;

   ---------------------
   -- On_Remove_Child --
   ---------------------

   procedure On_Remove_Child
     (Self : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Child       : constant MDI_Child  := MDI_Child (Self);
      MDI         : constant MDI_Window := Child.Get_MDI;
      Perspective : constant String     := Current_Perspective (MDI);
   begin

      --  Check if the current perspective is maximized
      if not MDI.Saved_Sizes.Contains (Perspective)
        or else MDI.Saved_Sizes (Perspective) = null
      then
         return;
      end if;

      declare
         Maximized_Container : constant Gtk_Widget :=
           MDI.Saved_Sizes (Perspective).Container;
         Cur_Container : constant Gtk_Widget := Get_Container (Child);
      begin

         if Maximized_Container = Cur_Container then
            --  If the last child in the container is destroyed or will float
            --  unmaximize the perspective
            if Maximized_Container = Gtk_Widget (MDI.Central) then
               if Gtk.Widget.Widget_List.Length
                 (Gtk_Container (Child.Get_Notebook).Get_Children) = 1
               then
                  --  Unmaximize the perspective
                  On_Toggle_Maximize (Child);
               end if;
            else
               if Gtk.Widget.Widget_List.Length
                 (Gtk_Container (Maximized_Container).Get_Children) = 1
               then
                  --  Unmaximize the perspective
                  On_Toggle_Maximize (Child);
               end if;
            end if;
         end if;
      end;
   end On_Remove_Child;

   -------------------
   -- Get_Container --
   -------------------

   function Get_Container (Child : MDI_Child) return Gtk.Widget.Gtk_Widget
   is
      MDI       : constant MDI_Window := Child.Get_MDI;
      Container : Gtk.Widget.Gtk_Widget;
   begin
      if MDI.In_Central_Area (Child) then
         Container := Gtk_Widget (MDI.Central);
      elsif Child.Get_Notebook /= null then
         Container := Gtk_Widget (Child.Get_Notebook);
      elsif Child.Is_Floating then
         Container := null;
      else
         Container := Gtk_Widget (Child);
      end if;
      return Container;
   end Get_Container;

   --------------------------
   -- Show_Hidden_Children --
   --------------------------

   procedure Show_Hidden_Children (MDI : access MDI_Window_Record'Class)
   is
      Iter         : Child_Iterator  :=
        MDI.First_Child (Group_By_Notebook => False, Visible_Only => False);
      Cur_Child    : MDI_Child;
      Container    : Gtk.Widget.Gtk_Widget;
   begin
      loop
         Cur_Child := Gtkada.MDI.Get (Iter);
         exit when Cur_Child = null;
         Container := Get_Container (Cur_Child);
         if Container /= null then
            Show (Container);
         end if;
         Gtkada.MDI.Next (Iter);
      end loop;
      Show (MDI.Central);
   end Show_Hidden_Children;

   -------------------------
   -- Hide_When_Maximized --
   -------------------------

   procedure Hide_When_Maximized (MDI : access MDI_Window_Record'Class)
   is
      Perspective  : constant String := Current_Perspective (MDI);
      Iter         : Child_Iterator  :=
        MDI.First_Child (Group_By_Notebook => False, Visible_Only => False);
      Cur_Child    : MDI_Child;
      Container    : Gtk.Widget.Gtk_Widget;
      Hide_Central : Boolean         := True;
   begin
      if MDI.Saved_Sizes.Contains (Perspective)
        and then MDI.Saved_Sizes (Perspective) /= null
      then
         loop
            Cur_Child := Gtkada.MDI.Get (Iter);
            exit when Cur_Child = null;
            Hide_Central :=
              Hide_Central and not MDI.In_Central_Area (Cur_Child);
            Container := Get_Container (Cur_Child);
            if Container /= null and then
              Container /= MDI.Saved_Sizes (Perspective).Container
            then
               Hide (Container);
            end if;
            Gtkada.MDI.Next (Iter);
         end loop;
         --  If Central possesses no MDI_Child then hide it
         if Hide_Central then
            Hide (MDI.Central);
         end if;
      end if;
   end Hide_When_Maximized;

   ---------------------
   -- Free_Saved_Data --
   ---------------------

   procedure Free_Saved_Data (MDI : access MDI_Window_Record'Class)
   is
   begin
      for Perspective of MDI.Saved_Sizes loop
         Free (Perspective);
      end loop;
      MDI.Saved_Sizes.Clear;
   end Free_Saved_Data;

end Maximize_Action;
