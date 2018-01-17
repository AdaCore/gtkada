------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2018-2018, AdaCore                --
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
     (Hidden_Container_Data_Record, Hidden_Container_Data);

   procedure Free is new Ada.Unchecked_Deallocation
     (Saved_Perspective_Record, Saved_Perspective);

   function Save_Sizes
     (MDI : access MDI_Window_Record'Class)
      return MDI_Child_Container_List.List;
   --  Save the sizes of the MDI children containers before maximizing

   procedure Restore_Sizes (MDI : access MDI_Window_Record'Class);
   --  Restore the sizes of the MDI children containers saved before maximizing

   function Get_Container (Child : MDI_Child) return Gtk.Widget.Gtk_Widget;
   --  Returns the container to hide Child when not maximized.
   --  Always return null is Child is in a floating window.

   -----------------------
   -- On_Maximize_Child --
   -----------------------

   procedure On_Maximize_Child
     (Child : access Gtk_Widget_Record'Class)
   is
      C           : constant MDI_Child      := MDI_Child (Child);
      MDI         : constant MDI_Window     := C.Get_MDI;
      Perspective : constant String         := Current_Perspective (MDI);
      Dummy       : Boolean;
   begin
      if Perspective = "Maximized Editors" then
         return;
      end if;

      if not MDI.Saved_Sizes.Contains (Perspective)
        or else MDI.Saved_Sizes (Perspective) = null
      then
         declare
            New_Element : constant Saved_Perspective :=
              new Saved_Perspective_Record;
         begin
            Print_Debug ("Maximize Container of """ & String (C.Get_Title)
                         & """ in perspective """ & Perspective & """");
            --  Save the widget currently maximized
            New_Element.Container := Get_Container (C);
            --  Save the current dimensions of the others containers
            New_Element.List_Hidden := Save_Sizes (MDI);
            MDI.Saved_Sizes.Include (Perspective, New_Element);
         end;
         --  Hide the other containers
         Hide_When_Maximized (MDI);
      else
         --  Restore the desktop and Free the saved XML
         if MDI.Saved_Sizes.Contains (Perspective) then
            Print_Debug ("Unmaximize perspective """ & Perspective & """");
            Restore_Sizes (MDI);
            Free (MDI.Saved_Sizes (Perspective));
         end if;
      end if;
   end On_Maximize_Child;

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
                  On_Maximize_Child (Child);
               end if;
            else
               if Gtk.Widget.Widget_List.Length
                 (Gtk_Container (Maximized_Container).Get_Children) = 1
               then
                  --  Unmaximize the perspective
                  On_Maximize_Child (Child);
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

   -------------------------
   -- Hide_When_Maximized --
   -------------------------

   procedure Hide_When_Maximized (MDI : access MDI_Window_Record'Class)
   is
      Perspective : constant String := Current_Perspective (MDI);
      Iter        : Child_Iterator  :=
        MDI.First_Child (Group_By_Notebook => False, Visible_Only => False);
      Cur_Child   : MDI_Child;
      Container   : Gtk.Widget.Gtk_Widget;
   begin
      if MDI.Saved_Sizes.Contains (Perspective)
        and then MDI.Saved_Sizes (Perspective) /= null
      then
         loop
            Cur_Child := Gtkada.MDI.Get (Iter);
            exit when Cur_Child = null;
            Container := Get_Container (Cur_Child);
            if Container /= null and then
              Container /= MDI.Saved_Sizes (Perspective).Container
            then
               Hide (Container);
            end if;
            Gtkada.MDI.Next (Iter);
         end loop;
      end if;
   end Hide_When_Maximized;

   ----------------
   -- Save_Sizes --
   ----------------

   function Save_Sizes
     (MDI : access MDI_Window_Record'Class)
      return MDI_Child_Container_List.List
   is
      List : MDI_Child_Container_List.List;
      function Is_Already_Saved (W : Gtk.Widget.Gtk_Widget) return Boolean;
      --  Return True if W is already in List

      function Is_Already_Saved (W : Gtk.Widget.Gtk_Widget) return Boolean
      is
         Res : Boolean := False;
      begin
         for Cur of List loop
            if Cur.Container = W then
               Res := True;
               exit;
            end if;
         end loop;
         return Res;
      end Is_Already_Saved;

      Iter       : Child_Iterator  :=
        MDI.First_Child (Group_By_Notebook => False, Visible_Only => False);
      Cur_Child  : MDI_Child;
      Container  : Gtk.Widget.Gtk_Widget;
      Saved_Data : Hidden_Container_Data;
   begin
      loop
         Cur_Child := Gtkada.MDI.Get (Iter);
         exit when Cur_Child = null;
         Container := Get_Container (Cur_Child);
         if Container /= null and not Is_Already_Saved (Container) then
            Saved_Data := new Hidden_Container_Data_Record;
            Saved_Data.Container := Container;
            Saved_Data.Width := Get_Allocated_Width (Container);
            Saved_Data.Height := Get_Allocated_Height (Container);
            List.Append (Saved_Data);
         end if;
         Gtkada.MDI.Next (Iter);
      end loop;
      return List;
   end Save_Sizes;

   -------------------
   -- Restore_Sizes --
   -------------------

   procedure Restore_Sizes (MDI : access MDI_Window_Record'Class)
   is
      Perspective : constant String := Current_Perspective (MDI);
      Cpt : Integer := 0;
   begin
      if MDI.Saved_Sizes.Contains (Perspective)
        and then MDI.Saved_Sizes (Perspective) /= null
      then
         for Container of MDI.Saved_Sizes (Perspective).List_Hidden loop
            Cpt := Cpt + 1;
            Print_Debug (Integer'Image (Cpt));
            MDI.Set_Size
              (Widget           => Container.Container,
               Width            => Container.Width,
               Height           => Container.Height,
               Fixed_Size       => False,
               Force_Given_Size => True);
         end loop;
         Show_All (MDI);
      end if;
   end Restore_Sizes;

   ---------------------
   -- Free_Saved_Data --
   ---------------------

   procedure Free_Saved_Data (MDI : access MDI_Window_Record'Class)
   is
   begin
      for Perspective of MDI.Saved_Sizes loop
         if Perspective /= null then
            for Container of Perspective.List_Hidden loop
               Free (Container);
            end loop;
            Perspective.List_Hidden.Clear;
            Free (Perspective);
         end if;
      end loop;
      MDI.Saved_Sizes.Clear;
   end Free_Saved_Data;

end Maximize_Action;
