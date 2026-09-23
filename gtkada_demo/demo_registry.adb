------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                    Copyright (C) 1998-2026, AdaCore                      --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Glib.List_Store; use Glib.List_Store;
with Glib.Object;     use Glib.Object;

with Demo_Items; use Demo_Items;

--  TRANSITION: the demos that are still waiting for their bindings are
--  listed, commented out, in main_windows.adb's own transition block.

with Create_Box;
with Create_Buttons;
with Create_Check_Buttons;
with Create_Color_Chooser;
with Create_Column_View;
with Create_Css_Accordion;
with Create_Custom_Widget;
with Create_Dialog;
with Create_Drawing_Area;
with Create_Entry;
with Create_Frame;
with Create_Label;
with Create_List_Store;
with Create_Menu;
with Create_Paned;
with Create_Reparent;
with Create_Scrolled;
with Create_Spin;
with Create_Test_Timeout;
with Create_Text_View;
with Create_Toggle_Buttons;
with Create_Tooltips;
with Create_Tree_Filter;
with Create_Tree_View;
with Create_Slice_List_Model;

package body Demo_Registry is

   type Demo_Info is record
      Path : Unbounded_String;
      Run  : Demo_Function;
      Help : Help_Function;
   end record;

   function To_Demo
     (Path : String; Runner : Demo_Function; Help : Help_Function)
      return Demo_Info
   is (Path => To_Unbounded_String (Path), Run => Runner, Help => Help);

   Demos : constant array (Positive range <>) of Demo_Info :=
     (To_Demo
        ("Buttons/Buttons",
         Create_Buttons.Run'Access,
         Create_Buttons.Help'Access),
      To_Demo
        ("Buttons/Check Buttons",
         Create_Check_Buttons.Run'Access,
         Create_Check_Buttons.Help'Access),
      To_Demo
        ("Buttons/Toggle Buttons",
         Create_Toggle_Buttons.Run'Access,
         Create_Toggle_Buttons.Help'Access),
      To_Demo
        ("Dialogs", Create_Dialog.Run'Access, Create_Dialog.Help'Access),
      To_Demo
        ("Drawing Area",
         Create_Drawing_Area.Run'Access,
         Create_Drawing_Area.Help'Access),
      To_Demo
        ("GtkAda/Column View",
         Create_Column_View.Run'Access,
         Create_Column_View.Help'Access),
      To_Demo
        ("GtkAda/Custom Widget",
         Create_Custom_Widget.Run'Access,
         Create_Custom_Widget.Help'Access),
      To_Demo
        ("GtkAda/Entry", Create_Entry.Run'Access, Create_Entry.Help'Access),
      To_Demo
        ("GtkAda/Labels", Create_Label.Run'Access, Create_Label.Help'Access),
      To_Demo
        ("GtkAda/Menus", Create_Menu.Run'Access, Create_Menu.Help'Access),
      To_Demo
        ("GtkAda/Reparent",
         Create_Reparent.Run'Access,
         Create_Reparent.Help'Access),
      To_Demo
        ("GtkAda/Timeout",
         Create_Test_Timeout.Run'Access,
         Create_Test_Timeout.Help'Access),
      To_Demo
        ("GtkAda/Tooltips",
         Create_Tooltips.Run'Access,
         Create_Tooltips.Help'Access),
      To_Demo
        ("Layout/Boxes", Create_Box.Run'Access, Create_Box.Help'Access),
      To_Demo
        ("Layout/Frames", Create_Frame.Run'Access, Create_Frame.Help'Access),
      To_Demo
        ("Layout/Scrolled Window",
         Create_Scrolled.Run'Access,
         Create_Scrolled.Help'Access),
      To_Demo
        ("Paned Widgets", Create_Paned.Run'Access, Create_Paned.Help'Access),
      To_Demo
        ("Pickers and Launchers",
         Create_Color_Chooser.Run'Access,
         Create_Color_Chooser.Help'Access),
      To_Demo
        ("Spin Buttons", Create_Spin.Run'Access, Create_Spin.Help'Access),
      To_Demo
        ("Text View/Multiple Views",
         Create_Text_View.Run'Access,
         Create_Text_View.Help'Access),
      To_Demo
        ("Theming/CSS Accordion",
         Create_Css_Accordion.Run'Access,
         Create_Css_Accordion.Help'Access),
      To_Demo
        ("Tree View/Filter Model",
         Create_Tree_Filter.Run'Access,
         Create_Tree_Filter.Help'Access),
      To_Demo
        ("Tree View/List Store",
         Create_List_Store.Run'Access,
         Create_List_Store.Help'Access),
      To_Demo
        ("Tree View/Tree Store",
         Create_Tree_View.Run'Access,
         Create_Tree_View.Help'Access),
      To_Demo
        ("Tree View/Slice List Model",
         Create_Slice_List_Model.Run'Access,
         Create_Slice_List_Model.Help'Access));
   --  The set of demos exposed in the selector, named after gtk4-demo's own
   --  taxonomy. A name with no "/" is a demo that upstream leaves
   --  uncategorised, and which therefore sits at the top level of the tree.

   type Root_Entry is record
      Name     : Unbounded_String;
      Item     : Demo_Item := null;
      Children : Glist_Store := null;
      --  Null for a top-level demo, which has no children and must be told
      --  apart from a category with none.

      Demo     : Natural := 0;
      --  Index into Demos for a top-level demo, 0 for a category.
   end record;

   Roots : array (1 .. Demos'Length) of Root_Entry;
   --  The top level of the tree, kept sorted by Name. At most one entry per
   --  demo, since two demos sharing a category share an entry.

   Last_Root : Natural := 0;

   Root : Glist_Store := null;

   Built : Boolean := False;

   procedure Build;
   --  Materialise Root, Roots and the per-category stores. Idempotent.

   function Category_Part (Path : String) return String;
   --  The part of Path before its last '/', or "" if it has none.

   function Title_Part (Path : String) return String;
   --  The part of Path after its last '/', or all of Path if it has none.

   -------------------
   -- Category_Part --
   -------------------

   function Category_Part (Path : String) return String is
   begin
      for I in reverse Path'Range loop
         if Path (I) = '/' then
            return Path (Path'First .. I - 1);
         end if;
      end loop;
      return "";
   end Category_Part;

   ----------------
   -- Title_Part --
   ----------------

   function Title_Part (Path : String) return String is
   begin
      for I in reverse Path'Range loop
         if Path (I) = '/' then
            return Path (I + 1 .. Path'Last);
         end if;
      end loop;
      return Path;
   end Title_Part;

   -----------
   -- Build --
   -----------

   procedure Build is
      procedure Add_Root (Name : String; Demo : Natural);
      --  Record Name as a top-level row, in alphabetical order, unless it is
      --  there already. A category and an uncategorised demo may therefore
      --  not share a name; nothing in Demos does.

      function Find_Root (Name : String) return Natural;
      --  The index in Roots of the row called Name, or 0.

      --------------
      -- Add_Root --
      --------------

      procedure Add_Root (Name : String; Demo : Natural) is
         Pos : Natural := Last_Root + 1;
      begin
         for I in 1 .. Last_Root loop
            if Roots (I).Name = Name then
               return;
            elsif Name < Roots (I).Name then
               Pos := I;
               exit;
            end if;
         end loop;

         for I in reverse Pos .. Last_Root loop
            Roots (I + 1) := Roots (I);
         end loop;

         Last_Root := Last_Root + 1;
         Roots (Pos) :=
           (Name     => To_Unbounded_String (Name),
            Item     => null,
            Children => null,
            Demo     => Demo);
      end Add_Root;

      ---------------
      -- Find_Root --
      ---------------

      function Find_Root (Name : String) return Natural is
      begin
         for I in 1 .. Last_Root loop
            if Roots (I).Name = Name then
               return I;
            end if;
         end loop;
         return 0;
      end Find_Root;

   begin
      if Built then
         return;
      end if;
      Built := True;

      --  Derive the top level from the demo paths themselves, so that adding
      --  a demo stays the one-line change it is meant to be.

      for I in Demos'Range loop
         declare
            Path     : constant String := To_String (Demos (I).Path);
            Category : constant String := Category_Part (Path);
         begin
            if Category = "" then
               Add_Root (Path, I);
            else
               Add_Root (Category, 0);
            end if;
         end;
      end loop;

      Glib.List_Store.G_New (Root, Demo_Items.Get_Type);

      for I in 1 .. Last_Root loop
         if Roots (I).Demo = 0 then
            Gtk_New (Roots (I).Item, To_String (Roots (I).Name));
            Glib.List_Store.G_New
              (Roots (I).Children, Demo_Items.Get_Type);
         else
            Gtk_New
              (Roots (I).Item,
               To_String (Roots (I).Name),
               Demos (Roots (I).Demo).Run,
               Demos (Roots (I).Demo).Help);
         end if;

         --  Append takes its own reference; the one held here is never given
         --  back, the registry living as long as the process does.
         Root.Append (Roots (I).Item);
      end loop;

      for I in Demos'Range loop
         declare
            Path     : constant String := To_String (Demos (I).Path);
            Category : constant String := Category_Part (Path);
            Index    : Natural;
            Leaf     : Demo_Item;
         begin
            if Category /= "" then
               Index := Find_Root (Category);
               Gtk_New
                 (Leaf, Title_Part (Path), Demos (I).Run, Demos (I).Help);
               if Index /= 0 and then Roots (Index).Children /= null then
                  Roots (Index).Children.Append (Leaf);
               end if;
            end if;
         end;
      end loop;
   end Build;

   ----------------
   -- Root_Model --
   ----------------

   function Root_Model return Glib.List_Model.Glist_Model is
   begin
      Build;
      return +Root;
   end Root_Model;

   -----------------
   -- Children_Of --
   -----------------

   function Children_Of
     (Item : Glib.Object.GObject) return Glib.List_Model.Glist_Model
   is
      Selected : constant Demo_Item := To_Demo_Item (Item);
   begin
      Build;

      if Selected = null then
         return Glib.List_Model.Null_Glist_Model;
      end if;

      for I in 1 .. Last_Root loop
         if Roots (I).Children /= null and then Roots (I).Item = Selected then
            --  The callback is transfer-full on its result whereas the
            --  registry keeps the store for the next expansion, so this has
            --  to be an extra reference. Getting it wrong the other way
            --  frees the store when the category is collapsed, and the next
            --  expansion reads freed memory.
            Ref (Roots (I).Children);
            return +Roots (I).Children;
         end if;
      end loop;

      --  A leaf. No model at all, as opposed to an empty one: an empty model
      --  would promise children that may yet arrive, and earn an arrow.
      return Glib.List_Model.Null_Glist_Model;
   end Children_Of;

end Demo_Registry;
