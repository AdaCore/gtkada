-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
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

with System;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Gtk.Toolbar is

   --------------------
   -- Append_Element --
   --------------------

   function Append_Element
     (Toolbar              : access Gtk_Toolbar_Record;
      The_Type             : in Gtk_Toolbar_Child_Type;
      Widget               : Gtk.Widget.Gtk_Widget := null;
      Text                 : in String := "";
      Tooltip_Text         : in String := "";
      Tooltip_Private_Text : in String := "";
      Icon                 : Gtk.Widget.Gtk_Widget := null)
      return Gtk.Widget.Gtk_Widget
   is
      function Internal
        (Toolbar              : in System.Address;
         The_Type             : in Gint;
         Widget               : in System.Address;
         Text                 : in System.Address;
         Tooltip_Text         : in System.Address;
         Tooltip_Private_Text : in System.Address;
         Icon                 : in System.Address;
         Callback             : in System.Address;
         User_Data            : in System.Address)
         return System.Address;
      pragma Import (C, Internal, "gtk_toolbar_append_element");

      Stub : Gtk.Widget.Gtk_Widget_Record;
      T    : aliased constant String := Text & ASCII.NUL;
      TA   : System.Address := T'Address;
      TT   : aliased constant String := Tooltip_Text & ASCII.NUL;
      TTA  : System.Address := TT'Address;
      TPT  : aliased constant String := Tooltip_Private_Text & ASCII.NUL;
      TPTA : System.Address := TPT'Address;
      W    : System.Address;
      I    : System.Address;

      use type Gtk.Widget.Gtk_Widget;

   begin
      if Text'Length = 0 then
         TA := System.Null_Address;
      end if;

      if Tooltip_Text'Length = 0 then
         TTA := System.Null_Address;
      end if;

      if Tooltip_Private_Text'Length = 0 then
         TPTA := System.Null_Address;
      end if;

      if Widget = null then
         W := System.Null_Address;
      else
         W := Get_Object (Widget);
      end if;

      if Icon = null then
         I := System.Null_Address;
      else
         I := Get_Object (Icon);
      end if;

      return Gtk.Widget.Gtk_Widget
        (Get_User_Data (Internal (Get_Object (Toolbar),
                                  Gtk_Toolbar_Child_Type'Pos (The_Type),
                                  W, TA, TTA, TPTA, I, System.Null_Address,
                                  System.Null_Address),
                        Stub));
   end Append_Element;

   -----------------
   -- Append_Item --
   -----------------

   function Append_Item
     (Toolbar              : access Gtk_Toolbar_Record;
      Text                 : in String := "";
      Tooltip_Text         : in String := "";
      Tooltip_Private_Text : in String := "";
      Icon                 : Gtk.Widget.Gtk_Widget := null)
      return Gtk.Button.Gtk_Button
   is
      function Internal
        (Toolbar              : in System.Address;
         Text                 : in System.Address;
         Tooltip_Text         : in System.Address;
         Tooltip_Private_Text : in System.Address;
         Icon                 : in System.Address;
         Callback             : in System.Address;
         User_Data            : in System.Address)
         return System.Address;
      pragma Import (C, Internal, "gtk_toolbar_append_item");
      Stub : Gtk.Button.Gtk_Button_Record;
      T    : aliased constant String := Text & ASCII.NUL;
      TA   : System.Address := T'Address;
      TT   : aliased constant String := Tooltip_Text & ASCII.NUL;
      TTA  : System.Address := TT'Address;
      TPT  : aliased constant String := Tooltip_Private_Text & ASCII.NUL;
      TPTA : System.Address := TPT'Address;
      I    : System.Address;

      use type Gtk.Widget.Gtk_Widget;

   begin
      if Text'Length = 0 then
         TA := System.Null_Address;
      end if;

      if Tooltip_Text'Length = 0 then
         TTA := System.Null_Address;
      end if;

      if Tooltip_Private_Text'Length = 0 then
         TPTA := System.Null_Address;
      end if;

      if Icon = null then
         I := System.Null_Address;
      else
         I := Get_Object (Icon);
      end if;

      return Gtk.Button.Gtk_Button
        (Get_User_Data (Internal (Get_Object (Toolbar),
                                  TA, TTA, TPTA, I, System.Null_Address,
                                  System.Null_Address),
                        Stub));
   end Append_Item;

   ------------------
   -- Append_Space --
   ------------------

   procedure Append_Space (Toolbar : access Gtk_Toolbar_Record) is
      procedure Internal (Toolbar : in System.Address);
      pragma Import (C, Internal, "gtk_toolbar_append_space");
   begin
      Internal (Get_Object (Toolbar));
   end Append_Space;

   -------------------
   -- Append_Widget --
   -------------------

   procedure Append_Widget
     (Toolbar              : access Gtk_Toolbar_Record;
      Widget               : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tooltip_Text         : in String := "";
      Tooltip_Private_Text : in String := "")
   is
      procedure Internal
        (Toolbar              : in System.Address;
         Widget               : in System.Address;
         Tooltip_Text         : in System.Address;
         Tooltip_Private_Text : in System.Address);
      pragma Import (C, Internal, "gtk_toolbar_append_widget");
      TT   : aliased constant String := Tooltip_Text & ASCII.NUL;
      TTA  : System.Address := TT'Address;
      TPT  : aliased constant String := Tooltip_Private_Text & ASCII.NUL;
      TPTA : System.Address := TPT'Address;

   begin
      if Tooltip_Text'Length = 0 then
         TTA := System.Null_Address;
      end if;

      if Tooltip_Private_Text'Length = 0 then
         TPTA := System.Null_Address;
      end if;

      Internal (Get_Object (Toolbar), Get_Object (Widget), TTA, TPTA);
   end Append_Widget;

   -----------------------
   -- Get_Button_Relief --
   -----------------------

   function Get_Button_Relief (Toolbar : access Gtk_Toolbar_Record)
     return Gtk_Relief_Style is
      function Internal (Toolbar : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_toolbar_get_button_relief");
   begin
      return Gtk_Relief_Style'Val (Internal (Get_Object (Toolbar)));
   end Get_Button_Relief;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget      : out Gtk_Toolbar;
      Orientation : in Gtk_Orientation;
      Style       : in Gtk_Toolbar_Style) is
   begin
      Widget := new Gtk_Toolbar_Record;
      Initialize (Widget, Orientation, Style);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget      : access Gtk_Toolbar_Record'Class;
      Orientation : in Gtk_Orientation;
      Style       : in Gtk_Toolbar_Style)
   is
      function Internal
        (Orientation : in Gint;
         Style       : in Gint)
         return System.Address;
      pragma Import (C, Internal, "gtk_toolbar_new");
   begin
      Set_Object (Widget, Internal (Gtk_Orientation'Pos (Orientation),
                                    Gtk_Toolbar_Style'Pos (Style)));
      Initialize_User_Data (Widget);
   end Initialize;

   --------------------
   -- Insert_Element --
   --------------------

   function Insert_Element
     (Toolbar              : access Gtk_Toolbar_Record;
      The_Type             : in Gtk_Toolbar_Child_Type;
      Widget               : access Gtk.Widget.Gtk_Widget_Record'Class;
      Text                 : in String := "";
      Tooltip_Text         : in String := "";
      Tooltip_Private_Text : in String := "";
      Icon                 : access Gtk.Widget.Gtk_Widget_Record'Class;
      Position             : in Gint)
      return Gtk.Widget.Gtk_Widget
   is
      function Internal
        (Toolbar              : in System.Address;
         The_Type             : in Gint;
         Widget               : in System.Address;
         Text                 : in System.Address;
         Tooltip_Text         : in System.Address;
         Tooltip_Private_Text : in System.Address;
         Icon                 : in System.Address;
         Callback             : in System.Address;
         User_Data            : in System.Address;
         Position             : in Gint)
         return System.Address;
      pragma Import (C, Internal, "gtk_toolbar_insert_element");

      Stub : Gtk.Widget.Gtk_Widget_Record;
      T    : aliased constant String := Text & ASCII.NUL;
      TA   : System.Address := T'Address;
      TT   : aliased constant String := Tooltip_Text & ASCII.NUL;
      TTA  : System.Address := TT'Address;
      TPT  : aliased constant String := Tooltip_Private_Text & ASCII.NUL;
      TPTA : System.Address := TPT'Address;

   begin
      if Text'Length = 0 then
         TA := System.Null_Address;
      end if;

      if Tooltip_Text'Length = 0 then
         TTA := System.Null_Address;
      end if;

      if Tooltip_Private_Text'Length = 0 then
         TPTA := System.Null_Address;
      end if;

      return Gtk.Widget.Gtk_Widget
        (Get_User_Data
          (Internal
            (Get_Object (Toolbar),
             Gtk_Toolbar_Child_Type'Pos (The_Type),
             Get_Object (Widget),
             TA, TTA, TPTA, Get_Object (Icon),
             System.Null_Address,
             System.Null_Address,
             Position),
           Stub));
   end Insert_Element;

   -----------------
   -- Insert_Item --
   -----------------

   function Insert_Item
     (Toolbar              : access Gtk_Toolbar_Record;
      Text                 : in String := "";
      Tooltip_Text         : in String := "";
      Tooltip_Private_Text : in String := "";
      Icon                 : access Gtk.Widget.Gtk_Widget_Record'Class;
      Position             : in Gint)
      return Gtk.Button.Gtk_Button
   is
      function Internal
        (Toolbar              : in System.Address;
         Text                 : in System.Address;
         Tooltip_Text         : in System.Address;
         Tooltip_Private_Text : in System.Address;
         Icon                 : in System.Address;
         Callback             : in System.Address;
         User_Data            : in System.Address;
         Position             : in Gint)
         return                    System.Address;
      pragma Import (C, Internal, "gtk_toolbar_insert_item");

      Stub : Gtk.Button.Gtk_Button_Record;
      T    : aliased constant String := Text & ASCII.NUL;
      TA   : System.Address := T'Address;
      TT   : aliased constant String := Tooltip_Text & ASCII.NUL;
      TTA  : System.Address := TT'Address;
      TPT  : aliased constant String := Tooltip_Private_Text & ASCII.NUL;
      TPTA : System.Address := TPT'Address;

   begin
      if Text'Length = 0 then
         TA := System.Null_Address;
      end if;

      if Tooltip_Text'Length = 0 then
         TTA := System.Null_Address;
      end if;

      if Tooltip_Private_Text'Length = 0 then
         TPTA := System.Null_Address;
      end if;

      return Gtk.Button.Gtk_Button
        (Get_User_Data
          (Internal
            (Get_Object (Toolbar),
             TA, TTA, TPTA,
             Get_Object (Icon),
             System.Null_Address, System.Null_Address,
             Position),
           Stub));
   end Insert_Item;

   ------------------
   -- Insert_Space --
   ------------------

   procedure Insert_Space
     (Toolbar  : access Gtk_Toolbar_Record;
      Position : in Gint)
   is
      procedure Internal
        (Toolbar  : in System.Address;
         Position : in Gint);
      pragma Import (C, Internal, "gtk_toolbar_insert_space");
   begin
      Internal (Get_Object (Toolbar), Position);
   end Insert_Space;

   -------------------
   -- Insert_Widget --
   -------------------

   procedure Insert_Widget
     (Toolbar              : access Gtk_Toolbar_Record;
      Widget               : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tooltip_Text         : in String := "";
      Tooltip_Private_Text : in String := "";
      Position             : in Gint)
   is
      procedure Internal
        (Toolbar              : in System.Address;
         Widget               : in System.Address;
         Tooltip_Text         : in System.Address;
         Tooltip_Private_Text : in System.Address;
         Position             : in Gint);
      pragma Import (C, Internal, "gtk_toolbar_insert_widget");

      TT   : aliased constant String := Tooltip_Text & ASCII.NUL;
      TTA  : System.Address := TT'Address;
      TPT  : aliased constant String := Tooltip_Private_Text & ASCII.NUL;
      TPTA : System.Address := TPT'Address;

   begin
      if Tooltip_Text'Length = 0 then
         TTA := System.Null_Address;
      end if;

      if Tooltip_Private_Text'Length = 0 then
         TPTA := System.Null_Address;
      end if;

      Internal
        (Get_Object (Toolbar), Get_Object (Widget), TTA, TPTA, Position);
   end Insert_Widget;

   ---------------------
   -- Prepend_Element --
   ---------------------

   function Prepend_Element
     (Toolbar              : access Gtk_Toolbar_Record;
      The_Type             : in Gtk_Toolbar_Child_Type;
      Widget               : access Gtk.Widget.Gtk_Widget_Record'Class;
      Text                 : in String := "";
      Tooltip_Text         : in String := "";
      Tooltip_Private_Text : in String := "";
      Icon                 : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Gtk.Widget.Gtk_Widget
   is
      function Internal
        (Toolbar              : in System.Address;
         The_Type             : in Gint;
         Widget               : in System.Address;
         Text                 : in System.Address;
         Tooltip_Text         : in System.Address;
         Tooltip_Private_Text : in System.Address;
         Icon                 : in System.Address;
         Callback             : in System.Address;
         User_Data            : in System.Address)
         return System.Address;
      pragma Import (C, Internal, "gtk_toolbar_prepend_element");

      Stub : Gtk.Widget.Gtk_Widget_Record;

      T    : aliased constant String := Text & ASCII.NUL;
      TA   : System.Address := T'Address;
      TT   : aliased constant String := Tooltip_Text & ASCII.NUL;
      TTA  : System.Address := TT'Address;
      TPT  : aliased constant String := Tooltip_Private_Text & ASCII.NUL;
      TPTA : System.Address := TPT'Address;

   begin
      if Text'Length = 0 then
         TA := System.Null_Address;
      end if;

      if Tooltip_Text'Length = 0 then
         TTA := System.Null_Address;
      end if;

      if Tooltip_Private_Text'Length = 0 then
         TPTA := System.Null_Address;
      end if;

      return Gtk.Widget.Gtk_Widget
        (Get_User_Data
          (Internal
            (Get_Object (Toolbar),
             Gtk_Toolbar_Child_Type'Pos (The_Type),
             Get_Object (Widget),
             TA, TTA, TPTA,
             Get_Object (Icon),
             System.Null_Address, System.Null_Address),
           Stub));
   end Prepend_Element;

   ------------------
   -- Prepend_Item --
   ------------------

   function Prepend_Item
     (Toolbar              : access Gtk_Toolbar_Record;
      Text                 : in String := "";
      Tooltip_Text         : in String := "";
      Tooltip_Private_Text : in String := "";
      Icon                 : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Gtk.Button.Gtk_Button
   is
      function Internal
        (Toolbar              : in System.Address;
         Text                 : in System.Address;
         Tooltip_Text         : in System.Address;
         Tooltip_Private_Text : in System.Address;
         Icon                 : in System.Address;
         Callback             : in System.Address;
         User_Data            : in System.Address)
         return System.Address;
      pragma Import (C, Internal, "gtk_toolbar_prepend_item");

      Stub : Gtk.Button.Gtk_Button_Record;
      T    : aliased constant String := Text & ASCII.NUL;
      TA   : System.Address := T'Address;
      TT   : aliased constant String := Tooltip_Text & ASCII.NUL;
      TTA  : System.Address := TT'Address;
      TPT  : aliased constant String := Tooltip_Private_Text & ASCII.NUL;
      TPTA : System.Address := TPT'Address;

   begin
      if Text'Length = 0 then
         TA := System.Null_Address;
      end if;

      if Tooltip_Text'Length = 0 then
         TTA := System.Null_Address;
      end if;

      if Tooltip_Private_Text'Length = 0 then
         TPTA := System.Null_Address;
      end if;

      return Gtk.Button.Gtk_Button
        (Get_User_Data
          (Internal
            (Get_Object (Toolbar),
             TA, TTA, TPTA,
             Get_Object (Icon),
             System.Null_Address, System.Null_Address),
           Stub));
   end Prepend_Item;

   -------------------
   -- Prepend_Space --
   -------------------

   procedure Prepend_Space (Toolbar : access Gtk_Toolbar_Record) is
      procedure Internal (Toolbar : in System.Address);
      pragma Import (C, Internal, "gtk_toolbar_prepend_space");
   begin
      Internal (Get_Object (Toolbar));
   end Prepend_Space;

   --------------------
   -- Prepend_Widget --
   --------------------

   procedure Prepend_Widget
     (Toolbar              : access Gtk_Toolbar_Record;
      Widget               : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tooltip_Text         : in String := "";
      Tooltip_Private_Text : in String := "")
   is
      procedure Internal
        (Toolbar              : in System.Address;
         Widget               : in System.Address;
         Tooltip_Text         : in System.Address;
         Tooltip_Private_Text : in System.Address);
      pragma Import (C, Internal, "gtk_toolbar_prepend_widget");

      TT   : aliased constant String := Tooltip_Text & ASCII.NUL;
      TTA  : System.Address := TT'Address;
      TPT  : aliased constant String := Tooltip_Private_Text & ASCII.NUL;
      TPTA : System.Address := TPT'Address;

   begin
      if Tooltip_Text'Length = 0 then
         TTA := System.Null_Address;
      end if;

      if Tooltip_Private_Text'Length = 0 then
         TPTA := System.Null_Address;
      end if;

      Internal (Get_Object (Toolbar), Get_Object (Widget), TTA, TPTA);
   end Prepend_Widget;

   -----------------------
   -- Set_Button_Relief --
   -----------------------

   procedure Set_Button_Relief (Toolbar : access Gtk_Toolbar_Record;
                                Relief  : in Gtk_Relief_Style) is
      procedure Internal (Toolbar : in System.Address;
                          Relief  : in Gint);
      pragma Import (C, Internal, "gtk_toolbar_set_button_relief");
   begin
      Internal (Get_Object (Toolbar), Gtk_Relief_Style'Pos (Relief));
   end Set_Button_Relief;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
     (Toolbar     : access Gtk_Toolbar_Record;
      Orientation : in Gtk_Orientation)
   is
      procedure Internal
        (Toolbar     : in System.Address;
         Orientation : in Gint);
      pragma Import (C, Internal, "gtk_toolbar_set_orientation");
   begin
      Internal (Get_Object (Toolbar), Gtk_Orientation'Pos (Orientation));
   end Set_Orientation;

   --------------------
   -- Set_Space_Size --
   --------------------

   procedure Set_Space_Size
     (Toolbar    : access Gtk_Toolbar_Record;
      Space_Size : in Gint)
   is
      procedure Internal
        (Toolbar    : in System.Address;
         Space_Size : in Gint);
      pragma Import (C, Internal, "gtk_toolbar_set_space_size");
   begin
      Internal (Get_Object (Toolbar), Space_Size);
   end Set_Space_Size;

   ---------------------
   -- Set_Space_Style --
   ---------------------

   procedure Set_Space_Style (Toolbar : access Gtk_Toolbar_Record;
                              Style   : in Gtk_Toolbar_Space_Style) is
      procedure Internal (Toolbar : in System.Address;
                          Style   : in Gint);
      pragma Import (C, Internal, "gtk_toolbar_set_space_style");
   begin
      Internal (Get_Object (Toolbar), Gtk_Toolbar_Space_Style'Pos (Style));
   end Set_Space_Style;

   ---------------
   -- Set_Style --
   ---------------

   procedure Set_Style
     (Toolbar : access Gtk_Toolbar_Record;
      Style   : in Gtk_Toolbar_Style)
   is
      procedure Internal
        (Toolbar : in System.Address;
         Style   : in Gint);
      pragma Import (C, Internal, "gtk_toolbar_set_style");
   begin
      Internal (Get_Object (Toolbar), Gtk_Toolbar_Style'Pos (Style));
   end Set_Style;

   ------------------
   -- Set_Tooltips --
   ------------------

   procedure Set_Tooltips
     (Toolbar : access Gtk_Toolbar_Record;
      Enable  : in Boolean)
   is
      procedure Internal
        (Toolbar : in System.Address;
         Enable  : in Gint);
      pragma Import (C, Internal, "gtk_toolbar_set_tooltips");
   begin
      Internal (Get_Object (Toolbar), Boolean'Pos (Enable));
   end Set_Tooltips;

   --------------
   -- Generate --
   --------------

   Widget_Class : aliased String := "GtkWidget";

   procedure Generate (N : in Node_Ptr; File : in File_Type) is
      P, Child   : Node_Ptr;
      Top_Widget : Node_Ptr := Find_Top_Widget (N);
      Top  : constant String_Ptr := Get_Field (Top_Widget, "name");
      Cur  : constant String_Ptr := Get_Field (N, "name");
      S, T : String_Ptr;
      Id   : constant Gtk_Type := Get_Type;
      pragma Warnings (Off, Id);

   begin
      Gen_New (N, "Toolbar", Get_Field (N, "orientation").all,
        Get_Field (N, "type").all, File => File);
      Container.Generate (N, File);
      Gen_Set (N, "Toolbar", "space_size", File);
      Gen_Set (N, "Toolbar", "space_style", File);
      Gen_Set (N, "Toolbar", "tooltips", File);
      Gen_Set (N, "Toolbar", "Button_Relief", "relief", File);

      --  Now look for widgets that should be added to this toolbar

      P := N.Child;

      while P /= null loop
         if P.Tag.all = "widget" then
            S := Get_Field (P, "class");

            if S.all = "GtkButton" or else S.all = "GtkToggleButton"
              or else S.all = "GtkRadioButton"
            then
               Child := Find_Child (P, "child");

               if Child /= null then
                  T := Get_Field (Child, "new_group");
               else
                  T := null;
               end if;

               if T /= null and then T.all = "True" then
                  Put_Line (File, "   Append_Space (" & To_Ada (Top.all) &
                    "." & To_Ada (Cur.all) & ");");
               end if;

               Put_Line (File, "   " & To_Ada (Top.all) & "." &
                 To_Ada (Get_Field (P, "name").all) &
                 " := Append_Element");
               Put (File, "     (Toolbar => ");

               if Top /= Cur then
                  Put (File, To_Ada (Top.all) & ".");
               end if;

               Put_Line (File, To_Ada (Cur.all) & ",");
               Put (File, "      The_Type => Toolbar_Child_" &
                 S (S'First + 3 .. S'Last));
               S := Get_Field (P, "label");

               if S /= null then
                  Put_Line (File, ",");

                  if Gettext_Support (Top_Widget) then
                     Put (File, "      Text => -""" & S.all & '"');
                  else
                     Put (File, "      Text => """ & S.all & '"');
                  end if;
               end if;

               S := Get_Field (P, "tooltip");

               if S /= null then
                  Put_Line (File, ",");

                  if Gettext_Support (Top_Widget) then
                     Put (File, "      Tooltip_Text => -""" & S.all & '"');
                  else
                     Put (File, "      Tooltip_Text => """ & S.all & '"');
                  end if;
               end if;

               T := Get_Field (P, "icon");

               if T /= null then
                  Add_Package ("Pixmap");
                  Put_Line (File, ",");
                  Put (File, "      Icon => Gtk_Widget (Create_Pixmap (");

                  if Index (T.all, ".") /= 0 then
                     Put (File, '"' & T.all & '"');
                  else
                     Put (File, T.all);
                  end if;

                  Put_Line (File, ", " & To_Ada (Top.all) & ")));");
               else
                  Put_Line (File, ");");
               end if;

               Add_Package ("Widget");
               Gen_Signal (P, File, Widget_Class'Access);

               P.Specific_Data.Created := True;
               P.Specific_Data.Initialized := True;
               P.Specific_Data.Has_Container := True;
            end if;
         end if;

         P := P.Next;
      end loop;
   end Generate;

end Gtk.Toolbar;
