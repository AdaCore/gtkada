-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU Library General Public       --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- Library General Public License for more details.                  --
--                                                                   --
-- You should have received a copy of the GNU Library General Public --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
-----------------------------------------------------------------------

with Gdk.Bitmap;
with Gdk.Color;
with Gdk.Pixmap;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Widget;
with Interfaces.C.Strings;
with Unchecked_Deallocation;
with Unchecked_Conversion;
with System;
with Gdk; use Gdk;

package body Gtk.CList is

   ------------
   -- Append --
   ------------

   function Append
      (Clist : in Gtk_CList'Class;
       Text  : in Line_Data)
       return     Gint
   is
      function Internal
         (Clist : in System.Address;
          Text  : in System.Address)
          return      Gint;
      pragma Import (C, Internal, "gtk_clist_append");
   begin
      return Internal (Get_Object (Clist),
                       Text'Address);
   end Append;

   -----------
   -- Clear --
   -----------

   procedure Clear (Clist : in Gtk_CList'Class)
   is
      procedure Internal (Clist : in System.Address);
      pragma Import (C, Internal, "gtk_clist_clear");
   begin
      Internal (Get_Object (Clist));
   end Clear;

   -------------------------
   -- Column_Title_Active --
   -------------------------

   procedure Column_Title_Active
      (Clist  : in Gtk_CList'Class;
       Column : in Gint)
   is
      procedure Internal
         (Clist  : in System.Address;
          Column : in Gint);
      pragma Import (C, Internal, "gtk_clist_column_title_active");
   begin
      Internal (Get_Object (Clist),
                Column);
   end Column_Title_Active;

   --------------------------
   -- Column_Title_Passive --
   --------------------------

   procedure Column_Title_Passive
      (Clist  : in Gtk_CList'Class;
       Column : in Gint)
   is
      procedure Internal
         (Clist  : in System.Address;
          Column : in Gint);
      pragma Import (C, Internal, "gtk_clist_column_title_passive");
   begin
      Internal (Get_Object (Clist),
                Column);
   end Column_Title_Passive;

   --------------------------
   -- Column_Titles_Active --
   --------------------------

   procedure Column_Titles_Active (Clist : in Gtk_CList'Class)
   is
      procedure Internal (Clist : in System.Address);
      pragma Import (C, Internal, "gtk_clist_column_titles_active");
   begin
      Internal (Get_Object (Clist));
   end Column_Titles_Active;

   ------------------------
   -- Column_Titles_Hide --
   ------------------------

   procedure Column_Titles_Hide (Clist : in Gtk_CList'Class)
   is
      procedure Internal (Clist : in System.Address);
      pragma Import (C, Internal, "gtk_clist_column_titles_hide");
   begin
      Internal (Get_Object (Clist));
   end Column_Titles_Hide;

   ---------------------------
   -- Column_Titles_Passive --
   ---------------------------

   procedure Column_Titles_Passive (Clist : in Gtk_CList'Class)
   is
      procedure Internal (Clist : in System.Address);
      pragma Import (C, Internal, "gtk_clist_column_titles_passive");
   begin
      Internal (Get_Object (Clist));
   end Column_Titles_Passive;

   ------------------------
   -- Column_Titles_Show --
   ------------------------

   procedure Column_Titles_Show (Clist : in Gtk_CList'Class)
   is
      procedure Internal (Clist : in System.Address);
      pragma Import (C, Internal, "gtk_clist_column_titles_show");
   begin
      Internal (Get_Object (Clist));
   end Column_Titles_Show;

   --------------------
   -- Free_Line_Data --
   --------------------

   procedure Free_Line_Data (Data : in out Line_Data) is
   begin
      for J in Data'Range loop
         Interfaces.C.Strings.Free (Data (J));
      end loop;
   end Free_Line_Data;

   ------------
   -- Freeze --
   ------------

   procedure Freeze (Clist : in Gtk_CList'Class)
   is
      procedure Internal (Clist : in System.Address);
      pragma Import (C, Internal, "gtk_clist_freeze");
   begin
      Internal (Get_Object (Clist));
   end Freeze;

   -------------------
   -- Get_Cell_Type --
   -------------------

   function Get_Cell_Type
      (Clist  : in Gtk_CList'Class;
       Row    : in Gint;
       Column : in Gint)
       return      Gtk_Cell_Type
   is
      function Internal
         (Clist  : in System.Address;
          Row    : in Gint;
          Column : in Gint)
          return      Gint;
      pragma Import (C, Internal, "gtk_clist_get_cell_type");
   begin
      return Gtk_Cell_Type'Val (Internal (Get_Object (Clist),
                                          Row,
                                          Column));
   end Get_Cell_Type;

   ----------------------
   -- Get_Clist_Window --
   ----------------------

   function Get_Clist_Window (Clist : in Gtk_CList'Class)
                              return     Gdk.Window.Gdk_Window is
      function Internal (Clist : in System.Address) return System.Address;
      pragma Import (C, Internal, "ada_clist_get_clist_window");
      Window : Gdk.Window.Gdk_Window;
   begin
      Gdk.Set_Object (Window, Internal (Get_Object (Clist)));
      return Window;
   end Get_Clist_Window;

   -----------------------
   -- Get_Column_Button --
   -----------------------

   function Get_Column_Button (Clist  : in Gtk_CList'Class;
                               Column : in Gint)
                               return Gtk.Button.Gtk_Button
   is
      function Internal (Clist  : in System.Address;
                         Column : in Gint)
                         return System.Address;
      pragma Import (C, Internal, "ada_clist_get_column_button");
      Button : Gtk.Button.Gtk_Button;
   begin
      Set_Object (Button, Internal (Get_Object (Clist), Column));
      return Button;
   end Get_Column_Button;

   ----------------
   -- Get_Pixmap --
   ----------------

   procedure Get_Pixmap
      (Clist    : in Gtk_CList'Class;
       Row      : in Gint;
       Column   : in Gint;
       Pixmap   : out Gdk.Pixmap.Gdk_Pixmap'Class;
       Mask     : out Gdk.Bitmap.Gdk_Bitmap'Class;
       Is_Valid : out Boolean)
   is
      function Internal
         (Clist  : in System.Address;
          Row    : in Gint;
          Column : in Gint;
          Pixmap : in System.Address;
          Mask   : in System.Address)
          return      Gint;
      pragma Import (C, Internal, "gtk_clist_get_pixmap");
      Pix : System.Address;
      Msk : System.Address;
   begin
      Is_Valid := Boolean'Val (Internal (Get_Object (Clist),
                                         Row,
                                         Column,
                                         Pix'Address,
                                         Msk'Address));
      Gdk.Set_Object (Pixmap, Pix);
      Gdk.Set_Object (Mask, Msk);
   end Get_Pixmap;

   -----------------
   -- Get_Pixtext --
   -----------------

   procedure Get_Pixtext
      (Clist    : in Gtk_CList'Class;
       Row      : in Gint;
       Column   : in Gint;
       Text     : in out String;
       Spacing  : in out Guint8;
       Pixmap   : in out Gdk.Pixmap.Gdk_Pixmap'Class;
       Mask     : in out Gdk.Bitmap.Gdk_Bitmap'Class;
       Is_Valid : out Boolean)
   is
      function Internal
         (Clist   : in System.Address;
          Row     : in Gint;
          Column  : in Gint;
          Text    : in System.Address;
          Spacing : in System.Address;
          Pixmap  : in System.Address;
          Mask    : in System.Address)
          return       Gint;
      pragma Import (C, Internal, "gtk_clist_get_pixtext");

      S    : Interfaces.C.Strings.chars_ptr;
      Pix  : System.Address;
      Msk : System.Address;
   begin
      Is_Valid := Boolean'Val (Internal (Get_Object (Clist),
                                         Row,
                                         Column,
                                         S'Address,
                                         Spacing'Address,
                                         Pix'Address,
                                         Msk'Address));
      Gdk.Set_Object (Pixmap, Pix);
      Gdk.Set_Object (Mask, Msk);
      Text := Interfaces.C.Strings.Value (S);
   end Get_Pixtext;

   -------------------
   -- Get_Selection --
   -------------------

   function Get_Selection (Widget : in Gtk_CList'Class)
                           return      Gint_List.Glist
   is
      function Internal (Widget : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "ada_clist_get_selection");
      List : Gint_List.Glist;
   begin
      Gint_List.Set_Object (List, Internal (Get_Object (Widget)));
      return List;
   end Get_Selection;

   ------------------------
   -- Get_Selection_Info --
   ------------------------

   procedure Get_Selection_Info
     (Clist   : in Gtk_CList'Class;
      X       : in Gint;
      Y       : in Gint;
      Row     : out Gint;
      Column  : out Gint;
     Is_Valid : out Boolean)
   is
      function Internal
         (Clist  : in System.Address;
          X      : in Gint;
          Y      : in Gint;
          Row    : in System.Address;
          Column : in System.Address)
          return      Gint;
      pragma Import (C, Internal, "gtk_clist_get_selection_info");
   begin
      Is_Valid := Boolean'Val (Internal (Get_Object (Clist),
                                         X,
                                         Y,
                                         Row'Address,
                                         Column'Address));
   end Get_Selection_Info;

   --------------
   -- Get_Text --
   --------------

   procedure Get_Text
      (Clist    : in Gtk_CList'Class;
       Row      : in Gint;
       Column   : in Gint;
       Text     : out String;
       Is_Valid : out Boolean)
   is
      function Internal
         (Clist  : in System.Address;
          Row    : in Gint;
          Column : in Gint;
          Text   : in System.Address)
          return      Gint;
      pragma Import (C, Internal, "gtk_clist_get_text");
      S : Interfaces.C.Strings.chars_ptr;
   begin
      Is_Valid := Boolean'Val (Internal (Get_Object (Clist),
                                         Row,
                                         Column,
                                         S'Address));
      Text := Interfaces.C.Strings.Value (S);
   end Get_Text;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget  : out Gtk_CList;
                      Columns : in Gint)
   is
      function Internal (Columns : in Gint)
                         return       System.Address;
      pragma Import (C, Internal, "gtk_clist_new");
   begin
      Set_Object (Widget, Internal (Columns));
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Widget  : out Gtk_CList;
       Columns : in Gint;
       Titles  : in Line_Data)
   is
      function Internal
         (Columns : in Gint;
          Titles  : in System.Address)
          return       System.Address;
      pragma Import (C, Internal, "gtk_clist_new_with_titles");
   begin
      Set_Object (Widget, Internal (Columns, Titles'Address));
   end Gtk_New;

   ------------
   -- Insert --
   ------------

   procedure Insert
      (Clist : in Gtk_CList'Class;
       Row   : in Gint;
       Text  : in Line_Data)
   is
      procedure Internal
         (Clist : in System.Address;
          Row   : in Gint;
          Text  : in System.Address);
      pragma Import (C, Internal, "gtk_clist_insert");
   begin
      Internal (Get_Object (Clist),
                Row, Text'Address);
   end Insert;

   ------------
   -- Moveto --
   ------------

   procedure Moveto
      (Clist     : in Gtk_CList'Class;
       Row       : in Gint;
       Column    : in Gint;
       Row_Align : in Gfloat;
       Col_Align : in Gfloat)
   is
      procedure Internal
         (Clist     : in System.Address;
          Row       : in Gint;
          Column    : in Gint;
          Row_Align : in Gfloat;
          Col_Align : in Gfloat);
      pragma Import (C, Internal, "gtk_clist_moveto");
   begin
      Internal (Get_Object (Clist),
                Row,
                Column,
                Row_Align,
                Col_Align);
   end Moveto;

   ------------
   -- Remove --
   ------------

   procedure Remove
      (Clist : in Gtk_CList'Class;
       Row   : in Gint)
   is
      procedure Internal
         (Clist : in System.Address;
          Row   : in Gint);
      pragma Import (C, Internal, "gtk_clist_remove");
   begin
      Internal (Get_Object (Clist),
                Row);
   end Remove;

   --------------------
   -- Row_Is_Visible --
   --------------------

   function Row_Is_Visible
      (Clist  : in Gtk_CList'Class;
       Row    : in Gint)
       return      Gtk_Visibility
   is
      function Internal
         (Clist  : in System.Address;
          Row    : in Gint)
          return      Gint;
      pragma Import (C, Internal, "gtk_clist_row_is_visible");
   begin
      return Gtk_Visibility'Val (Internal (Get_Object (Clist),
                                           Row));
   end Row_Is_Visible;

   ----------------
   -- Select_Row --
   ----------------

   procedure Select_Row
      (Clist  : in Gtk_CList'Class;
       Row    : in Gint;
       Column : in Gint)
   is
      procedure Internal
         (Clist  : in System.Address;
          Row    : in Gint;
          Column : in Gint);
      pragma Import (C, Internal, "gtk_clist_select_row");
   begin
      Internal (Get_Object (Clist),
                Row,
                Column);
   end Select_Row;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background
      (Clist : in Gtk_CList'Class;
       Row   : in Gint;
       Color : in Gdk.Color.Gdk_Color'Class)
   is
      procedure Internal
         (Clist : in System.Address;
          Row   : in Gint;
          Color : in System.Address);
      pragma Import (C, Internal, "gtk_clist_set_background");
   begin
      Internal (Get_Object (Clist),
                Row,
                Gdk.Get_Object (Color));
   end Set_Background;

   ----------------
   -- Set_Border --
   ----------------

   procedure Set_Border
      (Clist  : in Gtk_CList'Class;
       Border : in Gtk_Shadow_Type)
   is
      procedure Internal
         (Clist  : in System.Address;
          Border : in Gint);
      pragma Import (C, Internal, "gtk_clist_set_border");
   begin
      Internal (Get_Object (Clist),
                Gtk_Shadow_Type'Pos (Border));
   end Set_Border;

   ------------------------------
   -- Set_Column_Justification --
   ------------------------------

   procedure Set_Column_Justification
      (Clist         : in Gtk_CList'Class;
       Column        : in Gint;
       Justification : in Gtk_Justification)
   is
      procedure Internal
         (Clist         : in System.Address;
          Column        : in Gint;
          Justification : in Gint);
      pragma Import (C, Internal, "gtk_clist_set_column_justification");
   begin
      Internal (Get_Object (Clist),
                Column,
                Gtk_Justification'Pos (Justification));
   end Set_Column_Justification;

   ----------------------
   -- Set_Column_Title --
   ----------------------

   procedure Set_Column_Title
      (Clist  : in Gtk_CList'Class;
       Column : in Gint;
       Title  : in String)
   is
      procedure Internal
         (Clist  : in System.Address;
          Column : in Gint;
          Title  : in String);
      pragma Import (C, Internal, "gtk_clist_set_column_title");
   begin
      Internal (Get_Object (Clist),
                Column,
                Title & Ascii.NUL);
   end Set_Column_Title;

   -----------------------
   -- Set_Column_Widget --
   -----------------------

   procedure Set_Column_Widget
      (Clist  : in Gtk_CList'Class;
       Column : in Gint;
       Widget : in Gtk.Widget.Gtk_Widget'Class)
   is
      procedure Internal
         (Clist  : in System.Address;
          Column : in Gint;
          Widget : in System.Address);
      pragma Import (C, Internal, "gtk_clist_set_column_widget");
   begin
      Internal (Get_Object (Clist),
                Column,
                Get_Object (Widget));
   end Set_Column_Widget;

   ----------------------
   -- Set_Column_Width --
   ----------------------

   procedure Set_Column_Width
      (Clist  : in Gtk_CList'Class;
       Column : in Gint;
       Width  : in Gint)
   is
      procedure Internal
         (Clist  : in System.Address;
          Column : in Gint;
          Width  : in Gint);
      pragma Import (C, Internal, "gtk_clist_set_column_width");
   begin
      Internal (Get_Object (Clist),
                Column,
                Width);
   end Set_Column_Width;

   --------------------
   -- Set_Foreground --
   --------------------

   procedure Set_Foreground
      (Clist : in Gtk_CList'Class;
       Row   : in Gint;
       Color : in Gdk.Color.Gdk_Color'Class)
   is
      procedure Internal
         (Clist : in System.Address;
          Row   : in Gint;
          Color : in System.Address);
      pragma Import (C, Internal, "gtk_clist_set_foreground");
   begin
      Internal (Get_Object (Clist),
                Row,
                Gdk.Get_Object (Color));
   end Set_Foreground;

   ----------------
   -- Set_Pixmap --
   ----------------

   procedure Set_Pixmap
      (Clist  : in Gtk_CList'Class;
       Row    : in Gint;
       Column : in Gint;
       Pixmap : in Gdk.Pixmap.Gdk_Pixmap'Class;
       Mask   : in Gdk.Bitmap.Gdk_Bitmap'Class)
   is
      procedure Internal
         (Clist  : in System.Address;
          Row    : in Gint;
          Column : in Gint;
          Pixmap : in System.Address;
          Mask   : in System.Address);
      pragma Import (C, Internal, "gtk_clist_set_pixmap");
   begin
      Internal (Get_Object (Clist),
                Row,
                Column,
                Gdk.Get_Object (Pixmap),
                Gdk.Get_Object (Mask));
   end Set_Pixmap;

   -----------------
   -- Set_Pixtext --
   -----------------

   procedure Set_Pixtext
      (Clist   : in Gtk_CList'Class;
       Row     : in Gint;
       Column  : in Gint;
       Text    : in String;
       Spacing : in Guint8;
       Pixmap  : in Gdk.Pixmap.Gdk_Pixmap'Class;
       Mask    : in Gdk.Bitmap.Gdk_Bitmap'Class)
   is
      procedure Internal
         (Clist   : in System.Address;
          Row     : in Gint;
          Column  : in Gint;
          Text    : in String;
          Spacing : in Guint8;
          Pixmap  : in System.Address;
          Mask    : in System.Address);
      pragma Import (C, Internal, "gtk_clist_set_pixtext");
   begin
      Internal (Get_Object (Clist),
                Row,
                Column,
                Text & Ascii.NUL,
                Spacing,
                Gdk.Get_Object (Pixmap),
                Gdk.Get_Object (Mask));
   end Set_Pixtext;

   ----------------
   -- Set_Policy --
   ----------------

   procedure Set_Policy
      (Clist             : in Gtk_CList'Class;
       Vscrollbar_Policy : in Gtk_Policy_Type;
       Hscrollbar_Policy : in Gtk_Policy_Type)
   is
      procedure Internal
         (Clist             : in System.Address;
          Vscrollbar_Policy : in Gint;
          Hscrollbar_Policy : in Gint);
      pragma Import (C, Internal, "gtk_clist_set_policy");
   begin
      Internal (Get_Object (Clist),
                Gtk_Policy_Type'Pos (Vscrollbar_Policy),
                Gtk_Policy_Type'Pos (Hscrollbar_Policy));
   end Set_Policy;

   --------------------
   -- Set_Row_Height --
   --------------------

   procedure Set_Row_Height
      (Clist  : in Gtk_CList'Class;
       Height : in Gint)
   is
      procedure Internal
         (Clist  : in System.Address;
          Height : in Gint);
      pragma Import (C, Internal, "gtk_clist_set_row_height");
   begin
      Internal (Get_Object (Clist),
                Height);
   end Set_Row_Height;

   ------------------------
   -- Set_Selection_Mode --
   ------------------------

   procedure Set_Selection_Mode
      (Clist : in Gtk_CList'Class;
       Mode  : in Gtk_Selection_Mode)
   is
      procedure Internal
         (Clist : in System.Address;
          Mode  : in Gint);
      pragma Import (C, Internal, "gtk_clist_set_selection_mode");
   begin
      Internal (Get_Object (Clist),
                Gtk_Selection_Mode'Pos (Mode));
   end Set_Selection_Mode;

   ---------------
   -- Set_Shift --
   ---------------

   procedure Set_Shift
      (Clist      : in Gtk_CList'Class;
       Row        : in Gint;
       Column     : in Gint;
       Vertical   : in Gint;
       Horizontal : in Gint)
   is
      procedure Internal
         (Clist      : in System.Address;
          Row        : in Gint;
          Column     : in Gint;
          Vertical   : in Gint;
          Horizontal : in Gint);
      pragma Import (C, Internal, "gtk_clist_set_shift");
   begin
      Internal (Get_Object (Clist),
                Row,
                Column,
                Vertical,
                Horizontal);
   end Set_Shift;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
      (Clist  : in Gtk_CList'Class;
       Row    : in Gint;
       Column : in Gint;
       Text   : in String)
   is
      procedure Internal
         (Clist  : in System.Address;
          Row    : in Gint;
          Column : in Gint;
          Text   : in String);
      pragma Import (C, Internal, "gtk_clist_set_text");
   begin
      Internal (Get_Object (Clist),
                Row,
                Column,
                Text & Ascii.NUL);
   end Set_Text;

   ----------
   -- Thaw --
   ----------

   procedure Thaw (Clist : in Gtk_CList'Class)
   is
      procedure Internal (Clist : in System.Address);
      pragma Import (C, Internal, "gtk_clist_thaw");
   begin
      Internal (Get_Object (Clist));
   end Thaw;

   ------------------
   -- Unselect_Row --
   ------------------

   procedure Unselect_Row
      (Clist  : in Gtk_CList'Class;
       Row    : in Gint;
       Column : in Gint)
   is
      procedure Internal
         (Clist  : in System.Address;
          Row    : in Gint;
          Column : in Gint);
      pragma Import (C, Internal, "gtk_clist_unselect_row");
   begin
      Internal (Get_Object (Clist),
                Row,
                Column);
   end Unselect_Row;



   ---------------
   -- Row_Data --
   ---------------

   package body Row_Data is

      type Data_Access is access all Data_Type;
      type Cb_Record is
         record
            Ptr : Data_Access;
         end record;
      type Cb_Record_Access is access all Cb_Record;

      function Convert is new Unchecked_Conversion (System.Address,
                                                    Cb_Record_Access);
      procedure Free (Data : in System.Address);
      pragma Convention (C, Free);

      ----------
      -- Free --
      ----------

      procedure Free (Data : in System.Address) is
         procedure Internal is new Unchecked_Deallocation (Cb_Record,
                                                           Cb_Record_Access);
         procedure Internal2 is new Unchecked_Deallocation (Data_Type,
                                                            Data_Access);
         D : Cb_Record_Access := Convert (Data);
      begin
         Internal2 (D.Ptr);
         Internal (D);
      end Free;

      ---------
      -- Get --
      ---------

      function Get (Object : in Gtk_CList'Class;
                    Row    : in Gint)
                    return Data_Type
      is
         function Internal (Object : in System.Address;
                            Row    : in Gint)
                            return System.Address;
         pragma Import (C, Internal, "gtk_clist_get_row_data");
         D : Cb_Record_Access
           := Convert (Internal (Get_Object (Object), Row));
      begin
         if D = null then
            raise Constraint_Error;
         end if;
         return D.Ptr.all;
      end Get;

      ---------
      -- Set --
      ---------

      procedure Set (Object : in Gtk_CList'Class;
                     Row    : in Gint;
                     Data   : in Data_Type)
      is
         function Convert is new Unchecked_Conversion (Cb_Record_Access,
                                                       System.Address);
         procedure Internal (Object  : in System.Address;
                             Row     : in Gint;
                             Data    : in System.Address;
                             Destroy : in System.Address);
         pragma Import (C, Internal, "gtk_clist_set_row_data_full");
         D : Cb_Record_Access := new Cb_Record'(Ptr => new Data_Type'(Data));
      begin
         Internal (Get_Object (Object),
                   Row,
                   Convert (D),
                   Free'Address);
      end Set;

   end Row_Data;


end Gtk.CList;


