-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001                            --
--                         ACT-Europe                                --
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

with Glib;
with Gdk.Rectangle;
with Gdk.Window;
with Gtk.Container;
with Gtk.Enums;
with Gtk.Text_Buffer;
with Gtk.Text_Child;
with Gtk.Text_Iter;
with Gtk.Text_Mark;
with Gtk.Widget;

package Gtk.Text_View is

   type Gtk_Text_View_Record is
     new Gtk.Container.Gtk_Container_Record with private;
   type Gtk_Text_View is access all Gtk_Text_View_Record'Class;

   procedure Gtk_New
     (Widget : out Gtk_Text_View;
      Buffer : access Gtk.Text_Buffer.Gtk_Text_Buffer_Record'Class :=
                        Gtk.Text_Buffer.Gtk_Text_Buffer'(null));

   procedure Initialize
     (Widget : access Gtk_Text_View_Record'Class;
      Buffer : access Gtk.Text_Buffer.Gtk_Text_Buffer_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with this widget.

   procedure Set_Buffer
     (Text_View : access Gtk_Text_View_Record;
      Buffer    : access Gtk.Text_Buffer.Gtk_Text_Buffer_Record'Class);

   function Get_Buffer (Text_View : access Gtk_Text_View_Record)
                        return Gtk.Text_Buffer.Gtk_Text_Buffer;

   function Scroll_To_Iter
     (Text_View     : access Gtk_Text_View_Record;
      Iter          : Gtk.Text_Iter.Gtk_Text_Iter;
      Within_Margin : Gdouble;
      Use_Align     : Boolean;
      Xalign        : Gdouble;
      Yalign        : Gdouble)
      return Boolean;
   --  ??? XXX The Iterator should be a const in the gtk+ sources.

   procedure Scroll_To_Mark
     (Text_View     : access Gtk_Text_View_Record;
      Mark          : access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class;
      Within_Margin : Gdouble;
      Use_Align     : Boolean;
      Xalign        : Gdouble;
      Yalign        : Gdouble);

   procedure Scroll_Mark_Onscreen
     (Text_View : access Gtk_Text_View_Record;
      Mark      : access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class);

   function Move_Mark_Onscreen
     (Text_View : access Gtk_Text_View_Record;
      Mark      : access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class)
      return Boolean;

   function Place_Cursor_Onscreen (Text_View : access Gtk_Text_View_Record)
                                   return Boolean;

   procedure Get_Visible_Rect
     (Text_View    : access Gtk_Text_View_Record;
      Visible_Rect : out Gdk.Rectangle.Gdk_Rectangle);

   procedure Set_Cursor_Visible
     (Text_View : access Gtk_Text_View_Record;
      Setting   : Boolean := True);

   function Get_Cursor_Visible (Text_View : access Gtk_Text_View_Record)
                                return Boolean;

   procedure Get_Iter_Location
     (Text_View : access Gtk_Text_View_Record;
      Iter      : Gtk.Text_Iter.Gtk_Text_Iter;
      Location  : out Gdk.Rectangle.Gdk_Rectangle);

   procedure Get_Iter_At_Location
     (Text_View : access Gtk_Text_View_Record;
      Iter      : out Gtk.Text_Iter.Gtk_Text_Iter;
      X         : Gint;
      Y         : Gint);

   procedure Get_Line_Yrange
     (Text_View : access Gtk_Text_View_Record;
      Iter      : Gtk.Text_Iter.Gtk_Text_Iter;
      Y         : out Gint;
      Height    : out Gint);

   procedure Get_Line_At_Y
     (Text_View   : access Gtk_Text_View_Record;
      Target_Iter : out Gtk.Text_Iter.Gtk_Text_Iter;
      Y           : Gint;
      Line_Top    : out Gint);

   procedure Buffer_To_Window_Coords
     (Text_View : access Gtk_Text_View_Record;
      Win       : Gtk.Enums.Gtk_Text_Window_Type;
      Buffer_X  : Gint;
      Buffer_Y  : Gint;
      Window_X  : out Gint;
      Window_Y  : out Gint);

   procedure Window_To_Buffer_Coords
     (Text_View : access Gtk_Text_View_Record;
      Win       : Gtk.Enums.Gtk_Text_Window_Type;
      Window_X  : Gint;
      Window_Y  : Gint;
      Buffer_X  : out Gint;
      Buffer_Y  : out Gint);

   function Get_Window
     (Text_View : access Gtk_Text_View_Record;
      Win       : Gtk.Enums.Gtk_Text_Window_Type)
      return Gdk.Window.Gdk_Window;

   function Get_Window_Type
     (Text_View : access Gtk_Text_View_Record;
      Window    : Gdk.Window.Gdk_Window)
      return Gtk.Enums.Gtk_Text_Window_Type;

   procedure Set_Border_Window_Size
     (Text_View : access Gtk_Text_View_Record;
      The_Type  : Gtk.Enums.Gtk_Text_Window_Type;
      Size      : Gint);

   procedure Set_Text_Window_Size
     (Text_View : access Gtk_Text_View_Record;
      Width     : Gint;
      Height    : Gint);

   procedure Forward_Display_Line
     (Text_View : access Gtk_Text_View_Record;
      Iter      : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Result    : out    Boolean);

   procedure Backward_Display_Line
     (Text_View : access Gtk_Text_View_Record;
      Iter      : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Result    : out    Boolean);

   procedure Forward_Display_Line_End
     (Text_View : access Gtk_Text_View_Record;
      Iter      : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Result    : out    Boolean);

   procedure Backward_Display_Line_Start
     (Text_View : access Gtk_Text_View_Record;
      Iter      : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Result    : out    Boolean);

   function Starts_Display_Line
     (Text_View : access Gtk_Text_View_Record;
      Iter      : Gtk.Text_Iter.Gtk_Text_Iter)
      return Boolean;

   procedure Move_Visually
     (Text_View : access Gtk_Text_View_Record;
      Iter      : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Count     : Gint;
      Result    : out Boolean);

   procedure Add_Child_At_Anchor
     (Text_View : access Gtk_Text_View_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Anchor    : access Gtk.Text_Child.Gtk_Text_Child_Anchor_Record'Class);

   procedure Add_Child_In_Window
     (Text_View    : access Gtk_Text_View_Record;
      Child        : access Gtk.Widget.Gtk_Widget_Record'Class;
      Which_Window : Gtk.Enums.Gtk_Text_Window_Type;
      Xpos         : Gint;
      Ypos         : Gint);

   procedure Move_Child
     (Text_View : access Gtk_Text_View_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Xpos      : Gint;
      Ypos      : Gint);

   procedure Set_Wrap_Mode
     (Text_View : access Gtk_Text_View_Record;
      Wrap_Mode : Gtk.Enums.Gtk_Wrap_Mode);

   function Get_Wrap_Mode (Text_View : access Gtk_Text_View_Record)
                           return Gtk.Enums.Gtk_Wrap_Mode;

   procedure Set_Editable
     (Text_View : access Gtk_Text_View_Record;
      Setting   : Boolean := True);

   function Get_Editable (Text_View : access Gtk_Text_View_Record)
                          return Boolean;

   procedure Set_Pixels_Above_Lines
     (Text_View          : access Gtk_Text_View_Record;
      Pixels_Above_Lines : Gint);

   function Get_Pixels_Above_Lines (Text_View : access Gtk_Text_View_Record)
                                    return Gint;

   procedure Set_Pixels_Below_Lines
     (Text_View          : access Gtk_Text_View_Record;
      Pixels_Below_Lines : Gint);

   function Get_Pixels_Below_Lines (Text_View : access Gtk_Text_View_Record)
                                    return Gint;

   procedure Set_Pixels_Inside_Wrap
     (Text_View          : access Gtk_Text_View_Record;
      Pixels_Inside_Wrap : Gint);

   function Get_Pixels_Inside_Wrap (Text_View : access Gtk_Text_View_Record)
                                    return Gint;

   procedure Set_Justification
     (Text_View     : access Gtk_Text_View_Record;
      Justification : Gtk.Enums.Gtk_Justification);

   function Get_Justification (Text_View : access Gtk_Text_View_Record)
                               return Gtk.Enums.Gtk_Justification;

   procedure Set_Left_Margin
     (Text_View   : access Gtk_Text_View_Record;
      Left_Margin : Gint);

   function Get_Left_Margin (Text_View : access Gtk_Text_View_Record)
                             return Gint;

   procedure Set_Right_Margin
     (Text_View    : access Gtk_Text_View_Record;
      Right_Margin : Gint);

   function Get_Right_Margin (Text_View : access Gtk_Text_View_Record)
                              return Gint;

   procedure Set_Indent
     (Text_View : access Gtk_Text_View_Record;
      Indent    : Gint);

   function Get_Indent (Text_View : access Gtk_Text_View_Record)
                        return Gint;

   --  procedure Set_Tabs
   --    (Text_View : access Gtk_Text_View_Record;
   --     Tabs      : out Pango_Tab_Array);
   --  ??? Needs pango binding to be bound. Is this procedure essential?

   --  function Get_Tabs (Text_View : access Gtk_Text_View_Record)
   --                     return Pango_Tab_Array;
   --  ??? Needs pango binding to be bound. Is this procedure essential?

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "set_scroll_adjustments"
   --    procedure Handler (Widget : access Gtk_Text_View_Record'Class;
   --       Hadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class;
   --       Vadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --
   --  - "populate_popup"
   --    procedure Handler (Widget : access Gtk_Text_View_Record'Class;
   --       Menu : access Gtk.Menu.Gtk_Menu_Record'Class);
   --
   --  - "move_cursor"
   --    procedure Handler (Widget : access Gtk_Text_View_Record'Class;
   --       Step : Gtk_Movement_Step;
   --       Count : Gint;
   --       Extend_Selection : Boolean);
   --
   --  - "set_anchor"
   --    procedure Handler (Widget : access Gtk_Text_View_Record'Class);
   --
   --  - "insert_at_cursor"
   --    procedure Handler (Widget : access Gtk_Text_View_Record'Class;
   --       Str : String);
   --
   --  - "delete_from_cursor"
   --    procedure Handler (Widget : access Gtk_Text_View_Record'Class;
   --       The_Type : Gtk_Delete_Type;
   --       Count : Gint);
   --
   --  - "cut_clipboard"
   --    procedure Handler (Widget : access Gtk_Text_View_Record'Class);
   --
   --  - "copy_clipboard"
   --    procedure Handler (Widget : access Gtk_Text_View_Record'Class);
   --
   --  - "paste_clipboard"
   --    procedure Handler (Widget : access Gtk_Text_View_Record'Class);
   --
   --  - "toggle_overwrite"
   --    procedure Handler (Widget : access Gtk_Text_View_Record'Class);
   --
   --  </signals>

private
   type Gtk_Text_View_Record is new Gtk.Container.Gtk_Container_Record with
     null record;

   pragma Import (C, Get_Type, "gtk_text_view_get_type");
end Gtk.Text_View;
