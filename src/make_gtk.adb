-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2002 ACT-Europe                 --
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

pragma Warnings (Off);
with Glib.Convert;
with Glib.Enums;
with Glib.Error;
with Glib.Glist;
with Glib.Glade;
with Glib.Gnodes;
with Glib.Object;
with Glib.GSlist;
with Glib.Module;
with Glib.Properties;
with Glib.Properties.Creation;
with Glib.Type_Conversion_Hooks;
with Glib.Values;
with Glib.XML;

with Glib.Graphs;

with Gdk.Bitmap;
with Gdk.Color;
with Gdk.Cursor;
with Gdk.Dnd;
with Gdk.Drawable;
with Gdk.Event;
with Gdk.Font;
with Gdk.GC;
with Gdk.Image;
with Gdk.Input;
with Gdk.Keyval;
with Gdk.Main;
with Gdk.Pixbuf;
with Gdk.Pixmap;
with Gdk.Property;
with Gdk.Rectangle;
with Gdk.Region;
with Gdk.Rgb;
with Gdk.Threads;
with Gdk.Types.Keysyms;
with Gdk.Types;
with Gdk.Visual;
with Gdk.Window;
with Gdk.Window_Attr;

with Gtk.Type_Conversion;
with Gtk.Accel_Group;
with Gtk.Accel_Label;
with Gtk.Accel_Map;
with Gtk.Adjustment;
with Gtk.Alignment;
with Gtk.Arguments;
with Gtk.Arrow;
with Gtk.Aspect_Frame;
with Gtk.Bin;
with Gtk.Box;
with Gtk.Button;
with Gtk.Button_Box;
with Gtk.Calendar;
with Gtk.Cell_Editable;
with Gtk.Cell_Renderer;
with Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle;
with Gtk.Check_Button;
with Gtk.Check_Menu_Item;
with Gtk.Clipboard;
with Gtk.Clist;
with Gtk.Color_Selection;
with Gtk.Color_Selection_Dialog;
with Gtk.Combo;
with Gtk.Container;
with Gtk.Ctree;
with Gtk.Curve;
with Gtk.Dialog;
with Gtk.Dnd;
with Gtk.Drawing_Area;
with Gtk.Editable;
with Gtk.Enums;
with Gtk.Event_Box;
with Gtk.File_Selection;
with Gtk.Fixed;
with Gtk.Font_Selection;
with Gtk.Font_Selection_Dialog;
with Gtk.Frame;
with Gtk.Gamma_Curve;
with Gtk.GEntry;
with Gtk.GRange;
with Gtk.Handle_Box;
with Gtk.Handlers;
with Gtk.Hbutton_Box;
with Gtk.Image;
with Gtk.Image_Menu_Item;
with Gtk.Input_Dialog;
with Gtk.Invisible;
with Gtk.Item;
with Gtk.Item_Factory;
with Gtk.Label;
with Gtk.List;
with Gtk.List_Item;
with Gtk.List_Store;
with Gtk.Main;
with Gtk.Marshallers;
with Gtk.Menu;
with Gtk.Menu_Bar;
with Gtk.Menu_Item;
with Gtk.Menu_Shell;
with Gtk.Misc;
with Gtk.Notebook;
with Gtk.Object;
with Gtk.Option_Menu;
with Gtk.Paned;
with Gtk.Pixmap;
with Gtk.Plug;
with Gtk.Preview;
with Gtk.Progress;
with Gtk.Progress_Bar;
with Gtk.Radio_Button;
with Gtk.Radio_Menu_Item;
with Gtk.Rc;
with Gtk.Ruler;
with Gtk.Scale;
with Gtk.Scrollbar;
with Gtk.Scrolled_Window;
with Gtk.Selection;
with Gtk.Separator;
with Gtk.Size_Group;
with Gtk.Socket;
with Gtk.Spin_Button;
with Gtk.Status_Bar;
with Gtk.Stock;
with Gtk.Style;
with Gtk.Table;
with Gtk.Tearoff_Menu_Item;
with Gtk.Text;
with Gtk.Text_Attributes;
with Gtk.Text_Buffer;
with Gtk.Text_Child;
with Gtk.Text_Iter;
with Gtk.Text_Mark;
with Gtk.Text_Tag;
with Gtk.Text_Tag_Table;
with Gtk.Text_View;
with Gtk.Tree_Model;
with Gtk.Tree_Model_Sort;
with Gtk.Tree_Selection;
with Gtk.Tree_Store;
with Gtk.Tree_View;
with Gtk.Tree_View_Column;
with Gtk.Tips_Query;
with Gtk.Toggle_Button;
with Gtk.Toolbar;
with Gtk.Tooltips;
with Gtk.Vbutton_Box;
with Gtk.Viewport;
with Gtk.Widget;
with Gtk.Window;

with Gtk.Extra.Border_Combo;
with Gtk.Extra.Color_Combo;
with Gtk.Extra.Combo_Box;
with Gtk.Extra.Font_Combo;
with Gtk.Extra.PsFont;
--  disabled for now, until these widgets have been ported to Gtk+ 2.0:
--  with Gtk.Extra.Check_Item;
--  with Gtk.Extra.Item_Entry;
--  with Gtk.Extra.Plot;
--  with Gtk.Extra.Plot_Canvas;
--  with Gtk.Extra.Plot_Data;
--  with Gtk.Extra.Plot_Bar;
--  with Gtk.Extra.Plot_Box;
--  with Gtk.Extra.Plot_Polar;
--  with Gtk.Extra.Plot_3D;
--  with Gtk.Extra.Plot_Surface;
--  with Gtk.Extra.Plot_Ps;
--  with Gtk.Extra.Sheet;

with Gtkada.Canvas;
with Gtkada.Dialogs;
with Gtkada.File_Selection;
with Gtkada.Handlers;
with Gtkada.Intl;
with Gtkada.MDI;
with Gtkada.Pixmaps;
with Gtkada.Types;

with Pango;
with Pango.Attributes;
with Pango.Context;
with Pango.Layout;
with Pango.Enums;
with Pango.Font;

procedure Make is
begin
   null;
end Make;
