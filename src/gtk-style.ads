-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
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

with Gdk; use Gdk;
with Gdk.Color;
with Gdk.GC;
with Gdk.Font;
with Gdk.Types;
with Gdk.Window;
with Gtk.Enums;
with Gtk.Widget;

package Gtk.Style is

   type Gtk_Style is new Root_Type with private;

   --  NOTE: Gtk_Style is not an access type, since there is no easy
   --  way to automatically deallocate memory when the C widget is
   --  destroyed. It would be the responsability of the user to free
   --  memory, which is too different from the way the other widgets
   --  work.

   procedure Gtk_New (Style : out Gtk_Style);

   function Copy (Source : in Gtk_Style) return Gtk_Style;

   function Attach (Style  : in Gtk_Style;
                    Window : in Gdk.Window.Gdk_Window) return Gtk_Style;

   procedure Detach (Style : in Gtk_Style);

   procedure Set_Background (Style      : in Gtk_Style;
                             Window     : in Gdk.Window.Gdk_Window;
                             State_Type : in Enums.Gtk_State_Type);
   procedure Set_Foreground (Style      : in Gtk_Style;
                             State_Type : in Enums.Gtk_State_Type;
                             Color      : in Gdk.Color.Gdk_Color);
   procedure Set_Base (Style      : in Gtk_Style;
                       State_Type : in Enums.Gtk_State_Type;
                       Color      : in Gdk.Color.Gdk_Color);
   procedure Set_Font (Style      : in Gtk_Style;
                       Font       : in Gdk.Font.Gdk_Font);

   function Get_Bg (Style      : in Gtk_Style;
                    State_Type : in Enums.Gtk_State_Type)
                    return          Gdk.Color.Gdk_Color;

   function Get_Black (Style : in Gtk_Style)
                       return Gdk.Color.Gdk_Color;

   function Get_White (Style : in Gtk_Style)
                       return Gdk.Color.Gdk_Color;

   function Get_Style (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
                       return Gtk_Style;
   --  Warning: We return a record, not a pointer, since there is no way
   --  for us to automatically free the memory that would be associated with
   --  a pointer

   function Get_Black_GC (Style : in Gtk_Style)
                          return Gdk.GC.Gdk_GC;
   function Get_Bg_GC (Style : in Gtk_Style;
                       State : in Enums.Gtk_State_Type)
                       return Gdk.GC.Gdk_GC;
   function Get_White_GC (Style : in Gtk_Style)
                          return Gdk.GC.Gdk_GC;

   procedure Draw_Hline (Style      : in Gtk_Style;
                         Window     : in Gdk.Window.Gdk_Window;
                         State_Type : in Enums.Gtk_State_Type;
                         X1, X2     : in Gint;
                         Y          : in Gint);

   procedure Draw_Vline (Style      : in Gtk_Style;
                         Window     : in Gdk.Window.Gdk_Window;
                         State_Type : in Enums.Gtk_State_Type;
                         Y1, Y2     : in Gint;
                         X          : in Gint);

   procedure Draw_Shadow (Style       : in Gtk_Style;
                          Window      : in Gdk.Window.Gdk_Window;
                          State_Type  : in Enums.Gtk_State_Type;
                          Shadow_Type : in Enums.Gtk_Shadow_Type;
                          X, Y        : in Gint;
                          Width       : in Gint;
                          Height      : in Gint);

   procedure Draw_Polygon (Style      : in Gtk_Style;
                           Window     : in Gdk.Window.Gdk_Window;
                           State_Type : in Enums.Gtk_State_Type;
                           Shadow_Type : in Enums.Gtk_Shadow_Type;
                           Points     : in Gdk.Types.Gdk_Points_Array;
                           Fill       : in Gint);

   procedure Draw_Arrow (Style       : in Gtk_Style;
                         Window      : in Gdk.Window.Gdk_Window;
                         State_Type  : in Enums.Gtk_State_Type;
                         Shadow_Type : in Enums.Gtk_Shadow_Type;
                         Arrow_Type  : in Enums.Gtk_Arrow_Type;
                         Fill        : in Gint;
                         X, Y        : in Gint;
                         Width       : in Gint;
                         Height      : in Gint);

   procedure Draw_Diamond (Style       : in Gtk_Style;
                           Window      : in Gdk.Window.Gdk_Window;
                           State_Type  : in Enums.Gtk_State_Type;
                           Shadow_Type : in Enums.Gtk_Shadow_Type;
                           X, Y        : in Gint;
                           Width       : in Gint;
                           Height      : in Gint);

   procedure Draw_Oval (Style       : in Gtk_Style;
                        Window      : in Gdk.Window.Gdk_Window;
                        State_Type  : in Enums.Gtk_State_Type;
                        Shadow_Type : in Enums.Gtk_Shadow_Type;
                        X, Y        : in Gint;
                        Width       : in Gint;
                        Height      : in Gint);

   procedure Draw_String (Style       : in Gtk_Style;
                          Window      : in Gdk.Window.Gdk_Window;
                          State_Type  : in Enums.Gtk_State_Type;
                          X, Y        : in Gint;
                          Str         : in String);

   procedure Ref (Object : in Gtk_Style);
   procedure Unref (Object : in Gtk_Style);

private
   type Gtk_Style is new Root_Type with null record;

end Gtk.Style;
