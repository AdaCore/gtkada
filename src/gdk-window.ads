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

with Glib; use Glib;
with Glib.Glist;
with Gdk;
with Gdk.Color;
with Gdk.Cursor;
with Gdk.Types;
with Gdk.Visual;
with Gdk.Window_Attr;

package Gdk.Window is

   type Gdk_Window is new Root_Type with null record;
   --
   --  This type is not defined as private because we need the
   --  full declaration to instantiate the Glist package with it

   Null_Window : constant Gdk_Window;


   procedure Gdk_New
     (Window          :    out Gdk_Window;
      Parent          : in     Gdk_Window;
      Attributes      : in     Gdk.Window_Attr.Gdk_Window_Attr;
      Attributes_Mask : in     Gdk.Types.Gdk_Window_Attributes_Type);

   procedure Foreign_New (Window :    out Gdk_Window;
                          An_Id  : in     Guint32);


   procedure Destroy (Window : in out Gdk_Window);

   procedure Ref (Window : in out Gdk_Window);

   procedure Unref (Window : in out Gdk_Window);

   procedure Window_At_Pointer (Win_X  : out Gint;
                                Win_Y  : out Gint;
                                Window : out Gdk_Window);

   procedure Show (Window : in Gdk_Window);

   procedure Hide (Window : in Gdk_Window);

   procedure Withdraw (Window : in out Gdk_Window);

   procedure Move (Window : in out Gdk_Window;
                   X      : in     Gint;
                   Y      : in     Gint);

   procedure Resize (Window : in out Gdk_Window;
                     Width  : in     Gint;
                     Height : in     Gint);

   procedure Move_Resize (Window : in out Gdk_Window;
                          X      : in     Gint;
                          Y      : in     Gint;
                          Width  : in     Gint;
                          Height : in     Gint);

   procedure Reparent (Window     : in out Gdk_Window;
                       New_Parent : in     Gdk_Window;
                       X          : in     Gint;
                       Y          : in     Gint);

   procedure Clear (Window : in out Gdk_Window);

   procedure Clear_Area (Window : in Gdk_Window;
                         X      : in Gint;
                         Y      : in Gint;
                         Width  : in Gint;
                         Height : in Gint);

   procedure Clear_Area_E (Window : in Gdk_Window;
                           X      : in Gint;
                           Y      : in Gint;
                           Width  : in Gint;
                           Height : in Gint);

   ---------------------------
   --  procedure Copy_Area  --
   ---------------------------
   --
   --  Copy_Area needs a Gdk_Gc object. However, the Gdk.Gc package depends
   --  on this package, thus creating a circular dependency. This service
   --  has therefore been moved to the following child package : Gdk.Window.Gc.

   procedure Gdk_Raise (Window : in Gdk_Window);

   procedure Lower (Window : in Gdk_Window);

   procedure Set_Override_Redirect
     (Window            : in out Gdk_Window;
      Override_Redirect : in     Boolean := True);

   procedure Set_Child_Shapes (Window : in out Gdk_Window);

   procedure Merge_Child_Shapes (Window : in out Gdk_Window);

   function Is_Visible (Window : in Gdk_Window) return Boolean;

   function Is_Viewable (Window : in Gdk_Window) return Boolean;

   procedure Set_Hints (Window     : in out Gdk_Window;
                        X          : in     Gint;
                        Y          : in     Gint;
                        Min_Width  : in     Gint;
                        Min_Height : in     Gint;
                        Max_Width  : in     Gint;
                        Max_Height : in     Gint;
                        Flags      : in     Gdk.Types.Gdk_Window_Hints);

   procedure Set_Geometry_Hints (Window   : in out Gdk_Window;
                                 Geometry : in out Gdk.Types.Gdk_Geometry;
                                 Flags    : in     Gdk.Types.Gdk_Window_Hints);

   procedure Set_Title (Window : in out Gdk_Window;
                        Title  : in     String);

   procedure Set_Role (Window : in out Gdk_Window;
                       Role   : in     String);

   procedure Set_Transient_For (Window : in out Gdk_Window;
                                Leader : in     Gdk_Window);

   procedure Set_Background (Window : in out Gdk_Window;
                             Color  : in     Gdk.Color.Gdk_Color);

   ---------------------------------
   --  procedure Set_Back_Pixmap  --
   ---------------------------------
   --
   --  For circular dependency reasons, this procedure has been moved to
   --  The Gdk.Window.Pixmap child package.

   procedure Set_Cursor (Window : in out Gdk_Window;
                         Cursor : in     Gdk.Cursor.Gdk_Cursor);

   procedure Set_Colormap (Window   : in out Gdk_Window;
                           Colormap : in     Gdk.Color.Gdk_Colormap);

   procedure Get_Geometry (Window : in     Gdk_Window;
                           X      :    out Gint;
                           Y      :    out Gint;
                           Width  :    out Gint;
                           Height :    out Gint;
                           Depth  :    out Gint);

   procedure Get_Position (Window : in     Gdk_Window;
                           X      :    out Gint;
                           Y      :    out Gint);

   procedure Get_Size (Window : in     Gdk_Window;
                       Width  :    out Gint;
                       Height :    out Gint);

   function Get_Visual (Window : in Gdk_Window) return Gdk.Visual.Gdk_Visual;

   function Get_Colormap (Window : in Gdk_Window)
                          return Gdk.Color.Gdk_Colormap;

   function Get_Type (Window : in Gdk_Window) return Gdk.Types.Gdk_Window_Type;

   procedure Get_Origin (Window  : in     Gdk_Window;
                         X       :    out Gint;
                         Y       :    out Gint;
                         Success :    out Boolean);

   procedure Get_Desk_Relative_Origin (Window  : in     Gdk_Window;
                                       X       :    out Gint;
                                       Y       :    out Gint;
                                       Success :    out Boolean);

   procedure Get_Root_Origin (Window : in     Gdk_Window;
                              X      :    out Gint;
                              Y      :    out Gint);

   procedure Get_Pointer (Window : in out Gdk_Window;
                          X      :    out Gint;
                          Y      :    out Gint;
                          Mask   :    out Gdk.Types.Gdk_Modifier_Type;
                          Result :    out Gdk_Window);

   procedure Get_Parent (Window : in     Gdk_Window;
                         Parent :    out Gdk_Window);
   --
   --  NOTE : This is declared as a procedure instead of a function to avoid
   --         having to override it for all the derived types of Gdk_Window.

   procedure Get_Toplevel (Window   : in     Gdk_Window;
                           Toplevel :    out Gdk_Window);
   --
   --  NOTE : This is declared as a procedure instead of a function to avoid
   --         having to override it for all the derived types of Gdk_Window.

   function Get_Events (Window : in Gdk_Window)
                        return Gdk.Types.Gdk_Event_Mask;

   procedure Set_Events (Window     : in out Gdk_Window;
                         Event_Mask : in     Gdk.Types.Gdk_Event_Mask);

   -------------------------------
   --  procedure Set_Icon       --
   --  procedure Set_Icon_Name  --
   -------------------------------
   --
   --  For circular dependency issues, these 2 services have been moved
   --  to the Gdk.Window.Pixmap child package.

   procedure Set_Group (Window : in out Gdk_Window;
                        Leader : in     Gdk_Window);

   procedure Set_Decorations
     (Window      : in out Gdk_Window;
      Decorations : in     Gdk.Types.Gdk_Wm_Decoration);

   procedure Set_Functions (Window    : in out Gdk_Window;
                            Functions : in     Gdk.Types.Gdk_Wm_Function);


   --  Gdk_Window_List
   --
   function Convert (S : in System.Address) return Gdk_Window'Class;
   function Convert (P : in Gdk_Window'Class) return System.Address;

   package Gdk_Window_List is new Glib.Glist.Generic_List
     (Gpointer => Gdk_Window'Class);

   function Get_Children (Window : in Gdk_Window'Class)
                          return Gdk_Window_List.Glist;

   function Get_Toplevels return Gdk_Window_List.Glist;

private

   Null_Window : constant Gdk_Window := (Ptr => System.Null_Address);

end Gdk.Window;
