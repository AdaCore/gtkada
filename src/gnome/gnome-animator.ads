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

with Glib; use Glib;
--  with Gdk.ImlibImage;
with Gnome.Pixmap;
with Gtk;
with Gtk.Widget;

package Gnome.Animator is

   type Gnome_Animator_Record is new Gtk.Widget.Gtk_Widget_Record with private;
   type Gnome_Animator is access all Gnome_Animator_Record'Class;

   type Gnome_Animator_Status is (
      Status_Stopped,
      Status_Running);

   type Gnome_Animator_Loop_Type is (
      Loop_None,
      Loop_Restart,
      Loop_Ping_Pong);

   procedure Gnome_New
     (Widget : out Gnome_Animator;
      Width  : Guint;
      Height : Guint);

   procedure Initialize
     (Widget : access Gnome_Animator_Record'Class;
      Width  : Guint;
      Height : Guint);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   function Advance
     (Animator : access Gnome_Animator_Record;
      Num      : Gint)
      return Boolean;

   function Append_Frame_From_File
     (Animator : access Gnome_Animator_Record;
      Name     : String;
      X_Offset : Gint;
      Y_Offset : Gint;
      Interval : Guint32)
      return Boolean;

   function Append_Frame_From_File_At_Size
     (Animator : access Gnome_Animator_Record;
      Name     : String;
      X_Offset : Gint;
      Y_Offset : Gint;
      Interval : Guint32;
      Width    : Guint;
      Height   : Guint)
      return Boolean;

   function Append_Frame_From_Gnome_Pixmap
     (Animator : access Gnome_Animator_Record;
      Pixmap   : access Gnome.Pixmap.Gnome_Pixmap_Record'Class;
      X_Offset : Gint;
      Y_Offset : Gint;
      Interval : Guint32)
      return Boolean;

   --  function Append_Frame_From_Imlib
   --    (Animator : access Gnome_Animator_Record;
   --     Image    : Gdk.ImlibImage.Gdk_ImlibImage;
   --     X_Offset : Gint;
   --     Y_Offset : Gint;
   --     Interval : Guint32)
   --     return Boolean;

   --  function Append_Frame_From_Imlib_At_Size
   --    (Animator : access Gnome_Animator_Record;
   --     Image    : Gdk.ImlibImage.Gdk_ImlibImage;
   --     X_Offset : Gint;
   --     Y_Offset : Gint;
   --     Interval : Guint32;
   --     Width    : Guint;
   --     Height   : Guint)
   --     return Boolean;

   function Append_Frames_From_File
     (Animator : access Gnome_Animator_Record;
      Name     : String;
      X_Offset : Gint;
      Y_Offset : Gint;
      Interval : Guint32;
      X_Unit   : Gint)
      return Boolean;

   function Append_Frames_From_File_At_Size
     (Animator : access Gnome_Animator_Record;
      Name     : String;
      X_Offset : Gint;
      Y_Offset : Gint;
      Interval : Guint32;
      X_Unit   : Gint;
      Width    : Guint;
      Height   : Guint)
      return Boolean;

   --  function Append_Frames_From_Imlib
   --    (Animator : access Gnome_Animator_Record;
   --     Image    : Gdk.ImlibImage.Gdk_ImlibImage;
   --     X_Offset : Gint;
   --     Y_Offset : Gint;
   --     Interval : Guint32;
   --     X_Unit   : Gint)
   --     return Boolean;

   --  function Append_Frames_From_Imlib_At_Size
   --    (Animator : access Gnome_Animator_Record;
   --     Image    : Gdk.ImlibImage.Gdk_ImlibImage;
   --     X_Offset : Gint;
   --     Y_Offset : Gint;
   --     Interval : Guint32;
   --     X_Unit   : Gint;
   --     Width    : Guint;
   --     Height   : Guint)
   --     return Boolean;

   function Get_Current_Frame_Number (Animator : access Gnome_Animator_Record)
                                      return Guint;

   function Get_Loop_Type (Animator : access Gnome_Animator_Record)
                           return Gnome_Animator_Loop_Type;

   function Get_Playback_Direction (Animator : access Gnome_Animator_Record)
                                    return Gint;

   function Get_Playback_Speed (Animator : access Gnome_Animator_Record)
                                return Gdouble;

   function Get_Status (Animator : access Gnome_Animator_Record)
                        return Gnome_Animator_Status;

   procedure Goto_Frame
     (Animator     : access Gnome_Animator_Record;
      Frame_Number : Guint);

   procedure Set_Loop_Type
     (Animator  : access Gnome_Animator_Record;
      Loop_Type : Gnome_Animator_Loop_Type);

   procedure Set_Playback_Direction
     (Animator           : access Gnome_Animator_Record;
      Playback_Direction : Gint);

   procedure Set_Playback_Speed
     (Animator : access Gnome_Animator_Record;
      Speed    : Gdouble);

   procedure Start (Animator : access Gnome_Animator_Record);

   procedure Stop (Animator : access Gnome_Animator_Record);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  </signals>

private
   type Gnome_Animator_Record is new
     Gtk.Widget.Gtk_Widget_Record with null record;

   pragma Import (C, Get_Type, "gnome_animator_get_type");
end Gnome.Animator;
