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

--  with Gdk.ImlibImage;
with Gtk; use Gtk;
with Gnome.Pixmap;
with System;

package body Gnome.Animator is

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New
     (Widget : out Gnome_Animator;
      Width  : Guint;
      Height : Guint)
   is
   begin
      Widget := new Gnome_Animator_Record;
      Initialize (Widget, Width, Height);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget : access Gnome_Animator_Record'Class;
      Width  : Guint;
      Height : Guint)
   is
      function Internal
        (Width  : Guint;
         Height : Guint)
         return System.Address;
      pragma Import (C, Internal, "gnome_animator_new_with_size");
   begin
      Set_Object (Widget, Internal (Width,
                                    Height));
   end Initialize;

   -------------
   -- Advance --
   -------------

   function Advance
     (Animator : access Gnome_Animator_Record;
      Num      : Gint)
      return Boolean
   is
      function Internal
        (Animator : System.Address;
         Num      : Gint)
         return Gint;
      pragma Import (C, Internal, "gnome_animator_advance");
   begin
      return Boolean'Val (Internal (Get_Object (Animator),
                                    Num));
   end Advance;

   ----------------------------
   -- Append_Frame_From_File --
   ----------------------------

   function Append_Frame_From_File
     (Animator : access Gnome_Animator_Record;
      Name     : String;
      X_Offset : Gint;
      Y_Offset : Gint;
      Interval : Guint32)
      return Boolean
   is
      function Internal
        (Animator : System.Address;
         Name     : String;
         X_Offset : Gint;
         Y_Offset : Gint;
         Interval : Guint32)
         return Gint;
      pragma Import (C, Internal, "gnome_animator_append_frame_from_file");
   begin
      return Boolean'Val (Internal (Get_Object (Animator),
                                    Name & ASCII.NUL,
                                    X_Offset,
                                    Y_Offset,
                                    Interval));
   end Append_Frame_From_File;

   ------------------------------------
   -- Append_Frame_From_File_At_Size --
   ------------------------------------

   function Append_Frame_From_File_At_Size
     (Animator : access Gnome_Animator_Record;
      Name     : String;
      X_Offset : Gint;
      Y_Offset : Gint;
      Interval : Guint32;
      Width    : Guint;
      Height   : Guint)
      return Boolean
   is
      function Internal
        (Animator : System.Address;
         Name     : String;
         X_Offset : Gint;
         Y_Offset : Gint;
         Interval : Guint32;
         Width    : Guint;
         Height   : Guint)
         return Gint;
      pragma Import
        (C, Internal, "gnome_animator_append_frame_from_file_at_size");
   begin
      return Boolean'Val (Internal (Get_Object (Animator),
                                    Name & ASCII.NUL,
                                    X_Offset,
                                    Y_Offset,
                                    Interval,
                                    Width,
                                    Height));
   end Append_Frame_From_File_At_Size;

   ------------------------------------
   -- Append_Frame_From_Gnome_Pixmap --
   ------------------------------------

   function Append_Frame_From_Gnome_Pixmap
     (Animator : access Gnome_Animator_Record;
      Pixmap   : access Gnome.Pixmap.Gnome_Pixmap_Record'Class;
      X_Offset : Gint;
      Y_Offset : Gint;
      Interval : Guint32)
      return Boolean
   is
      function Internal
        (Animator : System.Address;
         Pixmap   : System.Address;
         X_Offset : Gint;
         Y_Offset : Gint;
         Interval : Guint32)
         return Gint;
      pragma Import
        (C, Internal, "gnome_animator_append_frame_from_gnome_pixmap");
   begin
      return Boolean'Val (Internal (Get_Object (Animator),
                                    Get_Object (Pixmap),
                                    X_Offset,
                                    Y_Offset,
                                    Interval));
   end Append_Frame_From_Gnome_Pixmap;

   -----------------------------
   -- Append_Frame_From_Imlib --
   -----------------------------

   --  function Append_Frame_From_Imlib
   --    (Animator : access Gnome_Animator_Record;
   --     Image    : Gdk.ImlibImage.Gdk_ImlibImage;
   --     X_Offset : Gint;
   --     Y_Offset : Gint;
   --     Interval : Guint32)
   --     return Boolean
   --  is
   --     function Internal
   --       (Animator : System.Address;
   --        Image    : GdkImlibImage;
   --        X_Offset : Gint;
   --        Y_Offset : Gint;
   --        Interval : Guint32)
   --        return Gint;
   --     pragma Import
   --       (C, Internal, "gnome_animator_append_frame_from_imlib");
   --  begin
   --     return Boolean'Val (Internal (Get_Object (Animator),
   --                                   Image,
   --                                   X_Offset,
   --                                   Y_Offset,
   --                                   Interval));
   --  end Append_Frame_From_Imlib;

   -------------------------------------
   -- Append_Frame_From_Imlib_At_Size --
   -------------------------------------

   --  function Append_Frame_From_Imlib_At_Size
   --    (Animator : access Gnome_Animator_Record;
   --     Image    : Gdk.ImlibImage.Gdk_ImlibImage;
   --     X_Offset : Gint;
   --     Y_Offset : Gint;
   --     Interval : Guint32;
   --     Width    : Guint;
   --     Height   : Guint)
   --     return Boolean
   --  is
   --     function Internal
   --       (Animator : System.Address;
   --        Image    : GdkImlibImage;
   --        X_Offset : Gint;
   --        Y_Offset : Gint;
   --        Interval : Guint32;
   --        Width    : Guint;
   --        Height   : Guint)
   --        return Gint;
   --     pragma Import
   --       (C, Internal, "gnome_animator_append_frame_from_imlib_at_size");
   --  begin
   --     return Boolean'Val (Internal (Get_Object (Animator),
   --                                   Image,
   --                                   X_Offset,
   --                                   Y_Offset,
   --                                   Interval,
   --                                   Width,
   --                                   Height));
   --  end Append_Frame_From_Imlib_At_Size;

   -----------------------------
   -- Append_Frames_From_File --
   -----------------------------

   function Append_Frames_From_File
     (Animator : access Gnome_Animator_Record;
      Name     : String;
      X_Offset : Gint;
      Y_Offset : Gint;
      Interval : Guint32;
      X_Unit   : Gint)
      return Boolean
   is
      function Internal
        (Animator : System.Address;
         Name     : String;
         X_Offset : Gint;
         Y_Offset : Gint;
         Interval : Guint32;
         X_Unit   : Gint)
         return Gint;
      pragma Import (C, Internal, "gnome_animator_append_frames_from_file");
   begin
      return Boolean'Val (Internal (Get_Object (Animator),
                                    Name & ASCII.NUL,
                                    X_Offset,
                                    Y_Offset,
                                    Interval,
                                    X_Unit));
   end Append_Frames_From_File;

   -------------------------------------
   -- Append_Frames_From_File_At_Size --
   -------------------------------------

   function Append_Frames_From_File_At_Size
     (Animator : access Gnome_Animator_Record;
      Name     : String;
      X_Offset : Gint;
      Y_Offset : Gint;
      Interval : Guint32;
      X_Unit   : Gint;
      Width    : Guint;
      Height   : Guint)
      return Boolean
   is
      function Internal
        (Animator : System.Address;
         Name     : String;
         X_Offset : Gint;
         Y_Offset : Gint;
         Interval : Guint32;
         X_Unit   : Gint;
         Width    : Guint;
         Height   : Guint)
         return Gint;
      pragma Import
        (C, Internal, "gnome_animator_append_frames_from_file_at_size");
   begin
      return Boolean'Val (Internal (Get_Object (Animator),
                                    Name & ASCII.NUL,
                                    X_Offset,
                                    Y_Offset,
                                    Interval,
                                    X_Unit,
                                    Width,
                                    Height));
   end Append_Frames_From_File_At_Size;

   ------------------------------
   -- Append_Frames_From_Imlib --
   ------------------------------

   --  function Append_Frames_From_Imlib
   --    (Animator : access Gnome_Animator_Record;
   --     Image    : Gdk.ImlibImage.Gdk_ImlibImage;
   --     X_Offset : Gint;
   --     Y_Offset : Gint;
   --     Interval : Guint32;
   --     X_Unit   : Gint)
   --     return Boolean
   --  is
   --     function Internal
   --       (Animator : System.Address;
   --        Image    : GdkImlibImage;
   --        X_Offset : Gint;
   --        Y_Offset : Gint;
   --        Interval : Guint32;
   --        X_Unit   : Gint)
   --        return Gint;
   --     pragma Import
   --       (C, Internal, "gnome_animator_append_frames_from_imlib");
   --  begin
   --     return Boolean'Val (Internal (Get_Object (Animator),
   --                                   Image,
   --                                   X_Offset,
   --                                   Y_Offset,
   --                                   Interval,
   --                                   X_Unit));
   --  end Append_Frames_From_Imlib;

   --------------------------------------
   -- Append_Frames_From_Imlib_At_Size --
   --------------------------------------

   --  function Append_Frames_From_Imlib_At_Size
   --    (Animator : access Gnome_Animator_Record;
   --     Image    : Gdk.ImlibImage.Gdk_ImlibImage;
   --     X_Offset : Gint;
   --     Y_Offset : Gint;
   --     Interval : Guint32;
   --     X_Unit   : Gint;
   --     Width    : Guint;
   --     Height   : Guint)
   --     return Boolean
   --  is
   --     function Internal
   --       (Animator : System.Address;
   --        Image    : GdkImlibImage;
   --        X_Offset : Gint;
   --        Y_Offset : Gint;
   --        Interval : Guint32;
   --        X_Unit   : Gint;
   --        Width    : Guint;
   --        Height   : Guint)
   --        return Gint;
   --     pragma Import
   --       (C, Internal, "gnome_animator_append_frames_from_imlib_at_size");
   --  begin
   --     return Boolean'Val (Internal (Get_Object (Animator),
   --                                   Image,
   --                                   X_Offset,
   --                                   Y_Offset,
   --                                   Interval,
   --                                   X_Unit,
   --                                   Width,
   --                                   Height));
   --  end Append_Frames_From_Imlib_At_Size;

   ------------------------------
   -- Get_Current_Frame_Number --
   ------------------------------

   function Get_Current_Frame_Number (Animator : access Gnome_Animator_Record)
                                      return Guint
   is
      function Internal (Animator : System.Address)
                         return Guint;
      pragma Import (C, Internal, "gnome_animator_get_current_frame_number");
   begin
      return Internal (Get_Object (Animator));
   end Get_Current_Frame_Number;

   -------------------
   -- Get_Loop_Type --
   -------------------

   function Get_Loop_Type (Animator : access Gnome_Animator_Record)
                           return Gnome_Animator_Loop_Type
   is
      function Internal (Animator : System.Address)
                         return Gint;
      pragma Import (C, Internal, "gnome_animator_get_loop_type");
   begin
      return Gnome_Animator_Loop_Type'Val (Internal (Get_Object (Animator)));
   end Get_Loop_Type;

   ----------------------------
   -- Get_Playback_Direction --
   ----------------------------

   function Get_Playback_Direction (Animator : access Gnome_Animator_Record)
                                    return Gint
   is
      function Internal (Animator : System.Address)
                         return Gint;
      pragma Import (C, Internal, "gnome_animator_get_playback_direction");
   begin
      return Internal (Get_Object (Animator));
   end Get_Playback_Direction;

   ------------------------
   -- Get_Playback_Speed --
   ------------------------

   function Get_Playback_Speed (Animator : access Gnome_Animator_Record)
                                return Gdouble
   is
      function Internal (Animator : System.Address)
                         return Gdouble;
      pragma Import (C, Internal, "gnome_animator_get_playback_speed");
   begin
      return Internal (Get_Object (Animator));
   end Get_Playback_Speed;

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status (Animator : access Gnome_Animator_Record)
                        return Gnome_Animator_Status
   is
      function Internal (Animator : System.Address)
                         return Gint;
      pragma Import (C, Internal, "gnome_animator_get_status");
   begin
      return Gnome_Animator_Status'Val (Internal (Get_Object (Animator)));
   end Get_Status;

   ----------------
   -- Goto_Frame --
   ----------------

   procedure Goto_Frame
     (Animator     : access Gnome_Animator_Record;
      Frame_Number : Guint)
   is
      procedure Internal
        (Animator     : System.Address;
         Frame_Number : Guint);
      pragma Import (C, Internal, "gnome_animator_goto_frame");
   begin
      Internal (Get_Object (Animator),
                Frame_Number);
   end Goto_Frame;

   -------------------
   -- Set_Loop_Type --
   -------------------

   procedure Set_Loop_Type
     (Animator  : access Gnome_Animator_Record;
      Loop_Type : Gnome_Animator_Loop_Type)
   is
      procedure Internal
        (Animator  : System.Address;
         Loop_Type : Gint);
      pragma Import (C, Internal, "gnome_animator_set_loop_type");
   begin
      Internal (Get_Object (Animator),
                Gnome_Animator_Loop_Type'Pos (Loop_Type));
   end Set_Loop_Type;

   ----------------------------
   -- Set_Playback_Direction --
   ----------------------------

   procedure Set_Playback_Direction
     (Animator           : access Gnome_Animator_Record;
      Playback_Direction : Gint)
   is
      procedure Internal
        (Animator           : System.Address;
         Playback_Direction : Gint);
      pragma Import (C, Internal, "gnome_animator_set_playback_direction");
   begin
      Internal (Get_Object (Animator),
                Playback_Direction);
   end Set_Playback_Direction;

   ------------------------
   -- Set_Playback_Speed --
   ------------------------

   procedure Set_Playback_Speed
     (Animator : access Gnome_Animator_Record;
      Speed    : Gdouble)
   is
      procedure Internal
        (Animator : System.Address;
         Speed    : Gdouble);
      pragma Import (C, Internal, "gnome_animator_set_playback_speed");
   begin
      Internal (Get_Object (Animator),
                Speed);
   end Set_Playback_Speed;

   -----------
   -- Start --
   -----------

   procedure Start (Animator : access Gnome_Animator_Record)
   is
      procedure Internal (Animator : System.Address);
      pragma Import (C, Internal, "gnome_animator_start");
   begin
      Internal (Get_Object (Animator));
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop (Animator : access Gnome_Animator_Record)
   is
      procedure Internal (Animator : System.Address);
      pragma Import (C, Internal, "gnome_animator_stop");
   begin
      Internal (Get_Object (Animator));
   end Stop;

end Gnome.Animator;
