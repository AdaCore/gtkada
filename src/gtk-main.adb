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

with Interfaces.C.Strings;
with Unchecked_Deallocation;
with Unchecked_Conversion;
with System;

with Ada.Text_IO; use Ada.Text_IO;

package body Gtk.Main is

   package C renames Interfaces.C;

   --------------
   -- Do_Event --
   --------------

   procedure Do_Event (Event : in Gdk.Event.Gdk_Event) is
      procedure Internal (Event : System.Address);
      pragma Import (C, Internal, "gtk_main_do_event");
   begin
      Internal (Gdk.Event.To_Address (Event));
   end Do_Event;

   --------------------
   -- Events_Pending --
   --------------------

   function Events_Pending return Boolean is
      function Internal return Gint;
      pragma Import (C, Internal, "gtk_events_pending");
   begin
      return Boolean'Val (Internal);
   end Events_Pending;

   --------------
   -- Quit_Add --
   --------------

   function Quit_Add (Main_Level : Guint;
                      Func       : Quit_Function)
                     return Quit_Handler_Id
   is
      function Internal (Main_Level : Guint;
                         Func       : Quit_Function;
                         Data       : System.Address)
                        return Quit_Handler_Id;
      pragma Import (C, Internal, "gtk_quit_add");
   begin
      return Internal (Main_Level, Func, System.Null_Address);
   end Quit_Add;

   ----------
   -- Quit --
   ----------

   package body Quit is
      type Data_Type_Access is access Data_Type;
      type Cb_Record is
         record
            Func : Quit_Function;
            Data : Data_Type_Access;
         end record;
      type Cb_Record_Access is access Cb_Record;

      procedure Free_Data (D : in System.Address);
      pragma Convention (C, Free_Data);

      function Convert is new Unchecked_Conversion
        (System.Address, Cb_Record_Access);

      function General_Cb (D : in System.Address) return Gint;
      pragma Convention (C, General_Cb);

      ----------
      -- Free --
      ----------

      procedure Free_Data (D : in System.Address) is
         procedure Internal is new Unchecked_Deallocation
           (Cb_Record, Cb_Record_Access);
         procedure Internal2 is new Unchecked_Deallocation
           (Data_Type, Data_Type_Access);
         Data : Cb_Record_Access := Convert (D);
      begin
         Internal2 (Data.Data);
         Internal (Data);
      end Free_Data;

      ----------------
      -- General_Cb --
      ----------------

      function General_Cb (D : in System.Address) return Gint is
         Data : Cb_Record_Access := Convert (D);
      begin
         return Boolean'Pos (Data.Func (Data.Data.all));
      end General_Cb;

      ---------
      -- Add --
      ---------

      function Quit_Add (Main_Level : Guint;
                         Func       : Quit_Function;
                         Data       : Data_Type)
                        return Quit_Handler_Id
      is
         function Internal (Main_Level : in Guint;
                            Func       : in System.Address;
                            Marshal    : in System.Address;
                            Data       : in System.Address;
                            Destroy    : in System.Address)
                            return     Quit_Handler_Id;
         pragma Import (C, Internal, "gtk_quit_add_full");
         function Convert is new Unchecked_Conversion
           (Cb_Record_Access, System.Address);
         D : Cb_Record_Access
           := new Cb_Record'(Func => Func,
                             Data => new Data_Type'(Data));
      begin
         return Internal (Main_Level, General_Cb'Address, System.Null_Address,
                          Convert (D), Free_Data'Address);
      end Quit_Add;
   end Quit;

   ----------------------
   -- Quit_Add_Destroy --
   ----------------------

   function Quit_Add_Destroy
     (Main_Level : Guint;
      Object     : access Gtk.Object.Gtk_Object_Record'Class)
     return Quit_Handler_Id
   is
      function Internal (Main_Level : Guint;
                         Object     : System.Address)
                        return Quit_Handler_Id;
      pragma Import (C, Internal, "gtk_quit_add_destroy");
   begin
      return Internal (Main_Level, Get_Object (Object));
   end Quit_Add_Destroy;

   --------------
   -- Idle_Add --
   --------------

   function Idle_Add (Cb       : in Idle_Callback;
                      Priority : in Idle_Priority := Priority_Default_Idle)
                     return Idle_Handler_Id
   is
      function Internal (Priority : in Idle_Priority;
                         Func     : in Idle_Callback;
                         Data     : System.Address)
                        return Idle_Handler_Id;
      pragma Import (C, Internal, "gtk_idle_add_priority");
   begin
      return Internal (Priority, Cb, System.Null_Address);
   end Idle_Add;

   ----------
   -- Init --
   ----------

   procedure Init is
      gnat_argc : Interfaces.C.int;
      pragma Import (C, gnat_argc);

      gnat_argv : System.Address;
      pragma Import (C, gnat_argv);

      procedure Internal (argc : System.Address; argv : System.Address);
      pragma Import (C, Internal, "gtk_init");

   begin
      Internal (gnat_argc'Address, gnat_argv'Address);
   end Init;

   ----------------
   -- Init_Check --
   ----------------

   function Init_Check return Boolean is
      gnat_argc : Interfaces.C.int;
      pragma Import (C, gnat_argc);

      gnat_argv : System.Address;
      pragma Import (C, gnat_argv);

      function Internal (argc : System.Address; argv : System.Address)
                        return Gboolean;
      pragma Import (C, Internal, "gtk_init_check");

   begin
      return Boolean'Val (Internal (gnat_argc'Address, gnat_argv'Address));
   end Init_Check;

   ----------------------
   -- Get_Event_Widget --
   ----------------------

   function Get_Event_Widget (Event : in Gdk.Event.Gdk_Event)
                             return Gtk.Widget.Gtk_Widget
   is
      function Internal (Event : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_get_event_widget");
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget
        (Get_User_Data (Internal (Gdk.Event.To_Address (Event)), Stub));
   end Get_Event_Widget;

   --------------
   -- Grab_Add --
   --------------

   procedure Grab_Add (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_grab_add");
   begin
      Internal (Get_Object (Widget));
   end Grab_Add;

   -----------------
   -- Grab_Remove --
   -----------------

   procedure Grab_Remove
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_grab_remove");
   begin
      Internal (Get_Object (Widget));
   end Grab_Remove;

   ----------------------
   -- Grab_Get_Current --
   ----------------------

   function Grab_Get_Current return Gtk.Widget.Gtk_Widget is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_grab_get_current");
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal, Stub));
   end Grab_Get_Current;

   --------------------
   -- Main_Iteration --
   --------------------

   function Main_Iteration (Blocking : Boolean := True) return Boolean is
      function Internal (Blocking : Boolean) return Gint;
      pragma Import (C, Internal, "gtk_main_iteration_do");
   begin
      return Boolean'Val (Internal (Blocking));
   end Main_Iteration;

   ----------------
   -- Set_Locale --
   ----------------

   function Set_Locale return String is
      function Internal return C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_set_locale");
   begin
      return C.Strings.Value (Internal);
   end Set_Locale;

   procedure Set_Locale is
      Dummy : constant String := Set_Locale;
      pragma Warnings (Off, Dummy);
   begin
      null;
   end Set_Locale;

   -----------------
   -- Timeout_Add --
   -----------------

   function Timeout_Add (Interval : in Guint32;
                         Func : Timeout_Callback)
                        return Timeout_Handler_Id
   is
      function Internal (Interval : Guint32;
                         Func     : Timeout_Callback;
                         Data     : System.Address)
                        return Timeout_Handler_Id;
      pragma Import (C, Internal, "gtk_timeout_add");
   begin
      return Internal (Interval, Func, System.Null_Address);
   end Timeout_Add;

   ----------
   -- Idle --
   ----------

   package body Idle is

      type Data_Type_Access is access Data_Type;
      type Cb_Record is
         record
            Func : Callback;
            Data : Data_Type_Access;
         end record;
      type Cb_Record_Access is access Cb_Record;

      procedure Free_Data (D : in System.Address);
      pragma Convention (C, Free_Data);

      function Convert is new Unchecked_Conversion
        (System.Address, Cb_Record_Access);

      function General_Cb (D : in System.Address) return Gint;
      pragma Convention (C, General_Cb);

      ----------
      -- Free --
      ----------

      procedure Free_Data (D : in System.Address) is
         procedure Internal is new Unchecked_Deallocation
           (Cb_Record, Cb_Record_Access);
         procedure Internal2 is new Unchecked_Deallocation
           (Data_Type, Data_Type_Access);
         Data : Cb_Record_Access := Convert (D);
      begin
         Internal2 (Data.Data);
         Internal (Data);
      end Free_Data;

      ----------------
      -- General_Cb --
      ----------------

      function General_Cb (D : in System.Address) return Gint is
         Data : Cb_Record_Access := Convert (D);
      begin
         return Boolean'Pos (Data.Func (Data.Data.all));
      end General_Cb;

      ---------
      -- Add --
      ---------

      function Add (Cb       : in Callback;
                    D        : in Data_Type;
                    Priority : in Idle_Priority := Priority_Default_Idle)
                   return Idle_Handler_Id
      is
         function Internal (Priority : in Idle_Priority;
                            Func     : in System.Address;
                            Marshal  : in System.Address;
                            Data     : in System.Address;
                            Destroy  : in System.Address)
                            return        Idle_Handler_Id;
         pragma Import (C, Internal, "gtk_idle_add_full");
         function Convert is new Unchecked_Conversion
           (Cb_Record_Access, System.Address);
         Data : Cb_Record_Access := new Cb_Record'(Func => Cb,
                                                   Data => new Data_Type'(D));
      begin
         return Internal (Priority, General_Cb'Address, System.Null_Address,
                          Convert (Data), Free_Data'Address);
      end Add;

   end Idle;

   -------------
   -- Timeout --
   -------------

   package body Timeout is

      type Data_Type_Access is access Data_Type;
      type Cb_Record is
         record
            Func : Callback;
            Data : Data_Type_Access;
         end record;
      type Cb_Record_Access is access Cb_Record;

      procedure Free_Data (D : in System.Address);
      pragma Convention (C, Free_Data);

      function Convert is new Unchecked_Conversion
        (System.Address, Cb_Record_Access);

      function General_Cb (D : in System.Address) return Gint;
      pragma Convention (C, General_Cb);

      ----------
      -- Free --
      ----------

      procedure Free_Data (D : in System.Address) is
         procedure Internal is new Unchecked_Deallocation
           (Cb_Record, Cb_Record_Access);
         procedure Internal2 is new Unchecked_Deallocation
           (Data_Type, Data_Type_Access);
         Data : Cb_Record_Access := Convert (D);
      begin
         Internal2 (Data.Data);
         Internal (Data);
      end Free_Data;

      ----------------
      -- General_Cb --
      ----------------

      function General_Cb (D : in System.Address) return Gint is
         Data : Cb_Record_Access := Convert (D);
      begin
         return Boolean'Pos (Data.Func (Data.Data.all));
      end General_Cb;

      ---------
      -- Add --
      ---------

      function Add (Interval : in Guint32;
                    Func     : in Callback;
                    D : in Data_Type)
                    return Timeout_Handler_Id
      is
         function Internal (Interval : in Guint32;
                            Func     : in System.Address;
                            Marshal  : in System.Address;
                            Data     : in System.Address;
                            Destroy  : in System.Address)
                            return        Timeout_Handler_Id;
         pragma Import (C, Internal, "gtk_timeout_add_full");
         function Convert is new Unchecked_Conversion
           (Cb_Record_Access, System.Address);
         Data : Cb_Record_Access := new Cb_Record'(Func => Func,
                                                   Data => new Data_Type'(D));
      begin
         return Internal (Interval, General_Cb'Address, System.Null_Address,
                          Convert (Data), Free_Data'Address);
      end Add;

   end Timeout;

end Gtk.Main;
