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

with Interfaces.C.Strings;
with Unchecked_Deallocation;
with Unchecked_Conversion;
with System;
with Gdk; use Gdk;

package body Gtk.Main is

   package C renames Interfaces.C;

   --------------
   -- Grab_Add --
   --------------

   procedure Grab_Add (Widget : Gtk.Widget.Gtk_Widget'Class) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_grab_add");
   begin
      Internal (Get_Object (Widget));
   end Grab_Add;

   -----------------
   -- Grab_Remove --
   -----------------

   procedure Grab_Remove (Widget : Gtk.Widget.Gtk_Widget'Class) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_grab_remove");
   begin
      Internal (Get_Object (Widget));
   end Grab_Remove;

   ----------------
   -- Set_Locale --
   ----------------

   function Set_Locale return String is
      function Internal return C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_set_locale");
   begin
      return C.Strings.Value (Internal);
   end Set_Locale;

   ----------------
   -- Set_Locale --
   ----------------

   procedure Set_Locale is
      Dummy : constant String := Set_Locale;
      pragma Warnings (Off, Dummy);
   begin
      null;
   end Set_Locale;

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

      function Add (Cb : in Callback;  D : in Data_Type) return Guint is
         function Internal (Priority : in Gint;
                            Func     : in System.Address;
                            Marshal  : in System.Address;
                            Data     : in System.Address;
                            Destroy  : in System.Address)
                            return        Guint;
         pragma Import (C, Internal, "gtk_idle_add_full");
         function Convert is new Unchecked_Conversion
           (Cb_Record_Access, System.Address);
         Data : Cb_Record_Access := new Cb_Record'(Func => Cb,
                                                   Data => new Data_Type'(D));
      begin
         return Internal (0, General_Cb'Address, System.Null_Address,
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
                    return Guint
      is
         function Internal (Interval : in Guint32;
                            Func     : in System.Address;
                            Marshal  : in System.Address;
                            Data     : in System.Address;
                            Destroy  : in System.Address)
                            return        Guint;
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
