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

with Unchecked_Conversion;
with Unchecked_Deallocation;
with System;
with Gdk; use Gdk;
with Gtk.Util; use Gtk.Util;
with Gtkada.Types;

package body Gtk.Object is

   ----------------------
   -- Connected_Is_Set --
   ----------------------

   function Connected_Is_Set (Object : access Gtk_Object_Record'Class)
                             return Boolean is
   begin
      return Flag_Is_Set (Object, Connected);
   end Connected_Is_Set;

   ------------------------
   -- Constructed_Is_Set --
   ------------------------

   function Constructed_Is_Set (Object : access Gtk_Object_Record'Class)
                               return Boolean is
   begin
      return Flag_Is_Set (Object, Constructed);
   end Constructed_Is_Set;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Object : access Gtk_Object_Record) is
      procedure Internal  (Object : in System.Address);
      pragma Import (C, Internal, "gtk_object_destroy");
      procedure Unref_Internal (Object : in System.Address);
      pragma Import (C, Unref_Internal, "gtk_object_unref");
      Ptr : System.Address := Get_Object (Object);
   begin
      --  Keep a reference on the object, so that the Ada structure is
      --  never automatically deleted when the C object is.
      --  We can't reset the content of Object to System.Null_Address before
      --  calling the C function, because we want the user's destroy callbacks
      --  to be called with the appropriate object.
      Ref (Object);
      Internal (Ptr);

      --  We then can make sure that the object won't be referenced any more,
      --  (The Ada structure won't be free before the ref count goes down to
      --  0, and we don't want the user to use a deleted object...).
      Set_Object (Object, System.Null_Address);

      --  Free the reference we had. In most cases, this results in the object
      --  being freed. We can't use directly Unref, since the Ptr field for
      --  Object is Null_Address.
      Unref_Internal (Ptr);
   end Destroy;

   ---------------
   -- Destroyed --
   ---------------

   function Destroyed_Is_Set (Object : access Gtk_Object_Record'Class)
                             return Boolean is
   begin
      return Flag_Is_Set (Object, Destroyed);
   end Destroyed_Is_Set;

   -----------
   -- Flags --
   -----------

   function Flags (Object : access Gtk_Object_Record) return Guint32 is
      function Internal (Object : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_object_flags");
   begin
      return Internal (Get_Object (Object.all));
   end Flags;

   --------------
   -- Floating --
   --------------

   function Floating_Is_Set (Object : access Gtk_Object_Record'Class)
                            return Boolean is
   begin
      return Flag_Is_Set (Object, Floating);
   end Floating_Is_Set;

   --------------
   -- Get_Type --
   --------------

   function Get_Type (Object : access Gtk_Object_Record) return Gtk_Type is
      function Internal (Object : in System.Address) return Gtk_Type;
      pragma Import (C, Internal, "ada_object_get_type");
   begin
      return Internal (Get_Object (Object.all));
   end Get_Type;

   -----------------------------
   -- Initialize_Class_Record --
   -----------------------------

   procedure Initialize_Class_Record
     (Object       : access Gtk_Object_Record'Class;
      Signals      : Signal_Array;
      Class_Record : in out System.Address)
   is
      function Internal (Object       : System.Address;
                         NSignals     : Gint;
                         Signals      : System.Address;
                         Class_Record : System.Address)
        return System.Address;
      pragma Import (C, Internal, "ada_initialize_class_record");

   begin
      Class_Record := Internal (Get_Object (Object),
                                Signals'Length,
                                Signals (Signals'First)'Address,
                                Class_Record);
   end Initialize_Class_Record;

   ---------
   -- Ref --
   ---------

   procedure Ref (Object : access Gtk_Object_Record) is
      procedure Internal (Object : in System.Address);
      pragma Import (C, Internal, "gtk_object_ref");
      use type System.Address;
   begin
      if Get_Object (Object.all) /= System.Null_Address then
         Internal (Get_Object (Object.all));
      end if;
   end Ref;

   -----------------
   --  Set_Flags  --
   -----------------

   procedure Set_Flags (Object : access Gtk_Object_Record;
                        Flags  : in     Guint32) is
      procedure Internal (Object : in System.Address;
                          Flags  : in Guint32);
      pragma Import (C, Internal, "ada_object_set_flags");
   begin
      Internal (Get_Object (Object.all), Flags);
   end Set_Flags;

   ----------
   -- Sink --
   ----------

   procedure Sink (Object : access Gtk_Object_Record) is
      procedure Internal (Object : in System.Address);
      pragma Import (C, Internal, "gtk_object_sink");
   begin
      Internal (Get_Object (Object));
   end Sink;

   -----------
   -- Unref --
   -----------

   procedure Unref (Object : access Gtk_Object_Record) is
      procedure Internal (Object : in System.Address);
      pragma Import (C, Internal, "gtk_object_unref");
      use type System.Address;
   begin
      if Get_Object (Object) /= System.Null_Address then
         Internal (Get_Object (Object));
      end if;
   end Unref;

   -------------------
   --  Unset_Flags  --
   -------------------

   procedure Unset_Flags (Object : access Gtk_Object_Record;
                          Flags  : in     Guint32) is
      procedure Internal (Object : in System.Address;
                          Flags  : in Guint32);
      pragma Import (C, Internal, "ada_object_unset_flags");
   begin
      Internal (Get_Object (Object), Flags);
   end Unset_Flags;

   -----------------
   -- Flag_Is_Set --
   -----------------

   function Flag_Is_Set (Object : access Gtk_Object_Record;
                         Flag   : in     Guint32)
                        return Boolean
   is
      function Internal (Object : in System.Address;
                         Flag   : in Guint32)
                        return Gint;
      pragma Import (C, Internal, "ada_object_flag_is_set");
   begin
      return Boolean'Val (Internal (Get_Object (Object), Flag));
   end Flag_Is_Set;

   ---------------
   -- User_Data --
   ---------------

   package body User_Data is
      type Data_Access is access all Data_Type;
      type Cb_Record is
         record
            Ptr      : Data_Access;
         end record;
      type Cb_Record_Access is access all Cb_Record;

      function Convert is new Unchecked_Conversion (System.Address,
                                                    Cb_Record_Access);
      procedure Free_Data (Data : in System.Address);
      pragma Convention (C, Free_Data);

      ----------
      -- Free --
      ----------

      procedure Free_Data (Data : in System.Address) is
         procedure Internal is new Unchecked_Deallocation (Cb_Record,
                                                           Cb_Record_Access);
         procedure Internal2 is new Unchecked_Deallocation (Data_Type,
                                                            Data_Access);
         D : Cb_Record_Access := Convert (Data);
      begin
         Internal2 (D.Ptr);
         Internal (D);
      end Free_Data;

      ---------
      -- Get --
      ---------

      function Get (Object : access Gtk_Object_Record'Class;
                    Id     : in String := "user_data") return Data_Type
      is
         function Internal (Object : in System.Address;
                            Key    : in String)
                            return System.Address;
         pragma Import (C, Internal, "gtk_object_get_data");
         D : Cb_Record_Access
           := Convert (Internal (Get_Object (Object),
                                 Id & ASCII.Nul));
      begin
         return D.Ptr.all;
      exception
         when Constraint_Error =>
            raise Gtkada.Types.Data_Error;
      end Get;

      ---------
      -- Get --
      ---------

      function Get (Object : access Gtk_Object_Record'Class;
                    Id     : in Glib.GQuark)
                   return Data_Type
      is
         function Internal (Object : in System.Address;
                            Key    : in Glib.GQuark)
                            return System.Address;
         pragma Import (C, Internal, "gtk_object_get_data_by_id");
         D : Cb_Record_Access := Convert (Internal (Get_Object (Object), Id));
      begin
         return D.Ptr.all;
      exception
         when Constraint_Error =>
            raise Gtkada.Types.Data_Error;
      end Get;

      ---------
      -- Set --
      ---------

      procedure Set (Object : access Gtk_Object_Record'Class;
                     Data   : in Data_Type;
                     Id     : in String := "user_data")
      is
         function Convert is new Unchecked_Conversion (Cb_Record_Access,
                                                       System.Address);
         procedure Internal (Object  : in System.Address;
                             Key     : in String;
                             Data    : in System.Address;
                             Destroy : in System.Address);
         pragma Import (C, Internal, "gtk_object_set_data_full");
         D : Cb_Record_Access := new Cb_Record'(Ptr => new Data_Type'(Data));
      begin
         Internal (Get_Object (Object.all),
                   Id & ASCII.NUL,
                   Convert (D),
                   Free_Data'Address);
      end Set;

      ---------
      -- Set --
      ---------

      procedure Set (Object : access Gtk_Object_Record'Class;
                     Data   : in Data_Type;
                     Id     : in Glib.GQuark)
      is
         function Convert is new Unchecked_Conversion (Cb_Record_Access,
                                                       System.Address);
         procedure Internal (Object  : in System.Address;
                             Key     : in Glib.GQuark;
                             Data    : in System.Address;
                             Destroy : in System.Address);
         pragma Import (C, Internal, "gtk_object_set_data_by_id_full");
         D : Cb_Record_Access := new Cb_Record'(Ptr => new Data_Type'(Data));
      begin
         Internal (Get_Object (Object.all),
                   Id,
                   Convert (D),
                   Free_Data'Address);
      end Set;

      ------------
      -- Remove --
      ------------

      procedure Remove (Object : access Gtk_Object_Record'Class;
                        Id     : in String := "user_data")
      is
         procedure Internal (Object : System.Address;
                             Id     : String);
         pragma Import (C, Internal, "gtk_object_remove_data");
      begin
         Internal (Get_Object (Object), Id & ASCII.NUL);
      end Remove;

      ------------
      -- Remove --
      ------------

      procedure Remove (Object : access Gtk_Object_Record'Class;
                        Id     : in Glib.GQuark)
      is
         procedure Internal (Object : System.Address;
                             Id     : Glib.GQuark);
         pragma Import (C, Internal, "gtk_object_remove_data_by_id");
      begin
         Internal (Get_Object (Object), Id);
      end Remove;

   end User_Data;

   --------------
   -- Generate --
   --------------

   procedure Generate (N      : in Node_Ptr;
                       File   : in File_Type) is
   begin
      null;
   end Generate;

   procedure Generate (Object : in out Gtk_Object;
                       N      : in Node_Ptr) is
   begin
      --  If Object is null at this point, it means that it has already been
      --  created and registered

      if Object = null then
         Object := Get_Object (Get_Field (N, "name"));
      end if;
   end Generate;

end Gtk.Object;
