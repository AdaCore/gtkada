------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 1998-2018, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

package body Glib.GSlist is

   package body Generic_SList is
      -----------
      -- Alloc --
      -----------

      procedure Alloc (List : out GSlist) is
         function Internal return System.Address;
         pragma Import (C, Internal, "g_slist_alloc");
      begin
         Set_Object (List, Internal);
      end Alloc;

      ------------
      -- Append --
      ------------

      procedure Append
        (List : in out GSlist;
         Data : Gpointer)
      is
         function Internal (List : System.Address;
                            Data : System.Address)
                            return System.Address;
         pragma Import (C, Internal, "g_slist_append");
      begin
         Set_Object (List, Internal (Get_Object (List),
                                     Convert (Data)));
      end Append;

      ------------
      -- Concat --
      ------------

      function Concat
        (List1 : GSlist;
         List2 : GSlist)
         return GSlist
      is
         function Internal (List1 : System.Address;
                            List2 : System.Address)
                            return System.Address;
         pragma Import (C, Internal, "g_slist_append");
         Tmp : GSlist;
      begin
         Set_Object (Tmp, Internal (Get_Object (List1),
                                    Get_Object (List2)));
         return Tmp;
      end Concat;

      --------------
      -- Get_Data --
      --------------

      function Get_Data (List : GSlist) return Gpointer is
         function Internal (List : System.Address) return System.Address;
         pragma Import (C, Internal, "ada_gslist_get_data");
      begin
         return Convert (Internal (Get_Object (List)));
      end Get_Data;

      ----------------------
      -- Get_Data_Address --
      ----------------------

      function Get_Data_Address (List : GSlist) return System.Address is
         function Internal (List : System.Address) return System.Address;
         pragma Import (C, Internal, "ada_slist_get_data");
      begin
         return Internal (Get_Object (List));
      end Get_Data_Address;

      ------------
      -- Insert --
      ------------

      procedure Insert
        (List     : in out GSlist;
         Data     :        Gpointer;
         Position :        Gint)
      is
         function Internal (List : System.Address;
                            Data : System.Address;
                            Pos  : Gint)
                            return System.Address;
         pragma Import (C, Internal, "g_slist_insert");
      begin
         Set_Object (List, Internal (Get_Object (List),
                                     Convert (Data), Position));
      end Insert;

      ----------
      -- Find --
      ----------

      function Find
        (List : GSlist;
         Data : Gpointer)
         return GSlist
      is
         function Internal (List : System.Address;
                            Data : System.Address)
                            return System.Address;
         pragma Import (C, Internal, "g_slist_find");
         Tmp : GSlist;
      begin
         Set_Object (Tmp, Internal (Get_Object (List),
                                    Convert (Data)));
         return Tmp;
      end Find;

      ----------
      -- Free --
      ----------

      procedure Free (List : in out GSlist) is
         procedure Internal (List : System.Address);
         pragma Import (C, Internal, "g_slist_free");
      begin
         Internal (Get_Object (List));
      end Free;

      ----------------
      -- Get_Object --
      ----------------

      function Get_Object (Obj : GSlist) return System.Address is
      begin
         return Obj.Ptr;
      end Get_Object;

      -----------
      -- Index --
      -----------

      function Index
        (List : GSlist;
         Data : Gpointer)
         return Gint
      is
         function Internal (List : System.Address;
                            Data : System.Address)
                            return Gint;
         pragma Import (C, Internal, "g_slist_index");
      begin
         return Internal (Get_Object (List),
                          Convert (Data));
      end Index;

      ----------
      -- Last --
      ----------

      function Last (List : GSlist) return GSlist is
         function Internal (List : System.Address) return System.Address;
         pragma Import (C, Internal, "g_slist_last");
         Tmp : GSlist;
      begin
         Set_Object (Tmp, Internal (Get_Object (List)));
         return Tmp;
      end Last;

      ------------
      -- Length --
      ------------

      function Length (List : GSlist) return Guint is
         function Internal (List : System.Address) return Guint;
         pragma Import (C, Internal, "g_slist_length");
      begin
         return Internal (Get_Object (List));
      end Length;

      ------------------
      -- List_Reverse --
      ------------------

      procedure List_Reverse (List : in out GSlist) is
         function Internal (List : System.Address) return System.Address;
         pragma Import (C, Internal, "g_slist_reverse");
      begin
         Set_Object (List, Internal (Get_Object (List)));
      end List_Reverse;

      ----------
      -- Next --
      ----------

      function Next (List : GSlist) return GSlist is
         function Internal (List : System.Address) return System.Address;
         pragma Import (C, Internal, "ada_gslist_next");
         Tmp : GSlist;
      begin
         Set_Object (Tmp, Internal (Get_Object (List)));
         return Tmp;
      end Next;

      ---------
      -- Nth --
      ---------

      function Nth
        (List : GSlist;
         N    : Guint)
         return GSlist
      is
         function Internal (List : System.Address;
                            N    : Guint)
                            return System.Address;
         pragma Import (C, Internal, "g_slist_nth");
         Tmp : GSlist;
      begin
         Set_Object (Tmp, Internal (Get_Object (List), N));
         return Tmp;
      end Nth;

      --------------
      -- Nth_Data --
      --------------

      function Nth_Data
        (List : GSlist;
         N    : Guint)
         return Gpointer
      is
         function Internal (List : System.Address;
                            N    : Guint)
                            return System.Address;
         pragma Import (C, Internal, "g_slist_nth_data");
      begin
         return Convert (Internal (Get_Object (List), N));
      end Nth_Data;

      --------------
      -- Position --
      --------------

      function Position
        (List : GSlist;
         Link : GSlist)
         return Gint
      is
         function Internal (List : System.Address;
                            Link : System.Address)
                            return Gint;
         pragma Import (C, Internal, "g_slist_position");
      begin
         return Internal (Get_Object (List), Get_Object (Link));
      end Position;

      -------------
      -- Prepend --
      -------------

      procedure Prepend
        (List : in out GSlist;
         Data : Gpointer)
      is
         function Internal (List : System.Address;
                            Data : System.Address)
                            return System.Address;
         pragma Import (C, Internal, "g_slist_prepend");
      begin
         Set_Object (List, Internal (Get_Object (List),
                                     Convert (Data)));
      end Prepend;

      ------------
      -- Remove --
      ------------

      procedure Remove
        (List : in out GSlist;
         Data : Gpointer)
      is
         function Internal (List : System.Address;
                            Data : System.Address)
                            return System.Address;
         pragma Import (C, Internal, "g_slist_remove");
      begin
         Set_Object (List, Internal (Get_Object (List),
                                     Convert (Data)));
      end Remove;

      -----------------
      -- Remove_Link --
      -----------------

      procedure Remove_Link
        (List : in out GSlist;
         Link : GSlist)
      is
         function Internal (List : System.Address;
                            Link : System.Address)
                            return System.Address;
         pragma Import (C, Internal, "g_slist_remove_link");
      begin
         Set_Object (List, Internal (Get_Object (List),
                                     Get_Object (Link)));
      end Remove_Link;

      ----------------
      -- Set_Object --
      ----------------

      procedure Set_Object (Obj   : in out GSlist;
                            Value :        System.Address) is
      begin
         Obj.Ptr := Value;
      end Set_Object;

   end Generic_SList;

end Glib.GSlist;
