with Gtk;

package body Glib.GSlist is

   -------------
   -- Convert --
   -------------

   function Convert (W : Gtk.Widget.Gtk_Widget'Class) return System.Address is
   begin
      return Gtk.Get_Object (W);
   end Convert;

   function Convert (W : System.Address) return Gtk.Widget.Gtk_Widget'Class is
      Widget : Gtk.Widget.Gtk_Widget;
   begin
      Gtk.Set_Object (Widget, W);
      return Widget;
   end Convert;


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
         Data : in Gpointer)
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
        (List1 : in GSlist;
         List2 : in GSlist)
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

      function Get_Data (List : in GSlist)
                         return Gpointer
      is
         function Internal (List : System.Address) return System.Address;
         pragma Import (C, Internal, "ada_gslist_get_data");
      begin
         return Convert (Internal (Get_Object (List)));
      end Get_Data;

      ------------
      -- Insert --
      ------------

      procedure Insert
        (List     : in out GSlist;
         Data     : in     Gpointer;
         Position : in     Gint)
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
        (List : in GSlist;
         Data : in Gpointer)
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

      function Get_Object (Obj : in GSlist)
                           return System.Address is
      begin
         return Obj.Ptr;
      end Get_Object;

      -----------
      -- Index --
      -----------

      function Index
        (List : in GSlist;
         Data : in Gpointer)
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

      function Last (List : in GSlist) return GSlist is
         function Internal (List : System.Address)
                            return System.Address;
         pragma Import (C, Internal, "g_slist_last");
         Tmp : GSlist;
      begin
         Set_Object (Tmp, Internal (Get_Object (List)));
         return Tmp;
      end Last;

      ------------
      -- Length --
      ------------

      function Length (List : in GSlist) return Guint is
         function Internal (List : System.Address)
                            return Guint;
         pragma Import (C, Internal, "g_slist_length");
      begin
         return Internal (Get_Object (List));
      end Length;

      ------------------
      -- List_Reverse --
      ------------------

      procedure List_Reverse (List : in out GSlist) is
         function Internal (List : System.Address)
                            return System.Address;
         pragma Import (C, Internal, "g_slist_reverse");
      begin
         Set_Object (List, Internal (Get_Object (List)));
      end List_Reverse;

      ----------
      -- Next --
      ----------

      function Next (List : in GSlist) return GSlist is
         function Internal (List : System.Address)
                            return System.Address;
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
        (List : in GSlist;
         N    : in Guint)
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
        (List : in GSlist;
         N    : in Guint)
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
        (List : in GSlist;
         Link : in GSlist)
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
         Data : in Gpointer)
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
         Data : in Gpointer)
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
         Link : in GSlist)
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
                            Value : in     System.Address) is
      begin
         Obj.Ptr := Value;
      end Set_Object;

   end Generic_SList;
end Glib.GSlist;

