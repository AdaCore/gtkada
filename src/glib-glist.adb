with Unchecked_Conversion;
with Gtk;
with Interfaces.C.Strings;

package body Glib.Glist is

   -------------
   -- Convert --
   -------------

   function Convert (S : String) return System.Address is
      function Internal is new Unchecked_Conversion
        (Interfaces.C.Strings.chars_ptr, System.Address);
   begin
      return Internal (Interfaces.C.Strings.New_String (S));
   end Convert;

   function Convert (S : System.Address) return String is
      function Internal is new Unchecked_Conversion
        (System.Address, Interfaces.C.Strings.chars_ptr);
   begin
      return Interfaces.C.Strings.Value (Internal (S));
   end Convert;

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


   package body Generic_List is
      -----------
      -- Alloc --
      -----------

      procedure Alloc (List : out Glist) is
         function Internal return System.Address;
         pragma Import (C, Internal, "g_list_alloc");
      begin
         Set_Object (List, Internal);
      end Alloc;

      ------------
      -- Append --
      ------------

      procedure Append
        (List : in out Glist;
         Data : in Gpointer)
      is
         function Internal (List : System.Address;
                            Data : System.Address)
                            return System.Address;
         pragma Import (C, Internal, "g_list_append");
      begin
         Set_Object (List, Internal (Get_Object (List),
                                     Convert (Data)));
      end Append;

      ------------
      -- Concat --
      ------------

      function Concat
        (List1 : in Glist;
         List2 : in Glist)
         return Glist
      is
         function Internal (List1 : System.Address;
                            List2 : System.Address)
                            return System.Address;
         pragma Import (C, Internal, "g_list_append");
         Tmp : Glist;
      begin
         Set_Object (Tmp, Internal (Get_Object (List1),
                                    Get_Object (List2)));
         return Tmp;
      end Concat;

      ------------
      -- Insert --
      ------------

      procedure Insert
        (List     : in out Glist;
         Data     : in     Gpointer;
         Position : in     Gint)
      is
         function Internal (List : System.Address;
                            Data : System.Address;
                            Pos  : Gint)
                            return System.Address;
         pragma Import (C, Internal, "g_list_insert");
      begin
         Set_Object (List, Internal (Get_Object (List),
                                     Convert (Data), Position));
      end Insert;

      ----------
      -- Find --
      ----------

      function Find
        (List : in Glist;
         Data : in Gpointer)
         return Glist
      is
         function Internal (List : System.Address;
                            Data : System.Address)
                            return System.Address;
         pragma Import (C, Internal, "g_list_find");
         Tmp : Glist;
      begin
         Set_Object (Tmp, Internal (Get_Object (List),
                                    Convert (Data)));
         return Tmp;
      end Find;

      -----------
      -- First --
      -----------

      function First (List : in Glist)
                      return Glist
      is
         function Internal (List : System.Address)
                            return System.Address;
         pragma Import (C, Internal, "g_list_first");
         Tmp : Glist;
      begin
         Set_Object (Tmp, Internal (Get_Object (List)));
         return Tmp;
      end First;

      ----------
      -- Free --
      ----------

      procedure Free (List : in out Glist) is
         procedure Internal (List : System.Address);
         pragma Import (C, Internal, "g_list_free");
      begin
         Internal (Get_Object (List));
      end Free;

      ----------------
      -- Get_Object --
      ----------------

      function Get_Object (Obj : in Glist)
                           return System.Address is
      begin
         return Obj.Ptr;
      end Get_Object;

      -----------
      -- Index --
      -----------

      function Index
        (List : in Glist;
         Data : in Gpointer)
         return Gint
      is
         function Internal (List : System.Address;
                            Data : System.Address)
                            return Gint;
         pragma Import (C, Internal, "g_list_index");
      begin
         return Internal (Get_Object (List),
                          Convert (Data));
      end Index;

      ----------
      -- Last --
      ----------

      function Last (List : in Glist) return Glist is
         function Internal (List : System.Address)
                            return System.Address;
         pragma Import (C, Internal, "g_list_last");
         Tmp : Glist;
      begin
         Set_Object (Tmp, Internal (Get_Object (List)));
         return Tmp;
      end Last;

      ------------
      -- Length --
      ------------

      function Length (List : in Glist) return Guint is
         function Internal (List : System.Address)
                            return Guint;
         pragma Import (C, Internal, "g_list_length");
      begin
         return Internal (Get_Object (List));
      end Length;

      ------------------
      -- List_Reverse --
      ------------------

      procedure List_Reverse (List : in out Glist) is
         function Internal (List : System.Address)
                            return System.Address;
         pragma Import (C, Internal, "g_list_reverse");
      begin
         Set_Object (List, Internal (Get_Object (List)));
      end List_Reverse;

      ---------
      -- Nth --
      ---------

      function Nth
        (List : in Glist;
         N    : in Guint)
         return Glist
      is
         function Internal (List : System.Address;
                            N    : Guint)
                            return System.Address;
         pragma Import (C, Internal, "g_list_nth");
         Tmp : Glist;
      begin
         Set_Object (Tmp, Internal (Get_Object (List), N));
         return Tmp;
      end Nth;

      --------------
      -- Nth_Data --
      --------------

      function Nth_Data
        (List : in Glist;
         N    : in Guint)
         return Gpointer
      is
         function Internal (List : System.Address;
                            N    : Guint)
                            return System.Address;
         pragma Import (C, Internal, "g_list_nth_data");
      begin
         return Convert (Internal (Get_Object (List), N));
      end Nth_Data;

      --------------
      -- Position --
      --------------

      function Position
        (List : in Glist;
         Link : in Glist)
         return Gint
      is
         function Internal (List : System.Address;
                            Link : System.Address)
                            return Gint;
         pragma Import (C, Internal, "g_list_position");
      begin
         return Internal (Get_Object (List), Get_Object (Link));
      end Position;

      -------------
      -- Prepend --
      -------------

      procedure Prepend
        (List : in out Glist;
         Data : in Gpointer)
      is
         function Internal (List : System.Address;
                            Data : System.Address)
                            return System.Address;
         pragma Import (C, Internal, "g_list_prepend");
      begin
         Set_Object (List, Internal (Get_Object (List),
                                     Convert (Data)));
      end Prepend;

      ------------
      -- Remove --
      ------------

      procedure Remove
        (List : in out Glist;
         Data : in Gpointer)
      is
         function Internal (List : System.Address;
                            Data : System.Address)
                            return System.Address;
         pragma Import (C, Internal, "g_list_remove");
      begin
         Set_Object (List, Internal (Get_Object (List),
                                     Convert (Data)));
      end Remove;

      -----------------
      -- Remove_Link --
      -----------------

      procedure Remove_Link
        (List : in out Glist;
         Link : in Glist)
      is
         function Internal (List : System.Address;
                            Link : System.Address)
                            return System.Address;
         pragma Import (C, Internal, "g_list_remove_link");
      begin
         Set_Object (List, Internal (Get_Object (List),
                                     Get_Object (Link)));
      end Remove_Link;

      ----------------
      -- Set_Object --
      ----------------

      procedure Set_Object (Obj   : in out Glist;
                            Value : in     System.Address) is
      begin
         Obj.Ptr := Value;
      end Set_Object;

   end Generic_List;
end Glib.Glist;

