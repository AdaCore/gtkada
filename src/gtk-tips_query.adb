

package body Gtk.Tips_Query is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Tips_Query)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_tips_query_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

   ----------------
   -- Set_Caller --
   ----------------

   procedure Set_Caller
      (Tips_Query : in Gtk_Tips_Query'Class;
       Caller     : in Gtk.Widget.Gtk_Widget'Class)
   is
      procedure Internal
         (Tips_Query : in System.Address;
          Caller     : in System.Address);
      pragma Import (C, Internal, "gtk_tips_query_set_caller");
   begin
      Internal (Get_Object (Tips_Query),
                Get_Object (Caller));
   end Set_Caller;

   ----------------
   -- Set_Labels --
   ----------------

   procedure Set_Labels
      (Tips_Query     : in Gtk_Tips_Query'Class;
       Label_Inactive : in String;
       Label_No_Tip   : in String)
   is
      procedure Internal
         (Tips_Query     : in System.Address;
          Label_Inactive : in String;
          Label_No_Tip   : in String);
      pragma Import (C, Internal, "gtk_tips_query_set_labels");
   begin
      Internal (Get_Object (Tips_Query),
                Label_Inactive & Ascii.NUL,
                Label_No_Tip & Ascii.NUL);
   end Set_Labels;

   -----------------
   -- Start_Query --
   -----------------

   procedure Start_Query (Tips_Query : in Gtk_Tips_Query'Class)
   is
      procedure Internal (Tips_Query : in System.Address);
      pragma Import (C, Internal, "gtk_tips_query_start_query");
   begin
      Internal (Get_Object (Tips_Query));
   end Start_Query;

   ----------------
   -- Stop_Query --
   ----------------

   procedure Stop_Query (Tips_Query : in Gtk_Tips_Query'Class)
   is
      procedure Internal (Tips_Query : in System.Address);
      pragma Import (C, Internal, "gtk_tips_query_stop_query");
   begin
      Internal (Get_Object (Tips_Query));
   end Stop_Query;

end Gtk.Tips_Query;
