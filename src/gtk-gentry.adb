

with Interfaces.C.Strings;

package body Gtk.GEntry is

   -----------------
   -- Append_Text --
   -----------------

   procedure Append_Text
      (The_Entry : in Gtk_Entry'Class;
       Text      : in String)
   is
      procedure Internal
         (The_Entry : in System.Address;
          Text  : in String);
      pragma Import (C, Internal, "gtk_entry_append_text");
   begin
      Internal (Get_Object (The_Entry),
                Text & Ascii.NUL);
   end Append_Text;

   --------------
   -- Get_Text --
   --------------

   function Get_Text (The_Entry : in Gtk_Entry'Class)
                      return      String
   is
      function Internal (The_Entry : in System.Address)
                         return      Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_entry_get_text");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (The_Entry)));
   end Get_Text;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Entry;
                      Max    : in Guint16)
   is
      function Internal (Max    : in Guint16)
                         return      System.Address;
      pragma Import (C, Internal, "gtk_entry_new_with_max_length");
   begin
      Set_Object (Widget, Internal (Max));
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Entry)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_entry_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

   ------------------
   -- Prepend_Text --
   ------------------

   procedure Prepend_Text
      (The_Entry : in Gtk_Entry'Class;
       Text      : in String)
   is
      procedure Internal
         (The_Entry : in System.Address;
          Text      : in String);
      pragma Import (C, Internal, "gtk_entry_prepend_text");
   begin
      Internal (Get_Object (The_Entry),
                Text & Ascii.NUL);
   end Prepend_Text;

   -------------------
   -- Select_Region --
   -------------------

   procedure Select_Region
      (The_Entry : in Gtk_Entry'Class;
       Start     : in Gint;
       The_End  : in Gint)
   is
      procedure Internal
         (The_Entry : in System.Address;
          Start     : in Gint;
          The_End   : in Gint);
      pragma Import (C, Internal, "gtk_entry_select_region");
   begin
      Internal (Get_Object (The_Entry),
                Start,
                The_End);
   end Select_Region;

   ------------------
   -- Set_Editable --
   ------------------

   procedure Set_Editable
      (The_Entry : in Gtk_Entry'Class;
       Editable  : in Boolean)
   is
      procedure Internal
         (The_Entry : in System.Address;
          Editable  : in Gint);
      pragma Import (C, Internal, "gtk_entry_set_editable");
   begin
      Internal (Get_Object (The_Entry),
                Boolean'Pos (Editable));
   end Set_Editable;

   --------------------
   -- Set_Max_Length --
   --------------------

   procedure Set_Max_Length
      (The_Entry : in Gtk_Entry'Class;
       Max       : in Guint16)
   is
      procedure Internal
         (The_Entry : in System.Address;
          Max       : in Guint16);
      pragma Import (C, Internal, "gtk_entry_set_max_length");
   begin
      Internal (Get_Object (The_Entry),
                Max);
   end Set_Max_Length;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position
      (The_Entry : in Gtk_Entry'Class;
       Position  : in Gint)
   is
      procedure Internal
         (The_Entry : in System.Address;
          Position  : in Gint);
      pragma Import (C, Internal, "gtk_entry_set_position");
   begin
      Internal (Get_Object (The_Entry),
                Position);
   end Set_Position;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
      (The_Entry : in Gtk_Entry'Class;
       Text      : in String)
   is
      procedure Internal
         (The_Entry : in System.Address;
          Text      : in String);
      pragma Import (C, Internal, "gtk_entry_set_text");
   begin
      Internal (Get_Object (The_Entry),
                Text & Ascii.NUL);
   end Set_Text;

   --------------------
   -- Set_Visibility --
   --------------------

   procedure Set_Visibility
      (The_Entry : in Gtk_Entry'Class;
       Visible   : in Boolean)
   is
      procedure Internal
         (The_Entry : in System.Address;
          Visible   : in Gint);
      pragma Import (C, Internal, "gtk_entry_set_visibility");
   begin
      Internal (Get_Object (The_Entry),
                Boolean'Pos (Visible));
   end Set_Visibility;

end Gtk.GEntry;
