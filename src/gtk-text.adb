
with Interfaces.C.Strings;
with Gdk; use Gdk;

package body Gtk.Text is

   ---------------------
   -- Backward_Delete --
   ---------------------

   function Backward_Delete
      (Text   : in Gtk_Text'Class;
       Nchars : in Guint)
       return      Gint
   is
      function Internal
         (Text   : in System.Address;
          Nchars : in Guint)
          return      Gint;
      pragma Import (C, Internal, "gtk_text_backward_delete");
   begin
      return Internal (Get_Object (Text),
                       Nchars);
   end Backward_Delete;

   --------------------
   -- Forward_Delete --
   --------------------

   function Forward_Delete
      (Text   : in Gtk_Text'Class;
       Nchars : in Guint)
       return      Gint
   is
      function Internal
         (Text   : in System.Address;
          Nchars : in Guint)
          return      Gint;
      pragma Import (C, Internal, "gtk_text_forward_delete");
   begin
      return Internal (Get_Object (Text),
                       Nchars);
   end Forward_Delete;

   ------------
   -- Freeze --
   ------------

   procedure Freeze (Text : in Gtk_Text'Class)
   is
      procedure Internal (Text : in System.Address);
      pragma Import (C, Internal, "gtk_text_freeze");
   begin
      Internal (Get_Object (Text));
   end Freeze;

   ----------------------
   -- Get_Gap_Position --
   ----------------------

   function Get_Gap_Position (Widget : in Gtk_Text'Class)
                              return      Guint
   is
      function Internal (Widget : in System.Address)
                         return      Guint;
      pragma Import (C, Internal, "ada_text_get_gap_position");
   begin
      return Internal (Get_Object (Widget));
   end Get_Gap_Position;

   ------------------
   -- Get_Gap_Size --
   ------------------

   function Get_Gap_Size (Widget : in Gtk_Text'Class)
                          return      Guint
   is
      function Internal (Widget : in System.Address)
                         return      Guint;
      pragma Import (C, Internal, "ada_text_get_gap_size");
   begin
      return Internal (Get_Object (Widget));
   end Get_Gap_Size;

   ----------------
   -- Get_Length --
   ----------------

   function Get_Length (Text   : in Gtk_Text'Class)
                        return      Guint
   is
      function Internal (Text   : in System.Address)
                         return      Guint;
      pragma Import (C, Internal, "gtk_text_get_length");
   begin
      return Internal (Get_Object (Text));
   end Get_Length;

   ---------------
   -- Get_Point --
   ---------------

   function Get_Point (Text   : in Gtk_Text'Class)
                       return      Guint
   is
      function Internal (Text   : in System.Address)
                         return      Guint;
      pragma Import (C, Internal, "gtk_text_get_point");
   begin
      return Internal (Get_Object (Text));
   end Get_Point;

   --------------
   -- Get_Text --
   --------------

   function Get_Text (Widget : in Gtk_Text'Class)
                      return      String
   is
      function Internal (Widget : in System.Address)
                         return      Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "ada_text_get_text");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Widget)));
   end Get_Text;

   ------------------
   -- Get_Text_End --
   ------------------

   function Get_Text_End (Widget : in Gtk_Text'Class)
                          return      Guint
   is
      function Internal (Widget : in System.Address)
                         return      Guint;
      pragma Import (C, Internal, "ada_text_get_text_end");
   begin
      return Internal (Get_Object (Widget));
   end Get_Text_End;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Widget : out Gtk_Text;
       Hadj   : in Gtk.Adjustment.Gtk_Adjustment'Class;
       Vadj   : in Gtk.Adjustment.Gtk_Adjustment'Class)
   is
      function Internal
         (Hadj   : in System.Address;
          Vadj   : in System.Address)
          return      System.Address;
      pragma Import (C, Internal, "gtk_text_new");
   begin
      Set_Object (Widget, Internal (Get_Object (Hadj),
                                    Get_Object (Vadj)));
   end Gtk_New;

   ------------
   -- Insert --
   ------------

   procedure Insert
      (Text   : in Gtk_Text'Class;
       Font   : in Gdk.Font.Gdk_Font'Class;
       Fore   : in Gdk.Color.Gdk_Color'Class;
       Back   : in Gdk.Color.Gdk_Color'Class;
       Chars  : in String;
       Length : in Gint)
   is
      procedure Internal
         (Text   : in System.Address;
          Font   : in System.Address;
          Fore   : in System.Address;
          Back   : in System.Address;
          Chars  : in String;
          Length : in Gint);
      pragma Import (C, Internal, "gtk_text_insert");
   begin
      Internal (Get_Object (Text),
                Get_Object (Font),
                Get_Object (Fore),
                Get_Object (Back),
                Chars & Ascii.NUL,
                Length);
   end Insert;

   ---------------------
   -- Set_Adjustments --
   ---------------------

   procedure Set_Adjustments
      (Text : in Gtk_Text'Class;
       Hadj : in Gtk.Adjustment.Gtk_Adjustment'Class;
       Vadj : in Gtk.Adjustment.Gtk_Adjustment'Class)
   is
      procedure Internal
         (Text : in System.Address;
          Hadj : in System.Address;
          Vadj : in System.Address);
      pragma Import (C, Internal, "gtk_text_set_adjustments");
   begin
      Internal (Get_Object (Text),
                Get_Object (Hadj),
                Get_Object (Vadj));
   end Set_Adjustments;

   ------------------
   -- Set_Editable --
   ------------------

   procedure Set_Editable
      (Text     : in Gtk_Text'Class;
       Editable : in Gint)
   is
      procedure Internal
         (Text     : in System.Address;
          Editable : in Gint);
      pragma Import (C, Internal, "gtk_text_set_editable");
   begin
      Internal (Get_Object (Text),
                Editable);
   end Set_Editable;

   ---------------
   -- Set_Point --
   ---------------

   procedure Set_Point
      (Text  : in Gtk_Text'Class;
       Index : in Guint)
   is
      procedure Internal
         (Text  : in System.Address;
          Index : in Guint);
      pragma Import (C, Internal, "gtk_text_set_point");
   begin
      Internal (Get_Object (Text),
                Index);
   end Set_Point;

   -------------------
   -- Set_Word_Wrap --
   -------------------

   procedure Set_Word_Wrap
      (Text      : in Gtk_Text'Class;
       Word_Wrap : in Gint)
   is
      procedure Internal
         (Text      : in System.Address;
          Word_Wrap : in Gint);
      pragma Import (C, Internal, "gtk_text_set_word_wrap");
   begin
      Internal (Get_Object (Text),
                Word_Wrap);
   end Set_Word_Wrap;

   ----------
   -- Thaw --
   ----------

   procedure Thaw (Text : in Gtk_Text'Class)
   is
      procedure Internal (Text : in System.Address);
      pragma Import (C, Internal, "gtk_text_thaw");
   begin
      Internal (Get_Object (Text));
   end Thaw;

end Gtk.Text;
